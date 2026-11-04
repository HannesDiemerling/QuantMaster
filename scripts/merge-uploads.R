# scripts/merge-uploads.R
#
# Kombiniert alle Studierenden-Uploads zu einem Gesamtdatensatz.
#   Ausgabe: data/combined/m15-gesamt.csv
#            data/combined/merge-report.md   (was wurde wie verarbeitet)
#            data/combined/meta-studien.csv  (ein Effekt je Seed-Datensatz, Sitzung 04)
#
# Fail-safe-Prinzip: Studierende machen beim Upload Fehler (falscher Name,
# falscher Ordner, Semikolon statt Komma, Zusatzspalten, ...). Das Skript
# verlässt sich deshalb NICHT auf Dateinamen, sondern auf den Inhalt:
#   - Gelesen werden alle .csv unter data/ (außer generated/ und combined/)
#     sowie im Repo-Root.
#   - Zeilen, die zu >= 19 von 21 BFI-Items mit einer vorgenerierten Zeile
#     übereinstimmen, sind "Seed-Zeilen". Jeder hochgeladene Seed-Datensatz
#     geht genau einmal ein — in der Referenz-Bereinigung (Fehlerzellen leer,
#     doppelte Probandennummer korrigiert).
#   - Alle anderen Zeilen sind eigene Zeilen der Studierenden. Werte außerhalb
#     der Codebook-Bereiche werden auf fehlend gesetzt, exakte Duplikate entfernt.
#
# Nur base R + MASS (wird mit R ausgeliefert). Aufruf aus dem Repo-Root:
#   Rscript scripts/merge-uploads.R

GENERATOR  <- "scripts/generate-datasets.R"
OUT_CSV    <- "data/combined/m15-gesamt.csv"
OUT_REPORT <- "data/combined/merge-report.md"
OUT_META   <- "data/combined/meta-studien.csv"
UPLOAD_DIR <- "data/student-uploads"
MIN_MATCH  <- 19          # so viele BFI-Items müssen übereinstimmen
# gültige Kennziffer: ganze Zahl mit 4 bis 12 Stellen, z. B. die Matrikelnummer
is_kz <- function(x) !is.na(x) & x == round(x) & x >= 1000 & x < 1e12
ALLE_SEEDS <- TRUE        # alle 10 Seed-Datensätze aufnehmen, auch wenn einer nicht hochgeladen wurde
                          # (hält N, Metaanalyse und Mixed Model bei kleinen Kursen stabil)

# Referenz = unveränderliche Kopie der Übungsdatensätze in scripts/referenz/.
# Studierende haben Schreibrechte auf das ganze Repo; lädt jemand versehentlich
# etwas nach data/generated/, bleibt der Merge trotzdem korrekt und meldet es.
# Nach einer gewollten Neuerzeugung der Datensätze die Kopien aktualisieren.
REF_DIR    <- "scripts/referenz"
SEED_NAMES <- sprintf("datensatz-%02d.csv", 1:10)

seed_hash <- function(path) {
  x <- read.csv(path, colClasses = "character")
  f <- tempfile()
  writeBin(charToRaw(enc2utf8(paste(c(paste(names(x), collapse = ","),
                                      apply(x, 1, paste, collapse = ",")), collapse = "\n"))), f)
  unname(tools::md5sum(f))
}

DEMO  <- c("probandennummer", "geschlecht", "alter", "bildungsgrad")
BFI   <- sprintf("BFI_%02d", 1:21)
COLS  <- c(DEMO, BFI)
RANGE <- c(list(geschlecht = c(1, 3), alter = c(18, 99), bildungsgrad = c(1, 7)),
           setNames(rep(list(c(1, 5)), 21), BFI))

# --- Referenz aus dem Generator ----------------------------------------------
# Nur die Zuweisungen (Konstanten + Funktionen) aus generate-datasets.R
# auswerten — so laufen weder library(readr) noch main().

load_generator <- function(path) {
  env <- new.env()
  for (e in parse(path)) {
    if (is.call(e) && identical(e[[1]], as.name("<-"))) eval(e, env)
  }
  env
}

# Referenz = die Dateien in data/generated/ (die Zufallsdaten selbst lassen
# sich plattformabhängig nicht bitgenau neu erzeugen). Die Fehlerpositionen
# hängen dagegen nur vom festen Seed in apply_errors() ab und werden von dort
# übernommen.
build_reference <- function() {
  g <- load_generator(GENERATOR)
  lapply(seq_len(g$N_DATASETS), function(i) {
    raw <- read.csv(file.path(REF_DIR, SEED_NAMES[i]))[COLS]
    errors <- g$apply_errors(raw, i)$errors
    clean <- raw
    for (err in errors) {
      if (err$spalte == "probandennummer") {
        clean$probandennummer[err$zeile] <- err$zeile
      } else {
        clean[err$zeile, err$spalte] <- NA
      }
    }
    list(nr = i, raw = raw, clean = clean, errors = errors)
  })
}

# --- Robustes Einlesen ---------------------------------------------------------

canonical_names <- function(x) {
  x <- tolower(trimws(gsub('"', "", x)))
  x <- sub("^﻿", "", x)
  bfi <- grepl("^bfi_?0*[0-9]+$", x)
  x[bfi] <- sprintf("BFI_%02d", as.integer(sub("^bfi_?0*", "", x[bfi])))
  x
}

# Liest eine Textdatei unabhängig von Kodierung und Zeilenenden (UTF-8 mit/ohne
# BOM, UTF-16 aus Excel "Unicode-Text", Latin-1, CRLF/LF/CR). Binärdateien
# (z. B. umbenannte .xlsx) werden erkannt statt das Skript abzubrechen.
read_lines_any <- function(path) {
  raw <- readBin(path, "raw", n = file.info(path)$size)
  if (length(raw) == 0) return(character(0))
  b <- function(...) as.raw(c(...))
  if (length(raw) >= 2 && identical(raw[1:2], b(0xff, 0xfe))) {
    txt <- iconv(list(raw[-(1:2)]), from = "UTF-16LE", to = "UTF-8")
  } else if (length(raw) >= 2 && identical(raw[1:2], b(0xfe, 0xff))) {
    txt <- iconv(list(raw[-(1:2)]), from = "UTF-16BE", to = "UTF-8")
  } else {
    if (any(raw == as.raw(0))) stop("keine Textdatei (z. B. eine umbenannte Excel- oder JASP-Datei)")
    if (length(raw) >= 3 && identical(raw[1:3], b(0xef, 0xbb, 0xbf))) raw <- raw[-(1:3)]
    txt <- rawToChar(raw)
    Encoding(txt) <- "UTF-8"
    if (!validUTF8(txt)) txt <- iconv(txt, from = "latin1", to = "UTF-8")
  }
  if (is.na(txt)) stop("Zeichenkodierung nicht lesbar")
  strsplit(txt, "\r\n|\n|\r")[[1]]
}

read_upload <- function(path) {
  txt <- read_lines_any(path)
  if (length(txt) > 0 && grepl("^\\s*\"?sep=", txt[1], ignore.case = TRUE)) txt <- txt[-1]   # Excel-Zeile "sep=;"
  txt <- txt[nzchar(trimws(gsub("[,;\t\"]", "", txt)))]
  if (length(txt) < 2) return(list(error = "Datei ist leer oder hat nur eine Kopfzeile"))
  counts <- c("," = lengths(regmatches(txt[1], gregexpr(",", txt[1]))),
              ";" = lengths(regmatches(txt[1], gregexpr(";", txt[1]))),
              "\t" = lengths(regmatches(txt[1], gregexpr("\t", txt[1]))))
  sep <- names(which.max(counts))
  df <- tryCatch(
    read.table(text = txt, sep = sep, header = TRUE, quote = "\"",
               colClasses = "character", na.strings = c("", "NA", "na", ".", "NaN", "nan", "-", "NULL", "n/a", "N/A", "#NV", "#N/A"),
               strip.white = TRUE, check.names = FALSE, fill = TRUE,
               comment.char = ""),
    error = function(e) NULL)
  if (is.null(df)) return(list(error = "CSV konnte nicht gelesen werden"))
  names(df) <- canonical_names(names(df))
  missing <- setdiff(COLS, names(df))
  if (length(missing) > 0) {
    return(list(error = sprintf("Spalten fehlen: %s",
                                paste(head(missing, 5), collapse = ", "))))
  }
  notes <- character(0)
  if (sep != ",") notes <- c(notes, sprintf("Trennzeichen '%s' statt Komma", ifelse(sep == "\t", "Tab", sep)))
  extra <- setdiff(names(df), COLS)
  if (length(extra) > 0) notes <- c(notes, sprintf("Zusatzspalten ignoriert: %s", paste(extra, collapse = ", ")))
  # Labels statt Codes (z. B. JASP-Export mit Wertelabels) für geschlecht zurückübersetzen
  g <- tolower(trimws(df$geschlecht))
  df$geschlecht[g %in% c("weiblich", "w", "female", "f", "frau")] <- "1"
  df$geschlecht[g %in% c("männlich", "maennlich", "m", "male", "mann")] <- "2"
  df$geschlecht[g %in% c("divers", "d", "diverse", "other")] <- "3"
  out <- as.data.frame(lapply(df[COLS], function(v) {
    v <- gsub(",", ".", trimws(v))
    x <- suppressWarnings(as.numeric(v))
    x[is.nan(x)] <- NA
    x
  }), check.names = FALSE)
  bad <- sum(is.na(out) & !is.na(as.matrix(df[COLS])))
  if (bad > 0) notes <- c(notes, sprintf("%d nicht-numerische Werte -> fehlend", bad))
  list(df = out, notes = notes)
}

# --- Zeilen den Seed-Datensätzen zuordnen ---------------------------------------

match_seed <- function(row, ref) {
  best <- c(ds = NA, zeile = NA, n = -1)
  for (r in ref) {
    m <- as.matrix(r$raw[BFI])
    same <- rowSums((m == matrix(row, nrow(m), 21, byrow = TRUE)) |
                      (is.na(m) & matrix(is.na(row), nrow(m), 21, byrow = TRUE)),
                    na.rm = TRUE)
    k <- which.max(same)
    if (same[k] > best["n"]) best <- c(ds = r$nr, zeile = k, n = same[k])
  }
  if (best["n"] >= MIN_MATCH) best[c("ds", "zeile")] else c(ds = NA, zeile = NA)
}

clean_range <- function(df) {
  n_bad <- 0
  for (v in names(RANGE)) {
    x <- df[[v]]
    bad <- !is.na(x) & (x < RANGE[[v]][1] | x > RANGE[[v]][2] | x != round(x))
    n_bad <- n_bad + sum(bad)
    df[[v]][bad] <- NA
  }
  attr(df, "n_bad") <- n_bad
  df
}

# --- Hauptteil ---------------------------------------------------------------

main <- function() {
  ref_files <- file.path(REF_DIR, SEED_NAMES)
  if (!all(file.exists(ref_files))) stop("Referenzkopien in scripts/referenz/ fehlen (mit git wiederherstellen)")
  seed_files <- file.path("data/generated", SEED_NAMES)
  same <- function(a, b) isTRUE(tryCatch(seed_hash(a) == seed_hash(b), error = function(e) FALSE))
  seed_changed <- seed_files[!vapply(seq_along(seed_files), function(i)
    file.exists(seed_files[i]) && same(seed_files[i], ref_files[i]), TRUE)]
  ref <- build_reference()

  files <- c(list.files("data", pattern = "\\.csv$", recursive = TRUE,
                        full.names = TRUE, ignore.case = TRUE),
             list.files(".", pattern = "\\.csv$", full.names = TRUE, ignore.case = TRUE))
  files <- sub("^\\./", "", files)
  gen_ok <- setdiff(file.path("data/generated", SEED_NAMES), seed_changed)
  files <- sort(unique(files[!grepl("^data/combined/", files) & !(files %in% gen_ok)]))
  files <- sort(unique(c(files, list.files(UPLOAD_DIR, pattern = "\\.(txt|tsv)$", recursive = TRUE,
                                           full.names = TRUE, ignore.case = TRUE))))
  other <- list.files(UPLOAD_DIR, recursive = TRUE, full.names = TRUE)
  other <- other[!grepl("\\.(csv|txt|tsv|gitkeep)$", other, ignore.case = TRUE) & basename(other) != "README.md"]

  report <- list()
  seeds_used <- list()       # ds -> Dateien
  own <- list()

  for (f in files) {
    info <- list(datei = f, status = "ok", ds = NA, eigene = 0, hinweise = character(0),
                 gefunden = NA, abweichungen = NA, kz = NULL)
    if (!startsWith(f, paste0(UPLOAD_DIR, "/")))
      info$hinweise <- c(info$hinweise, "liegt NICHT in data/student-uploads/")
    if (!grepl("^datensatz-[0-9]{2}-[0-9]{4,12}\\.csv$", basename(f)))
      info$hinweise <- c(info$hinweise, "Dateiname weicht vom Schema datensatz-XX-<Kennziffer>.csv ab (für den Merge unkritisch)")

    up <- tryCatch(read_upload(f),
                   error = function(e) list(error = paste("Datei nicht lesbar:", conditionMessage(e))))
    if (!is.null(up$error) && grepl("^Spalten fehlen", up$error))
      up$error <- paste0(up$error, " (Kopfzeile fehlt oder Trennzeichen gemischt? Datei im Texteditor prüfen)")
    if (!is.null(up$error)) {
      info$status <- "IGNORIERT"
      info$hinweise <- c(info$hinweise, up$error)
      report[[f]] <- info
      next
    }
    info$hinweise <- c(info$hinweise, up$notes)
    df <- up$df

    m <- t(apply(as.matrix(df[BFI]), 1, match_seed, ref = ref))
    is_seed <- !is.na(m[, "ds"])
    ds_in_file <- unique(m[is_seed, "ds"])
    if (length(ds_in_file) > 0) {
      info$ds <- paste(sprintf("%02d", sort(ds_in_file)), collapse = ", ")
      for (d in ds_in_file) seeds_used[[as.character(d)]] <- c(seeds_used[[as.character(d)]], f)
    }
    if (length(ds_in_file) == 1) {
      # Feedback zur Bereinigung: Abweichungen von der Referenz und gefundene Fehler
      r <- ref[[ds_in_file]]
      rows <- df[is_seed, COLS]
      cl <- r$clean[m[is_seed, "zeile"], COLS]
      diff <- (is.na(rows) != is.na(cl)) | (!is.na(rows) & !is.na(cl) & rows != cl)
      info$abweichungen <- sum(diff)
      found <- 0
      for (err in r$errors) {
        hit <- which(m[is_seed, "zeile"] == err$zeile)
        if (err$spalte == "probandennummer") {
          ok <- !anyDuplicated(rows$probandennummer)
        } else {
          # Zeile ganz entfernt zählt ebenfalls als behandelt
          ok <- length(hit) == 0 || all(is.na(rows[hit, err$spalte]))
        }
        found <- found + ok
      }
      info$gefunden <- sprintf("%d/%d", found, length(r$errors))
      if (sum(is_seed) < 9) info$hinweise <- c(info$hinweise, sprintf("nur %d von 9 Seed-Zeilen", sum(is_seed)))
    } else if (length(ds_in_file) > 1) {
      info$hinweise <- c(info$hinweise, "enthält mehrere Seed-Datensätze (kombinierte Datei?)")
    }

    mine <- df[!is_seed, COLS, drop = FALSE]
    leer <- rowSums(!is.na(mine)) <= 1
    if (any(leer)) {
      mine <- mine[!leer, , drop = FALSE]
      info$status <- "PRÜFEN"
      info$hinweise <- c(info$hinweise, sprintf("%d fast leere Zeile(n) verworfen (Trennzeichen gemischt? Datei im Texteditor prüfen)", sum(leer)))
    }
    pruefen <- function(txt) { info$status <<- "PRÜFEN"; info$hinweise <<- c(info$hinweise, txt) }
    if (length(ds_in_file) > 1) {
      # kombinierte Datei (z. B. aus Sitzung 02 oder ein alter Gesamtdatensatz): nichts übernehmen
      if (nrow(mine) > 0) pruefen(sprintf("kombinierte Datei: %d fiktive Person(en) NICHT übernommen", nrow(mine)))
      mine <- mine[0, , drop = FALSE]
    } else if (nrow(mine) > 1) {
      pruefen(sprintf("%d fiktive Personen in einer Datei, keine übernommen (bitte genau eine)", nrow(mine)))
      info$kz <- head(mine$probandennummer, 3)
      mine <- mine[0, , drop = FALSE]
    } else if (nrow(mine) == 0) {
      pruefen("keine Zeile einer fiktiven Person gefunden")
    }
    if (length(ds_in_file) == 0 && nrow(mine) == 1)
      pruefen("keine Übungsdaten erkannt (nur die fiktive Person? Bereinigte Übungsdaten fehlen)")
    if (nrow(mine) == 1) {
      info$kz <- mine$probandennummer
      if (!is_kz(mine$probandennummer))
        pruefen(sprintf("Kennziffer %s ungültig (ganze Zahl mit mindestens 4 Stellen, z. B. Deine Matrikelnummer)",
                        ifelse(is.na(mine$probandennummer), "fehlt oder ist keine Zahl,", format(mine$probandennummer, scientific = FALSE))))
      mine <- clean_range(mine)
      if (attr(mine, "n_bad") > 0)
        pruefen(sprintf("%d Wert(e) der fiktiven Person außerhalb des erlaubten Bereichs -> fehlend", attr(mine, "n_bad")))
      if (sum(is.na(mine[COLS])) > 0 && attr(mine, "n_bad") == 0)
        pruefen(sprintf("%d Wert(e) der fiktiven Person fehlen oder sind keine Zahl", sum(is.na(mine[COLS]))))
      mine$quelle_datensatz <- if (length(ds_in_file) == 1) ds_in_file else NA
      mine$quelle_datei <- f
      own[[f]] <- mine
    }
    info$eigene <- nrow(mine)
    report[[f]] <- info
  }

  # Seed-Zeilen: jeder hochgeladene Datensatz genau einmal, referenzbereinigt
  seed_ids <- if (ALLE_SEEDS) seq_along(ref) else sort(as.integer(names(seeds_used)))
  seed_df <- do.call(rbind, lapply(seed_ids, function(d) {
    x <- ref[[d]]$clean[COLS]
    data.frame(id = d * 100 + x$probandennummer, x, quelle = "generiert",
               quelle_datensatz = d, quelle_datei = sprintf("data/generated/datensatz-%02d.csv", d),
               check.names = FALSE)
  }))

  # Eigene Zeilen: exakte Duplikate (z. B. doppelt hochgeladen) entfernen, IDs vergeben
  own_df <- if (length(own)) do.call(rbind, unname(own)) else NULL
  dup_notes <- character(0)
  if (!is.null(own_df) && nrow(own_df) > 0) {
    key <- apply(own_df[COLS], 1, paste, collapse = "|")
    dup <- duplicated(key)
    for (f in unique(own_df$quelle_datei[dup])) {
      dup_notes <- c(dup_notes, sprintf("`%s`: %d eigene Zeile(n) schon in anderer Datei enthalten -> nur einmal übernommen",
                                        f, sum(dup & own_df$quelle_datei == f)))
    }
    own_df <- own_df[!dup, ]
    pn <- own_df$probandennummer
    valid <- is_kz(pn) & !(pn %in% pn[duplicated(pn)]) & !(pn %in% seed_df$id)
    own_df$id <- ifelse(valid, pn, NA)
    fallback <- which(!valid)
    own_df$id[fallback] <- max(c(seed_df$id, own_df$id), na.rm = TRUE) + seq_along(fallback)
    for (f in unique(own_df$quelle_datei[fallback])) {
      k <- fallback[own_df$quelle_datei[fallback] == f]
      dup_notes <- c(dup_notes, sprintf("`%s`: Probandennummer %s keine gültige/eindeutige Kennziffer -> id %s",
                                        f, paste(own_df$probandennummer[k], collapse = ", "),
                                        paste(own_df$id[k], collapse = ", ")))
    }
    own_df$quelle <- "studierende"
    own_df <- own_df[, names(seed_df)]
  }

  combined <- rbind(seed_df, own_df)
  if (is.null(combined)) {
    # Noch keine Uploads: leere Tabellen mit Kopfzeile schreiben statt abzustürzen
    combined <- data.frame(id = numeric(0), as.data.frame(setNames(rep(list(numeric(0)), length(COLS)), COLS)),
                           quelle = character(0), quelle_datensatz = numeric(0), quelle_datei = character(0),
                           check.names = FALSE)
  }
  dir.create(dirname(OUT_CSV), showWarnings = FALSE, recursive = TRUE)
  write.csv(combined, OUT_CSV, row.names = FALSE, na = "", fileEncoding = "UTF-8")

  # Für die Metaanalyse (Sitzung 04): jeder Seed-Datensatz samt zugeordneter
  # eigener Zeilen ist eine "Studie"; Effekt = Korrelation E_mean × O_mean.
  sc <- function(i) rowSums(as.matrix(combined[sprintf("BFI_%02d", i)])) / length(i)
  e <- sc(c(1, 6, 11, 16)); o <- sc(c(5, 10, 15, 20, 21))
  meta <- do.call(rbind, lapply(sort(unique(na.omit(combined$quelle_datensatz))), function(s) {
    ok <- which(combined$quelle_datensatz == s & !is.na(e) & !is.na(o))
    if (length(ok) < 5) return(NULL)
    r <- cor(e[ok], o[ok])
    data.frame(studie = sprintf("Datensatz %02d", s), quelle_datensatz = s, n = length(ok),
               r = round(r, 3), fisher_z = round(atanh(r), 3), se = round(1 / sqrt(length(ok) - 3), 3))
  }))
  if (is.null(meta)) meta <- data.frame(studie = character(0), quelle_datensatz = numeric(0), n = numeric(0),
                                        r = numeric(0), fisher_z = numeric(0), se = numeric(0))
  write.csv(meta, OUT_META, row.names = FALSE, fileEncoding = "UTF-8")

  # --- Report ------------------------------------------------------------------
  cell <- function(x) if (length(x) == 0 || all(is.na(x))) "–" else paste(x, collapse = "; ")
  lines <- c(
    "# Merge-Report",
    "",
    sprintf("Automatisch erzeugt von `scripts/merge-uploads.R` am %s.", format(Sys.time(), "%d.%m.%Y %H:%M", tz = "Europe/Berlin")),
    "",
    sprintf("**Gesamtdatensatz:** `%s` mit **%d Zeilen** (%d vorgeneriert aus %d Datensätzen, %d fiktive Personen).",
            OUT_CSV, nrow(combined), NROW(seed_df), length(seed_ids), NROW(own_df)),
    "",
    if (length(seed_changed)) paste0("> **ACHTUNG (Lehrperson):** Übungsdatensatz verändert: ",
                                     paste0("`", seed_changed, "`", collapse = ", "),
                                     ". Der Merge nutzt trotzdem die richtigen Daten (Kopie in scripts/referenz/). Datei mit `git log -- data/generated` prüfen und zurücksetzen, damit niemand die falsche Datei herunterlädt.\n") else NULL,
    "Vorgenerierte Zeilen gehen in der Referenz-Bereinigung ein (Fehlerzellen leer, doppelte Probandennummer korrigiert), unabhängig davon, wie sie in den Uploads bereinigt wurden.",
    "",
    "## Dateien",
    "",
    "Suche Deine Datei über Deine **Kennziffer**. Handeln musst Du nur bei Status IGNORIERT, PRÜFEN oder DOPPELT oder wenn bei „Fehler gefunden“ nicht 2/2 steht. Hinweise zu Dateiname oder Trennzeichen sind unkritisch.",
    "",
    "| Datei | Kennziffer | Status | Datensatz | fiktive Personen | Fehler gefunden | Abweichungen von Referenz | Hinweise |",
    "|---|---|---|---|---:|---|---:|---|"
  )
  # Dieselbe Kennziffer in mehreren Dateien: alle betroffenen Dateien markieren
  kz_tab <- do.call(rbind, lapply(report, function(i) {
    k <- unique(i$kz[!is.na(i$kz)])
    if (length(k) && !grepl(",", cell(i$ds))) data.frame(datei = i$datei, kz = k) else NULL
  }))
  for (k in names(report)) {
    i <- report[[k]]
    if (is.null(kz_tab) || !length(i$kz)) next
    fremd <- kz_tab[kz_tab$datei != i$datei & kz_tab$kz %in% i$kz, , drop = FALSE]
    d <- unique(fremd$kz)
    if (length(d)) {
      andere <- unique(fremd$datei)
      report[[k]]$status <- "DOPPELT"
      report[[k]]$hinweise <- c(i$hinweise, sprintf("Kennziffer %s auch in %s: sind beide Dateien von Dir, die ältere löschen; sonst Lehrperson fragen",
                                                     paste(d, collapse = ", "), paste0("`", unique(andere), "`", collapse = ", ")))
    }
  }
  for (i in report) {
    lines <- c(lines, sprintf("| `%s` | %s | %s | %s | %d | %s | %s | %s |", i$datei, cell(i$kz), i$status, cell(i$ds),
                              i$eigene, cell(i$gefunden), cell(i$abweichungen), cell(i$hinweise)))
  }
  missing_ds <- setdiff(1:10, as.integer(names(seeds_used)))
  lines <- c(lines, "", "## Weitere Hinweise", "",
             if (length(missing_ds)) sprintf("- Nicht hochgeladene Seed-Datensätze: %s%s", paste(sprintf("%02d", missing_ds), collapse = ", "),
                                             if (ALLE_SEEDS) " (trotzdem im Gesamtdatensatz enthalten)" else ""),
             if (length(other)) sprintf("- Ignoriert (kein CSV): `%s`", other),
             if (length(dup_notes)) paste0("- ", dup_notes))
  con <- file(OUT_REPORT, open = "w", encoding = "UTF-8")
  writeLines(lines, con)
  close(con)
  message(sprintf("Wrote %s (%d rows) and %s", OUT_CSV, nrow(combined), OUT_REPORT))
}

if (sys.nframe() == 0) {
  # Bricht der Merge ab, steht der Grund im Report (statt eines veralteten Reports)
  tryCatch(main(), error = function(e) {
    msg <- conditionMessage(e)
    con <- file(OUT_REPORT, open = "w", encoding = "UTF-8")
    writeLines(c("# Merge-Report", "",
                 sprintf("Automatisch erzeugt am %s.", format(Sys.time(), "%d.%m.%Y %H:%M", tz = "Europe/Berlin")), "",
                 "**Der Merge ist abgebrochen.** Der Gesamtdatensatz wurde nicht aktualisiert. Bitte der Lehrperson Bescheid geben.",
                 "", paste("Fehler:", msg)), con)
    close(con)
    cat(sprintf("::error::Merge abgebrochen: %s\n", msg))
  })
}
