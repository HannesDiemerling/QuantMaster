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
KENNZIFFER <- 2001:2099   # gültige Kennziffern (2000 + Listennummer)

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
    raw <- read.csv(sprintf("data/generated/datensatz-%02d.csv", i))[COLS]
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

read_upload <- function(path) {
  txt <- readLines(path, warn = FALSE, encoding = "UTF-8")
  txt <- sub("^﻿", "", txt)
  txt <- txt[nzchar(trimws(gsub("[,;\t]", "", txt)))]
  if (length(txt) < 2) return(list(error = "Datei ist leer oder hat nur eine Kopfzeile"))
  counts <- c("," = lengths(regmatches(txt[1], gregexpr(",", txt[1]))),
              ";" = lengths(regmatches(txt[1], gregexpr(";", txt[1]))),
              "\t" = lengths(regmatches(txt[1], gregexpr("\t", txt[1]))))
  sep <- names(which.max(counts))
  df <- tryCatch(
    read.table(text = txt, sep = sep, header = TRUE, quote = "\"",
               colClasses = "character", na.strings = c("", "NA", "na", "."),
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
  out <- as.data.frame(lapply(df[COLS], function(v) {
    v <- gsub(",", ".", trimws(v))
    suppressWarnings(as.numeric(v))
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
  ref <- build_reference()

  files <- c(list.files("data", pattern = "\\.csv$", recursive = TRUE,
                        full.names = TRUE, ignore.case = TRUE),
             list.files(".", pattern = "\\.csv$", full.names = TRUE, ignore.case = TRUE))
  files <- sub("^\\./", "", files)
  files <- sort(unique(files[!grepl("^data/(generated|combined)/", files)]))
  other <- list.files(UPLOAD_DIR, recursive = TRUE, full.names = TRUE)
  other <- other[!grepl("\\.(csv|gitkeep)$", other, ignore.case = TRUE)]

  report <- list()
  seeds_used <- list()       # ds -> Dateien
  own <- list()

  for (f in files) {
    info <- list(datei = f, status = "ok", ds = NA, eigene = 0, hinweise = character(0),
                 gefunden = NA, abweichungen = NA)
    if (!startsWith(f, paste0(UPLOAD_DIR, "/")))
      info$hinweise <- c(info$hinweise, "liegt NICHT in data/student-uploads/")
    if (!grepl("^datensatz-[0-9]{2}-[a-z]{2,3}\\.csv$", basename(f)))
      info$hinweise <- c(info$hinweise, "Dateiname weicht vom Schema datensatz-XX-kuerzel.csv ab")

    up <- read_upload(f)
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
          ok <- length(hit) > 0 && all(is.na(rows[hit, err$spalte]))
        }
        found <- found + ok
      }
      info$gefunden <- sprintf("%d/%d", found, length(r$errors))
      if (sum(is_seed) < 9) info$hinweise <- c(info$hinweise, sprintf("nur %d von 9 Seed-Zeilen", sum(is_seed)))
    } else if (length(ds_in_file) > 1) {
      info$hinweise <- c(info$hinweise, "enthält mehrere Seed-Datensätze (kombinierte Datei?)")
    }

    mine <- df[!is_seed, COLS, drop = FALSE]
    if (nrow(mine) > 0) {
      mine <- clean_range(mine)
      if (attr(mine, "n_bad") > 0)
        info$hinweise <- c(info$hinweise, sprintf("%d Werte außerhalb des Bereichs -> fehlend", attr(mine, "n_bad")))
      mine$quelle_datensatz <- if (length(ds_in_file) == 1) ds_in_file else NA
      mine$quelle_datei <- f
      own[[f]] <- mine
    }
    info$eigene <- nrow(mine)
    if (nrow(mine) == 0 && length(ds_in_file) <= 1) info$hinweise <- c(info$hinweise, "keine eigene Zeile gefunden")
    if (nrow(mine) > 1 && length(ds_in_file) <= 1) info$hinweise <- c(info$hinweise, "mehr als eine eigene Zeile")
    report[[f]] <- info
  }

  # Seed-Zeilen: jeder hochgeladene Datensatz genau einmal, referenzbereinigt
  seed_df <- do.call(rbind, lapply(sort(as.integer(names(seeds_used))), function(d) {
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
    valid <- !is.na(pn) & pn %in% KENNZIFFER & !(pn %in% pn[duplicated(pn)])
    own_df$id <- ifelse(valid, pn, NA)
    fallback <- which(!valid)
    own_df$id[fallback] <- 3000 + seq_along(fallback)
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
  dir.create(dirname(OUT_CSV), showWarnings = FALSE, recursive = TRUE)
  write.csv(combined, OUT_CSV, row.names = FALSE, na = "", fileEncoding = "UTF-8")

  # Für die Metaanalyse (Sitzung 04): jeder Seed-Datensatz samt zugeordneter
  # eigener Zeilen ist eine "Studie"; Effekt = Korrelation E_mean × O_mean.
  sc <- function(i) rowSums(combined[sprintf("BFI_%02d", i)]) / length(i)
  e <- sc(c(1, 6, 11, 16)); o <- sc(c(5, 10, 15, 20, 21))
  meta <- do.call(rbind, lapply(sort(unique(na.omit(combined$quelle_datensatz))), function(s) {
    ok <- which(combined$quelle_datensatz == s & !is.na(e) & !is.na(o))
    if (length(ok) < 5) return(NULL)
    r <- cor(e[ok], o[ok])
    data.frame(studie = sprintf("Datensatz %02d", s), quelle_datensatz = s, n = length(ok),
               r = round(r, 3), fisher_z = round(atanh(r), 3), se = round(1 / sqrt(length(ok) - 3), 3))
  }))
  write.csv(meta, OUT_META, row.names = FALSE, fileEncoding = "UTF-8")

  # --- Report ------------------------------------------------------------------
  cell <- function(x) if (length(x) == 0 || all(is.na(x))) "–" else paste(x, collapse = "; ")
  lines <- c(
    "# Merge-Report",
    "",
    sprintf("Automatisch erzeugt von `scripts/merge-uploads.R` am %s.", format(Sys.time(), "%d.%m.%Y %H:%M")),
    "",
    sprintf("**Gesamtdatensatz:** `%s` mit **%d Zeilen** (%d vorgeneriert aus %d Datensätzen, %d eigene Zeilen).",
            OUT_CSV, nrow(combined), NROW(seed_df), length(seeds_used), NROW(own_df)),
    "",
    "Vorgenerierte Zeilen gehen in der Referenz-Bereinigung ein (Fehlerzellen leer, doppelte Probandennummer korrigiert) — unabhängig davon, wie sie in den Uploads bereinigt wurden.",
    "",
    "## Dateien",
    "",
    "| Datei | Status | Datensatz | eigene Zeilen | Fehler gefunden | Abweichungen von Referenz | Hinweise |",
    "|---|---|---|---:|---|---:|---|"
  )
  for (i in report) {
    lines <- c(lines, sprintf("| `%s` | %s | %s | %d | %s | %s | %s |", i$datei, i$status, cell(i$ds),
                              i$eigene, cell(i$gefunden), cell(i$abweichungen), cell(i$hinweise)))
  }
  missing_ds <- setdiff(1:10, as.integer(names(seeds_used)))
  lines <- c(lines, "", "## Weitere Hinweise", "",
             if (length(missing_ds)) sprintf("- Nicht hochgeladene Seed-Datensätze: %s", paste(sprintf("%02d", missing_ds), collapse = ", ")),
             if (length(other)) sprintf("- Ignoriert (kein CSV): `%s`", other),
             if (length(dup_notes)) paste0("- ", dup_notes))
  con <- file(OUT_REPORT, open = "w", encoding = "UTF-8")
  writeLines(lines, con)
  close(con)
  message(sprintf("Wrote %s (%d rows) and %s", OUT_CSV, nrow(combined), OUT_REPORT))
}

if (sys.nframe() == 0) {
  main()
}
