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
#   - Verwendet werden nur Dateien in data/student-uploads/. CSV-Dateien an
#     anderen Stellen (außer data/combined/ und den Übungsdatensätzen) gehen
#     nicht ein, stehen aber als IGNORIERT im Report.
#   - Zeilen, die zu >= 19 von 21 BFI-Items mit einer vorgenerierten Zeile
#     übereinstimmen, sind "Seed-Zeilen". Jeder hochgeladene Seed-Datensatz
#     geht genau einmal ein — in der Referenz-Bereinigung (Fehlerzellen leer,
#     doppelte Probandennummer korrigiert).
#   - Alle anderen Zeilen sind eigene Zeilen der Studierenden. Werte außerhalb
#     der Codebook-Bereiche werden auf fehlend gesetzt, exakte Duplikate entfernt.
#
# Nur base R + MASS (wird mit R ausgeliefert). Aufruf aus dem Repo-Root:
#   Rscript scripts/merge-uploads.R

options(scipen = 100)     # Kennziffern wie 100000 nie als 1e+05 schreiben
source("scripts/meta-studien.R", local = TRUE, encoding = "UTF-8")

GENERATOR  <- "scripts/generate-datasets.R"
# Die beiden Umgebungsvariablen setzt nur freeze-dataset.R (reparierte Kopien, eigene Ausgabe)
OUT_DIR    <- Sys.getenv("MERGE_OUT_DIR", "data/combined")
OUT_CSV    <- file.path(OUT_DIR, "m15-gesamt.csv")
OUT_REPORT <- file.path(OUT_DIR, "merge-report.md")
OUT_META   <- file.path(OUT_DIR, "meta-studien.csv")
UPLOAD_DIR <- Sys.getenv("MERGE_UPLOAD_DIR", "data/student-uploads")
MIN_MATCH  <- 19          # so viele BFI-Items müssen übereinstimmen
# Gültige Kennziffer: jede ganze Zahl ab 10. Gedacht sind die letzten 6 Ziffern der
# Matrikelnummer, andere Zahlen sind auch ok, wichtig ist nur, bei einer zu bleiben.
# 1 bis 9 sind die Nummern der Übungszeilen. Ab 16 Stellen kann R die Zahl nicht mehr
# genau speichern (zwei Personen würden zu einer), solche Zeilen bekommen eine Ersatz-id.
is_kz <- function(x) !is.na(x) & x == round(x) & x >= 10 & x < 1e15
ALLE_SEEDS <- TRUE        # alle 10 Seed-Datensätze aufnehmen, auch wenn einer nicht hochgeladen wurde
                          # (hält N, Metaanalyse und Mixed Model bei kleinen Kursen stabil)

# Referenz = unveränderliche Kopie der Übungsdatensätze in scripts/referenz/.
# Studierende haben Schreibrechte auf das ganze Repo; lädt jemand versehentlich
# etwas nach data/generated/, bleibt der Merge trotzdem korrekt und meldet es.
# Nach einer gewollten Neuerzeugung der Datensätze die Kopien aktualisieren.
REF_DIR    <- "scripts/referenz"
SEED_NAMES <- sprintf("datensatz-%02d.csv", 1:10)

# Konto-Zuordnung (nur für den Report, sie ändert nie die Daten)
ORIG_DIR   <- "data/student-uploads"   # immer der echte Ordner, auch wenn der Merge über Kopien läuft
LEHR_DATEI <- "scripts/lehrpersonen.txt"

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

# --- Konten aus der Git-Historie ------------------------------------------------
# Nie eine Abbruchursache: Ohne Git, ohne Repository oder bei flachem Klon bleibt die
# Spalte Konto leer. Ein flacher Klon ist ausgeschlossen, weil dort alle Dateien scheinbar
# aus dem letzten Commit stammen. E-Mail-Adressen werden nur verglichen, nie ausgegeben.

lehrpersonen_lesen <- function(pfad = LEHR_DATEI) {
  z <- tryCatch(readLines(pfad, encoding = "UTF-8", warn = FALSE), error = function(e) character(0), warning = function(w) character(0))
  z <- iconv(z, "UTF-8", "UTF-8", sub = "")
  z <- sub("^\ufeff", "", z)
  z <- trimws(sub("#.*$", "", z))
  tolower(z[nzchar(z)])
}

konto_text <- function(x) {
  x <- sub("@.*$", "", iconv(x, "UTF-8", "UTF-8", sub = ""))      # falls jemand die E-Mail als Namen eingetragen hat
  trimws(substr(gsub("[^\\p{L}\\p{N} ._-]", "", x, perl = TRUE), 1, 60))
}

konten_lesen <- function() {
  leer <- data.frame(pfad = character(0), konto = character(0), schluessel = character(0), lehr = logical(0))
  git <- function(args) {
    f <- tempfile("git-")
    on.exit(unlink(f))
    rc <- tryCatch(suppressWarnings(system2("git", args, stdout = f, stderr = FALSE)), error = function(e) 1L)
    if (!identical(as.integer(rc), 0L) || !file.exists(f)) return(NULL)
    readLines(f, encoding = "UTF-8", warn = FALSE)
  }
  flach <- git(c("rev-parse", "--is-shallow-repository"))
  if (is.null(flach) || identical(trimws(flach[1]), "true")) return(leer)
  z <- git(c("-c", "core.quotepath=false", "log", "--format=%x01%an%x1f%ae", "--name-only",
             "--no-renames", "--diff-filter=ACMR", "--relative", "--", ORIG_DIR))
  if (is.null(z)) return(leer)
  lehr <- lehrpersonen_lesen()
  praefix <- paste0(ORIG_DIR, "/")
  gesehen <- new.env()
  out <- list()
  aktuell <- NULL
  for (l in z) {
    if (startsWith(l, "\001")) {
      teile <- strsplit(substring(l, 2), "\037", fixed = TRUE)[[1]]
      name <- if (length(teile) >= 1) teile[1] else ""
      mail <- if (length(teile) >= 2) teile[2] else ""
      aktuell <- if (grepl("[bot]", paste(name, mail), fixed = TRUE)) NULL else {   # Bot-Commits zählen nicht
        m <- regmatches(mail, regexec("^(?:[0-9]+\\+)?([^@+]+)@users\\.noreply\\.github\\.com$", mail, ignore.case = TRUE, perl = TRUE))[[1]]
        login <- if (length(m) == 2) m[2] else ""
        anzeige <- if (nzchar(login)) login else konto_text(name)
        schl <- tolower(if (nzchar(login)) login else if (nzchar(mail)) mail else name)
        list(konto = anzeige, schluessel = schl, lehr = any(tolower(trimws(c(login, name, anzeige))) %in% lehr))
      }
    } else if (nzchar(l) && !is.null(aktuell) && startsWith(l, praefix) && is.null(gesehen[[l]])) {
      gesehen[[l]] <- TRUE                                 # der neueste Commit steht zuerst
      out[[length(out) + 1]] <- c(list(pfad = l), aktuell)
    }
  }
  if (!length(out)) return(leer)
  do.call(rbind, lapply(out, as.data.frame))
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

  files <- sort(unique(list.files(UPLOAD_DIR, pattern = "\\.(csv|txt|tsv)$", recursive = TRUE,
                                  full.names = TRUE, ignore.case = TRUE)), method = "radix")   # unabhängig von der Locale
  # Datendateien außerhalb des Upload-Ordners gehen nicht ein, werden aber gemeldet
  anderswo <- sub("^\\./", "", c(list.files("data", pattern = "\\.(csv|xlsx|xls|ods|numbers|jasp)$", recursive = TRUE,
                                            full.names = TRUE, ignore.case = TRUE),
                                 list.files(".", pattern = "\\.(csv|xlsx|xls|ods|numbers|jasp)$", full.names = TRUE, ignore.case = TRUE)))
  anderswo <- sort(setdiff(anderswo[!grepl("^data/(combined|student-uploads)/", anderswo)],
                           c(files, file.path("data/generated", SEED_NAMES))))
  other <- list.files(UPLOAD_DIR, recursive = TRUE, full.names = TRUE)
  other <- other[!grepl("\\.(csv|txt|tsv|gitkeep)$", other, ignore.case = TRUE) & basename(other) != "README.md"]

  konten <- tryCatch(konten_lesen(), error = function(e) data.frame(pfad = character(0), konto = character(0), schluessel = character(0), lehr = logical(0)))
  # Kopien aus repair-uploads.py zurück auf die Originaldatei abbilden (anderer Ordner, angehängtes
  # .csv bei umgewandelten Dateien, __personN bei mehreren Personen in einer Datei)
  herkunft <- function(f) {
    rel <- substring(f, nchar(UPLOAD_DIR) + 2)
    ohne <- sub("__person[0-9]+(\\.[^./]*)$", "\\1", rel)
    kand <- unique(c(rel, ohne, sub("\\.csv$", "", ohne, ignore.case = TRUE)))
    pfade <- file.path(ORIG_DIR, kand)
    treffer <- pfade[pfade %in% konten$pfad | file.exists(pfade)]
    orig <- if (length(treffer)) treffer[1] else file.path(ORIG_DIR, sub("\\.(numbers|jasp|xlsx)\\.csv$", ".\\1", ohne, ignore.case = TRUE))
    k <- match(orig, konten$pfad)
    if (is.na(k)) return(list(original = orig, konto = "", schluessel = "", lehr = FALSE))
    list(original = orig, konto = konten$konto[k], schluessel = konten$schluessel[k], lehr = konten$lehr[k])
  }
  konto_zelle <- function(i) {
    if (is.null(i$konto) || !nzchar(i$konto)) return("–")
    if (isTRUE(i$lehr)) paste0(i$konto, " (für andere hochgeladen)") else i$konto
  }

  report <- list()
  seeds_used <- list()       # ds -> Dateien
  own <- list()

  for (f in anderswo) {
    report[[f]] <- list(datei = f, status = "IGNORIERT", ds = NA, eigene = 0, gefunden = NA, abweichungen = NA, kz = NULL,
                        hinweise = "liegt NICHT in data/student-uploads/ und wurde deshalb nicht verwendet. Bitte dort hochladen")
  }

  for (f in files) {
    info <- list(datei = f, status = "ok", ds = NA, eigene = 0, hinweise = character(0),
                 gefunden = NA, abweichungen = NA, kz = NULL)
    info <- c(info, herkunft(f))
    if (!grepl("^datensatz-[0-9]{2}-[0-9]+\\.csv$", basename(f)))
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
    # Zwei Zeilen treffen dieselbe Übungszeile, eine davon mit Kennziffer: Das ist eine
    # fiktive Person, die aus einer Übungszeile abgeschrieben wurde.
    zeile_key <- paste(m[, "ds"], m[, "zeile"])
    doppelt <- is_seed & zeile_key %in% zeile_key[is_seed & duplicated(zeile_key)]
    kz_ok <- is_kz(df$probandennummer)
    if (any(doppelt & kz_ok) && any(doppelt & !kz_ok)) is_seed[doppelt & kz_ok] <- FALSE
    # Fremde Übungszeile mit Kennziffer: ebenfalls eine abgeschriebene Person
    if (length(unique(m[is_seed, "ds"])) > 1) {
      haupt <- as.numeric(names(which.max(table(m[is_seed, "ds"]))))
      is_seed[is_seed & m[, "ds"] != haupt & kz_ok] <- FALSE
    }
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
      if (!is_kz(mine$probandennummer)) {
        pn <- mine$probandennummer
        pruefen(if (!is.na(pn) && pn >= 1e15)
          sprintf("Kennziffer %s hat mehr als 15 Stellen und lässt sich nicht genau speichern. Nimm die letzten 6 Ziffern Deiner Matrikelnummer", format(pn, scientific = FALSE))
        else
          sprintf("Kennziffer %s ungültig. Trag die letzten 6 Ziffern Deiner Matrikelnummer ein (nur Ziffern, 1 bis 9 sind die Nummern der Übungszeilen). Andere Zahlen sind auch ok, wichtig ist, dass Du bei einer bleibst",
                  ifelse(is.na(pn), "fehlt oder ist keine Zahl,", format(pn, scientific = FALSE))))
      }
      mine <- clean_range(mine)
      if (attr(mine, "n_bad") > 0)
        pruefen(sprintf("%d Wert(e) der fiktiven Person außerhalb des erlaubten Bereichs -> fehlend", attr(mine, "n_bad")))
      if (sum(is.na(mine[COLS])) > 0 && attr(mine, "n_bad") == 0)
        pruefen(sprintf("%d Wert(e) der fiktiven Person fehlen oder sind keine Zahl", sum(is.na(mine[COLS]))))
      # ohne erkannte Übungszeilen: Datensatznummer aus dem Dateinamen, falls vorhanden
      ds_name <- suppressWarnings(as.integer(sub("^datensatz[^0-9]*([0-9]{1,2})[^0-9].*$", "\\1", basename(f))))
      mine$quelle_datensatz <- if (length(ds_in_file) == 1) ds_in_file else if (!is.na(ds_name) && ds_name %in% 1:10) ds_name else NA
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
    # Bei Doppelten gewinnt die Datei, deren Name die Kennziffer enthält (die eigene Datei)
    eigen <- mapply(function(k, f) !is.na(k) && grepl(format(k, scientific = FALSE), basename(f), fixed = TRUE),
                    own_df$probandennummer, own_df$quelle_datei)
    ord <- order(!eigen)
    dup <- logical(length(key))
    dup[ord] <- duplicated(key[ord])
    for (f in unique(own_df$quelle_datei[dup])) {
      dup_notes <- c(dup_notes, sprintf("`%s`: %d eigene Zeile(n) schon in anderer Datei enthalten -> nur einmal übernommen",
                                        f, sum(dup & own_df$quelle_datei == f)))
    }
    own_df <- own_df[!dup, ]
    pn <- own_df$probandennummer
    valid <- is_kz(pn) & !(pn %in% pn[duplicated(pn)]) & !(pn %in% seed_df$id)
    own_df$id <- ifelse(valid, pn, NA)
    fallback <- which(!valid)
    # Ersatz-ids unterhalb sehr langer Kennziffern, sonst wären sie im CSV nicht mehr genau
    belegt <- c(seed_df$id, own_df$id[!is.na(own_df$id)])
    kand <- max(belegt[belegt < 1e12])
    neu <- numeric(0)
    while (length(neu) < length(fallback)) {
      kand <- kand + 1
      if (!(kand %in% belegt)) neu <- c(neu, kand)
    }
    own_df$id[fallback] <- neu
    for (f in unique(own_df$quelle_datei[fallback])) {
      k <- fallback[own_df$quelle_datei[fallback] == f]
      dup_notes <- c(dup_notes, sprintf("`%s`: Probandennummer %s ist ungültig, doppelt oder als id schon vergeben -> id %s (die Kennziffer bleibt in der Spalte probandennummer)",
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
  write.csv(meta_studien(combined), OUT_META, row.names = FALSE, fileEncoding = "UTF-8")

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
    "Suche Deine Datei über Deine **Kennziffer** (die letzten 6 Ziffern Deiner Matrikelnummer; andere Zahlen sind auch ok, wichtig ist, dass Du bei einer bleibst). Handeln musst Du nur bei Status IGNORIERT, PRÜFEN oder DOPPELT oder wenn bei „Fehler gefunden“ nicht 2/2 steht. Hinweise zu Dateiname oder Trennzeichen sind unkritisch.",
    "",
    "| Datei | Kennziffer | Status | Datensatz | fiktive Personen | Fehler gefunden | Abweichungen von Referenz | Hinweise | Konto |",
    "|---|---|---|---|---:|---|---:|---|---|"
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
      report[[k]]$hinweise <- c(i$hinweise, sprintf("Kennziffer %s auch in %s: sind beide Dateien von Dir, die ältere löschen; gehören sie zwei Personen, nimmt eine von Euch eine Ziffer mehr aus der Matrikelnummer",
                                                     paste(d, collapse = ", "), paste0("`", unique(andere), "`", collapse = ", ")))
    }
  }
  # Ein Konto, mehrere Kennziffern: sichtbar machen, nichts automatisch ändern. Lehrpersonen laden für andere hoch.
  bitte <- character(0)
  mit_konto <- Filter(function(i) nzchar(if (is.null(i$schluessel)) "" else i$schluessel) && !isTRUE(i$lehr) &&
                        length(i$kz) == 1 && is_kz(i$kz), report)
  schl <- vapply(mit_konto, function(i) i$schluessel, "")
  for (s in unique(schl[duplicated(schl)])) {
    gr <- mit_konto[schl == s]
    kzs <- vapply(gr, function(i) format(i$kz, scientific = FALSE, trim = TRUE), "")
    if (length(unique(kzs)) < 2 || length(unique(vapply(gr, function(i) i$original, ""))) < 2) next
    werte <- vapply(gr, function(i) {
      o <- own[[i$datei]]
      if (is.null(o)) NA_character_ else paste(unlist(o[setdiff(COLS, "probandennummer")]), collapse = "|")
    }, "")
    gleich <- !anyNA(werte) && length(unique(werte)) == 1
    bitte <- c(bitte, sprintf("- Konto `%s` hat mehrere Kennziffern hochgeladen: %s. %s", gr[[1]]$konto,
                              paste0(kzs, " (`", vapply(gr, function(i) i$original, ""), "`)", collapse = ", "),
                              if (gleich) "Die Werte der fiktiven Person sind gleich, das ist sehr wahrscheinlich eine Person: bitte bei einer Nummer bleiben und die andere Datei löschen."
                              else "Ist das eine Person, bitte bei einer Nummer bleiben und die andere Datei löschen. Hast Du für jemand anderen hochgeladen, ist alles in Ordnung."))
    for (i in gr) report[[i$datei]]$hinweise <- c(report[[i$datei]]$hinweise, "dieses Konto hat mehrere Kennziffern, siehe „Bitte ansehen“")
  }
  for (i in report) {
    lines <- c(lines, sprintf("| `%s` | %s | %s | %s | %d | %s | %s | %s | %s |", i$datei, cell(i$kz), i$status, cell(i$ds),
                              i$eigene, cell(i$gefunden), cell(i$abweichungen), cell(i$hinweise), konto_zelle(i)))
  }
  if (length(bitte)) lines <- c(lines, "", "## Bitte ansehen", "", bitte)
  missing_ds <- setdiff(1:10, as.integer(names(seeds_used)))
  konto_ohne <- function(p) {
    h <- herkunft(p)
    if (nzchar(h$konto)) sprintf(" (Konto: %s)", konto_zelle(h)) else ""
  }
  lines <- c(lines, "", "## Weitere Hinweise", "",
             if (length(missing_ds)) sprintf("- Nicht hochgeladene Seed-Datensätze: %s%s", paste(sprintf("%02d", missing_ds), collapse = ", "),
                                             if (ALLE_SEEDS) " (trotzdem im Gesamtdatensatz enthalten)" else ""),
             if (length(other)) sprintf("- Ignoriert (kein CSV): `%s`%s", other, vapply(other, konto_ohne, "")),
             if (length(dup_notes)) paste0("- ", dup_notes),
             if (!nrow(konten)) "- Konto-Zuordnung nicht verfügbar (keine vollständige Git-Historie), die Spalte Konto bleibt leer.")
  con <- file(OUT_REPORT, open = "w", encoding = "UTF-8")
  writeLines(lines, con)
  close(con)
  # Nur beim Einfrieren gesetzt: Zuordnung Kennziffer zu Datei und Konto für freeze-dataset.R
  konten_csv <- Sys.getenv("MERGE_KONTEN_CSV")
  if (nzchar(konten_csv)) {
    ein <- Filter(function(i) !is.null(i$original), report)
    kz1 <- function(i) if (length(i$kz) == 1 && !is.na(i$kz) && i$kz == round(i$kz) && i$kz < 1e15) format(i$kz, scientific = FALSE, trim = TRUE) else ""
    d <- data.frame(kennziffer = vapply(ein, kz1, ""),
                    datei = vapply(ein, function(i) i$original, ""),
                    konto = vapply(ein, function(i) i$konto, ""),
                    lehrperson = vapply(ein, function(i) if (!nzchar(i$konto)) "" else if (isTRUE(i$lehr)) "ja" else "nein", ""),
                    quelle_datei = vapply(ein, function(i) i$datei, ""))
    write.csv(d, konten_csv, row.names = FALSE, fileEncoding = "UTF-8")
  }
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
