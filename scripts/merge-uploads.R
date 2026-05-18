# scripts/merge-uploads.R
#
# Liest alle CSVs in data/student-uploads/, validiert das Schema,
# konkateniert sie zu einem Gesamtdatensatz und schreibt
# data/combined/m15-gesamt.csv.
#
# Duplikate aus den vorgenerierten Stichproben werden entfernt:
# Sobald zwei Zeilen aus demselben Herkunftsdatensatz (= gleicher Dateiname
# bis auf das Kuerzel-Suffix) dieselbe probandennummer haben und die
# probandennummer < 10 ist (= aus dem vorgenerierten Block), wird das Duplikat
# entfernt. Studierenden-eigene Zeilen (probandennummer >= 10) bleiben alle.

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
})

UPLOADS_DIR  <- "data/student-uploads"
OUT_DIR      <- "data/combined"
OUT_FILE     <- file.path(OUT_DIR, "m15-gesamt.csv")
CODEBOOK     <- "materials/codebook.csv"

# --- Hilfsfunktionen ---------------------------------------------------------

read_expected_columns <- function(codebook_path) {
  cb <- readr::read_csv(codebook_path, show_col_types = FALSE)
  cb$variable
}

# extrahiert die Datensatz-Nummer aus "datensatz-03-xy.csv" -> 3
extract_dataset_nr <- function(filename) {
  m <- regmatches(filename, regexpr("datensatz-(\\d{1,2})", filename))
  if (length(m) == 0) return(NA_integer_)
  as.integer(sub("datensatz-", "", m))
}

validate_upload <- function(df, expected_cols, source_file) {
  if (!identical(colnames(df), expected_cols)) {
    missing  <- setdiff(expected_cols, colnames(df))
    extra    <- setdiff(colnames(df), expected_cols)
    warning(sprintf(
      "Schema-Mismatch in '%s'\n  fehlende Spalten: %s\n  unerwartete Spalten: %s",
      source_file,
      paste(missing, collapse = ", "),
      paste(extra,   collapse = ", ")
    ))
    return(FALSE)
  }
  TRUE
}

# --- Hauptlogik --------------------------------------------------------------

main <- function() {
  if (!dir.exists(OUT_DIR)) {
    dir.create(OUT_DIR, recursive = TRUE)
  }

  if (!dir.exists(UPLOADS_DIR)) {
    message("No uploads directory yet — nothing to merge.")
    return(invisible(NULL))
  }

  expected_cols <- read_expected_columns(CODEBOOK)

  upload_files <- list.files(UPLOADS_DIR, pattern = "\\.csv$",
                             full.names = TRUE)

  if (length(upload_files) == 0) {
    message("No CSV uploads found in ", UPLOADS_DIR, " — writing empty file.")
    empty <- as.data.frame(matrix(ncol = length(expected_cols) + 2, nrow = 0))
    colnames(empty) <- c(expected_cols, "herkunft_datensatz", "quelle_datei")
    readr::write_csv(empty, OUT_FILE, na = "")
    return(invisible(NULL))
  }

  all_chunks <- list()

  for (f in upload_files) {
    df <- tryCatch(
      readr::read_csv(f, show_col_types = FALSE),
      error = function(e) {
        warning(sprintf("Konnte '%s' nicht lesen: %s", f, conditionMessage(e)))
        NULL
      }
    )
    if (is.null(df)) next

    ok <- validate_upload(df, expected_cols, basename(f))
    if (!ok) next

    df$herkunft_datensatz <- extract_dataset_nr(basename(f))
    df$quelle_datei       <- basename(f)

    all_chunks[[length(all_chunks) + 1]] <- df
  }

  if (length(all_chunks) == 0) {
    message("No valid uploads — writing empty file.")
    empty <- as.data.frame(matrix(ncol = length(expected_cols) + 2, nrow = 0))
    colnames(empty) <- c(expected_cols, "herkunft_datensatz", "quelle_datei")
    readr::write_csv(empty, OUT_FILE, na = "")
    return(invisible(NULL))
  }

  combined <- dplyr::bind_rows(all_chunks)

  # Duplikate aus vorgenerierten Stichproben (probandennummer < 10):
  # pro (herkunft_datensatz, probandennummer) nur eine Zeile behalten.
  seeded <- combined %>%
    dplyr::filter(!is.na(probandennummer), probandennummer < 10) %>%
    dplyr::distinct(herkunft_datensatz, probandennummer, .keep_all = TRUE)

  own <- combined %>%
    dplyr::filter(is.na(probandennummer) | probandennummer >= 10)

  final <- dplyr::bind_rows(seeded, own) %>%
    dplyr::arrange(herkunft_datensatz, probandennummer)

  readr::write_csv(final, OUT_FILE, na = "")
  message("Wrote ", OUT_FILE, " (", nrow(final), " rows from ",
          length(all_chunks), " uploads)")

  invisible(final)
}

if (sys.nframe() == 0) {
  main()
}
