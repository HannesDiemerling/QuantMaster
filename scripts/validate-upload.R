# scripts/validate-upload.R
#
# Optionaler Schema-Check fuer eine einzelne hochgeladene CSV.
# Aufruf: Rscript scripts/validate-upload.R <pfad-zur-csv>
#
# Prueft:
#   - Spaltenzahl und Spaltennamen gegen materials/codebook.csv
#   - Wertebereiche pro Spalte (min/max gemaess Codebook)
#
# Exit-Code: 0 wenn ok, 1 wenn Probleme.

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
})

CODEBOOK <- "materials/codebook.csv"

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1) {
  stop("Usage: Rscript scripts/validate-upload.R <csv-file>")
}

file <- args[1]
if (!file.exists(file)) {
  stop("Datei nicht gefunden: ", file)
}

cb <- readr::read_csv(CODEBOOK, show_col_types = FALSE)
df <- readr::read_csv(file, show_col_types = FALSE)

problems <- character()

# Spalten-Schema
if (!identical(colnames(df), cb$variable)) {
  missing <- setdiff(cb$variable, colnames(df))
  extra   <- setdiff(colnames(df), cb$variable)
  if (length(missing) > 0) {
    problems <- c(problems,
                  paste("Fehlende Spalten:", paste(missing, collapse = ", ")))
  }
  if (length(extra) > 0) {
    problems <- c(problems,
                  paste("Unerwartete Spalten:", paste(extra, collapse = ", ")))
  }
}

# Wertebereich pro Spalte
common_cols <- intersect(colnames(df), cb$variable)
for (var in common_cols) {
  row <- cb[cb$variable == var, ]
  values <- df[[var]]
  values <- values[!is.na(values)]
  if (length(values) == 0) next
  lo <- suppressWarnings(as.numeric(row$min))
  hi <- suppressWarnings(as.numeric(row$max))
  if (!is.na(lo) && any(values < lo)) {
    problems <- c(problems,
                  sprintf("%s: Werte unter %s gefunden", var, lo))
  }
  if (!is.na(hi) && any(values > hi)) {
    problems <- c(problems,
                  sprintf("%s: Werte ueber %s gefunden", var, hi))
  }
}

# Probandennummer-Duplikate
if ("probandennummer" %in% colnames(df)) {
  dup <- df$probandennummer[duplicated(df$probandennummer) &
                              !is.na(df$probandennummer)]
  if (length(dup) > 0) {
    problems <- c(problems,
                  paste("Doppelte Probandennummer(n):",
                        paste(unique(dup), collapse = ", ")))
  }
}

if (length(problems) == 0) {
  message("OK — ", file, " entspricht dem Codebook.")
  quit(status = 0)
} else {
  message("Probleme in ", file, ":")
  for (p in problems) message("  - ", p)
  quit(status = 1)
}
