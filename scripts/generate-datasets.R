# scripts/generate-datasets.R
#
# Erzeugt 10 vorgenerierte Datensätze für den Workshop M15 (Tag 1).
# - Pro Datensatz 9 Probanden, 25 Spalten (vgl. materials/codebook.csv).
# - BFI-K-Items sind bereits rekodiert (Reverse-Items umgepolt).
# - Pro Datensatz werden GENAU 2 absichtliche Fehler eingebaut.
# - Schreibt zusätzlich scripts/error-overview.md (intern, nicht in der Site).
#
# Deterministisch: fester Seed pro Datensatz (1000 + dataset_nr).

suppressPackageStartupMessages({
  library(MASS)
  library(readr)
  library(dplyr)
})

# --- Konfiguration -----------------------------------------------------------

N_DATASETS    <- 10
N_PER_DATASET <- 9
OUT_DIR       <- "data/generated"
ERROR_DOC     <- "scripts/error-overview.md"

BFI_ITEMS <- c(
  paste0("BFI_E", 1:4),
  paste0("BFI_V", 1:4),
  paste0("BFI_G", 1:4),
  paste0("BFI_N", 1:4),
  paste0("BFI_O", 1:5)
)
FACTOR_OF <- c(rep("E", 4), rep("V", 4), rep("G", 4), rep("N", 4), rep("O", 5))

# Korrelationsmatrix der 21 BFI-K-Items (vereinfacht):
#   innerhalb eines Faktors: r = 0.40
#   zwischen Faktoren:       r = 0.10
make_bfi_corr <- function() {
  k <- length(BFI_ITEMS)
  R <- matrix(0.10, nrow = k, ncol = k)
  for (i in seq_len(k)) {
    for (j in seq_len(k)) {
      if (i == j) {
        R[i, j] <- 1.0
      } else if (FACTOR_OF[i] == FACTOR_OF[j]) {
        R[i, j] <- 0.40
      }
    }
  }
  R
}

# --- Hilfsfunktionen ---------------------------------------------------------

# Latente Normalvariable auf 1..5 diskretisieren (annähernd gleichverteilt).
discretize_likert <- function(z) {
  # 5 Quintil-Cuts der Standardnormalverteilung
  cuts <- qnorm(c(0.20, 0.40, 0.60, 0.80))
  as.integer(cut(z, breaks = c(-Inf, cuts, Inf), labels = 1:5))
}

sample_geschlecht <- function(n) {
  sample(c(1L, 2L, 3L), size = n, replace = TRUE,
         prob = c(0.65, 0.30, 0.05))
}

sample_alter <- function(n) {
  # leicht jünger geschiefte Verteilung um ~28 Jahre, geclippt auf 18..65
  raw <- round(rgamma(n, shape = 2.5, rate = 0.22) + 18)
  pmin(pmax(raw, 18L), 65L)
}

sample_bildung <- function(n) {
  sample(1L:7L, size = n, replace = TRUE,
         prob = c(0.01, 0.03, 0.06, 0.20, 0.40, 0.25, 0.05))
}

# --- BFI-K-Daten erzeugen (rekodiert, also: hohe Zahl = hohe Ausprägung) -----

generate_bfi_block <- function(n, corr) {
  z <- MASS::mvrnorm(n = n, mu = rep(0, nrow(corr)), Sigma = corr,
                     empirical = FALSE)
  out <- apply(z, 2, discretize_likert)
  if (n == 1) out <- matrix(out, nrow = 1)
  colnames(out) <- BFI_ITEMS
  as.data.frame(out)
}

# --- Fehler einbauen ---------------------------------------------------------
#
# Fehlerarten gemäß CLAUDE.md §4.4:
#   F1: doppelte Probandennummer
#   F2: fehlender Wert in einer BFI-Spalte (leeres Feld)
#   F3: BFI-Wert außerhalb 1..5
#   F4: unmögliches Alter
#   F5: ungültiger Geschlechts-Code
#   F6: fehlender Demografie-Wert (bildungsgrad leer)
#
# Pro Datensatz exakt zwei Fehler, deterministisch verteilt.

ERROR_PLAN <- list(
  list(F1 = TRUE,  F2 = TRUE),   # Datensatz 01
  list(F3 = TRUE,  F4 = TRUE),   # Datensatz 02
  list(F2 = TRUE,  F5 = TRUE),   # Datensatz 03
  list(F1 = TRUE,  F6 = TRUE),   # Datensatz 04
  list(F4 = TRUE,  F3 = TRUE),   # Datensatz 05
  list(F5 = TRUE,  F2 = TRUE),   # Datensatz 06
  list(F6 = TRUE,  F1 = TRUE),   # Datensatz 07
  list(F3 = TRUE,  F2 = TRUE),   # Datensatz 08
  list(F4 = TRUE,  F5 = TRUE),   # Datensatz 09
  list(F1 = TRUE,  F3 = TRUE)    # Datensatz 10
)

apply_errors <- function(df, dataset_nr) {
  plan <- ERROR_PLAN[[dataset_nr]]
  errors <- list()

  # Deterministischer lokaler Seed für Zeilen-/Spaltenwahl
  set.seed(2000 + dataset_nr)

  if (isTRUE(plan$F1)) {
    # zwei Probandennummern gleich machen
    r1 <- sample(seq_len(nrow(df)), 1)
    r2 <- sample(setdiff(seq_len(nrow(df)), r1), 1)
    old_val <- df$probandennummer[r2]
    df$probandennummer[r2] <- df$probandennummer[r1]
    errors[[length(errors) + 1]] <- list(
      zeile = r2, spalte = "probandennummer",
      typ = "F1: doppelte Probandennummer",
      note = sprintf("Zeile %d hat jetzt Nummer %d (gleich Zeile %d), urspruenglich %d",
                     r2, df$probandennummer[r2], r1, old_val)
    )
  }

  if (isTRUE(plan$F2)) {
    r <- sample(seq_len(nrow(df)), 1)
    col <- sample(BFI_ITEMS, 1)
    df[r, col] <- NA_integer_
    errors[[length(errors) + 1]] <- list(
      zeile = r, spalte = col,
      typ = "F2: fehlender Wert in BFI-Spalte",
      note = sprintf("Zeile %d, Spalte %s leer", r, col)
    )
  }

  if (isTRUE(plan$F3)) {
    r <- sample(seq_len(nrow(df)), 1)
    col <- sample(BFI_ITEMS, 1)
    bad <- sample(c(0L, 6L, 7L, 9L), 1)
    df[r, col] <- bad
    errors[[length(errors) + 1]] <- list(
      zeile = r, spalte = col,
      typ = "F3: BFI-Wert ausserhalb 1..5",
      note = sprintf("Zeile %d, Spalte %s = %d", r, col, bad)
    )
  }

  if (isTRUE(plan$F4)) {
    r <- sample(seq_len(nrow(df)), 1)
    bad <- sample(c(5L, 7L, 12L, 150L, 200L), 1)
    df$alter[r] <- bad
    errors[[length(errors) + 1]] <- list(
      zeile = r, spalte = "alter",
      typ = "F4: unmoegliches Alter",
      note = sprintf("Zeile %d, alter = %d", r, bad)
    )
  }

  if (isTRUE(plan$F5)) {
    r <- sample(seq_len(nrow(df)), 1)
    bad <- sample(c(0L, 4L, 9L), 1)
    df$geschlecht[r] <- bad
    errors[[length(errors) + 1]] <- list(
      zeile = r, spalte = "geschlecht",
      typ = "F5: ungueltiger Geschlechts-Code",
      note = sprintf("Zeile %d, geschlecht = %d", r, bad)
    )
  }

  if (isTRUE(plan$F6)) {
    r <- sample(seq_len(nrow(df)), 1)
    df$bildungsgrad[r] <- NA_integer_
    errors[[length(errors) + 1]] <- list(
      zeile = r, spalte = "bildungsgrad",
      typ = "F6: fehlender Bildungsgrad",
      note = sprintf("Zeile %d, bildungsgrad leer", r)
    )
  }

  list(df = df, errors = errors)
}

# --- Einen Datensatz erzeugen ------------------------------------------------

generate_one_dataset <- function(dataset_nr, corr) {
  seed <- 1000L + dataset_nr
  set.seed(seed)

  n <- N_PER_DATASET

  demo <- data.frame(
    probandennummer = seq_len(n),
    geschlecht      = sample_geschlecht(n),
    alter           = sample_alter(n),
    bildungsgrad    = sample_bildung(n)
  )

  bfi <- generate_bfi_block(n, corr)

  df <- cbind(demo, bfi)

  result <- apply_errors(df, dataset_nr)
  list(
    dataset_nr = dataset_nr,
    df         = result$df,
    errors     = result$errors
  )
}

# --- Schreiben des Fehler-Reports --------------------------------------------

write_error_overview <- function(all_results, path) {
  lines <- c(
    "# Fehler-Uebersicht der vorgenerierten Datensaetze",
    "",
    "**INTERN — nur fuer die Lehrperson.** Diese Datei wird automatisch von",
    "`scripts/generate-datasets.R` erzeugt und ist NICHT in der Quarto-Site verlinkt.",
    "Sie dokumentiert, welche absichtlichen Fehler in welchem Datensatz stecken,",
    "damit die Lehrperson Studierenden-Loesungen pruefen kann.",
    "",
    "| Datensatz | Zeile | Spalte | Fehlertyp | Notiz |",
    "|---|---:|---|---|---|"
  )
  for (res in all_results) {
    ds <- sprintf("%02d", res$dataset_nr)
    for (err in res$errors) {
      lines <- c(lines, sprintf(
        "| %s | %d | `%s` | %s | %s |",
        ds, err$zeile, err$spalte, err$typ, err$note
      ))
    }
  }
  writeLines(lines, con = path)
  message("Wrote ", path)
}

# --- Hauptloop ---------------------------------------------------------------

main <- function() {
  if (!dir.exists(OUT_DIR)) {
    dir.create(OUT_DIR, recursive = TRUE)
  }

  corr <- make_bfi_corr()

  all_results <- vector("list", N_DATASETS)
  for (i in seq_len(N_DATASETS)) {
    res <- generate_one_dataset(dataset_nr = i, corr = corr)
    file <- file.path(OUT_DIR, sprintf("datensatz-%02d.csv", i))
    readr::write_csv(res$df, file, na = "")
    message("Wrote ", file, " (", nrow(res$df), " rows, ",
            length(res$errors), " errors)")
    all_results[[i]] <- res
  }

  write_error_overview(all_results, ERROR_DOC)

  invisible(all_results)
}

if (sys.nframe() == 0) {
  main()
}
