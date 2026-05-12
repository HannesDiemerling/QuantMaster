# =============================================================================
# validate-upload.R
# Prueft eine hochgeladene CSV-Datei auf Gueltigkeit
#
# Zweck:
#   Wird von der GitHub Action validate-pr.yml aufgerufen, wenn eine neue
#   CSV-Datei in einen Pull Request eingereicht wird.
#   Gibt Fehlermeldungen auf Deutsch aus und stoppt mit Exit Code 1 bei Fehler.
#
# Benutzung:
#   Rscript scripts/validate-upload.R pfad/zur/datei.csv
#
# Exit Codes:
#   0 = Datei ist gueltig
#   1 = Fehler gefunden
# =============================================================================

# --- 0. Konfiguration ---------------------------------------------------------

PFLICHT_SPALTEN <- c(
  "id", "source", "student_id",
  "age", "gender", "semester", "stats_prior", "therapy_exp",
  "BFI_E1", "BFI_E2", "BFI_E3", "BFI_E4",
  "BFI_V1", "BFI_V2", "BFI_V3", "BFI_V4",
  "BFI_G1", "BFI_G2", "BFI_G3", "BFI_G4",
  "BFI_N1", "BFI_N2", "BFI_N3", "BFI_N4",
  "BFI_O1", "BFI_O2", "BFI_O3", "BFI_O4", "BFI_O5",
  "SWLS_1", "SWLS_2", "SWLS_3", "SWLS_4", "SWLS_5",
  "PHQ2_1", "PHQ2_2",
  "GAD2_1", "GAD2_2"
)

BFI_SPALTEN  <- grep("^BFI_",  PFLICHT_SPALTEN, value = TRUE)
SWLS_SPALTEN <- grep("^SWLS_", PFLICHT_SPALTEN, value = TRUE)
PHQ_SPALTEN  <- c("PHQ2_1", "PHQ2_2")
GAD_SPALTEN  <- c("GAD2_1", "GAD2_2")

GUELTIGE_QUELLEN <- c("echt", "synth_student")

# --- 1. Kommandozeilen-Argument lesen -----------------------------------------

args <- commandArgs(trailingOnly = TRUE)

if (length(args) < 1) {
  cat("Fehler: Kein Dateipfad angegeben.\n")
  cat("Benutzung: Rscript scripts/validate-upload.R pfad/zur/datei.csv\n")
  quit(status = 1)
}

dateipfad <- args[1]

cat("=== Validierung startet ===\n")
cat("Datei:", dateipfad, "\n\n")

# Sammelt alle Fehler, damit am Ende eine vollstaendige Liste ausgegeben wird
fehler <- character(0)

# --- 2. Datei existiert? ------------------------------------------------------

if (!file.exists(dateipfad)) {
  cat("FEHLER: Die Datei existiert nicht:", dateipfad, "\n")
  quit(status = 1)
}

# --- 3. Datei einlesen --------------------------------------------------------

df <- tryCatch(
  read.csv(dateipfad,
           header           = TRUE,
           sep              = ",",
           dec              = ".",
           fileEncoding     = "UTF-8",
           stringsAsFactors = FALSE,
           na.strings       = c("", "NA")),
  error = function(e) {
    cat("FEHLER: Datei konnte nicht eingelesen werden.\n")
    cat("Technische Meldung:", conditionMessage(e), "\n")
    cat("\nMoegliche Ursachen:\n")
    cat("  - Datei ist nicht UTF-8 kodiert\n")
    cat("  - Trenner ist kein Komma (z.B. Semikolon aus Excel)\n")
    cat("  - Datei ist kein gueltiges CSV\n")
    quit(status = 1)
  }
)

cat("Datei eingelesen: ", nrow(df), "Zeilen,", ncol(df), "Spalten.\n\n")

# --- 4. Mindestens eine Datenzeile vorhanden? ---------------------------------

if (nrow(df) < 1) {
  fehler <- c(fehler,
    "Die Datei enthaelt keine Datenzeilen (nur Spaltenkoepfe oder leer).")
}

# --- 5. Alle Pflichtspalten vorhanden? ----------------------------------------

fehlende_spalten <- setdiff(PFLICHT_SPALTEN, colnames(df))
if (length(fehlende_spalten) > 0) {
  fehler <- c(fehler,
    paste0("Fehlende Spalten (", length(fehlende_spalten), "): ",
           paste(fehlende_spalten, collapse = ", "),
           ". Vorlage unter materials/csv-template.csv verwenden."))
}

# Extra-Spalten sind eine Warnung, kein Fehler
extra_spalten <- setdiff(colnames(df), PFLICHT_SPALTEN)
if (length(extra_spalten) > 0) {
  cat("Hinweis: Folgende Spalten sind nicht im Codebook und werden beim Merge ignoriert:\n")
  cat(" ", paste(extra_spalten, collapse = ", "), "\n\n")
}

# Falls Pflichtspalten fehlen, koennen die naechsten Pruefungen nicht sinnvoll laufen
if (length(fehler) > 0) {
  cat("--- Fehler gefunden ---\n")
  for (i in seq_along(fehler)) {
    cat(i, ". ", fehler[i], "\n", sep = "")
  }
  cat("\nValidierung fehlgeschlagen.\n")
  quit(status = 1)
}

# --- 6. source-Variable pruefen -----------------------------------------------

ungueltige_quellen <- df$source[!df$source %in% GUELTIGE_QUELLEN & !is.na(df$source)]
if (length(ungueltige_quellen) > 0) {
  fehler <- c(fehler,
    paste0("Ungueltige Werte in 'source': ",
           paste(unique(ungueltige_quellen), collapse = ", "),
           ". Erlaubt: ", paste(GUELTIGE_QUELLEN, collapse = ", "), "."))
}

# source darf nicht leer sein
if (any(is.na(df$source) | df$source == "")) {
  fehler <- c(fehler,
    "Die Spalte 'source' enthaelt leere Werte. Bitte 'echt' oder 'synth_student' eintragen.")
}

# --- 7. BFI-Werte im Bereich 1-5 pruefen -------------------------------------

prueife_bereich <- function(daten, spalten, min_val, max_val, name) {
  # Nur nicht-leere Werte pruefen
  werte <- unlist(daten[, spalten, drop = FALSE])
  werte_numerisch <- suppressWarnings(as.numeric(werte))
  werte_gefuellt  <- werte_numerisch[!is.na(werte_numerisch)]

  if (length(werte_gefuellt) == 0) {
    return(character(0))  # Alle leer -- das ist erlaubt (fehlende Werte)
  }

  ausser_bereich <- werte_gefuellt[werte_gefuellt < min_val | werte_gefuellt > max_val]
  if (length(ausser_bereich) > 0) {
    return(paste0(name, "-Werte ausserhalb des erlaubten Bereichs (",
                  min_val, "-", max_val, "): ",
                  paste(unique(ausser_bereich), collapse = ", ")))
  }
  character(0)
}

fehler <- c(fehler, prueife_bereich(df, BFI_SPALTEN,  1, 5, "BFI"))
fehler <- c(fehler, prueife_bereich(df, SWLS_SPALTEN, 1, 7, "SWLS"))
fehler <- c(fehler, prueife_bereich(df, PHQ_SPALTEN,  0, 3, "PHQ-2"))
fehler <- c(fehler, prueife_bereich(df, GAD_SPALTEN,  0, 3, "GAD-2"))

# --- 8. Ergebnis ausgeben -----------------------------------------------------

if (length(fehler) > 0) {
  cat("--- Fehler gefunden ---\n")
  for (i in seq_along(fehler)) {
    cat(i, ". ", fehler[i], "\n", sep = "")
  }
  cat("\nValidierung fehlgeschlagen. Bitte die Fehler korrigieren und erneut einreichen.\n")
  quit(status = 1)
} else {
  cat("--- Validierung erfolgreich ---\n\n")
  cat("Zeilen:     ", nrow(df), "\n")
  cat("Spalten:    ", ncol(df), "\n")

  cat("\nVerteilung 'source':\n")
  print(table(df$source, useNA = "ifany"))

  fehlende_je_spalte <- colSums(is.na(df))
  fehlende_gefunden  <- fehlende_je_spalte[fehlende_je_spalte > 0]
  if (length(fehlende_gefunden) > 0) {
    cat("\nFehlende Werte (erlaubt, aber zur Kenntnis):\n")
    print(fehlende_gefunden)
  } else {
    cat("\nKeine fehlenden Werte.\n")
  }

  cat("\nDie Datei ist gueltig und kann gemergt werden.\n")
  cat("=== Validierung abgeschlossen ===\n")
  quit(status = 0)
}
