# =============================================================================
# merge-data.R
# Alle CSV-Dateien aus data/raw/ und data/seed/ zusammenfuehren
#
# Zweck:
#   Wird von der GitHub Action merge-data.yml aufgerufen, wenn neue CSV-Dateien
#   in data/raw/ oder data/seed/ eingecheckt werden.
#   Das Ergebnis landet in data/combined/m15-combined.csv
#
# Benutzung (lokal):
#   Rscript scripts/merge-data.R
#
# Voraussetzungen:
#   - Ordner data/raw/ und data/seed/ muessen existieren
#   - Alle CSVs muessen die 37 Pflichtspalten enthalten
# =============================================================================

# --- 0. Konfiguration ---------------------------------------------------------

# Erwartete Spaltennamen (Single Source of Truth)
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

AUSGABE_PFAD <- "data/combined/m15-combined.csv"

# --- 1. CSV-Dateien einsammeln ------------------------------------------------

cat("=== Merge-Datensatz startet ===\n\n")

# Alle relevanten CSV-Dateien finden
dateien_raw  <- list.files("data/raw",  pattern = "\\.(csv|CSV)$",
                            full.names = TRUE, recursive = FALSE)
dateien_seed <- list.files("data/seed", pattern = "\\.(csv|CSV)$",
                            full.names = TRUE, recursive = FALSE)

alle_dateien <- c(dateien_seed, dateien_raw)

cat("Gefundene Dateien:\n")
cat("  data/seed/ :", length(dateien_seed), "Datei(en)\n")
cat("  data/raw/  :", length(dateien_raw),  "Datei(en)\n")
cat("  Gesamt     :", length(alle_dateien), "Datei(en)\n\n")

if (length(alle_dateien) == 0) {
  cat("Keine CSV-Dateien gefunden. Abbruch.\n")
  quit(status = 1)
}

# --- 2. Dateien einlesen und pruefen ------------------------------------------

liste_dfs <- list()
fehler_count <- 0

for (pfad in alle_dateien) {
  cat("Lese:", pfad, "... ")

  # Einlesen (UTF-8, Komma-getrennt, Punkt = Dezimaltrenner)
  df <- tryCatch(
    read.csv(pfad,
             header         = TRUE,
             sep            = ",",
             dec            = ".",
             fileEncoding   = "UTF-8",
             stringsAsFactors = FALSE,
             na.strings     = c("", "NA")),
    error = function(e) {
      cat("FEHLER beim Einlesen:", conditionMessage(e), "\n")
      NULL
    }
  )

  if (is.null(df)) {
    fehler_count <- fehler_count + 1
    next
  }

  # Spaltenpruefung
  fehlende_spalten <- setdiff(PFLICHT_SPALTEN, colnames(df))
  if (length(fehlende_spalten) > 0) {
    cat("WARNUNG -- fehlende Spalten:", paste(fehlende_spalten, collapse = ", "), "\n")
    fehler_count <- fehler_count + 1
    next
  }

  # Nur die Pflichtspalten behalten (keine Extra-Spalten im Output)
  df <- df[, PFLICHT_SPALTEN, drop = FALSE]

  cat("OK (", nrow(df), "Zeilen)\n")
  liste_dfs[[length(liste_dfs) + 1]] <- df
}

cat("\n")

if (length(liste_dfs) == 0) {
  cat("Keine gueltigen Dateien eingelesen. Abbruch.\n")
  quit(status = 1)
}

if (fehler_count > 0) {
  cat("Hinweis:", fehler_count, "Datei(en) konnten nicht eingelesen werden (siehe oben).\n\n")
}

# --- 3. Zusammenfuehren -------------------------------------------------------

combined <- do.call(rbind, liste_dfs)

cat("Zusammengefuehrt:", nrow(combined), "Zeilen aus", length(liste_dfs), "Dateien.\n\n")

# --- 4. IDs vergeben ----------------------------------------------------------
# Fortlaufende IDs nur wenn id-Spalte leer oder NA

if (all(is.na(combined$id) | combined$id == "")) {
  combined$id <- seq_len(nrow(combined))
  cat("Neue IDs vergeben: 1 bis", nrow(combined), "\n")
} else {
  # Nur fehlende IDs auffuellen
  fehlende_ids <- is.na(combined$id) | combined$id == ""
  if (any(fehlende_ids)) {
    max_id <- max(as.integer(combined$id[!fehlende_ids]), na.rm = TRUE)
    combined$id[fehlende_ids] <- seq(max_id + 1L, max_id + sum(fehlende_ids))
    cat("Fehlende IDs ergaenzt ab ID", max_id + 1L, "\n")
  }
}

# ID als Integer sicherstellen
combined$id <- as.integer(combined$id)

# Nach ID sortieren
combined <- combined[order(combined$id), ]

# --- 5. Kurze Qualitaetspruefung ----------------------------------------------

cat("\n--- Qualitaetspruefung ---\n")
cat("Zeilen gesamt:  ", nrow(combined), "\n")
cat("Spalten gesamt: ", ncol(combined), "\n\n")

cat("Verteilung nach 'source':\n")
print(table(combined$source, useNA = "ifany"))

cat("\nFehlende Werte je Spalte (nur Spalten mit NAs):\n")
na_pro_spalte <- colSums(is.na(combined))
na_spalten    <- na_pro_spalte[na_pro_spalte > 0]
if (length(na_spalten) > 0) {
  print(na_spalten)
} else {
  cat("  Keine fehlenden Werte.\n")
}

# --- 6. Ausgabe schreiben -----------------------------------------------------

# Ausgabeordner anlegen falls nicht vorhanden
if (!dir.exists(dirname(AUSGABE_PFAD))) {
  dir.create(dirname(AUSGABE_PFAD), recursive = TRUE)
}

write.csv(combined,
          file          = AUSGABE_PFAD,
          row.names     = FALSE,
          fileEncoding  = "UTF-8",
          na            = "")

cat("\nErfolgreich geschrieben:", AUSGABE_PFAD, "\n")
cat("=== Merge abgeschlossen ===\n")
