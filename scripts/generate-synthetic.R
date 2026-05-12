# =============================================================================
# generate-synthetic.R
# Synthetische Fragebogen-Daten fuer den M15-Workshop generieren
#
# Zweck:
#   Studierende fuhren dieses Skript aus, um 3-5 plausible, aber erfundene
#   Datensatze zu erzeugen. Das dient dem Verstandnis von:
#   - Korrelationsmatrizen (wie hangt alles zusammen?)
#   - Seeds (Reproduzierbarkeit)
#   - Dem Unterschied zwischen synthetisch, erfunden und echt
#
# Benutzung:
#   source("scripts/generate-synthetic.R")
#   generate_synth(n = 5, seed = 42, scenario = "realistic")
#
# Argumente:
#   n        : Anzahl Zeilen (Default: 5)
#   seed     : Zufallszahl fuer Reproduzierbarkeit (Default: 42)
#   scenario : "realistic" (moderate Korrelationen) oder
#              "biased" (extreme Korrelationen, fuer Unterrichtsdiskussion)
#   student_nr: Zweistellige Nummer fuer Dateinamen, z.B. "01" (Default: "01")
#
# Output:
#   data/raw/synth-student-XX.csv
# =============================================================================

generate_synth <- function(n = 5,
                           seed = 42,
                           scenario = "realistic",
                           student_nr = "01") {

  # --- 0. Pakete pruefen -------------------------------------------------------
  # Das Paket MASS wird benoetigt, um korrelierte Zufallsdaten zu erzeugen.
  # Es ist Teil von R und muss nicht extra installiert werden.
  if (!requireNamespace("MASS", quietly = TRUE)) {
    stop("Das Paket 'MASS' wird benoetigt. Bitte mit install.packages('MASS') installieren.")
  }

  # --- 1. Seed setzen ----------------------------------------------------------
  # Ein Seed stellt sicher, dass alle die gleichen "Zufalls"-Zahlen bekommen,
  # wenn sie den gleichen Seed verwenden. Das ist Reproduzierbarkeit!
  set.seed(seed)

  # --- 2. Korrelationsstruktur definieren --------------------------------------
  # Wir arbeiten mit 5 "latenten" Dimensionen:
  #   E = Extraversion, V = Vertraglichkeit, G = Gewissenhaftigkeit,
  #   N = Neurotizismus, O = Offenheit
  # Dazu 3 Outcome-Dimensionen: SWLS (Lebenszufriedenheit), PHQ (Depressivitat), GAD (Angst)

  if (scenario == "realistic") {
    # Moderate, psychologisch plausible Korrelationen
    # Zeilen/Spalten: E, V, G, N, O, SWLS, PHQ, GAD
    korrelation_matrix <- matrix(c(
    #  E     V     G     N     O   SWLS   PHQ   GAD
      1.00, 0.20, 0.10,-0.30, 0.25, 0.35,-0.20,-0.20,  # E
      0.20, 1.00, 0.15,-0.10, 0.15, 0.20,-0.10,-0.10,  # V
      0.10, 0.15, 1.00,-0.20, 0.10, 0.25,-0.15,-0.10,  # G
     -0.30,-0.10,-0.20, 1.00,-0.15,-0.40, 0.55, 0.50,  # N
      0.25, 0.15, 0.10,-0.15, 1.00, 0.20,-0.10,-0.10,  # O
      0.35, 0.20, 0.25,-0.40, 0.20, 1.00,-0.45,-0.40,  # SWLS
     -0.20,-0.10,-0.15, 0.55,-0.10,-0.45, 1.00, 0.65,  # PHQ
     -0.20,-0.10,-0.10, 0.50,-0.10,-0.40, 0.65, 1.00   # GAD
    ), nrow = 8, ncol = 8)

  } else if (scenario == "biased") {
    # Extreme Korrelationen -- fuer die Unterrichtsdiskussion ueber Bias
    # Hier sieht alles "zu sauber" aus: alle neurotisch, kaum zufrieden.
    # Das wuerde man bei einer verzerrten Stichprobe sehen.
    korrelation_matrix <- matrix(c(
    #  E     V     G     N     O   SWLS   PHQ   GAD
      1.00, 0.10, 0.05,-0.70, 0.15, 0.60,-0.55,-0.55,  # E
      0.10, 1.00, 0.10,-0.05, 0.10, 0.15,-0.05,-0.05,  # V
      0.05, 0.10, 1.00,-0.10, 0.05, 0.20,-0.10,-0.05,  # G
     -0.70,-0.05,-0.10, 1.00,-0.10,-0.80, 0.80, 0.75,  # N
      0.15, 0.10, 0.05,-0.10, 1.00, 0.15,-0.08,-0.08,  # O
      0.60, 0.15, 0.20,-0.80, 0.15, 1.00,-0.75,-0.70,  # SWLS
     -0.55,-0.05,-0.10, 0.80,-0.08,-0.75, 1.00, 0.80,  # PHQ
     -0.55,-0.05,-0.05, 0.75,-0.08,-0.70, 0.80, 1.00   # GAD
    ), nrow = 8, ncol = 8)

  } else {
    stop("Unbekanntes Szenario: '", scenario, "'. Bitte 'realistic' oder 'biased' verwenden.")
  }

  # Sicherstellen, dass die Matrix positiv-definit ist (technische Voraussetzung)
  # Das ist notwendig damit MASS::mvrnorm() funktioniert
  korrelation_matrix <- (korrelation_matrix + t(korrelation_matrix)) / 2
  diag(korrelation_matrix) <- 1

  # --- 3. Latente Normalwerte ziehen -------------------------------------------
  # MASS::mvrnorm erzeugt normalverteilte Zufallswerte mit der gewuenschten
  # Korrelationsstruktur. Diese sind noch nicht auf Likert-Skalen begrenzt.
  latent <- MASS::mvrnorm(n = n,
                          mu = rep(0, 8),
                          Sigma = korrelation_matrix,
                          empirical = FALSE)
  colnames(latent) <- c("E", "V", "G", "N", "O", "SWLS_lat", "PHQ_lat", "GAD_lat")

  # --- 4. Hilfsfunktion: Normalwert -> Likert -----------------------------------
  # Konvertiert einen normalverteilten Wert in einen Likert-Wert (1 bis max_val)
  # durch Einteilung in gleich grosse Quantile.
  zu_likert <- function(x, max_val) {
    # Quantile-Grenzen bestimmen
    grenzen <- qnorm(seq(0, 1, length.out = max_val + 1))
    # Wert in Kategorie einteilen
    wert <- findInterval(x, grenzen[-c(1, length(grenzen))]) + 1
    # Auf gueltigen Bereich begrenzen (Sicherheitsnetz)
    pmin(pmax(wert, 1L), as.integer(max_val))
  }

  # --- 5. BFI-Items generieren (je 4-5 Items pro Subskala, 1-5) ----------------
  # Jedes Item bekommt leicht unterschiedliche Streuung um die latente Dimension
  bfi_item <- function(latent_dim, n_items, jitter_sd = 0.4) {
    sapply(seq_len(n_items), function(i) {
      roh <- latent_dim + rnorm(n, 0, jitter_sd)
      zu_likert(roh, 5L)
    })
  }

  E_items <- bfi_item(latent[, "E"],  4)
  V_items <- bfi_item(latent[, "V"],  4)
  G_items <- bfi_item(latent[, "G"],  4)
  N_items <- bfi_item(latent[, "N"],  4)
  O_items <- bfi_item(latent[, "O"],  5)

  # Spaltennamen vergeben
  colnames(E_items) <- paste0("BFI_E", 1:4)
  colnames(V_items) <- paste0("BFI_V", 1:4)
  colnames(G_items) <- paste0("BFI_G", 1:4)
  colnames(N_items) <- paste0("BFI_N", 1:4)
  colnames(O_items) <- paste0("BFI_O", 1:5)

  # --- 6. SWLS-Items generieren (5 Items, 1-7) ---------------------------------
  swls_items <- sapply(1:5, function(i) {
    roh <- latent[, "SWLS_lat"] + rnorm(n, 0, 0.4)
    zu_likert(roh, 7L)
  })
  colnames(swls_items) <- paste0("SWLS_", 1:5)

  # --- 7. PHQ-2 und GAD-2 Items generieren (je 2 Items, 0-3) ------------------
  # Skalierung: 0-3 (4 Stufen)
  phq_items <- sapply(1:2, function(i) {
    roh <- latent[, "PHQ_lat"] + rnorm(n, 0, 0.4)
    zu_likert(roh, 4L) - 1L  # 1-4 -> 0-3
  })
  colnames(phq_items) <- c("PHQ2_1", "PHQ2_2")

  gad_items <- sapply(1:2, function(i) {
    roh <- latent[, "GAD_lat"] + rnorm(n, 0, 0.4)
    zu_likert(roh, 4L) - 1L  # 1-4 -> 0-3
  })
  colnames(gad_items) <- c("GAD2_1", "GAD2_2")

  # --- 8. Demografik generieren -------------------------------------------------
  age        <- sample(22:45, n, replace = TRUE)
  gender     <- sample(c("w", "m", "d"), n, replace = TRUE,
                       prob = c(0.60, 0.25, 0.15))
  semester   <- sample(1:4, n, replace = TRUE)
  stats_prior <- sample(1:5, n, replace = TRUE,
                        prob = c(0.20, 0.35, 0.25, 0.15, 0.05))
  therapy_exp <- sample(0:24, n, replace = TRUE)

  # --- 9. Datensatz zusammenbauen -----------------------------------------------
  df <- data.frame(
    id         = NA_integer_,         # wird beim Merge vergeben
    source     = "synth_student",
    student_id = paste0("synth-", seed),
    age        = age,
    gender     = gender,
    semester   = semester,
    stats_prior = stats_prior,
    therapy_exp = therapy_exp,
    E_items,
    V_items,
    G_items,
    N_items,
    O_items,
    swls_items,
    phq_items,
    gad_items,
    stringsAsFactors = FALSE
  )

  # --- 10. Ausgabepfad und Datei schreiben --------------------------------------
  # Ordner erstellen falls nicht vorhanden
  if (!dir.exists("data/raw")) {
    dir.create("data/raw", recursive = TRUE)
  }

  dateiname  <- paste0("data/raw/synth-student-", student_nr, ".csv")

  write.csv(df, file = dateiname, row.names = FALSE, fileEncoding = "UTF-8")

  # --- 11. Rueckmeldung an Benutzer ---------------------------------------------
  cat("======================================================\n")
  cat("Synthetische Daten erfolgreich generiert!\n")
  cat("------------------------------------------------------\n")
  cat("Szenario:   ", scenario, "\n")
  cat("Seed:       ", seed, "\n")
  cat("Zeilen:     ", nrow(df), "\n")
  cat("Spalten:    ", ncol(df), "\n")
  cat("Gespeichert:", dateiname, "\n")
  cat("======================================================\n")
  cat("\nErste Zeilen des Datensatzes:\n")
  print(head(df[, c("source", "student_id", "age", "gender",
                    "BFI_E1", "BFI_N1", "SWLS_1", "PHQ2_1", "GAD2_1")]))

  # Datensatz unsichtbar zurueckgeben (kann mit <- gespeichert werden)
  invisible(df)
}

# =============================================================================
# Beispielaufruf (auskommentiert, um beim source() nicht automatisch auszufuehren)
# Einfach die Zeile markieren und mit Strg+Enter ausfuehren.
# =============================================================================

# generate_synth(n = 5, seed = 42, scenario = "realistic", student_nr = "01")
# generate_synth(n = 5, seed = 99, scenario = "biased",    student_nr = "02")
