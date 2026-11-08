# scripts/freeze-dataset.R
#
# Friert den aktuellen Gesamtdatensatz für eine Gruppe ein.
#   Aufruf aus dem Repo-Root: Rscript scripts/freeze-dataset.R <gruppe>
#   Ausgabe: data/combined/eingefroren/<gruppe>/
#            m15-gesamt.csv, meta-studien.csv, merge-report.md, pruefbericht.md
#
# Ablauf:
#   1. Reparieren: Formal Falsches wird geleert oder entfernt, jede Änderung
#      steht im Prüfbericht. Plausible, aber inhaltlich falsche Werte (etwa
#      vergessenes Umpolen) kann kein Prüfer erkennen.
#   2. Rechen-Check: Die Analysen laufen so, wie die Studierenden rechnen.
#   3. Nur wenn alles besteht, wird der Ordner geschrieben (erst temporär,
#      dann umbenennen). Ein vorhandener Ordner wird nie überschrieben.
#
# Nur base R. Bereiche und Spalten kommen aus merge-uploads.R.

options(scipen = 100)

IN_CSV    <- "data/combined/m15-gesamt.csv"
IN_REPORT <- "data/combined/merge-report.md"
OUT_ROOT  <- "data/combined/eingefroren"
MIN_FAELLE <- 30          # so viele vollständige Fälle braucht jede Skala
MIN_GRUPPE <- 5           # so viele Personen braucht jede Gruppe im Vergleich

fail <- function(...) {
  cat(sprintf("::error::Nicht eingefroren: %s\n", paste0(...)))
  quit(save = "no", status = 1)
}

m <- new.env()
sys.source("scripts/meta-studien.R", envir = m, keep.source = FALSE)
for (e in parse("scripts/merge-uploads.R", encoding = "UTF-8")) {
  # nur die Konstanten, damit Bereiche und Spalten nur an einer Stelle stehen
  if (is.call(e) && identical(e[[1]], as.name("<-")) &&
      is.name(e[[2]]) && as.character(e[[2]]) %in% c("DEMO", "BFI", "COLS", "RANGE")) eval(e, m)
}

# Gibt die Zeilennummern und alten Werte der Reparaturen zurück
reparieren <- function(x) {
  log <- list()
  notiere <- function(z, sp, alt, was) {
    if (length(z)) log[[length(log) + 1]] <<- data.frame(zeile = z, id = x$id[z], spalte = sp, alt = alt, aktion = was)
  }
  zahl <- function(v) suppressWarnings(as.numeric(v))

  # Zellen der Codebook-Spalten
  for (sp in m$COLS) {
    v <- x[[sp]]
    n <- zahl(v)
    bad <- !is.na(v) & (is.na(n) | !is.finite(n))
    if (sp %in% names(m$RANGE)) {
      r <- m$RANGE[[sp]]
      bad <- bad | (!is.na(n) & is.finite(n) & (n < r[1] | n > r[2] | n != round(n)))
    }
    z <- which(bad)
    notiere(z, sp, v[z], "geleert")
    x[[sp]][z] <- NA
  }

  # quelle_datensatz
  n <- zahl(x$quelle_datensatz)
  z <- which(!is.na(x$quelle_datensatz) & (is.na(n) | !(n %in% 1:10)))
  notiere(z, "quelle_datensatz", x$quelle_datensatz[z], "geleert")
  x$quelle_datensatz[z] <- NA

  # Studierenden-Zeilen ohne jede BFI-Antwort
  z <- which(x$quelle == "studierende" & rowSums(!is.na(x[m$BFI])) == 0)
  notiere(z, "(Zeile)", "keine BFI-Antwort", "entfernt")
  entfernen <- z

  # exakte Doppelzeilen unter den Studierenden
  st <- setdiff(which(x$quelle == "studierende"), entfernen)
  key <- apply(x[st, m$COLS, drop = FALSE], 1, paste, collapse = "|")
  z <- st[duplicated(key)]
  notiere(z, "(Zeile)", "exakte Doppelzeile", "entfernt")
  entfernen <- c(entfernen, z)

  # id: fehlend, keine ganze Zahl oder doppelt
  id <- zahl(x$id)
  behalten <- setdiff(seq_len(nrow(x)), entfernen)
  gueltig <- !is.na(id) & is.finite(id) & id == round(id)
  gueltig[entfernen] <- FALSE
  gueltig[behalten[duplicated(id[behalten])]] <- FALSE
  z <- setdiff(which(!gueltig), entfernen)
  if (length(z)) {
    neu <- max(c(0, id[gueltig])) + seq_along(z)
    notiere(z, "id", x$id[z], paste("neu vergeben:", neu))
    x$id[z] <- as.character(neu)
  }

  if (length(entfernen)) x <- x[-entfernen, , drop = FALSE]
  rownames(x) <- NULL
  list(x = x, log = if (length(log)) do.call(rbind, log) else NULL)
}

# Gibt die nicht bestandenen Prüfungen zurück (leer = alles gut)
rechen_check <- function(x, meta) {
  probleme <- character(0)
  nicht_ok <- function(...) probleme <<- c(probleme, paste0(...))
  gen <- x[x$quelle == "generiert", ]
  n_ds <- vapply(1:10, function(i) sum(gen$quelle_datensatz == i, na.rm = TRUE), 0L)
  if (any(n_ds != 9)) nicht_ok("Übungsdatensätze unvollständig (Zeilen je Datensatz: ", paste(n_ds, collapse = ", "), ")")

  sc <- function(i) rowSums(as.matrix(x[sprintf("BFI_%02d", i)])) / length(i)
  sk <- list(E = sc(c(1, 6, 11, 16)), V = sc(c(2, 7, 12, 17)), G = sc(c(3, 8, 13, 18)),
             N = sc(c(4, 9, 14, 19)), O = sc(c(5, 10, 15, 20, 21)))
  for (s in names(sk)) {
    k <- sum(!is.na(sk[[s]]))
    if (k < MIN_FAELLE) nicht_ok("Skala ", s, ": nur ", k, " vollständige Fälle")
    else if (!isTRUE(sd(sk[[s]], na.rm = TRUE) > 0)) nicht_ok("Skala ", s, ": keine Streuung")
  }
  for (g in 1:2) {
    k <- sum(x$geschlecht == g, na.rm = TRUE)
    if (k < MIN_GRUPPE) nicht_ok("geschlecht = ", g, ": nur ", k, " Personen")
  }
  bg <- ifelse(x$bildungsgrad >= 6, "hoch", "niedrig")
  for (g in c("hoch", "niedrig")) {
    k <- sum(bg == g, na.rm = TRUE)
    if (k < MIN_GRUPPE) nicht_ok("Bildungsgruppe ", g, ": nur ", k, " Personen")
  }

  if (!length(probleme)) {
    laeuft <- function(name, expr) {
      p <- tryCatch(suppressWarnings(expr), error = function(e) NA_real_)
      if (!length(p) || !is.finite(p[1])) nicht_ok(name, " lässt sich nicht berechnen")
    }
    laeuft("Korrelation", cor.test(sk$E, sk$O)$p.value)
    laeuft("t-Test", t.test(sk$E[x$geschlecht %in% 1:2] ~ x$geschlecht[x$geschlecht %in% 1:2])$p.value)
    laeuft("Regression", summary(lm(sk$N ~ sk$E + sk$G))$coefficients[2, 4])
    laeuft("ANOVA", summary(aov(sk$E ~ factor(x$bildungsgrad)))[[1]][1, 5])
  }

  if (nrow(meta) != 10) nicht_ok("Meta-Tabelle hat ", nrow(meta), " statt 10 Zeilen")
  if (nrow(meta) && !all(is.finite(as.matrix(meta[c("n", "r", "fisher_z", "se")]))))
    nicht_ok("Meta-Tabelle enthält nicht endliche Werte")
  probleme
}

main <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  gruppe <- if (length(args) == 1) args else ""
  if (!grepl("^[a-z0-9]+(-[a-z0-9]+)*$", gruppe) || nchar(gruppe) > 40)
    fail("Gruppenname ungültig. Erlaubt sind Kleinbuchstaben, Ziffern und Bindestriche, z. B. diemerling")
  ziel <- file.path(OUT_ROOT, gruppe)
  if (file.exists(ziel))
    fail("Für die Gruppe „", gruppe, "“ gibt es schon einen eingefrorenen Stand. Er wird nicht überschrieben")
  if (!file.exists(IN_CSV) || !file.exists(IN_REPORT)) fail(IN_CSV, " oder ", IN_REPORT, " fehlt")
  report <- readLines(IN_REPORT, encoding = "UTF-8", warn = FALSE)
  if (any(grepl("Der Merge ist abgebrochen", report, fixed = TRUE)))
    fail("Der letzte Merge ist abgebrochen, der Gesamtdatensatz ist nicht aktuell. Siehe ", IN_REPORT)

  x <- tryCatch(read.csv(IN_CSV, colClasses = "character", check.names = FALSE, na.strings = "", encoding = "UTF-8"),
                error = function(e) fail("Gesamtdatensatz nicht lesbar: ", conditionMessage(e)))
  spalten <- c("id", m$COLS, "quelle", "quelle_datensatz", "quelle_datei")
  fehlt <- setdiff(spalten, names(x))
  if (length(fehlt)) fail("Spalten fehlen im Gesamtdatensatz: ", paste(fehlt, collapse = ", "))
  zusatz <- setdiff(names(x), spalten)
  x <- x[spalten]
  falsch <- which(is.na(x$quelle) | !(x$quelle %in% c("generiert", "studierende")))
  if (length(falsch)) fail("Unbekannter Wert in Spalte quelle, Zeile ", paste(head(falsch, 5), collapse = ", "))

  rep <- reparieren(x)
  x <- rep$x
  zahlen <- x
  for (sp in c("id", m$COLS, "quelle_datensatz")) zahlen[[sp]] <- as.numeric(x[[sp]])
  meta <- m$meta_studien(zahlen)
  probleme <- rechen_check(zahlen, meta)
  if (length(probleme)) fail(paste(probleme, collapse = "; "))

  # Bericht
  n_st <- sum(x$quelle == "studierende")
  n_rep <- if (is.null(rep$log)) 0 else nrow(rep$log)
  zelle <- function(v) gsub("|", "\\|", ifelse(is.na(v), "(leer)", v), fixed = TRUE)
  bericht <- c(
    sprintf("# Prüfbericht für die Gruppe %s", gruppe), "",
    sprintf("Eingefroren am %s.", format(Sys.time(), "%d.%m.%Y %H:%M", tz = "Europe/Berlin")), "",
    sprintf("**Datensatz:** %d Zeilen (%d vorgeneriert, %d fiktive Personen).", nrow(x), nrow(x) - n_st, n_st), "",
    "**Rechen-Check:** bestanden (Übungsdatensätze vollständig, fünf Skalen, Gruppenvergleiche, Korrelation, t-Test, Regression, ANOVA, Meta-Tabelle).", "",
    if (length(zusatz)) c(sprintf("Zusatzspalten entfernt: %s", paste(zusatz, collapse = ", ")), "") else NULL,
    sprintf("## Reparaturen (%d)", n_rep), "",
    if (n_rep == 0) "Keine. Der Gesamtdatensatz ging unverändert ein."
    else c("Zeile = Datenzeile im Gesamtdatensatz vor der Reparatur.", "",
           "| Zeile | id | Spalte | Alter Wert | Aktion |", "|---:|---|---|---|---|",
           sprintf("| %d | %s | %s | %s | %s |", rep$log$zeile, zelle(rep$log$id), rep$log$spalte, zelle(rep$log$alt), rep$log$aktion))
  )

  # Erst vollständig temporär schreiben, dann umbenennen
  dir.create(OUT_ROOT, showWarnings = FALSE, recursive = TRUE)
  tmp <- file.path(OUT_ROOT, sprintf(".tmp-%s-%d", gruppe, Sys.getpid()))
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  write.csv(x, file.path(tmp, "m15-gesamt.csv"), row.names = FALSE, na = "", fileEncoding = "UTF-8",
            quote = which(names(x) %in% c("quelle", "quelle_datei")))
  write.csv(meta, file.path(tmp, "meta-studien.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  if (!file.copy(IN_REPORT, file.path(tmp, "merge-report.md"))) fail("Merge-Report nicht kopierbar")
  con <- file(file.path(tmp, "pruefbericht.md"), open = "w", encoding = "UTF-8")
  writeLines(bericht, con)
  close(con)
  if (!file.rename(tmp, ziel)) fail("Ordner ", ziel, " konnte nicht angelegt werden")
  cat(sprintf("Eingefroren: %s (%d Zeilen, %d fiktive Personen, %d Reparaturen)\n", ziel, nrow(x), n_st, n_rep))
}

if (sys.nframe() == 0) main()
