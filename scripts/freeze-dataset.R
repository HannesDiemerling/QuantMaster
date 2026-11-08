# scripts/freeze-dataset.R
#
# Friert den aktuellen Gesamtdatensatz für eine Gruppe ein.
#   Aufruf aus dem Repo-Root:
#     python scripts/repair-uploads.py data/student-uploads <arbeitsordner>
#     Rscript scripts/freeze-dataset.R <gruppe> <arbeitsordner>
#   Ausgabe: data/combined/eingefroren/<gruppe>/
#            m15-gesamt.csv, meta-studien.csv, merge-report.md, pruefbericht.md,
#            zuordnung.csv (Kennziffer, id, Datei, Konto, Lehrperson)
#
# Ablauf:
#   1. Merge über die reparierten Kopien der Uploads (repair-uploads.py). Wurde ein
#      Arbeitsordner angegeben, aber die Reparatur lief nicht durch, bricht der Lauf ab.
#      Nur ohne Arbeitsordner (lokal) werden die Originale genommen, der Bericht sagt das.
#   2. Reparieren: Formal Falsches wird geleert oder entfernt, jede Änderung
#      steht im Prüfbericht. Plausible, aber inhaltlich falsche Werte (etwa
#      vergessenes Umpolen) kann kein Prüfer erkennen.
#   3. Rechen-Check: Die Analysen laufen so, wie die Studierenden rechnen.
#   4. Nur wenn alles besteht, wird der Ordner geschrieben (erst temporär,
#      dann umbenennen). Ein vorhandener Ordner wird nie überschrieben.
#
# Der Live-Stand in data/combined/ bleibt dabei unberührt.
#
# Nur base R. Bereiche und Spalten kommen aus merge-uploads.R.

options(scipen = 100)

UPLOADS   <- "data/student-uploads"
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

# Führt den Merge über die reparierten Kopien aus. Gibt Eingabedateien und Upload-Log zurück.
merge_lauf <- function(arbeit) {
  test <- Sys.getenv("FREEZE_TEST_INPUT")   # nur für Tests: fertige Merge-Ausgabe verwenden
  if (nzchar(test)) return(list(ordner = test, log = NULL, repariert = TRUE, up = UPLOADS))
  up <- UPLOADS
  log <- NULL
  repariert <- FALSE
  if (!is.na(arbeit)) {
    # Das Log entsteht erst am Ende des Laufs: Fehlt es, ist die Reparatur nicht durchgelaufen
    f <- file.path(arbeit, "reparatur-log.tsv")
    if (!file.exists(f) || !dir.exists(file.path(arbeit, "uploads")))
      fail("Die Upload-Reparatur ist nicht durchgelaufen, ohne sie würden Personen fehlen. Lauf neu starten")
    up <- normalizePath(file.path(arbeit, "uploads"), winslash = "/")
    repariert <- TRUE
    z <- readLines(f, encoding = "UTF-8", warn = FALSE)
    z <- z[grepl("\t", z, fixed = TRUE)]
    if (length(z)) log <- data.frame(datei = sub("\t.*$", "", z), was = sub("^[^\t]*\t", "", z))
    if (any(grepl("nicht installiert", z, fixed = TRUE)))
      fail("Python-Pakete fehlen (Schritt „Pakete für Numbers und Excel“), Numbers- oder Excel-Dateien wären verloren. Lauf neu starten")
  } else {
    cat("::warning::Ohne Arbeitsordner: Uploads wurden nicht repariert.\n")
  }
  aus <- tempfile("freeze-merge-")
  dir.create(aus)
  aus <- normalizePath(aus, winslash = "/")
  Sys.setenv(MERGE_UPLOAD_DIR = up, MERGE_OUT_DIR = aus, MERGE_KONTEN_CSV = file.path(aus, "konten-roh.csv"))
  rc <- system2(file.path(R.home("bin"), "Rscript"), "scripts/merge-uploads.R")
  Sys.unsetenv(c("MERGE_UPLOAD_DIR", "MERGE_OUT_DIR", "MERGE_KONTEN_CSV"))
  if (rc != 0) fail("Merge beendet mit Status ", rc)
  list(ordner = aus, log = log, repariert = repariert, up = up)
}

# Gibt die Zeilennummern und alten Werte der Reparaturen zurück. konten: je Zeile von x die
# Spalten konto und lehr (kann fehlen)
reparieren <- function(x, konten = NULL) {
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

  # dieselbe Person unter anderer Nummer, etwa erneut hochgeladen mit längerer Kennziffer.
  # Fast leere Zeilen zählen nicht, dort wären gleiche Werte Zufall.
  st <- setdiff(which(x$quelle == "studierende"), entfernen)
  ohne_nr <- setdiff(m$COLS, "probandennummer")
  st <- st[rowSums(!is.na(x[st, ohne_nr, drop = FALSE])) >= 20]
  if (length(st) > 1) {
    key <- apply(x[st, ohne_nr, drop = FALSE], 1, paste, collapse = "|")
    nr <- ifelse(is.na(x$probandennummer[st]), "", x$probandennummer[st])
    # Nur wenn eine Nummer das Ende oder der Anfang der anderen ist (817 und 81712).
    # Zwei verschiedene Kennziffern mit gleichen Werten bleiben zwei Personen.
    bester <- order(-nchar(nr))              # die längste Kennziffer bleibt
    z <- integer(0)
    # Lehrpersonen laden für andere hoch, dort zählt nur die Kennziffer. Zwei verschiedene
    # Konten sind zwei Personen. In beiden Fällen bleiben beide Zeilen.
    lehr <- function(k) !is.null(konten) && isTRUE(konten$lehr[st[k]])
    fremd <- function(p, q) !is.null(konten) && nzchar(konten$konto[st[p]]) && nzchar(konten$konto[st[q]]) &&
      tolower(konten$konto[st[p]]) != tolower(konten$konto[st[q]])
    for (i in seq_along(bester)) for (j in seq_len(i - 1)) {
      a <- nr[bester[i]]; b <- nr[bester[j]]
      if (key[bester[i]] == key[bester[j]] && nzchar(a) && (startsWith(b, a) || endsWith(b, a)) &&
          !lehr(bester[i]) && !lehr(bester[j]) && !fremd(bester[i], bester[j])) {
        z <- c(z, st[bester[i]])
        break
      }
    }
    notiere(z, "(Zeile)", x$probandennummer[z], "entfernt: gleiche Person wie eine andere Datei, nur andere Nummer")
    entfernen <- c(entfernen, z)
  }

  # id: fehlend, keine ganze Zahl oder doppelt
  id <- zahl(x$id)
  behalten <- setdiff(seq_len(nrow(x)), entfernen)
  gueltig <- !is.na(id) & is.finite(id) & id == round(id)
  gueltig[entfernen] <- FALSE
  gueltig[behalten[duplicated(id[behalten])]] <- FALSE
  z <- setdiff(which(!gueltig), entfernen)
  if (length(z)) {
    # unterhalb sehr langer Kennziffern, sonst wären die neuen ids nicht mehr genau
    belegt <- id[gueltig]
    kand <- max(c(0, belegt[belegt < 1e12]))
    neu <- numeric(0)
    while (length(neu) < length(z)) {
      kand <- kand + 1
      if (!(kand %in% belegt)) neu <- c(neu, kand)
    }
    neu <- format(neu, scientific = FALSE, trim = TRUE)
    notiere(z, "id", x$id[z], paste("neu vergeben:", neu))
    x$id[z] <- neu
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
    # dieselben Modelle wie in den Abgaben (Sitzungen 03, 04 und 07)
    laeuft("t-Test Geschlecht", t.test(sk$V[x$geschlecht %in% 1:2] ~ x$geschlecht[x$geschlecht %in% 1:2])$p.value)
    laeuft("t-Test Bildung", t.test(sk$O ~ bg)$p.value)
    laeuft("Regression", summary(lm(sk$N ~ sk$E + sk$V + sk$G + sk$O))$coefficients[2, 4])
    laeuft("ANOVA", summary(aov(sk$O ~ factor(x$bildungsgrad)))[[1]][1, 5])
  }

  if (nrow(meta) != 10) nicht_ok("Meta-Tabelle hat ", nrow(meta), " statt 10 Zeilen")
  if (nrow(meta) && !all(is.finite(as.matrix(meta[c("n", "r", "fisher_z", "se")]))))
    nicht_ok("Meta-Tabelle enthält nicht endliche Werte")
  probleme
}

main <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  gruppe <- if (length(args) %in% 1:2) args[1] else ""
  arbeit <- if (length(args) == 2) args[2] else NA_character_
  if (!grepl("^[a-z0-9]+(-[a-z0-9]+)*$", gruppe) || nchar(gruppe) > 40)
    fail("Gruppenname ungültig. Erlaubt sind Kleinbuchstaben, Ziffern und Bindestriche, z. B. hannes")
  ziel <- file.path(OUT_ROOT, gruppe)
  if (identical(suppressWarnings(system2("git", c("check-ignore", "-q", paste0(ziel, "/")), stdout = FALSE, stderr = FALSE)), 0L))
    fail("Gruppenname „", gruppe, "“ ist in .gitignore gesperrt und ließe sich nicht speichern. Bitte einen anderen Namen wählen")
  if (file.exists(ziel))
    fail("Für die Gruppe „", gruppe, "“ gibt es schon einen eingefrorenen Stand. Er wird nicht überschrieben")
  ml <- merge_lauf(arbeit)
  IN_CSV <- file.path(ml$ordner, "m15-gesamt.csv")
  IN_REPORT <- file.path(ml$ordner, "merge-report.md")
  if (!file.exists(IN_CSV) || !file.exists(IN_REPORT)) fail("Merge hat ", IN_CSV, " oder ", IN_REPORT, " nicht geschrieben")
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
  # Arbeitspfade, __personN und angehängte Endung umgewandelter Dateien durch die echten Namen ersetzen
  echt <- function(v) gsub("\\.(numbers|jasp|xlsx)\\.csv", ".\\1", gsub("__person[0-9]+(\\.[A-Za-z]+)", "\\1", gsub(ml$up, UPLOADS, v, fixed = TRUE)))
  x$quelle_datei <- echt(x$quelle_datei)
  report <- echt(gsub(ml$ordner, ziel, report, fixed = TRUE))
  if (!any(x$quelle == "studierende", na.rm = TRUE))
    fail("Keine einzige fiktive Person im Datensatz. Liegen die Uploads in data/student-uploads/?")
  falsch <- which(is.na(x$quelle) | !(x$quelle %in% c("generiert", "studierende")))
  if (length(falsch)) fail("Unbekannter Wert in Spalte quelle, Zeile ", paste(head(falsch, 5), collapse = ", "))

  # Zuordnung Kennziffer, Datei, Konto aus dem Merge (fehlt nur im Testmodus)
  roh <- file.path(ml$ordner, "konten-roh.csv")
  konten <- if (file.exists(roh)) tryCatch(read.csv(roh, colClasses = "character", check.names = FALSE, na.strings = character(0), encoding = "UTF-8"),
                                           error = function(e) NULL) else NULL
  if (!is.null(konten) && nrow(konten)) konten$quelle_datei <- echt(konten$quelle_datei)
  je_zeile <- function(x) {
    leer <- data.frame(konto = rep("", nrow(x)), lehr = rep(FALSE, nrow(x)))
    if (is.null(konten) || !nrow(konten)) return(leer)
    k <- match(x$quelle_datei, konten$quelle_datei)
    k[x$quelle != "studierende"] <- NA
    data.frame(konto = ifelse(is.na(k), "", konten$konto[k]), lehr = !is.na(k) & konten$lehrperson[k] == "ja")
  }

  rep <- reparieren(x, je_zeile(x))
  x <- rep$x
  zahlen <- x
  for (sp in c("id", m$COLS, "quelle_datensatz")) zahlen[[sp]] <- as.numeric(x[[sp]])
  meta <- m$meta_studien(zahlen)
  probleme <- rechen_check(zahlen, meta)
  if (length(probleme)) fail(paste(probleme, collapse = "; "))

  # Zuordnung für die Abgabenprüfung. Eine Zeile je Upload (mehrere Personen in einer Datei: je eine)
  pn <- ifelse(is.na(x$probandennummer), "", x$probandennummer)
  zuordnung <- data.frame(kennziffer = character(0), id = character(0), datei = character(0), konto = character(0), lehrperson = character(0))
  if (!is.null(konten) && nrow(konten)) {
    k <- match(paste(konten$quelle_datei, konten$kennziffer), paste(x$quelle_datei, pn))
    # ohne brauchbare Kennziffer (keine ganze Zahl, sehr lang): über die Datei
    frei <- is.na(k) & !nzchar(konten$kennziffer)
    k[frei] <- match(konten$quelle_datei[frei], x$quelle_datei)
    zuordnung <- data.frame(kennziffer = konten$kennziffer, id = ifelse(is.na(k), "", x$id[k]), datei = konten$datei,
                            konto = konten$konto, lehrperson = konten$lehrperson)
    zuordnung <- zuordnung[order(zuordnung$datei, zuordnung$kennziffer, method = "radix"), , drop = FALSE]
  }
  ohne_konto <- is.null(konten) || !nrow(konten) || !any(nzchar(konten$konto))

  # Der Abschnitt „Bitte ansehen“ aus dem Merge-Report (Konten mit mehreren Kennziffern)
  ab <- which(report == "## Bitte ansehen")
  bitte <- if (length(ab)) {
    ende <- which(startsWith(report, "## ") & seq_along(report) > ab[1])
    z <- report[(ab[1] + 1):(if (length(ende)) ende[1] - 1 else length(report))]
    z[nzchar(trimws(z))]
  } else character(0)

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
    if (!ml$repariert) c("**Achtung:** Die Upload-Reparatur lief nicht, es wurden die Originaldateien verwendet.", "") else NULL,
    if (ohne_konto) c("**Hinweis:** Keine Konto-Zuordnung, die Git-Historie fehlt (flacher Klon oder kein Repository). `zuordnung.csv` enthält dann nur Kennziffer und Datei.", "") else NULL,
    sprintf("**Nicht verwendet:** %d Datei(en), siehe `merge-report.md`.",
            sum(grepl("| IGNORIERT |", report, fixed = TRUE)) + sum(grepl("^- Ignoriert \\(kein CSV\\)", report))), "",
    if (any(grepl("\\| (DOPPELT|PRÜFEN) \\|", report)) || length(bitte)) c("## Bitte ansehen", "",
      if (any(grepl("\\| (DOPPELT|PRÜFEN) \\|", report))) c("Diese Personen sind enthalten, aber auffällig (etwa zwei Dateien mit derselben Kennziffer).", "",
      report[grepl("^\\| Datei \\|", report)], report[grepl("^\\|---", report)][1],
      report[grepl("\\| (DOPPELT|PRÜFEN) \\|", report)], "") else NULL,
      bitte, if (length(bitte)) "" else NULL) else NULL,
    sprintf("## Reparaturen an den Uploads (%d)", NROW(ml$log)), "",
    if (NROW(ml$log) == 0) "Keine." else c("| Datei | Was |", "|---|---|",
      sprintf("| `%s` | %s |", ml$log$datei, zelle(ml$log$was))),
    "",
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
  con <- file(file.path(tmp, "merge-report.md"), open = "w", encoding = "UTF-8")
  writeLines(report, con)
  close(con)
  con <- file(file.path(tmp, "pruefbericht.md"), open = "w", encoding = "UTF-8")
  writeLines(bericht, con)
  close(con)
  write.csv(zuordnung, file.path(tmp, "zuordnung.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  if (!file.rename(tmp, ziel)) fail("Ordner ", ziel, " konnte nicht angelegt werden")
  cat(sprintf("Eingefroren: %s (%d Zeilen, %d fiktive Personen, %d Reparaturen)\n", ziel, nrow(x), n_st, n_rep))
}

if (sys.nframe() == 0) main()
