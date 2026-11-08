# scripts/antwortbogen.R
#
# Eine Quelle für alle Abgabefelder, ab Sitzung 02 (Sitzung 01 hat keine Abgabe).
# Das Skript schreibt:
#   1. sitzungXX/_abgabe.md   Abgabe-Vorlagen der Studierenden (im Repo, per
#                             {{< include >}} in 99-abschluss.qmd)
#   2. intern/antwortbogen-<gruppe>.md  Sollwerte und akzeptierte Bereiche im
#                             Klartext (gitignored, NIE committen)
#   3. --spec <pfad>          Felddefinitionen für den Prüfer (JSON). Daraus
#                             baut scripts/pruefe-abgaben.py die gehashte
#                             sollwerte.json. Der Pfad muss außerhalb des Repos
#                             oder in intern/ liegen, die Datei enthält Klartext.
#
# Aufruf aus dem Repo-Root:
#   Rscript scripts/antwortbogen.R <gruppe>   -> Klartext-Bogen und Vorlagen
#   Rscript scripts/antwortbogen.R            -> Live-Stand, intern/antwortbogen.md
# Auf GitHub Actions (Workflow "Sollwerte erzeugen"):
#   Rscript scripts/antwortbogen.R <gruppe> --nur-spec --streng --spec <pfad>
#   --nur-spec schreibt nichts außer der Spec-Datei, --streng bricht ab, wenn
#   lme4, lavaan oder BayesFactor fehlen.
# Lokal schreibt das Skript nie sollwerte.json, das macht nur der Workflow.
#
# Benötigt: base R, MASS, lme4, lavaan, BayesFactor. Fehlt ein Paket ohne
# --streng, wird das Feld als "nicht berechnet" markiert.
#
# Felder werden pro Kapitel definiert; Nummern (S04-1, S04-2, ...) vergibt das
# Skript anhand von SITZUNG automatisch. Wer Kapitel verschiebt, ändert nur
# SITZUNG und lässt das Skript neu laufen. Jedes Feld hat einen Typ:
# zahl, ganzzahl, p, intervall, label_zahl, f_df, wahl, text, nicht_berechnet.
# Tipps sind öffentlich und dürfen keine Lösung verraten.

.args <- commandArgs(trailingOnly = TRUE)
SPEC <- NA_character_; NUR_SPEC <- FALSE; STRENG <- FALSE
if ("--spec" %in% .args) {
  i <- match("--spec", .args); SPEC <- .args[i + 1]; .args <- .args[-c(i, i + 1)]
}
if ("--nur-spec" %in% .args) { NUR_SPEC <- TRUE; .args <- setdiff(.args, "--nur-spec") }
if ("--streng" %in% .args) { STRENG <- TRUE; .args <- setdiff(.args, "--streng") }
if (NUR_SPEC && is.na(SPEC)) stop("--nur-spec braucht --spec <pfad>", call. = FALSE)

GRUPPE <- NA_character_
DIR <- "data/combined"
OUT <- "intern/antwortbogen.md"
if (length(.args) >= 1) {
  GRUPPE <- .args[1]
  DIR <- file.path("data/combined/eingefroren", GRUPPE)
  OUT <- sprintf("intern/antwortbogen-%s.md", GRUPPE)
  if (!dir.exists(DIR)) stop("Für die Gruppe '", GRUPPE, "' gibt es keinen eingefrorenen Stand (", DIR, "). Erst git pull, Namen prüfen.", call. = FALSE)
}
DATA <- file.path(DIR, "m15-gesamt.csv")
set.seed(2026)

SITZUNG <- c(kombinieren = 2, deskriptiv = 2,
             skalen = 3, korrelation = 3, ttest = 3,
             meta = 4, regression = 4, moderation = 4,
             mixed = 5, efa = 5, sem = 6, mediation = 6,
             bias = 7, power = 7, anova = 7, nhst = 8, bayes = 8,
             logistisch = 9, epi = 9, quali = 10, abschluss = 10)

d <- read.csv(DATA)

# --- Spalten so bauen, wie die Studierenden sie in JASP bauen ----------------
sc <- function(items) rowSums(d[items]) / length(items)   # NA, sobald ein Item fehlt
d$E_mean <- sc(c("BFI_01", "BFI_06", "BFI_11", "BFI_16"))
d$V_mean <- sc(c("BFI_02", "BFI_07", "BFI_12", "BFI_17"))
d$G_mean <- sc(c("BFI_03", "BFI_08", "BFI_13", "BFI_18"))
d$N_mean <- sc(c("BFI_04", "BFI_09", "BFI_14", "BFI_19"))
d$O_mean <- sc(c("BFI_05", "BFI_10", "BFI_15", "BFI_20", "BFI_21"))
d$bildung_gruppe <- ifelse(d$bildungsgrad >= 6, "hoch", "niedrig")
d$N_hoch <- ifelse(d$N_mean >= median(d$N_mean, na.rm = TRUE), 1, 0)
d$E_c <- d$E_mean - mean(d$E_mean, na.rm = TRUE)
d$G_c <- d$G_mean - mean(d$G_mean, na.rm = TRUE)
mw <- d[d$geschlecht %in% 1:2, ]
BFI <- sprintf("BFI_%02d", 1:21)

# --- Felder --------------------------------------------------------------------
fmt <- function(x, k = 2) formatC(as.numeric(x), format = "f", digits = k)
has <- function(p) requireNamespace(p, quietly = TRUE)
rows <- list()
push <- function(kap, frage, typ, soll, bereich, hinweis = "", tipp = "", spec = list()) {
  rows[[length(rows) + 1]] <<- list(kap = kap, frage = frage, typ = typ, soll = as.character(soll)[1],
                                    bereich = bereich, hinweis = hinweis, tipp = tipp, spec = spec)
}
bereich1 <- function(von, bis) list(list(von = von, bis = bis))

add <- function(kap, frage, hinweis = "", tipp = "") push(kap, frage, "text", "Freitext", "–", hinweis, tipp)
num <- function(kap, frage, x, tol, k = 2, hinweis = "", abs = FALSE, tipp = "") {
  x <- as.numeric(x); y <- if (abs) base::abs(x) else x
  v <- fmt(y - tol, k); b <- fmt(y + tol, k)
  push(kap, frage, "zahl", fmt(x, k), paste(v, "bis", b), hinweis, tipp, list(k = k, abs = abs, bereiche = bereich1(v, b)))
}
range2 <- function(kap, frage, a, b, tol, k = 2, hinweis = "", abs = FALSE, tipp = "") {
  w <- as.numeric(c(a, b)); y <- if (abs) base::abs(w) else w
  v <- fmt(min(y) - tol, k); bb <- fmt(max(y) + tol, k)
  push(kap, frage, "zahl", fmt(w[1], k), paste(v, "bis", bb), hinweis, tipp, list(k = k, abs = abs, bereiche = bereich1(v, bb)))
}
zahlen <- function(kap, frage, soll, werte, tol, k = 2, hinweis = "", tipp = "") {
  rng <- lapply(as.numeric(werte), function(w) list(von = fmt(w - tol, k), bis = fmt(w + tol, k)))
  txt <- paste(vapply(rng, function(r) paste(r$von, "bis", r$bis), ""), collapse = " oder ")
  push(kap, frage, "zahl", soll, txt, hinweis, tipp, list(k = k, abs = FALSE, bereiche = rng))
}
ganz <- function(kap, frage, x, von = x, bis = x, hinweis = "", erlaubt = NULL, plausibel = FALSE, tipp = "", soll = x) {
  x <- as.integer(round(as.numeric(x))); von <- as.integer(round(as.numeric(von))); bis <- as.integer(round(as.numeric(bis)))
  b <- if (!is.null(erlaubt)) paste(erlaubt, collapse = " / ") else paste(von, "bis", bis)
  sp <- list(plausibel = plausibel)
  if (!is.null(erlaubt)) sp$erlaubt <- as.list(as.integer(erlaubt)) else sp$bereiche <- bereich1(as.character(von), as.character(bis))
  push(kap, frage, "ganzzahl", soll, b, hinweis, tipp, sp)
}
exact <- function(kap, frage, x, hinweis = "", tipp = "") ganz(kap, frage, x, x, x, hinweis, tipp = tipp)
pval <- function(kap, frage, p, hinweis = "", tipp = "") {
  p <- as.numeric(p)
  if (p < .001) push(kap, frage, "p", "< .001", "< .001 bis .002", hinweis, tipp, list(k = 3, bereiche = bereich1("0.000", "0.002")))
  else { v <- fmt(max(p - .01, 0), 3); b <- fmt(min(p + .01, 1), 3)
    push(kap, frage, "p", fmt(p, 3), paste(v, "bis", b), hinweis, tipp, list(k = 3, bereiche = bereich1(v, b))) }
}
pval_von_bis <- function(kap, frage, soll, von, bis, hinweis = "") {
  v <- fmt(max(von, 0), 3); b <- fmt(min(bis, 1), 3)
  push(kap, frage, "p", soll, paste(v, "bis", b), hinweis, "", list(k = 3, bereiche = bereich1(v, b)))
}
intervall <- function(kap, frage, soll, unten, oben, tol, k = 2, hinweis = "", abs = FALSE, tipp = "") {
  rng <- function(w) lapply(as.numeric(w), function(x) list(von = fmt(x - tol, k), bis = fmt(x + tol, k)))
  push(kap, frage, "intervall", soll, sprintf("Grenzen ±%s", fmt(tol, k)), hinweis, tipp,
       list(k = k, abs = abs, unten = rng(unten), oben = rng(oben)))
}
label_zahl <- function(kap, frage, soll, ok, kand, von, bis, k = 2, hinweis = "", tipp = "") {
  ok <- unique(ifelse(ok %in% names(kand), unname(kand[ok]), ok))   # gehasht wird immer die Kurzform (G statt G_mean)
  push(kap, frage, "label_zahl", soll, paste(paste(ok, collapse = " / "), fmt(von, k), "bis", fmt(bis, k)), hinweis, tipp,
       list(k = k, labels = as.list(ok), kandidaten = as.list(kand), bereiche = bereich1(fmt(von, k), fmt(bis, k))))
}
f_df <- function(kap, frage, df1, df2, f, tol = .01, k = 2, hinweis = "", tipp = "") {
  df1 <- as.integer(df1); df2 <- as.integer(df2)
  v <- fmt(f - tol, k); b <- fmt(f + tol, k)
  push(kap, frage, "f_df", sprintf("F(%d, %d) = %s", df1, df2, fmt(f, k)), paste("F:", v, "bis", b), hinweis, tipp,
       list(k = k, df1 = df1, df2 = df2, bereiche = bereich1(v, b)))
}
wahl <- function(kap, frage, soll, ok, kandidaten = c("ja", "nein"), hinweis = "", tipp = "") {
  push(kap, frage, "wahl", soll, paste(ok, collapse = " oder "), hinweis, tipp, list(ok = as.list(ok), kandidaten = as.list(kandidaten)))
}
# Rechnet einen paketabhängigen Block. Scheitert er, stehen alle Felder als
# "nicht berechnet" da, die Nummern verrutschen nicht. Mit --streng bricht ein
# fehlendes Paket ab, damit der Lauf wiederholt wird.
block <- function(kap, fragen, pkg, expr) {
  if (!has(pkg)) {
    if (STRENG) stop("Paket fehlt: ", pkg, call. = FALSE)
    for (f in fragen) push(kap, f, "nicht_berechnet", sprintf("nicht berechnet (%s fehlt)", pkg), "–")
    return(invisible())
  }
  n0 <- length(rows)
  ok <- tryCatch({ force(expr); TRUE }, error = function(e) { message("Block ", kap, " nicht berechnet: ", conditionMessage(e)); FALSE })
  if (!ok) {
    if (STRENG) stop("Block ", kap, " liess sich nicht berechnen, es wird keine sollwerte.json geschrieben.", call. = FALSE)
    rows <<- rows[seq_len(n0)]
    for (f in fragen) push(kap, f, "nicht_berechnet", "nicht berechnet (Fehler in der Rechnung)", "–")
  }
}

SKALA <- c(E = "E", E_mean = "E", Extraversion = "E", V = "V", V_mean = "V", "Verträglichkeit" = "V", Vertraeglichkeit = "V",
           G = "G", G_mean = "G", Gewissenhaftigkeit = "G", N = "N", N_mean = "N", Neurotizismus = "N",
           O = "O", O_mean = "O", Offenheit = "O")
BFI_ALIAS <- unlist(lapply(1:21, function(i) {
  a <- c(sprintf("BFI_%02d", i), sprintf("BFI_%d", i), sprintf("BFI %02d", i), sprintf("BFI %d", i),
         sprintf("BFI%02d", i), sprintf("BFI%d", i), sprintf("Item %d", i), sprintf("Item %02d", i))
  setNames(rep(sprintf("BFI_%02d", i), length(a)), a)
}))

skew_g1 <- function(x) { x <- na.omit(x); n <- length(x); m <- mean(x)
  (sum((x - m)^3) / n) / (sum((x - m)^2) / n)^1.5 }
skew_G1 <- function(x) { n <- sum(!is.na(x)); skew_g1(x) * sqrt(n * (n - 1)) / (n - 2) }
alpha <- function(items) { x <- na.omit(d[items]); k <- ncol(x)
  k / (k - 1) * (1 - sum(apply(x, 2, var)) / var(rowSums(x))) }
levene <- function(y, g, center) { ok <- !is.na(y) & !is.na(g)
  z <- abs(y[ok] - ave(y[ok], g[ok], FUN = center)); anova(lm(z ~ factor(g[ok])))$`Pr(>F)`[1] }
auc <- function(score, y) { r <- rank(score); n1 <- sum(y == 1); n0 <- sum(y == 0)
  (sum(r[y == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0) }

# === Sitzung 2: Gesamtdatensatz & Deskription ==================================
ganz("kombinieren", "Datenzeilen in Deiner selbst kombinierten Datei (ohne Kopfzeile)", 70, erlaubt = seq(10, 100, by = 10), plausibel = TRUE,
     hinweis = "typisch 70, nur Plausibilität", soll = "10 × Anzahl Dateien",
     tipp = "Jede Datei hat 10 Zeilen. Schau noch einmal, wie viele Dateien Du kombiniert hast.")
exact("kombinieren", "Zeilen in m15-gesamt.csv", nrow(d), tipp = "Nimm die Datei aus dem Ordner Deiner Gruppe, nicht den Live-Stand.")
exact("kombinieren", "Davon Zeilen mit quelle = studierende", sum(d$quelle == "studierende"), tipp = "Frequency table von quelle im Datensatz Deiner Gruppe.")
a <- na.omit(d$alter)
exact("deskriptiv", "Anzahl Personen mit geschlecht = 1", sum(d$geschlecht == 1, na.rm = TRUE), tipp = "Frequency table von geschlecht.")
num("deskriptiv", "Mittelwert alter", mean(a), .01, tipp = "Zwei Nachkommastellen, Datensatz Deiner Gruppe.")
num("deskriptiv", "SD alter", sd(a), .01, tipp = "Zwei Nachkommastellen, Datensatz Deiner Gruppe.")
num("deskriptiv", "Median alter", median(a), 0)
range2("deskriptiv", "IQR alter", IQR(a, type = 7), IQR(a, type = 6), .01, hinweis = "Quantil-Definitionen 6/7")
range2("deskriptiv", "Schiefe (Skewness) alter (3 Nachkommastellen)", skew_G1(a), skew_g1(a), .02, k = 3, tipp = "Hier sind 3 Nachkommastellen gefragt.")
q <- quantile(a, c(.25, .75)); out_n <- sum(a < q[1] - 1.5 * diff(q) | a > q[2] + 1.5 * diff(q))
ganz("deskriptiv", "Anzahl Ausreißer-Punkte im Boxplot alter (abgezählt)", out_n, max(out_n - 1, 0), out_n + 1, hinweis = "±1 ok", tipp = "Punkte über und unter den Whiskern zählen.")
num("deskriptiv", "Mittelwert BFI_06", mean(d$BFI_06, na.rm = TRUE), .01)
tb <- table(d$bildungsgrad)
ganz("deskriptiv", "Häufigste Stufe von bildungsgrad (aus dem Balkendiagramm)", as.integer(names(tb)[which.max(tb)]), erlaubt = as.integer(names(tb)[tb == max(tb)]),
     soll = paste(names(tb)[tb == max(tb)], collapse = " / "), tipp = "Gefragt ist die Stufe, also die Zahl unter dem höchsten Balken, nicht die Höhe des Balkens.")
add("deskriptiv", "Q-Q-Plot alter: Liegen die Punkte auf der Diagonalen? (ja/nein + 1 Satz)",
    hinweis = sprintf("Schiefe = %s, Shapiro p = %s; begründete Antwort in beide Richtungen ok", fmt(skew_G1(a)), fmt(shapiro.test(a)$p.value, 3)))

# === Sitzung 3: Skalen & Effektgrößen ==========================================
exact("skalen", "Anzahl gültiger Werte (Valid) von E_mean", sum(!is.na(d$E_mean)),
      hinweis = "höher -> Mittelwert mit na.rm statt Formel aus dem Kapitel", tipp = "Prüfe, ob Deine Berechnung Zeilen mit fehlenden Items als fehlend zählt.")
num("skalen", "Mittelwert N_mean", mean(d$N_mean, na.rm = TRUE), .01)
num("skalen", "SD N_mean", sd(d$N_mean, na.rm = TRUE), .01)
items <- list(E = c(1, 6, 11, 16), V = c(2, 7, 12, 17), G = c(3, 8, 13, 18), N = c(4, 9, 14, 19), O = c(5, 10, 15, 20, 21))
al <- sapply(items, function(i) alpha(sprintf("BFI_%02d", i)))
label_zahl("skalen", "Skala mit dem höchsten Cronbach's α (Skala + α)", sprintf("%s (%s)", names(which.max(al)), fmt(max(al))),
           names(al)[al >= max(al) - .005], SKALA, max(al) - .02, max(al) + .02,
           hinweis = paste("alle α:", paste(names(al), fmt(al), collapse = ", ")), tipp = "Skala und Wert angeben, zum Beispiel: E 0.80")
label_zahl("skalen", "Skala mit dem niedrigsten Cronbach's α (Skala + α)", sprintf("%s (%s)", names(which.min(al)), fmt(min(al))),
           names(al)[al <= min(al) + .005], SKALA, min(al) - .02, min(al) + .02, tipp = "Skala und Wert angeben, zum Beispiel: E 0.80")
h1 <- cor.test(d$V_mean, d$N_mean)
num("korrelation", "H1: Pearson r (V_mean × N_mean)", h1$estimate, .01)
pval("korrelation", "H1: p-Wert", h1$p.value)
ps <- c(h1$p.value, cor.test(d$O_mean, d$E_mean)$p.value, cor.test(d$G_mean, d$alter)$p.value,
        suppressWarnings(cor.test(d$N_mean, d$bildungsgrad, method = "spearman"))$p.value)
exact("korrelation", "Wie viele der Hypothesen H1–H4 sind signifikant (p < .05, zweiseitig getestet)?", sum(ps < .05),
      hinweis = paste("p H1–H4:", paste(fmt(ps, 3), collapse = ", ")), tipp = "Achte auf zweiseitige Tests und bei H4 auf Spearman.")
add("korrelation", "Eine alternative Erklärung (Drittvariable) für eine der Korrelationen, 1 Satz", hinweis = "plausible Drittvariable")
t1 <- t.test(d$E_mean, mu = 3)
num("ttest", "t1: t", t1$statistic, .01, tipp = "Testwert 3 im Einstichproben-t-Test.")
num("ttest", "t1: Cohen's d", (mean(d$E_mean, na.rm = TRUE) - 3) / sd(d$E_mean, na.rm = TRUE), .01)
t2 <- t.test(V_mean ~ geschlecht, data = mw)
num("ttest", "t2 (Welch-Zeile): t", t2$statistic, .01, hinweis = "Vorzeichen egal (hängt von der Gruppenreihenfolge ab), Betrag zählt", abs = TRUE, tipp = "Welch-Zeile nehmen, nicht Student. Das Vorzeichen zählt hier nicht.")
pval("ttest", "t2 (Welch-Zeile): p", t2$p.value, tipp = "Welch-Zeile nehmen, nicht Student.")
g <- lapply(split(mw$V_mean, mw$geschlecht), na.omit)
d_pool <- (mean(g[[1]]) - mean(g[[2]])) / sqrt(((length(g[[1]]) - 1) * var(g[[1]]) + (length(g[[2]]) - 1) * var(g[[2]])) / (length(g[[1]]) + length(g[[2]]) - 2))
d_avg <- (mean(g[[1]]) - mean(g[[2]])) / sqrt((var(g[[1]]) + var(g[[2]])) / 2)
range2("ttest", "t2: Cohen's d", d_avg, d_pool, .01, hinweis = "Welch-d bzw. gepoolt; Vorzeichen egal, Betrag zählt", abs = TRUE, tipp = "Das Vorzeichen zählt hier nicht.")
t3 <- t.test(O_mean ~ bildung_gruppe, data = d)
pval("ttest", "t3 (Welch-Zeile): p", t3$p.value, tipp = "Welch-Zeile nehmen, nicht Student.")
add("ttest", "Ein Satz: Welche Schlussfolgerung ziehst Du aus t2, und welchen Vorbehalt hast Du?", hinweis = "Effektgröße/KI statt nur p")

# === Sitzung 4: Metaanalyse & Regression =======================================
ms <- read.csv(file.path(DIR, "meta-studien.csv"))
yi <- ms$fisher_z; vi <- ms$se^2; k <- length(yi)
nll <- function(t2) { w <- 1 / (vi + t2); mu <- sum(w * yi) / sum(w)
  0.5 * (sum(log(vi + t2)) + log(sum(w)) + sum(w * (yi - mu)^2)) }
tau2 <- optimize(nll, c(0, 5))$minimum; if (nll(0) <= nll(tau2)) tau2 <- 0
w <- 1 / (vi + tau2); mu <- sum(w * yi) / sum(w); se_mu <- sqrt(1 / sum(w))
wf <- 1 / vi; Q <- sum(wf * (yi - sum(wf * yi) / sum(wf))^2)
s2 <- (k - 1) * sum(wf) / (sum(wf)^2 - sum(wf^2)); I2 <- 100 * tau2 / (tau2 + s2)
r_ds <- setNames(as.list(lapply(ms$r, function(x) list(von = fmt(x - .02), bis = fmt(x + .02)))), sprintf("%d", ms$quelle_datensatz))
push("meta", "r in Deinem eigenen Seed-Datensatz (E_mean × O_mean, 2 Nachkommastellen)", "zahl", "siehe Hinweis", "je Datensatz ±0.02",
     paste("r je Datensatz:", paste(sprintf("%02d: %s", ms$quelle_datensatz, fmt(ms$r)), collapse = "; ")),
     "Datensatz Deiner Kennziffer, Fehlerzeilen bereinigt.", list(k = 2, abs = FALSE, pro_datensatz = r_ds))
num("meta", "Gepoolter Effekt (Fisher-z, Random Effects)", mu, .02)
se_kh <- sqrt(sum(w * (yi - mu)^2) / (k - 1)) * se_mu; t_kh <- qt(.975, k - 1)  # JASP-Standard: Knapp-Hartung
intervall("meta", "95 %-KI des gepoolten Effekts [untere, obere]", sprintf("[%s, %s]", fmt(mu - t_kh * se_kh), fmt(mu + t_kh * se_kh)),
          c(mu - t_kh * se_kh, mu - 1.96 * se_mu), c(mu + t_kh * se_kh, mu + 1.96 * se_mu), .03,
          hinweis = sprintf("JASP-Standard Knapp-Hartung; mit z-Test [%s, %s] ebenfalls ok; Grenzen ±0.03", fmt(mu - 1.96 * se_mu), fmt(mu + 1.96 * se_mu)),
          tipp = "Beide Grenzen in eckigen Klammern, zum Beispiel [0.10, 0.35].")
num("meta", "I² (in %)", I2, 10, k = 1, hinweis = sprintf("Q = %s; bei τ² = 0 ist I² = 0", fmt(Q)))
mA <- lm(N_mean ~ E_mean + V_mean + G_mean + O_mean, data = d); sA <- summary(mA); fA <- sA$fstatistic
num("regression", "Modell A: R²", sA$r.squared, .01)
num("regression", "Modell A: F", fA[1], .01)
pval("regression", "Modell A: p", pf(fA[1], fA[2], fA[3], lower.tail = FALSE))
mf <- model.frame(mA); beta <- coef(mA)[-1] * sapply(mf[-1], sd) / sd(mf[[1]]); bb <- which.max(abs(beta))
label_zahl("regression", "Modell A: stärkster Prädiktor (Variable + β)", sprintf("%s (β = %s)", names(beta)[bb], fmt(beta[bb])),
           names(beta)[abs(abs(beta) - max(abs(beta))) <= .005], SKALA, beta[bb] - .01, beta[bb] + .01,
           tipp = "Variable und standardisiertes β angeben.")
vif <- sapply(names(mf)[-1], function(v) 1 / (1 - summary(lm(reformulate(setdiff(names(mf)[-1], v), v), data = mf))$r.squared))
num("regression", "Modell A: größter VIF", max(vif), .01)
range2("regression", "Modell B: R²", summary(lm(N_mean ~ alter + bildungsgrad + factor(geschlecht), d))$r.squared,
       summary(lm(N_mean ~ alter + bildungsgrad + geschlecht, d))$r.squared, .01, hinweis = "geschlecht als Faktor bzw. Zahl")
mM <- summary(lm(N_mean ~ E_c * G_c, data = d))$coefficients["E_c:G_c", ]
num("moderation", "Moderation: b der Interaktion E_c * G_c", mM["Estimate"], .01, tipp = "Interaktion aus den zentrierten Spalten E_c und G_c?")
pval("moderation", "Moderation: p der Interaktion", mM["Pr(>|t|)"], tipp = "Interaktion aus den zentrierten Spalten E_c und G_c?")
add("moderation", "Ein Satz: Warum findet Modell B (fast) nichts?", hinweis = "Demografie unabhängig generiert -> wahrer Nulleffekt")

# === Sitzung 5: Mehrebenenmodelle & Faktorenanalyse ============================
block("mixed", c("Mixed Model: fester Effekt E_mean (3 Nachkommastellen)", "Mixed Model: Varianz quelle_datensatz (3 Nachkommastellen)",
                 "Mixed Model: ICC, selbst berechnet (3 Nachkommastellen)"), "lme4", {
  m <- lme4::lmer(O_mean ~ E_mean + (1 | quelle_datensatz), data = d, REML = TRUE)
  vc <- as.data.frame(lme4::VarCorr(m))$vcov
  num("mixed", "Mixed Model: fester Effekt E_mean (3 Nachkommastellen)", lme4::fixef(m)["E_mean"], .01, k = 3)
  num("mixed", "Mixed Model: Varianz quelle_datensatz (3 Nachkommastellen)", vc[1], .005, k = 3)
  num("mixed", "Mixed Model: ICC, selbst berechnet (3 Nachkommastellen)", vc[1] / sum(vc), .01, k = 3)
})
x <- na.omit(d[BFI]); R <- cor(x); Ri <- solve(R)
P <- -Ri / sqrt(outer(diag(Ri), diag(Ri))); diag(P) <- 0; R0 <- R; diag(R0) <- 0
kmo <- sum(R0^2) / (sum(R0^2) + sum(P^2))
ev <- eigen(R, only.values = TRUE)$values
pa <- apply(replicate(200, eigen(cor(matrix(rnorm(nrow(x) * 21), ncol = 21)), only.values = TRUE)$values), 1, quantile, .95)
num("efa", "KMO (Overall MSA)", kmo, .02)
ganz("efa", "Anzahl Faktoren, die JASP vorschlägt", sum(ev > pa), 1, max(sum(ev > 1), sum(ev > pa)) + 1, plausibel = TRUE,
     hinweis = sprintf("nur Plausibilität; Parallelanalyse (PC) %d, Kaiser %d", sum(ev > pa), sum(ev > 1)))
add("efa", "Laden die vier E-Items gemeinsam auf einem Faktor? (ja/nein)", hinweis = "ja (meist); bei kleiner Stichprobe auch 'teilweise' ok")

# === Sitzung 6: Strukturgleichungsmodelle ======================================
block("sem", c("CFA: χ²", "CFA: df", "CFA: CFI", "CFA: RMSEA"), "lavaan", {
  mod <- "E =~ BFI_01 + BFI_06 + BFI_11 + BFI_16
          V =~ BFI_02 + BFI_07 + BFI_12 + BFI_17
          G =~ BFI_03 + BFI_08 + BFI_13 + BFI_18
          N =~ BFI_04 + BFI_09 + BFI_14 + BFI_19
          O =~ BFI_05 + BFI_10 + BFI_15 + BFI_20 + BFI_21"
  fm <- lapply(c("listwise", "ml"), function(ms) suppressWarnings(
    lavaan::fitMeasures(lavaan::cfa(mod, data = d, missing = ms), c("chisq", "df", "cfi", "rmsea"))))
  range2("sem", "CFA: χ²", fm[[1]]["chisq"], fm[[2]]["chisq"], 1, hinweis = "listwise bzw. FIML")
  exact("sem", "CFA: df", fm[[1]]["df"])
  range2("sem", "CFA: CFI", fm[[1]]["cfi"], fm[[2]]["cfi"], .02)
  range2("sem", "CFA: RMSEA", fm[[1]]["rmsea"], fm[[2]]["rmsea"], .01)
})
md <- na.omit(d[c("G_mean", "E_mean", "O_mean")])
ind <- function(x) coef(lm(E_mean ~ G_mean, x))[2] * coef(lm(O_mean ~ E_mean + G_mean, x))[2]
boot <- replicate(5000, ind(md[sample(nrow(md), replace = TRUE), ]))
num("mediation", "Mediation: indirekter Effekt (3 Nachkommastellen)", ind(md), .01, k = 3)
ci_lo <- quantile(boot, .025); ci_hi <- quantile(boot, .975)
intervall("mediation", "Mediation: 95 %-Bootstrap-KI [untere, obere]", sprintf("[%s, %s]", fmt(ci_lo, 3), fmt(ci_hi, 3)),
          ci_lo, ci_hi, .03, k = 3, hinweis = "Grenzen ±0.03 ok; entscheidend: enthält es 0?", tipp = "Beide Grenzen in eckigen Klammern, 3 Nachkommastellen.")
enthaelt_soll <- if (ci_lo <= 0 && ci_hi >= 0) "ja" else "nein"
enthaelt_ok <- if (ci_lo > .03 || ci_hi < -.03) "nein" else if (ci_lo < -.03 && ci_hi > .03) "ja" else c("ja", "nein")
wahl("mediation", "Mediation: Enthält das 95 %-KI die 0? (ja/nein)", enthaelt_soll, enthaelt_ok,
     hinweis = "liegt eine Grenze nahe 0, zählen ja und nein", tipp = "Liegen beide Grenzen auf derselben Seite der 0?")
num("mediation", "Mediation: direkter Effekt c' (3 Nachkommastellen)", coef(lm(O_mean ~ E_mean + G_mean, md))["G_mean"], .01, k = 3)
add("mediation", "Ein Satz: Was hat der Bootstrap hier gemacht?", hinweis = "5000-mal mit Zurücklegen neu gezogen -> Perzentil-KI")

# === Sitzung 7: Stichprobe, Versuchsplanung & Power, ANOVA =====================
miss <- colSums(is.na(d[BFI])); top <- names(miss)[miss == max(miss)]
label_zahl("bias", "BFI-Item mit den meisten fehlenden Werten (Item + Anzahl)", sprintf("%s (%d)", paste(top, collapse = " / "), max(miss)),
           top, BFI_ALIAS, max(miss), max(miss), k = 0, hinweis = "bei Gleichstand ist jedes genannte Item korrekt", tipp = "Item und Anzahl angeben, zum Beispiel: BFI_05 3")
add("bias", "Stichprobe in 2–3 Sätzen: Wer ist drin, welche (mind. zwei) Bias-Typen sind relevant?", hinweis = "jung, weiblich, hoch gebildet; Selection-Bias")
n1 <- ceiling(power.t.test(delta = .5, power = .8)$n)
n2 <- ceiling(power.t.test(delta = .3, power = .8)$n)
ganz("power", "Nötiges N pro Gruppe: d = 0.5, Power .80, α = .05, zweiseitig", n1, n1 - 1, n1 + 1, hinweis = "±1 ok")
ganz("power", "Nötiges N pro Gruppe: d = 0.3, sonst gleich", n2, n2 - 1, n2 + 1, hinweis = "±1 ok")
num("power", "Power bei n = 30 pro Gruppe und d = 0.5 (als Anteil zwischen 0 und 1)", power.t.test(n = 30, delta = .5)$power, .01)
a1 <- anova(lm(O_mean ~ factor(bildungsgrad), data = d))
f_df("anova", "A1: F(df1, df2)", a1$Df[1], a1$Df[2], a1$`F value`[1], tipp = "Beide Freiheitsgrade und den F-Wert angeben, zum Beispiel: F(4, 85) = 1.23")
pval("anova", "A1: p", a1$`Pr(>F)`[1])
num("anova", "A1: η²", a1$`Sum Sq`[1] / sum(a1$`Sum Sq`), .01)
lv <- c(levene(d$O_mean, d$bildungsgrad, mean), levene(d$O_mean, d$bildungsgrad, median))
pval_von_bis("anova", "A1: p des Levene-Tests", fmt(lv[1], 3), min(lv) - .01, max(lv) + .01, "Mittelwert- bzw. Median-zentriert")
a3 <- anova(lm(E_mean ~ factor(quelle_datensatz), data = d))
pval("anova", "A3: p", a3$`Pr(>F)`[1])

# === Sitzung 8: Kritik am Signifikanztest & Bayes ==============================
num("nhst", "Wahrscheinlichkeit für mind. ein falsch-positives Ergebnis bei 10 Tests (α = .05, als Anteil zwischen 0 und 1)", 1 - .95^10, .01)
gg <- na.omit(mw[c("G_mean", "geschlecht")]); y1 <- gg$G_mean[gg$geschlecht == 1]; y2 <- gg$G_mean[gg$geschlecht == 2]
sp <- sqrt(((length(y1) - 1) * var(y1) + (length(y2) - 1) * var(y2)) / (length(y1) + length(y2) - 2))
tost <- function(welch) { tt <- t.test(y1, y2, var.equal = !welch); se <- tt$stderr; df <- tt$parameter; dm <- mean(y1) - mean(y2)
  max(pt((dm + .5 * sp) / se, df, lower.tail = FALSE), pt((dm - .5 * sp) / se, df)) }
range2("nhst", "Äquivalenztest G_mean ~ geschlecht (Grenzen d = ±0.5): p (größerer der beiden einseitigen Tests)", tost(FALSE), tost(TRUE), .01, k = 3)
p_tost <- c(tost(FALSE), tost(TRUE))
wahl("nhst", "Sind die Gruppen im Sinne des Äquivalenztests äquivalent? (ja/nein)", if (p_tost[1] < .05) "ja" else "nein",
     if (min(p_tost) >= .06) "nein" else if (max(p_tost) <= .04) "ja" else c("ja", "nein"),
     hinweis = "liegt p nahe .05, zählen ja und nein", tipp = "Äquivalent heißt: p des Äquivalenztests unter .05.")
block("bayes", c("Bayes-t-Test (G_mean ~ geschlecht): Median δ", "Bayes-t-Test: 95 %-Credible-Interval [untere, obere]",
                 "Bayes-t-Test: BF10", "Bayes-Korrelation (E_mean × O_mean): Median rho (ρ)",
                 "Bayes-Korrelation: 95 %-Credible-Interval [untere, obere]"), "BayesFactor", {
  suppressPackageStartupMessages(library(BayesFactor))
  gb <- gg; gb$geschlecht <- factor(gb$geschlecht)
  bt <- ttestBF(formula = G_mean ~ geschlecht, data = gb, rscale = "medium")
  dl <- -as.numeric(posterior(bt, iterations = 20000, progress = FALSE)[, "delta"])
  num("bayes", "Bayes-t-Test (G_mean ~ geschlecht): Median δ", median(dl), .03, hinweis = "Vorzeichen je nach Gruppenreihenfolge", abs = TRUE, tipp = "Das Vorzeichen zählt hier nicht.")
  intervall("bayes", "Bayes-t-Test: 95 %-Credible-Interval [untere, obere]", sprintf("[%s, %s]", fmt(quantile(dl, .025)), fmt(quantile(dl, .975))),
            quantile(dl, .025), quantile(dl, .975), .03, hinweis = "±0.03 ok, Vorzeichen je nach Gruppenreihenfolge", abs = TRUE)
  bf <- extractBF(bt)$bf; num("bayes", "Bayes-t-Test: BF10", bf, max(.05, .05 * bf))
  eo <- na.omit(d[c("E_mean", "O_mean")])
  rh <- as.numeric(posterior(correlationBF(eo$E_mean, eo$O_mean, rscale = 1), iterations = 20000, progress = FALSE)[, "rho"])
  num("bayes", "Bayes-Korrelation (E_mean × O_mean): Median rho (ρ)", median(rh), .02)
  intervall("bayes", "Bayes-Korrelation: 95 %-Credible-Interval [untere, obere]", sprintf("[%s, %s]", fmt(quantile(rh, .025)), fmt(quantile(rh, .975))),
            quantile(rh, .025), quantile(rh, .975), .03, hinweis = "±0.03 ok")
})
add("bayes", "Ein Satz: Welche Wahrscheinlichkeitsaussage erlaubt der Posterior, die ein p-Wert nicht erlaubt?")

# === Sitzung 9: Intervention, Evaluation & Epidemiologie =======================
ct <- table(factor(mw$geschlecht), factor(mw$N_hoch))
chi <- suppressWarnings(chisq.test(ct, correct = FALSE))
or <- (ct[1, 1] * ct[2, 2]) / (ct[1, 2] * ct[2, 1])
num("epi", "Kreuztabelle geschlecht × N_hoch: χ²", chi$statistic, .02)
zahlen("epi", "Kreuztabelle: Odds Ratio", fmt(or), c(or, 1 / or), .02, hinweis = "OR oder Kehrwert, je nach Anordnung")
num("epi", "Beispiel-RCT aus dem Kapitel: relatives Risiko (RR) für Remission", 1.5, 0, hinweis = "45 % / 30 %")
exact("epi", "Beispiel-RCT: Number Needed to Treat (aufgerundet)", 7, hinweis = "1 / (0.45 − 0.30) = 6.67")
lg <- glm(N_hoch ~ E_mean + V_mean + G_mean + O_mean, data = d, family = binomial)
chil <- lg$null.deviance - lg$deviance; n <- length(lg$y)
num("logistisch", "Logistisch: χ² des Modelltests", chil, .02)
pval("logistisch", "Logistisch: p des Modelltests", pchisq(chil, lg$df.null - lg$df.residual, lower.tail = FALSE))
num("logistisch", "Logistisch: Nagelkerke R²", (1 - exp(-chil / n)) / (1 - exp(-lg$null.deviance / n)), .01)
num("logistisch", "Logistisch: AUC", auc(fitted(lg), lg$y), .01)
cf <- summary(lg)$coefficients[-1, ]; best <- rownames(cf)[which.min(cf[, 4])]
label_zahl("logistisch", "Logistisch: stärkster Prädiktor (Variable + Odds Ratio)", sprintf("%s: OR = %s", best, fmt(exp(cf[best, 1]))),
           best, SKALA, exp(cf[best, 1]) - .02, exp(cf[best, 1]) + .02, tipp = "Variable und Odds Ratio angeben, nicht das b.")

# === Sitzung 10: Qualitative Methoden & Abschluss ==============================
kr <- read.csv("materials/kodierung-ratings.csv")
tk <- table(factor(kr$rater_a, 1:4), factor(kr$rater_b, 1:4)); po <- sum(diag(tk)) / sum(tk)
pe <- sum(rowSums(tk) * colSums(tk)) / sum(tk)^2
num("quali", "Prozentuale Übereinstimmung der beiden Rater (in %)", 100 * po, .5, k = 1)
num("quali", "Cohen's κ", (po - pe) / (1 - pe), .01)
add("quali", "Ein Satz: Warum ist κ kleiner als die prozentuale Übereinstimmung?", hinweis = "κ korrigiert Zufallsübereinstimmung")
add("abschluss", "Feedback zum Seminar (optional, 1–3 Sätze)")

# --- Nummern vergeben ------------------------------------------------------------
sitz <- SITZUNG[vapply(rows, `[[`, "", "kap")]
stopifnot(!anyNA(sitz))
ord <- order(sitz, match(vapply(rows, `[[`, "", "kap"), names(SITZUNG)))
rows <- rows[ord]; sitz <- as.integer(sitz[ord])
nr <- ave(sitz, sitz, FUN = seq_along)
for (i in seq_along(rows)) rows[[i]]$sitzung <- sitz[i]
for (i in seq_along(rows)) rows[[i]]$nr <- nr[i]
for (i in seq_along(rows)) rows[[i]]$feld <- sprintf("S%02d-%d", sitz[i], nr[i])

# --- Spec für den Prüfer (Klartext, nie ins Repo) --------------------------------
js <- function(s) {
  s <- enc2utf8(as.character(s))
  s <- gsub("\\", "\\\\", s, fixed = TRUE); s <- gsub('"', '\\"', s, fixed = TRUE)
  paste0('"', gsub("[\r\n\t]", " ", s), '"')
}
to_json <- function(x) {
  if (is.null(x)) return("null")
  if (is.list(x)) {
    if (!is.null(names(x)) && all(nzchar(names(x))))
      return(paste0("{", paste(mapply(function(n, v) paste0(js(n), ":", to_json(v)), names(x), x), collapse = ","), "}"))
    return(paste0("[", paste(vapply(x, to_json, ""), collapse = ","), "]"))
  }
  if (length(x) != 1) return(paste0("[", paste(vapply(as.list(x), to_json, ""), collapse = ","), "]"))
  if (is.logical(x)) return(if (is.na(x)) "null" else if (x) "true" else "false")
  if (is.numeric(x)) return(format(x, digits = 15, trim = TRUE))
  js(x)
}
if (!is.na(SPEC)) {
  pv <- function(p) if (has(p)) as.character(packageVersion(p)) else NA_character_
  felder <- lapply(rows, function(r) c(list(feld = r$feld, sitzung = r$sitzung, nr = r$nr, frage = r$frage, typ = r$typ, tipp = r$tipp), r$spec))
  spec <- list(gruppe = GRUPPE, r_version = R.version.string,
               pakete = list(lme4 = pv("lme4"), lavaan = pv("lavaan"), BayesFactor = pv("BayesFactor"), MASS = pv("MASS")),
               seed = 2026L, felder = felder)
  con <- file(SPEC, "wb"); writeBin(charToRaw(enc2utf8(to_json(spec))), con); close(con)
  message("Spec geschrieben: ", length(rows), " Felder (nicht committen)")
}
if (NUR_SPEC) quit(save = "no", status = 0)

# --- 1. Abgabe-Vorlagen ----------------------------------------------------------
frage_vorlage <- function(f) gsub(" = ", " = ", gsub("\\((\\d) ", "(\\1 ", f))
for (s in unique(sitz)) {
  x <- rows[sitz == s]
  feld <- vapply(x, `[[`, "", "feld"); frage <- vapply(x, `[[`, "", "frage")
  nn <- sprintf("%02d", s)
  con <- file(sprintf("sitzung%02d/_abgabe.md", s), "w", encoding = "UTF-8")
  writeLines(c("<!-- automatisch erzeugt von scripts/antwortbogen.R, nicht von Hand bearbeiten -->", "",
               "So gibst Du ab:", "",
               sprintf("1. Öffne den Ordner [`abgaben/`]({{< meta kurs.repo >}}/tree/main/abgaben) im Repo und dort den Ordner Deiner Gruppe (Name siehe @sec-lehrperson). In der `README.md` steht ein Link „Datei anlegen“ für Sitzung %s. Er öffnet die neue Datei schon im richtigen Ordner.", nn),
               "2. Nenne die Datei `<Deine Kennziffer>.txt`, zum Beispiel `234567.txt`.",
               "3. Die Vorlage steht schon in der Datei. Trag hinter jeder Frage Deine Antwort ein. (Klappt der Link nicht, kopiere die Vorlage unten hinein.) Dezimalzahlen mit 2 Nachkommastellen, wo nichts anderes steht, *p*-Werte mit 3. Komma oder Punkt ist egal, `p < .001` kannst Du so schreiben.",
               "4. Klicke auf *Commit changes*. Kurz darauf liegt neben Deiner Datei `<Deine Kennziffer>-rueckmeldung.md`, dort steht je Feld richtig, falsch oder fehlt.", "",
               "Zum Korrigieren öffnest Du Deine Datei im Ordner, klickst oben rechts auf das Stift-Symbol, änderst die Werte und klickst auf *Commit changes*.", "",
               sprintf("Ohne GitHub-Account schickst Du denselben Text per Mail an Deine Lehrperson (@sec-lehrperson), Betreff `M15 Abgabe S%s <Kennziffer>`. Sie stellt die Datei für Dich ein. Deine Rückmeldung siehst Du ohne Anmeldung im Browser neben Deiner Datei im Ordner Deiner Gruppe und Sitzung. Für eine Korrektur schickst Du eine neue Mail.", nn), "",
               "```default", sprintf("%s %s%s ", feld, frage_vorlage(frage), ifelse(grepl("\\?$", frage), "", ":")), "```"), con)
  close(con)
}

# --- 2. Antwortbogen -------------------------------------------------------------
fehler <- tryCatch({
  gen <- new.env()
  for (e in parse("scripts/generate-datasets.R")) {
    if (is.call(e) && identical(e[[1]], as.name("<-"))) eval(e, gen)
  }
  out <- character(0)
  for (i in 1:10) {
    f <- read.csv(sprintf("data/generated/datensatz-%02d.csv", i))
    txt <- vapply(gen$apply_errors(f, i)$errors, function(er) {
      if (er$spalte == "probandennummer") {
        twin <- which(f$probandennummer == f$probandennummer[er$zeile])
        sprintf("Probandennummer %d doppelt (Zeilen %s)", f$probandennummer[er$zeile], paste(twin, collapse = " + "))
      } else {
        v <- f[er$zeile, er$spalte]
        sprintf("Zeile %d, `%s` = %s", er$zeile, er$spalte, ifelse(is.na(v), "leer", v))
      }
    }, "")
    out <- c(out, sprintf("| %02d | %s | %s |", i, txt[1], txt[2]))
  }
  out
}, error = function(e) character(0))

lines <- c("# Antwortbogen (INTERN)", "",
           sprintf("Erzeugt am %s aus `%s` (%d Zeilen). **Nicht ins Repo committen.**",
                   format(Sys.time(), "%d.%m.%Y %H:%M"), DATA, nrow(d)), "",
           "Prüfregel: Wert im Bereich → richtig. Bei Abweichung zuerst prüfen, ob eine ältere Version des Gesamtdatensatzes benutzt wurde.", "")
if (length(fehler)) lines <- c(lines, "## Sitzung 01 (keine Abgabe, nur zur Hilfe in der Sitzung)", "",
                               "### Fehler je Datensatz (Reihenfolge egal)", "",
                               "| Datensatz | Fehler A | Fehler B |", "|---|---|---|", fehler, "")
for (s in unique(sitz)) {
  x <- rows[sitz == s]
  lines <- c(lines, sprintf("## Sitzung %02d", s), "",
             "| Feld | Frage | Soll | akzeptiert | Hinweis |", "|---|---|---|---|---|",
             vapply(x, function(r) sprintf("| %s | %s | %s | %s | %s |", r$feld, r$frage, r$soll, r$bereich, r$hinweis), ""), "")
}
dir.create(dirname(OUT), showWarnings = FALSE)
con <- file(OUT, "w", encoding = "UTF-8"); writeLines(lines, con); close(con)
message("Wrote ", OUT, " and sitzungXX/_abgabe.md")
