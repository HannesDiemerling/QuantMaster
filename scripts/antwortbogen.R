# scripts/antwortbogen.R
#
# Eine Quelle für alle Abgabefelder. Das Skript schreibt
#   1. sitzungXX/_abgabe.md   — Abgabe-Vorlagen der Studierenden (im Repo,
#                               per {{< include >}} in 99-abschluss.qmd)
#   2. intern/antwortbogen.md — Sollwerte + akzeptierte Bereiche (gitignored,
#                               landet NICHT im Repo)
#
# Felder werden pro Kapitel definiert; Nummern (S04-1, S04-2, …) vergibt das
# Skript anhand von SITZUNG automatisch. Wer Kapitel verschiebt, ändert nur
# SITZUNG und lässt das Skript neu laufen.
#
# Grundlage der Sollwerte: der eingefrorene Gesamtdatensatz
# data/combined/m15-gesamt.csv. Aufruf aus dem Repo-Root:
#   Rscript scripts/antwortbogen.R
# Benötigt: base R, MASS, lme4, lavaan, BayesFactor. Fehlt ein Paket, wird das
# Feld als "nicht berechnet" markiert.

DATA <- "data/combined/m15-gesamt.csv"
OUT  <- "intern/antwortbogen.md"
set.seed(2026)

SITZUNG <- c(einstieg = 1, bereinigung = 1, kombinieren = 2, deskriptiv = 2,
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
d$bildung_gruppe <- ifelse(d$bildungsgrad >= 6, "hoch", "niedriger")
d$N_hoch <- ifelse(d$N_mean >= median(d$N_mean, na.rm = TRUE), 1, 0)
d$E_c <- d$E_mean - mean(d$E_mean, na.rm = TRUE)
d$G_c <- d$G_mean - mean(d$G_mean, na.rm = TRUE)
mw <- d[d$geschlecht %in% 1:2, ]
BFI <- sprintf("BFI_%02d", 1:21)

# --- Hilfsfunktionen -----------------------------------------------------------
fmt <- function(x, k = 2) formatC(x, format = "f", digits = k)
rows <- list()
add <- function(kap, frage, soll = "Freitext", von = NA, bis = NA, hinweis = "") {
  rows[[length(rows) + 1]] <<- data.frame(
    kap = kap, frage = frage, soll = as.character(soll),
    bereich = if (is.na(von)) "–" else paste(von, "bis", bis), hinweis = hinweis)
}
num <- function(kap, frage, x, tol, k = 2, hinweis = "")
  add(kap, frage, fmt(x, k), fmt(x - tol, k), fmt(x + tol, k), hinweis)
exact <- function(kap, frage, x, hinweis = "") add(kap, frage, x, x, x, hinweis)
range2 <- function(kap, frage, a, b, tol, k = 2, hinweis = "")
  add(kap, frage, fmt(a, k), fmt(min(a, b) - tol, k), fmt(max(a, b) + tol, k), hinweis)
pval <- function(kap, frage, p, hinweis = "") {
  if (p < .001) add(kap, frage, "< .001", "< .001", "< .002", hinweis)
  else add(kap, frage, fmt(p, 3), fmt(max(p - .01, 0), 3), fmt(min(p + .01, 1), 3), hinweis)
}
skew_g1 <- function(x) { x <- na.omit(x); n <- length(x); m <- mean(x)
  (sum((x - m)^3) / n) / (sum((x - m)^2) / n)^1.5 }
skew_G1 <- function(x) { n <- sum(!is.na(x)); skew_g1(x) * sqrt(n * (n - 1)) / (n - 2) }
alpha <- function(items) { x <- na.omit(d[items]); k <- ncol(x)
  k / (k - 1) * (1 - sum(apply(x, 2, var)) / var(rowSums(x))) }
levene <- function(y, g, center) { ok <- !is.na(y) & !is.na(g)
  z <- abs(y[ok] - ave(y[ok], g[ok], FUN = center)); anova(lm(z ~ factor(g[ok])))$`Pr(>F)`[1] }
auc <- function(score, y) { r <- rank(score); n1 <- sum(y == 1); n0 <- sum(y == 0)
  (sum(r[y == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0) }
has <- function(p) requireNamespace(p, quietly = TRUE)

# === Sitzung 1: Einstieg & Bereinigung =========================================
add("einstieg", "Kennziffer", "2000 + Listennummer", hinweis = "mit Teilnehmerliste abgleichen")
add("einstieg", "GitHub-Benutzername (oder „kein Account“)", hinweis = "falls vorhanden: Collaborator-Einladung verschicken")
add("einstieg", "Ein Reverse-Item wurde mit 2 beantwortet. Rekodierter Wert", "4")
add("einstieg", "Anzahl Reverse-Items im BFI-K", "8", hinweis = "Items 1, 2, 8, 9, 11, 12, 17, 21")
gen <- new.env()
for (e in parse("scripts/generate-datasets.R")) {
  if (is.call(e) && identical(e[[1]], as.name("<-"))) eval(e, gen)
}
fehler <- character(0)
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
  fehler <- c(fehler, sprintf("| %02d | %s | %s |", i, txt[1], txt[2]))
}
add("bereinigung", "Datensatz-Nummer", "((Listennummer − 1) mod 10) + 1")
add("bereinigung", "Fehler 1 (Zeile, Spalte, gefundener Wert)", "siehe Fehlertabelle unten")
add("bereinigung", "Fehler 2 (Zeile, Spalte, gefundener Wert)", "siehe Fehlertabelle unten")
add("bereinigung", "Name Deiner hochgeladenen Datei (+ „GitHub“ oder „per Mail“)", "datensatz-XX-<Kennziffer>.csv", hinweis = "Abgleich mit data/combined/merge-report.md")

# === Sitzung 2: Gesamtdatensatz & Deskription ==================================
add("kombinieren", "Datenzeilen in Deiner selbst kombinierten Datei (ohne Kopfzeile)", "10 × Anzahl Dateien", hinweis = "typisch 70")
exact("kombinieren", "Zeilen in m15-gesamt.csv", nrow(d))
exact("kombinieren", "Davon Zeilen mit quelle = studierende", sum(d$quelle == "studierende"))
a <- na.omit(d$alter)
exact("deskriptiv", "Anzahl Personen mit geschlecht = 1", sum(d$geschlecht == 1, na.rm = TRUE))
num("deskriptiv", "Mittelwert alter", mean(a), .01)
num("deskriptiv", "SD alter", sd(a), .01)
num("deskriptiv", "Median alter", median(a), 0)
range2("deskriptiv", "IQR alter", IQR(a, type = 7), IQR(a, type = 6), .01, hinweis = "Quantil-Definitionen 6/7")
range2("deskriptiv", "Schiefe (Skewness) alter (3 Nachkommastellen)", skew_G1(a), skew_g1(a), .02, k = 3)
q <- quantile(a, c(.25, .75)); out_n <- sum(a < q[1] - 1.5 * diff(q) | a > q[2] + 1.5 * diff(q))
add("deskriptiv", "Anzahl Ausreißer-Punkte im Boxplot alter (abgezählt)", out_n, max(out_n - 1, 0), out_n + 1, "±1 ok")
num("deskriptiv", "Mittelwert BFI_06", mean(d$BFI_06, na.rm = TRUE), .01)
tb <- table(d$bildungsgrad)
add("deskriptiv", "Häufigste Stufe von bildungsgrad (aus dem Balkendiagramm)", paste(names(tb)[tb == max(tb)], collapse = " / "))
add("deskriptiv", "Q-Q-Plot alter: Liegen die Punkte auf der Diagonalen? (ja/nein + 1 Satz)",
    if (skew_G1(a) > .5) "nein" else "ja / eher ja", hinweis = sprintf("Schiefe = %s", fmt(skew_G1(a))))

# === Sitzung 3: Skalen & Effektgrößen ==========================================
exact("skalen", "Anzahl gültiger Werte (Valid) von E_mean", sum(!is.na(d$E_mean)),
      hinweis = "höher -> Mittelwert mit na.rm statt Formel aus dem Kapitel")
num("skalen", "Mittelwert N_mean", mean(d$N_mean, na.rm = TRUE), .01)
num("skalen", "SD N_mean", sd(d$N_mean, na.rm = TRUE), .01)
items <- list(E = c(1, 6, 11, 16), V = c(2, 7, 12, 17), G = c(3, 8, 13, 18), N = c(4, 9, 14, 19), O = c(5, 10, 15, 20, 21))
al <- sapply(items, function(i) alpha(sprintf("BFI_%02d", i)))
add("skalen", "Skala mit dem höchsten Cronbach's α (Skala + α)", sprintf("%s (%s)", names(which.max(al)), fmt(max(al))),
    fmt(max(al) - .02), fmt(max(al) + .02), paste("alle α:", paste(names(al), fmt(al), collapse = ", ")))
add("skalen", "Skala mit dem niedrigsten Cronbach's α (Skala + α)", sprintf("%s (%s)", names(which.min(al)), fmt(min(al))),
    fmt(min(al) - .02), fmt(min(al) + .02))
h1 <- cor.test(d$V_mean, d$N_mean)
num("korrelation", "H1: Pearson r (V_mean × N_mean)", h1$estimate, .01)
pval("korrelation", "H1: p-Wert", h1$p.value)
ps <- c(h1$p.value, cor.test(d$O_mean, d$E_mean)$p.value, cor.test(d$G_mean, d$alter)$p.value,
        suppressWarnings(cor.test(d$N_mean, d$bildungsgrad, method = "spearman"))$p.value)
exact("korrelation", "Wie viele der Hypothesen H1–H4 sind signifikant (p < .05)?", sum(ps < .05),
      hinweis = paste("p H1–H4:", paste(fmt(ps, 3), collapse = ", ")))
add("korrelation", "Eine alternative Erklärung (Drittvariable) für eine der Korrelationen, 1 Satz", hinweis = "plausible Drittvariable")
t1 <- t.test(d$E_mean, mu = 3)
num("ttest", "t1: t", t1$statistic, .01)
num("ttest", "t1: Cohen's d", (mean(d$E_mean, na.rm = TRUE) - 3) / sd(d$E_mean, na.rm = TRUE), .01)
t2 <- t.test(V_mean ~ geschlecht, data = mw)
num("ttest", "t2 (Welch-Zeile): t", t2$statistic, .01, hinweis = "Vorzeichen = Gruppe 1 − Gruppe 2")
pval("ttest", "t2 (Welch-Zeile): p", t2$p.value)
g <- lapply(split(mw$V_mean, mw$geschlecht), na.omit)
d_pool <- (mean(g[[1]]) - mean(g[[2]])) / sqrt(((length(g[[1]]) - 1) * var(g[[1]]) + (length(g[[2]]) - 1) * var(g[[2]])) / (length(g[[1]]) + length(g[[2]]) - 2))
d_avg <- (mean(g[[1]]) - mean(g[[2]])) / sqrt((var(g[[1]]) + var(g[[2]])) / 2)
range2("ttest", "t2: Cohen's d", d_avg, d_pool, .01, hinweis = "Welch-d bzw. gepoolt")
t3 <- t.test(O_mean ~ bildung_gruppe, data = d)
pval("ttest", "t3 (Welch-Zeile): p", t3$p.value)
add("ttest", "Ein Satz: Welche Schlussfolgerung ziehst Du aus t2, und welchen Vorbehalt hast Du?", hinweis = "Effektgröße/KI statt nur p")

# === Sitzung 4: Metaanalyse & Regression =======================================
ms <- read.csv("data/combined/meta-studien.csv")
yi <- ms$fisher_z; vi <- ms$se^2; k <- length(yi)
nll <- function(t2) { w <- 1 / (vi + t2); mu <- sum(w * yi) / sum(w)
  0.5 * (sum(log(vi + t2)) + log(sum(w)) + sum(w * (yi - mu)^2)) }
tau2 <- optimize(nll, c(0, 5))$minimum; if (nll(0) <= nll(tau2)) tau2 <- 0
w <- 1 / (vi + tau2); mu <- sum(w * yi) / sum(w); se_mu <- sqrt(1 / sum(w))
wf <- 1 / vi; Q <- sum(wf * (yi - sum(wf * yi) / sum(wf))^2)
s2 <- (k - 1) * sum(wf) / (sum(wf)^2 - sum(wf^2)); I2 <- 100 * tau2 / (tau2 + s2)
add("meta", "r in Deinem eigenen Seed-Datensatz (E_mean × O_mean, 2 Nachkommastellen)", "siehe Spalte r",
    hinweis = paste("r je Datensatz:", paste(sprintf("%02d: %s", ms$quelle_datensatz, fmt(ms$r)), collapse = "; ")))
num("meta", "Gepoolter Effekt (Fisher-z, Random Effects)", mu, .02)
add("meta", "95 %-KI des gepoolten Effekts [untere, obere]", sprintf("[%s, %s]", fmt(mu - 1.96 * se_mu), fmt(mu + 1.96 * se_mu)), hinweis = "Grenzen ±0.03 ok")
num("meta", "I² (in %)", I2, 10, k = 1, hinweis = sprintf("Q = %s; bei τ² = 0 ist I² = 0", fmt(Q)))
mA <- lm(N_mean ~ E_mean + V_mean + G_mean + O_mean, data = d); sA <- summary(mA); fA <- sA$fstatistic
num("regression", "Modell A: R²", sA$r.squared, .01)
num("regression", "Modell A: F", fA[1], .01)
pval("regression", "Modell A: p", pf(fA[1], fA[2], fA[3], lower.tail = FALSE))
mf <- model.frame(mA); beta <- coef(mA)[-1] * sapply(mf[-1], sd) / sd(mf[[1]]); bb <- which.max(abs(beta))
add("regression", "Modell A: stärkster Prädiktor (Variable + β)", sprintf("%s (β = %s)", names(beta)[bb], fmt(beta[bb])),
    fmt(beta[bb] - .01), fmt(beta[bb] + .01))
vif <- sapply(names(mf)[-1], function(v) 1 / (1 - summary(lm(reformulate(setdiff(names(mf)[-1], v), v), data = mf))$r.squared))
num("regression", "Modell A: größter VIF", max(vif), .01)
range2("regression", "Modell B: R²", summary(lm(N_mean ~ alter + bildungsgrad + factor(geschlecht), d))$r.squared,
       summary(lm(N_mean ~ alter + bildungsgrad + geschlecht, d))$r.squared, .01, hinweis = "geschlecht als Faktor bzw. Zahl")
mM <- summary(lm(N_mean ~ E_c * G_c, data = d))$coefficients["E_c:G_c", ]
num("moderation", "Moderation: b der Interaktion E_c * G_c", mM["Estimate"], .01)
pval("moderation", "Moderation: p der Interaktion", mM["Pr(>|t|)"])
add("moderation", "Ein Satz: Warum findet Modell B (fast) nichts?", hinweis = "Demografie unabhängig generiert -> wahrer Nulleffekt")

# === Sitzung 5: Mehrebenenmodelle & Faktorenanalyse ============================
if (has("lme4")) {
  m <- lme4::lmer(O_mean ~ E_mean + (1 | quelle_datensatz), data = d, REML = TRUE)
  vc <- as.data.frame(lme4::VarCorr(m))$vcov
  num("mixed", "Mixed Model: fester Effekt E_mean (3 Nachkommastellen)", lme4::fixef(m)["E_mean"], .01, k = 3)
  num("mixed", "Mixed Model: Varianz quelle_datensatz (3 Nachkommastellen)", vc[1], .005, k = 3)
  num("mixed", "Mixed Model: ICC, selbst berechnet (3 Nachkommastellen)", vc[1] / sum(vc), .01, k = 3)
} else for (i in 1:3) add("mixed", "Mixed Model", "nicht berechnet (lme4 fehlt)")
x <- na.omit(d[BFI]); R <- cor(x); Ri <- solve(R)
P <- -Ri / sqrt(outer(diag(Ri), diag(Ri))); diag(P) <- 0; R0 <- R; diag(R0) <- 0
kmo <- sum(R0^2) / (sum(R0^2) + sum(P^2))
ev <- eigen(R, only.values = TRUE)$values
pa <- apply(replicate(200, eigen(cor(matrix(rnorm(nrow(x) * 21), ncol = 21)), only.values = TRUE)$values), 1, quantile, .95)
num("efa", "KMO (Overall MSA)", kmo, .02)
add("efa", "Anzahl Faktoren, die JASP vorschlägt", sum(ev > pa), 1, max(sum(ev > 1), sum(ev > pa)) + 1,
    hinweis = sprintf("Parallelanalyse (PC) %d, Kaiser %d", sum(ev > pa), sum(ev > 1)))
add("efa", "Laden die vier E-Items gemeinsam auf einem Faktor? (ja/nein)", "ja (meist)", hinweis = "bei kleiner Stichprobe auch 'teilweise' ok")

# === Sitzung 6: Strukturgleichungsmodelle ======================================
if (has("lavaan")) {
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
} else for (i in 1:4) add("sem", "CFA", "nicht berechnet (lavaan fehlt)")
md <- na.omit(d[c("G_mean", "E_mean", "O_mean")])
ind <- function(x) coef(lm(E_mean ~ G_mean, x))[2] * coef(lm(O_mean ~ E_mean + G_mean, x))[2]
boot <- replicate(5000, ind(md[sample(nrow(md), replace = TRUE), ]))
num("mediation", "Mediation: indirekter Effekt (3 Nachkommastellen)", ind(md), .01, k = 3)
add("mediation", "Mediation: 95 %-Bootstrap-KI [untere, obere]", sprintf("[%s, %s]", fmt(quantile(boot, .025), 3), fmt(quantile(boot, .975), 3)),
    hinweis = "Grenzen ±0.02 ok; entscheidend: enthält es 0?")
num("mediation", "Mediation: direkter Effekt c' (3 Nachkommastellen)", coef(lm(O_mean ~ E_mean + G_mean, md))["G_mean"], .01, k = 3)
add("mediation", "Ein Satz: Was hat der Bootstrap hier gemacht?", hinweis = "5000-mal mit Zurücklegen neu gezogen -> Perzentil-KI")

# === Sitzung 7: Stichprobe, Versuchsplanung & Power, ANOVA =====================
miss <- colSums(is.na(d[BFI])); top <- names(miss)[miss == max(miss)]
add("bias", "BFI-Item mit den meisten fehlenden Werten (Item + Anzahl)", sprintf("%s (%d)", paste(top, collapse = " / "), max(miss)),
    hinweis = "bei Gleichstand ist jedes genannte Item korrekt")
add("bias", "Stichprobe in 2–3 Sätzen: Wer ist drin, welcher Bias-Typ ist am relevantesten?", hinweis = "jung, weiblich, hoch gebildet; Selection-Bias")
add("power", "Nötiges N pro Gruppe: d = 0.5, Power .80, α = .05, zweiseitig", ceiling(power.t.test(delta = .5, power = .8)$n),
    ceiling(power.t.test(delta = .5, power = .8)$n) - 1, ceiling(power.t.test(delta = .5, power = .8)$n) + 1)
add("power", "Nötiges N pro Gruppe: d = 0.3, sonst gleich", ceiling(power.t.test(delta = .3, power = .8)$n),
    ceiling(power.t.test(delta = .3, power = .8)$n) - 1, ceiling(power.t.test(delta = .3, power = .8)$n) + 1)
num("power", "Power bei n = 30 pro Gruppe und d = 0.5", power.t.test(n = 30, delta = .5)$power, .01)
a1 <- anova(lm(O_mean ~ factor(bildungsgrad), data = d))
add("anova", "A1: F(df1, df2) =", sprintf("F(%d, %d) = %s", a1$Df[1], a1$Df[2], fmt(a1$`F value`[1])),
    fmt(a1$`F value`[1] - .01), fmt(a1$`F value`[1] + .01))
pval("anova", "A1: p", a1$`Pr(>F)`[1])
num("anova", "A1: η²", a1$`Sum Sq`[1] / sum(a1$`Sum Sq`), .01)
lv <- c(levene(d$O_mean, d$bildungsgrad, mean), levene(d$O_mean, d$bildungsgrad, median))
add("anova", "A1: p des Levene-Tests", fmt(lv[1], 3), fmt(max(min(lv) - .01, 0), 3), fmt(min(max(lv) + .01, 1), 3), "Mittelwert- bzw. Median-zentriert")
a3 <- anova(lm(E_mean ~ factor(quelle_datensatz), data = d))
pval("anova", "A3: p", a3$`Pr(>F)`[1])

# === Sitzung 8: Kritik am Signifikanztest & Bayes ==============================
num("nhst", "Wahrscheinlichkeit für mind. ein falsch-positives Ergebnis bei 10 Tests (α = .05)", 1 - .95^10, .01)
gg <- na.omit(mw[c("G_mean", "geschlecht")]); y1 <- gg$G_mean[gg$geschlecht == 1]; y2 <- gg$G_mean[gg$geschlecht == 2]
sp <- sqrt(((length(y1) - 1) * var(y1) + (length(y2) - 1) * var(y2)) / (length(y1) + length(y2) - 2))
tost <- function(welch) { tt <- t.test(y1, y2, var.equal = !welch); se <- tt$stderr; df <- tt$parameter; dm <- mean(y1) - mean(y2)
  max(pt((dm + .5 * sp) / se, df, lower.tail = FALSE), pt((dm - .5 * sp) / se, df)) }
range2("nhst", "Äquivalenztest G_mean ~ geschlecht (Grenzen d = ±0.5): p (größerer der beiden einseitigen Tests)", tost(FALSE), tost(TRUE), .01, k = 3)
add("nhst", "Sind die Gruppen im Sinne des Äquivalenztests äquivalent? (ja/nein)", if (tost(FALSE) < .05) "ja" else "nein")
if (has("BayesFactor")) {
  suppressPackageStartupMessages(library(BayesFactor))
  gb <- gg; gb$geschlecht <- factor(gb$geschlecht)
  bt <- ttestBF(formula = G_mean ~ geschlecht, data = gb, rscale = "medium")
  dl <- -as.numeric(posterior(bt, iterations = 20000, progress = FALSE)[, "delta"])
  num("bayes", "Bayes-t-Test (G_mean ~ geschlecht): Median δ", median(dl), .03, hinweis = "Vorzeichen je nach Gruppenreihenfolge")
  add("bayes", "Bayes-t-Test: 95 %-Credible-Interval [untere, obere]", sprintf("[%s, %s]", fmt(quantile(dl, .025)), fmt(quantile(dl, .975))), hinweis = "±0.03 ok")
  bf <- extractBF(bt)$bf; num("bayes", "Bayes-t-Test: BF10", bf, max(.05, .05 * bf))
  eo <- na.omit(d[c("E_mean", "O_mean")])
  rh <- as.numeric(posterior(correlationBF(eo$E_mean, eo$O_mean, rscale = 1), iterations = 20000, progress = FALSE)[, "rho"])
  num("bayes", "Bayes-Korrelation (E_mean × O_mean): Median ρ", median(rh), .02)
  add("bayes", "Bayes-Korrelation: 95 %-Credible-Interval [untere, obere]", sprintf("[%s, %s]", fmt(quantile(rh, .025)), fmt(quantile(rh, .975))), hinweis = "±0.03 ok")
} else for (i in 1:5) add("bayes", "Bayes", "nicht berechnet (BayesFactor fehlt)")
add("bayes", "Ein Satz: Welche Wahrscheinlichkeitsaussage erlaubt der Posterior, die ein p-Wert nicht erlaubt?")

# === Sitzung 9: Intervention, Evaluation & Epidemiologie =======================
ct <- table(factor(mw$geschlecht), factor(mw$N_hoch))
chi <- suppressWarnings(chisq.test(ct, correct = FALSE))
or <- (ct[1, 1] * ct[2, 2]) / (ct[1, 2] * ct[2, 1])
num("epi", "Kreuztabelle geschlecht × N_hoch: χ²", chi$statistic, .02)
add("epi", "Kreuztabelle: Odds Ratio", fmt(or), fmt(min(or, 1 / or) - .02), fmt(max(or, 1 / or) + .02), "OR oder Kehrwert, je nach Anordnung")
add("epi", "Beispiel-RCT aus dem Kapitel: relatives Risiko (RR) für Remission", "1.50", "1.50", "1.50", "45 % / 30 %")
add("epi", "Beispiel-RCT: Number Needed to Treat (aufgerundet)", "7", "7", "7", "1 / (0.45 − 0.30) = 6.67")
lg <- glm(N_hoch ~ E_mean + V_mean + G_mean + O_mean, data = d, family = binomial)
chil <- lg$null.deviance - lg$deviance; n <- length(lg$y)
num("logistisch", "Logistisch: χ² des Modelltests", chil, .02)
pval("logistisch", "Logistisch: p des Modelltests", pchisq(chil, lg$df.null - lg$df.residual, lower.tail = FALSE))
num("logistisch", "Logistisch: Nagelkerke R²", (1 - exp(-chil / n)) / (1 - exp(-lg$null.deviance / n)), .01)
num("logistisch", "Logistisch: AUC", auc(fitted(lg), lg$y), .01)
cf <- summary(lg)$coefficients[-1, ]; best <- rownames(cf)[which.min(cf[, 4])]
add("logistisch", "Logistisch: stärkster Prädiktor (Variable + Odds Ratio)", sprintf("%s: OR = %s", best, fmt(exp(cf[best, 1]))),
    fmt(exp(cf[best, 1]) - .02), fmt(exp(cf[best, 1]) + .02))

# === Sitzung 10: Qualitative Methoden & Abschluss ==============================
kr <- read.csv("materials/kodierung-ratings.csv")
tk <- table(factor(kr$rater_a, 1:4), factor(kr$rater_b, 1:4)); po <- sum(diag(tk)) / sum(tk)
pe <- sum(rowSums(tk) * colSums(tk)) / sum(tk)^2
num("quali", "Prozentuale Übereinstimmung der beiden Rater (in %)", 100 * po, .5, k = 1)
num("quali", "Cohen's κ", (po - pe) / (1 - pe), .01)
add("quali", "Ein Satz: Warum ist κ kleiner als die prozentuale Übereinstimmung?", hinweis = "κ korrigiert Zufallsübereinstimmung")
add("abschluss", "Feedback zum Seminar (optional, 1–3 Sätze)")

# --- Nummern vergeben ------------------------------------------------------------
all <- do.call(rbind, rows)
all$sitzung <- SITZUNG[all$kap]
stopifnot(!anyNA(all$sitzung))
all <- all[order(all$sitzung, match(all$kap, names(SITZUNG))), ]
all$feld <- sprintf("S%02d-%d", all$sitzung, ave(all$sitzung, all$sitzung, FUN = seq_along))

# --- 1. Abgabe-Vorlagen ----------------------------------------------------------
for (s in unique(all$sitzung)) {
  x <- all[all$sitzung == s, ]
  con <- file(sprintf("sitzung%02d/_abgabe.md", s), "w", encoding = "UTF-8")
  writeLines(c("<!-- automatisch erzeugt von scripts/antwortbogen.R — nicht von Hand bearbeiten -->",
               "```text", sprintf("%s %s: ", x$feld, x$frage), "```"), con)
  close(con)
}

# --- 2. Antwortbogen -------------------------------------------------------------
lines <- c("# Antwortbogen (INTERN)", "",
           sprintf("Erzeugt am %s aus `%s` (%d Zeilen). **Nicht ins Repo committen.**",
                   format(Sys.time(), "%d.%m.%Y %H:%M"), DATA, nrow(d)), "",
           "Prüfregel: Wert im Bereich → richtig. Bei Abweichung zuerst prüfen, ob eine ältere Version des Gesamtdatensatzes benutzt wurde.", "")
for (s in unique(all$sitzung)) {
  x <- all[all$sitzung == s, ]
  lines <- c(lines, sprintf("## Sitzung %02d", s), "",
             "| Feld | Frage | Soll | akzeptiert | Hinweis |", "|---|---|---|---|---|",
             sprintf("| %s | %s | %s | %s | %s |", x$feld, x$frage, x$soll, x$bereich, x$hinweis), "")
  if (s == 1) lines <- c(lines, "### Fehler je Datensatz (Reihenfolge egal)", "",
                         "| Datensatz | Fehler A | Fehler B |", "|---|---|---|", fehler, "")
}
dir.create(dirname(OUT), showWarnings = FALSE)
con <- file(OUT, "w", encoding = "UTF-8"); writeLines(lines, con); close(con)
message("Wrote ", OUT, " and sitzungXX/_abgabe.md")
