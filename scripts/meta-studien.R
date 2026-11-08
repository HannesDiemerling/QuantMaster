# scripts/meta-studien.R
#
# Effekttabelle für die Metaanalyse (Sitzung 04). Genutzt von merge-uploads.R
# und freeze-dataset.R, damit es nur eine Fassung der Berechnung gibt.
# Jeder Seed-Datensatz samt zugeordneter eigener Zeilen ist eine "Studie";
# Effekt = Korrelation E_mean × O_mean.

meta_studien <- function(combined) {
  sc <- function(i) rowSums(as.matrix(combined[sprintf("BFI_%02d", i)])) / length(i)
  e <- sc(c(1, 6, 11, 16)); o <- sc(c(5, 10, 15, 20, 21))
  meta <- do.call(rbind, lapply(sort(unique(na.omit(combined$quelle_datensatz))), function(s) {
    ok <- which(combined$quelle_datensatz == s & !is.na(e) & !is.na(o))
    if (length(ok) < 5) return(NULL)
    r <- cor(e[ok], o[ok])
    data.frame(studie = sprintf("Datensatz %02d", s), quelle_datensatz = s, n = length(ok),
               r = round(r, 3), fisher_z = round(atanh(r), 3), se = round(1 / sqrt(length(ok) - 3), 3))
  }))
  if (is.null(meta)) meta <- data.frame(studie = character(0), quelle_datensatz = numeric(0), n = numeric(0),
                                        r = numeric(0), fisher_z = numeric(0), se = numeric(0))
  meta
}
