# M15 — Quantitative Forschungsmethoden

Workshop-Materialien für **M15 — Multivariate Verfahren, Forschungsmethoden & Psychotherapieforschung**, Masterstudiengang Psychotherapie, HMU Health and Medical University Potsdam.

**Site:** [Workshop-Site aufrufen](https://hannes-diemerling.github.io/QuantMaster/) *(Link nach erstem Deploy aktiv)*

---

## Quick-Start für Studierende

1. Gehe zur [Workshop-Site](https://hannes-diemerling.github.io/QuantMaster/)
2. Installiere [JASP](https://jasp-stats.org) auf Deinem Laptop
3. Lege einen [GitHub-Account](https://github.com) an
4. Installiere [GitHub Desktop](https://desktop.github.com) (empfohlen)
5. Am ersten Kurstag: Fragebogen ausfüllen, CSV hochladen — Anleitung unter `tag1/03-erhebung-praktisch.qmd` und `tag1/04-github-upload.qmd`

### CSV hochladen (Kurzanleitung)

```
1. Diese Repository auf GitHub forken oder Branch anlegen
2. Deine ausgefüllte CSV-Datei nach data/raw/ hochladen
   - Echtdaten:     data/raw/echt-student-XX.csv
   - Synthetisch:   data/raw/synth-student-XX.csv
3. Pull Request öffnen
4. Warten bis Validation-Check grün ist
5. Lehrperson merged den PR
```

---

## Quick-Start für die Lehrperson

### Seed-Datensatz einspielen

```bash
# Seed-Datensatz (150-200 Faelle) nach data/seed/ legen
# Dateiname-Schema: data/seed/seed-batch01.csv
# Format: UTF-8, Komma-getrennt, Punkt als Dezimaltrenner
```

### Workshop-Site lokal bauen

```bash
# Quarto installieren: https://quarto.org/docs/get-started/
quarto render

# Oder nur eine Vorschau:
quarto preview
```

### Neuen Branch fuer Studierenden-PRs anlegen

GitHub-Einstellungen: Branch protection für `main` aktivieren, sodass nur die Lehrperson mergen kann.

---

## Struktur

```
m15-quant-workshop/
├── tag1/          # Tag 1: Foundations & Erhebung (5 Kapitel)
├── tag2/          # Tag 2: Daten verstehen & aufbereiten (5 Kapitel)
├── tag3/          # Tag 3: Multivariate Verfahren (6 Kapitel)
├── materials/     # Fragebogen, Codebook, CSV-Vorlage, Einverstaendnis
├── scripts/       # R-Skripte fuer Datengeneration und -aufbereitung
├── data/
│   ├── seed/      # Vom Lehrenden bereitgestellte Seed-Daten (nicht im Repo)
│   ├── raw/       # Studierenden-Uploads
│   └── combined/  # Automatisch zusammengefuehrter Datensatz
├── jasp/          # JASP-Vorlagen
└── exam-prep/     # Lernzielkatalog
```

---

## Lizenzen

- Inhalte: [CC BY 4.0](LICENSE.md)
- Code: [MIT](LICENSE-CODE.md)

---

## Termine

| Tag | Datum | Thema |
|-----|-------|-------|
| Tag 1 | 21.05.2026 | Foundations & Erhebung |
| Tag 2 | 28.05.2026 | Daten verstehen & aufbereiten |
| Tag 3 | 04.06.2026 | Multivariate Verfahren |

Je 09:45–17:00 Uhr, HMU Potsdam.
