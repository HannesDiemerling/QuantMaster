# M15 — Quantitative Forschungsmethoden & Multivariate Verfahren

Workshop zum Modul **M15 — Multivariate Verfahren, Forschungsmethoden & Psychotherapieforschung**, Masterstudiengang Psychotherapie, HMU Health and Medical University Potsdam.

- **Termine:** 21.05.2026, 28.05.2026, 04.06.2026 — jeweils 09:45–17:00
- **Format:** Präsenz, Laptop-pflichtig, self-paced über eine Quarto-Site
- **Software:** [JASP](https://jasp-stats.org)
- **Verantwortlich:** Prof. Dr. Daniel Schad

## Quick-Start für Studierende

1. **Vor Tag 1:**
   - JASP installieren (siehe [jasp-stats.org](https://jasp-stats.org))
   - GitHub-Account anlegen und Benutzernamen per Mail an die Lehrperson schicken
2. **An Tag 1:** Workshop-Site öffnen (Link bekommst Du am ersten Tag), und mit Kapitel 0 starten.
3. Alle Inhalte sind auf der Site — diese Repository ist die Quelle dieser Site.

Die gerenderte Site liegt unter GitHub Pages dieses Repos. Falls Du sie lokal bauen willst:

```bash
quarto render
```

## Quick-Start für die Lehrperson

| Aufgabe | Befehl / Ort |
|---|---|
| Site lokal rendern | `quarto render` |
| Vorgenerierte Datensätze neu bauen | `Rscript scripts/generate-datasets.R` oder Workflow `Generate seed datasets` manuell triggern |
| Studierenden-Uploads zusammenführen | Passiert automatisch via Workflow `Merge student uploads`, sobald in `data/student-uploads/` etwas committed wird |
| Fehler-Übersicht der vorgenerierten Datensätze | `scripts/error-overview.md` (wird automatisch erzeugt, **nicht** in der Site verlinkt) |

## Repository-Struktur

```
.
├── _quarto.yml                   # Quarto-Buch-Konfiguration
├── index.qmd                     # Landing Page
├── tag1/                         # Tag-1-Kapitel (00 … 09)
├── tag2/                         # Tag-2-Übersicht (Platzhalter)
├── tag3/                         # Tag-3-Übersicht (Platzhalter)
├── materials/                    # BFI-K-Fragebogen, Codebook, Einverständnis, CSV-Template
├── data/
│   ├── generated/                # 10 vorgenerierte Datensätze (vom Workflow erzeugt)
│   ├── student-uploads/          # Hier laden Studierende ihre bereinigten + erweiterten Datensätze hoch
│   └── combined/                 # Gesamtdatensatz, automatisch zusammengeführt
├── scripts/
│   ├── generate-datasets.R       # erzeugt die 10 Datensätze deterministisch
│   ├── merge-uploads.R           # kombiniert Uploads
│   ├── validate-upload.R         # Schema-Check (optional)
│   └── error-overview.md         # INTERN: welche Fehler stecken in welchem Datensatz
└── .github/workflows/            # CI/CD: Site-Build + Daten-Workflows
```

## Lizenz

- **Inhalte (Texte, Aufgaben, Materialien):** CC BY 4.0 — siehe `LICENSE.md`
- **Code (Skripte, Workflows):** MIT — siehe `LICENSE-CODE.md`

## Kontakt

Fragen zum Workshop: an die Lehrperson direkt oder per GitHub-Issue.
