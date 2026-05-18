# M15 — Quantitative Forschungsmethoden & Multivariate Verfahren

Workshop zum Modul **M15 — Multivariate Verfahren, Forschungsmethoden & Psychotherapieforschung**, Masterstudiengang Psychotherapie, HMU Health and Medical University Potsdam.

- **Termine:** 21.05.2026, 28.05.2026, 04.06.2026 — jeweils 09:45–17:00
- **Format:** Präsenz, Laptop-pflichtig, self-paced über eine Quarto-Site
- **Software:** [JASP](https://jasp-stats.org)
- **Verantwortlich:** Hannes Diemerling

## Quick-Start für Studierende

1. **Vor Tag 1:**
   - JASP installieren (siehe [jasp-stats.org](https://jasp-stats.org))
   - GitHub-Account anlegen und Benutzernamen per Mail an die Lehrperson schicken
2. **An Tag 1:** Workshop-Site öffnen (Link bekommst Du am ersten Tag), und mit Kapitel 0 starten.
3. Alle Inhalte sind auf der Site — dieses Repository ist die Quelle der Site.

Die gerenderte Site liegt unter GitHub Pages dieses Repos. Falls Du sie lokal bauen willst:

```bash
quarto render
```

## Quick-Start für die Lehrperson

| Aufgabe | Wo |
|---|---|
| Site lokal rendern | `quarto render` |
| Vorgenerierte Datensätze einmalig erzeugen | Workflow `Generate seed datasets (manual)` im Actions-Tab manuell starten — oder lokal `Rscript scripts/generate-datasets.R` und committen |
| Fehler-Übersicht der vorgenerierten Datensätze | `scripts/error-overview.md` (wird vom Generierungsskript automatisch geschrieben, **nicht** in der Site verlinkt) |
| Studierenden-Uploads kombinieren | passiert **nicht** automatisch — das ist Lernstoff an Tag 2 (die Studierenden machen das selbst) |

::: Hinweis: Keine Automation für studentische Lerninhalte
Das Repo enthält **bewusst** keinen Merge-Workflow für `data/student-uploads/`. Das Zusammenführen der Uploads ist Teil des Lehrplans an Tag 2 — die Studierenden lernen, das selbst zu tun.
:::

## Repository-Struktur

```
.
├── _quarto.yml                   # Quarto-Buch-Konfiguration
├── index.qmd                     # Landing Page
├── tag1/                         # Tag-1-Kapitel (00 … 09) — voll ausgearbeitet
├── tag2/                         # Tag-2-Übersicht (Platzhalter)
├── tag3/                         # Tag-3-Übersicht (Platzhalter)
├── materials/                    # BFI-K-Fragebogen (PDF), Codebook, Einverständnis, CSV-Template
├── data/
│   ├── generated/                # 10 vorgenerierte Übungsdatensätze (über Manual-Workflow erzeugt)
│   ├── student-uploads/          # Hier laden Studierende ihre bereinigten + erweiterten Datensätze hoch
│   └── combined/                 # bleibt leer bis Tag 2 — Studierende erzeugen hier ihren Gesamtdatensatz
├── scripts/
│   ├── generate-datasets.R       # erzeugt die 10 Datensätze deterministisch
│   └── error-overview.md         # INTERN: welche Fehler stecken in welchem Datensatz (vom Skript geschrieben)
└── .github/workflows/
    ├── publish.yml               # rendert die Site und deployed nach GitHub Pages
    └── generate-datasets.yml     # manuell ausführbar: erzeugt + committet die 10 Übungsdatensätze
```

## Lizenz

- **Inhalte (Texte, Aufgaben, Materialien):** CC BY 4.0 — siehe `LICENSE.md`
- **Code (Skripte, Workflows):** MIT — siehe `LICENSE-CODE.md`

## Kontakt

Fragen zum Workshop: an die Lehrperson direkt oder per GitHub-Issue.
