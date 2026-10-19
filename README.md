# M15: Quantitative Forschungsmethoden & Multivariate Verfahren

Seminar zum Modul **M15: Multivariate Verfahren, Forschungsmethoden & Psychotherapieforschung**, Masterstudiengang Psychotherapie, HMU Health and Medical University.

- **Format:** 10 Online-Sitzungen per Videokonferenz in den Semesterwochen 5–14 (abgestimmt auf die Vorlesung, siehe `index.qmd`), self-paced über eine Quarto-Site mit Plenumsphasen und festen Kleingruppen (Breakout-Räume)
- **Gruppengröße:** ausgelegt für 20–40 Studierende
- **Software:** [JASP](https://jasp-stats.org)
- **Verantwortlich:** Hannes Diemerling

## Quick-Start für Studierende

1. **Vor Sitzung 01:** JASP installieren, empfohlen: GitHub-Account anlegen.
2. Seminar-Site öffnen und mit Sitzung 01 starten. Alle Inhalte stehen dort.

## Quick-Start für die Lehrperson

| Wann | Aufgabe | Wo / Wie |
|---|---|---|
| Vor Semesterstart | Alte Uploads löschen | `data/student-uploads/` leeren (`.gitkeep` behalten), `data/combined/` leeren |
| Vor Semesterstart | Termine eintragen | Tabelle in `index.qmd` |
| Vor Sitzung 01 | Teilnehmerliste (alphabetisch, Listennummer $n$) erstellen, Studierende mit GitHub-Account als Collaborator einladen (Account ist freiwillig) | Daraus: Kennziffer 2000 + n, Datensatz ((n − 1) mod 10) + 1, Kleingruppe n/4 aufgerundet; GitHub-Benutzernamen per Vorab-Mail |
| Sitzung 01 | Mail-Uploads (Betreff `M15 Upload <Kennziffer>`) selbst einstellen | Anhang im Repo unter `data/student-uploads/` hochladen (Add file → Upload files); der Merge erkennt den Datensatz am Inhalt, der Dateiname ist egal |
| Sitzung 01 | Uploads laufen automatisch zusammen | Workflow **Merge student uploads** (Actions-Tab) schreibt `data/combined/m15-gesamt.csv`, `merge-report.md` und `meta-studien.csv` |
| 2 Tage vor Sitzung 02 | Datensatz **einfrieren** | Actions → *Merge student uploads* → **Disable workflow**. Danach rechnen alle mit derselben Datei. |
| danach | Antwortbogen erzeugen | lokal `Rscript scripts/antwortbogen.R` → `intern/antwortbogen.md` (gitignored, **nie committen**); schreibt auch die Abgabe-Vorlagen `sitzungXX/_abgabe.md` neu |
| laufend | Abgaben prüfen | eine Mail pro Person und Sitzung, Betreff `M15 SNN <Kennziffer>`; Felder gegen den Antwortbogen abgleichen |

Hinweise:

- Der **Merge-Report** zeigt pro Upload-Datei: erkannt ja/nein, Seed-Datensatz, gefundene Fehler (z. B. `2/2`), Abweichungen von der Referenz-Bereinigung und Warnungen (falscher Name/Ordner, Semikolon, Zusatzspalten, ungültige Kennziffer). Damit hast Du auch das Feedback zur Bereinigung aus Sitzung 01.
- Der Merge ist robust gegen typische Upload-Fehler: Er liest alle CSVs unter `data/` und im Repo-Root, erkennt vorgenerierte Zeilen am Inhalt und übernimmt jeden Seed-Datensatz genau einmal.
- `antwortbogen.R` braucht R mit `MASS`, `lme4`, `lavaan` und `BayesFactor` (`install.packages(c("lme4", "lavaan", "BayesFactor"))`). Abgabefelder werden dort pro Kapitel definiert; wer Kapitel verschiebt, passt nur `SITZUNG` an.
- Fehler-Übersicht der vorgenerierten Datensätze: steht im Antwortbogen (Abschnitt Sitzung 01).
- Die Kontakt-Mailadresse steht zentral in `_quarto.yml` (`kurs.mail`).

## Repository-Struktur

```
.
├── _quarto.yml                   # Quarto-Buch-Konfiguration (Kapitel, kurs.mail)
├── index.qmd                     # Landing Page mit Terminübersicht
├── sitzung01/ … sitzung10/       # je 00-uebersicht, Inhaltskapitel, 99-abschluss + _abgabe.md (Vorlage, generiert)
├── anhang/                       # freiwillige Kapitel (Machine Learning, weitere Verfahren)
├── materials/                    # Codebook, CSV-Template
├── data/
│   ├── generated/                # 10 vorgenerierte Übungsdatensätze
│   ├── student-uploads/          # Uploads der Studierenden
│   └── combined/                 # m15-gesamt.csv + merge-report.md (vom Workflow erzeugt)
├── scripts/
│   ├── generate-datasets.R       # erzeugt die 10 Datensätze deterministisch
│   ├── merge-uploads.R           # kombiniert alle Uploads (fail-safe), schreibt Merge-Report
│   └── antwortbogen.R            # INTERN: Sollwerte + Toleranzbereiche aller Abgabefelder
└── .github/workflows/
    ├── publish.yml               # rendert die Site und deployed nach GitHub Pages
    ├── merge-uploads.yml         # bei jedem CSV-Upload: Gesamtdatensatz neu bauen
    └── generate-datasets.yml     # manuell: erzeugt + committet die 10 Übungsdatensätze
```

Site lokal bauen: `quarto render`

## Lizenz

- **Inhalte (Texte, Aufgaben, Materialien):** CC BY 4.0, siehe `LICENSE.md`
- **Code (Skripte, Workflows):** MIT, siehe `LICENSE-CODE.md`
