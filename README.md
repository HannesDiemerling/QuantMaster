# M15: Quantitative Forschungsmethoden & Multivariate Verfahren

Seminar zum Modul **M15: Multivariate Verfahren, Forschungsmethoden & Psychotherapieforschung**, Masterstudiengang Psychotherapie, HMU Health and Medical University.

- **Format:** 10 Online-Sitzungen per Videokonferenz (abgestimmt auf die Vorlesung, siehe `index.qmd`), self-paced über eine Quarto-Site mit Plenumsphasen und festen Kleingruppen (Breakout-Räume)
- **Gruppengröße:** ausgelegt für 20–40 Studierende
- **Software:** [JASP](https://jasp-stats.org)
- **Verantwortlich:** Hannes Diemerling

## Quick-Start für Studierende

1. **Vor Sitzung 01:** JASP installieren und auf Englisch stellen, empfohlen: GitHub-Account anlegen.
2. Seminar-Site öffnen und mit Sitzung 01 starten. Alle Inhalte stehen dort.

## Quick-Start für die Lehrperson

| Wann | Aufgabe | Wo / Wie |
|---|---|---|
| Vor Semesterstart | Alte Uploads löschen | `data/student-uploads/` leeren (`.gitkeep` und `README.md` behalten) |
| Vor Semesterstart | Termine eintragen | Tabelle in `index.qmd` |
| Vor Semesterstart | Kollegin ohne Repo-Kenntnis einbinden | Als Collaborator mit Schreibrechten einladen. GitHub-Login der Kollegin als neue Zeile in `scripts/lehrpersonen.txt` eintragen, damit ihre Uploads für Personen ohne Account richtig zählen. `intern/hilfe-spickzettel.md` und `intern/zeitplan.md` direkt schicken (gitignored). Sie friert ihre Gruppe selbst über Actions ein. Den Antwortbogen erzeugt die Lehrperson mit R und schickt ihn. Lehrperson, Mail, Ordner und Termine in `sitzung01/00-uebersicht.qmd` und `index.qmd` eintragen |
| Sitzung 01 | Kennziffer erklären (die letzten 6 Ziffern der Matrikelnummer, nur Ziffern; andere Zahlen gehen auch, Hauptsache, die Person bleibt bei einer Nummer; beginnt sie mit 0, so viele Ziffern mehr nehmen, bis vorne keine 0 steht), Kleingruppen einteilen. Sitzung 01 hat keine Abgabe, Ziel ist der Upload. Studierende mit GitHub-Account als Collaborator einladen (Account ist freiwillig; max. 50 Einladungen pro Tag, Einladungen verfallen nach 7 Tagen, offene Einladungen unter Settings → Collaborators) | Datensatz = letzte Ziffer der Kennziffer, 0 = 10; Kleingruppen zu viert einteilen und notieren; GitHub-Benutzernamen per Vorab-Mail |
| Sitzung 01 | Mail-Uploads (Betreff `M15 Upload <Kennziffer>`) selbst einstellen | Anhang vorher in `datensatz-XX-<Kennziffer>.csv` umbenennen (gleichnamige Dateien überschreiben sich!), dann unter `data/student-uploads/` hochladen (Add file → Upload files). Möglichst noch in der Sitzung, damit die Person den Report prüfen kann |
| Sitzung 01 | Uploads laufen automatisch zusammen | Workflow **Merge student uploads** (Actions-Tab) prüft jeden Upload und schreibt `data/combined/merge-report.md` (Status je Datei). Einen Live-Datensatz gibt es nicht, gerechnet wird mit dem eingefrorenen Stand der Gruppe. Dateinamen, die unter Windows nicht gehen (etwa mit `<>` oder zwei Namen, die sich nur in Groß/Klein unterscheiden), benennt der Workflow selbst um, gelöscht oder überschrieben wird dabei nichts |
| Sitzung 01 | Uploads zählen | Zahl der Personen (Kennziffern) im Merge-Report mit der Zahl der Anwesenden vergleichen. Dateien, die keine CSV sind (.jasp, .xlsx, „umgewandelt“), stehen als eigene Zeile zusätzlich da, eine Person kann also zwei Zeilen haben. Fehlt jemand trotz Upload, wurde evtl. eine gleichnamige Datei überschrieben: Datei öffnen → *History* → ältere Version → *Download raw file* → unter dem richtigen Namen neu hochladen |
| Sitzung 01 | Doppelte Uploads aufräumen | Status **DOPPELT** im Merge-Report: die ältere der beiden Dateien löschen (Datei öffnen → „…“ → *Delete file* → Commit) |
| 2 Tage vor Sitzung 02 der Gruppe | Datensatz **einfrieren** | Actions → *Datensatz einfrieren* → **Run workflow** → Gruppenname genau wie in der Tabelle „Deine Lehrperson“: `hannes` oder `frederike`. Der Lauf merged zuerst alle Uploads, repariert formale Fehler, prüft, ob sich rechnen lässt, und schreibt `data/combined/eingefroren/<gruppe>/`. Danach kurz in `pruefbericht.md` schauen und der Gruppe den Ordnernamen nennen. Ein vorhandener Ordner wird nie überschrieben. Änderungen von Studierenden in einem Stand dreht der Workflow *Eingefrorene Stände schützen* automatisch zurück. Einen Stand ändern oder löschen geht nur mit `[stand-aendern]` im Commit-Titel, gedacht nur für Testläufe. Rot wird das Einfrieren bei ungültigem Gruppennamen, vorhandenem Stand, gescheiterter Reparatur oder fehlenden Python-Paketen (einfach neu starten) oder wenn keine fiktive Person da ist; dann wird nichts geschrieben. Der Merge-Workflow bleibt immer aktiv, die Gruppen laden in denselben Ordner hoch, und jeder Stand enthält alle bis dahin vorhandenen Personen. **Einfrieren rot:** Lauf öffnen, Zeile `Nicht eingefroren: …` lesen, es wurde nichts geschrieben. Gruppenname ungültig: neu starten. Stand vorhanden („gibt es schon einen eingefrorenen Stand“): Das ist Absicht, ein Stand bleibt fest. Nur bei einem Testlauf: Ordner löschen, im Commit-Titel `[stand-aendern]`, dann neu starten. Merge abgebrochen: erst Merge reparieren. Skala oder Gruppe zu klein: Uploads prüfen. **Lauf grün:** `pruefbericht.md` querlesen. Steht dort „Upload-Reparatur lief nicht“: Ordner löschen und neu einfrieren. |
| danach | Sollwerte und Antwortbogen | Die gehashten Prüfwerte entstehen automatisch: Nach dem Einfrieren läuft *Sollwerte erzeugen* und legt `sollwerte.json` in den Ordner der Gruppe, ohne lokales R (bei rotem Lauf einfach neu starten). Der Klartext für die Lehrperson entsteht nur lokal: `git pull`, dann `Rscript scripts/antwortbogen.R <gruppe>` → `intern/antwortbogen-<gruppe>.md` (gitignored, **nie committen**, an Kolleginnen und Kollegen direkt schicken); schreibt auch die Abgabe-Vorlagen `sitzungXX/_abgabe.md` neu |
| ab Sitzung 02 | Abgaben | Jede Person lädt pro Sitzung eine Datei nach `abgaben/<gruppe>/sitzungNN/<Kennziffer>.txt` hoch, Ziel ist innerhalb oder am Ende der Sitzung, nur notfalls danach. Ohne GitHub-Account kommt die Datei per Mail an die Lehrperson, die sie dort einstellt. Rückmeldung und Ergebnisse je Kennziffer entstehen automatisch, die Lehrperson sieht sie in `UEBERSICHT.md` ihrer Gruppe und Sitzung |

Hinweise:

- **Hilfe-Spickzettel** für Probleme in Sitzung 01–03: `intern/hilfe-spickzettel.md` (lokal, gitignored).
- Der **Merge-Report** zeigt pro Upload-Datei Kennziffer, Status (ok / PRÜFEN / DOPPELT / IGNORIERT, im Live-Report außerdem „umgewandelt“ für .jasp-, .xlsx- oder .numbers-Dateien, die schon in einem eingefrorenen Stand stecken), Seed-Datensatz, gefundene Fehler (z. B. `2/2`), Abweichungen von der Referenz-Bereinigung und Hinweise (falscher Name/Ordner, Semikolon, Zusatzspalten, ungültige Kennziffer). Fiktive Personen werden nur aus eindeutigen Dateien übernommen (genau ein Übungsdatensatz, genau eine fiktive Person). Damit hast Du auch das Feedback zur Bereinigung aus Sitzung 01.
- Der Merge ist robust gegen typische Upload-Fehler: Er nimmt nur Dateien aus `data/student-uploads/` (Datendateien im Hauptordner und unter `data/` stehen als IGNORIERT im Report), erkennt vorgenerierte Zeilen am Inhalt und übernimmt jeden Seed-Datensatz genau einmal. Mit `ALLE_SEEDS <- TRUE` (Standard) sind immer alle zehn Seed-Datensätze enthalten, auch wenn bei kleinen Kursen einer nicht hochgeladen wurde.
- Bei vielen gleichzeitigen Uploads läuft immer nur ein Merge; der nächste wartet und verarbeitet dann alle neuen Dateien. Rote Läufe sollten nicht vorkommen; falls doch: Actions → *Merge student uploads* → *Run workflow*.
- `antwortbogen.R` braucht R mit `MASS`, `lme4`, `lavaan` und `BayesFactor` (`install.packages(c("lme4", "lavaan", "BayesFactor"))`). Abgabefelder werden dort pro Kapitel definiert; wer Kapitel verschiebt, passt nur `SITZUNG` an.
- Fehler-Übersicht der vorgenerierten Datensätze: steht im Antwortbogen (Abschnitt Sitzung 01).
- Der Merge rechnet mit den Referenzkopien in `scripts/referenz/`. Überschreibt jemand eine Datei in `data/generated/`, bleibt der Gesamtdatensatz korrekt; der Report zeigt dann oben eine ACHTUNG-Zeile, und die Datei sollte zurückgesetzt werden. Werden die Übungsdatensätze absichtlich neu erzeugt, die Kopien in `scripts/referenz/` ebenfalls ersetzen.
- Die Kontakt-Mailadresse steht zentral in `_quarto.yml` (`kurs.mail`) und gilt nur für Gruppe 1. Weitere Lehrpersonen stehen in der Tabelle in `sitzung01/00-uebersicht.qmd`.

## Repository-Struktur

```
.
├── _quarto.yml                   # Quarto-Buch-Konfiguration (Kapitel, kurs.mail)
├── index.qmd                     # Landing Page mit Terminübersicht
├── sitzung01/ … sitzung10/       # je 00-uebersicht, Inhaltskapitel, 99-abschluss + _abgabe.md (Vorlage, generiert)
├── anhang/                       # freiwillige Kapitel (Machine Learning, weitere Verfahren)
├── materials/                    # Codebook, CSV-Template
├── abgaben/                      # Abgaben: <gruppe>/sitzungNN/<Kennziffer>.txt, Übersicht je Sitzung
├── data/
│   ├── generated/                # 10 vorgenerierte Übungsdatensätze
│   ├── student-uploads/          # Uploads der Studierenden
│   └── combined/                 # merge-report.md (vom Workflow erzeugt), eingefroren/<gruppe>/ (Datensatz zum Rechnen)
├── scripts/
│   ├── generate-datasets.R       # erzeugt die 10 Datensätze deterministisch
│   ├── merge-uploads.R           # kombiniert alle Uploads (fail-safe), schreibt Merge-Report
│   ├── dateinamen.py             # benennt Uploads um, deren Namen unter Windows nicht gehen
│   ├── meta-studien.R            # Effekttabelle für die Metaanalyse (Sitzung 04)
│   ├── repair-uploads.py         # repariert Kopien der Uploads fürs Einfrieren
│   ├── freeze-dataset.R          # friert den Stand einer Gruppe ein, schreibt pruefbericht.md
│   ├── check-upload-pr.py        # prüft, ob ein Pull Request nur Uploads enthält
│   ├── antwortbogen.R            # INTERN: Sollwerte + Toleranzbereiche aller Abgabefelder
│   └── referenz/                 # unveränderliche Kopien der Übungsdatensätze für den Merge
└── .github/workflows/
    ├── publish.yml               # rendert die Site und deployed nach GitHub Pages
    ├── merge-uploads.yml         # bei jedem CSV-Upload: Merge-Report neu schreiben
    ├── freeze-dataset.yml        # manuell: friert den Datensatz einer Gruppe ein
    ├── accept-upload-pr.yml      # übernimmt Upload-Pull-Requests automatisch
    └── generate-datasets.yml     # manuell: erzeugt + committet die 10 Übungsdatensätze
```

Site lokal bauen: `quarto render`

## Lizenz

- **Inhalte (Texte, Aufgaben, Materialien):** CC BY 4.0, siehe `LICENSE.md`
- **Code (Skripte, Workflows):** MIT, siehe `LICENSE-CODE.md`
