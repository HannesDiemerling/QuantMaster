# CLAUDE.md — Workshop „Quantitative Forschungsmethoden & Multivariate Verfahren" (M15)

> **Zweck:** Spezifikation für einen agent-gestützten Aufbau eines self-paced Workshops als Quarto-Book auf GitHub Pages.
>
> **Aktueller Bauauftrag:** **Nur Tag 1** vollständig umsetzen. Tag 2 und Tag 3 sind hier nur als Outline beschrieben — sie werden später in eigenen Iterationen detailliert.

> **Stand 10/2026 — hat Vorrang vor dem Rest dieser Datei:** Das Material ist auf **10 Online-Sitzungen** (`sitzung01/`–`sitzung10/` in Semesterwoche 4–13, auf den Vorlesungsplan abgestimmt, je ca. 2 h, Videokonferenz mit Breakout-Kleingruppen; Machine Learning u. a. im `anhang/`) für **20–40 Studierende** umgestellt. Zuweisung über die Listennummer $n$: Kennziffer $2000+n$, Datensatz $((n-1) \bmod 10)+1$, Kleingruppe $\lceil n/4\rceil$. Pro Sitzung eine Mail mit Zahlenfeldern (Vorlage in `99-abschluss.qmd`), Sollwerte per `scripts/antwortbogen.R` → `intern/` (gitignored). Uploads kombiniert `scripts/merge-uploads.R` automatisch (Workflow `merge-uploads.yml`). Die Kapitel-IDs `#sec-tag1-…` usw. sind aus Kompatibilität unverändert. JASP-Anweisungen sind gegen JASP 0.98.21 (Module 0.95/0.96) geprüft: Menüpfade beginnen mit dem Modul-Button (kein „Analyses"-Menü).

---

## 0 · Vorhandene Artefakte — anpassen, nicht ersetzen

Im Projektordner liegen aus einem früheren Workshop bereits:

| Pfad | Vorgehen |
|---|---|
| `.github/` (Workflows, Actions-Configs) | Existierende Workflows als Vorlage nutzen. Pfade/Titel/Branches anpassen. Für dieses Projekt zwei zusätzliche Workflows hinzufügen (siehe §7). |
| `_quarto.yml` | Datei übernehmen und Inhalte anpassen (Title, Author, Kapitel-Liste, Theme falls nötig). Format-Block möglichst beibehalten. |
| `requirements.txt` | Anschauen und ergänzen, falls nötig. Nicht neu schreiben. |

**Erster Agent-Schritt:** Diese drei Artefakte lesen und verstehen.

---

## 1 · Projektkontext

| Feld | Wert |
|---|---|
| Hochschule | HMU Health and Medical University Potsdam |
| Studiengang | Masterstudiengang Psychotherapie (Vollzeit), M.Sc. |
| Modul | M15 — Multivariate Verfahren, Forschungsmethoden & Psychotherapieforschung |
| Modulverantwortlicher | Prof. Dr. Daniel Schad |
| Workshop-Teil | Quantitatives Seminar (Teil Ia) |
| Termine | 21.05.2026, 28.05.2026, 04.06.2026 — je 09:45–17:00 |
| Format | Präsenz, Laptop-pflichtig, drei Blocktage |
| Zielgruppe | Master-Studierende am Studienbeginn |
| Software | **JASP** (https://jasp-stats.org) |
| Plattform | GitHub + GitHub Pages |
| Site-Format | Quarto Book |
| Sprache | Deutsch |

---

## 2 · Didaktische Grundprinzipien

**Self-paced mit Lehrendem im Raum.** Die Quarto-Site ist das primäre Lehrmaterial. Studierende arbeiten überwiegend selbständig durch klar abgegrenzte Kapitel. Der Lehrende ist anwesend, gibt am Tagesanfang Input und steht zwischendurch für Rückfragen zur Verfügung — aber kein klassischer Frontalunterricht.

**Konsequenz für die Materialien:**
- Knappe, präzise Texte — keine ausschweifenden „Wenn-Du-hängst"-Sektionen. Studierende fragen den Lehrenden, wenn sie stecken bleiben.
- **Minimale Screenshots.** Alles bestmöglich in Text beschreiben (Menü-Pfade, Button-Bezeichnungen). Screenshots nur dort einsetzen, wo eine textliche Beschreibung wirklich nicht ausreicht — z. B. bei der ersten JASP-Oberflächenorientierung.
- Sprache: **„Du"-Form**, Workshop-Ton.

**JASP ist der didaktische Kern.** Studierende sollen alles, was geht, praktisch in JASP umsetzen. Das ist die Vorbereitung für eigene quantitative Analysen in Masterarbeit und Forschungspraxis. Daher liegt der Fokus auf praktischer Anwendung, nicht auf Theorie-Auswalzen.

**Konzept der Datenerhebung simulieren.** Studierende durchlaufen über drei Tage einen vollständigen Forschungsworkflow: Erhebung → Eingabe → Versionierung → Kombination → Bereinigung → Analyse. Das ist gleichzeitig Lehrgegenstand (Open Science, Reproduzierbarkeit) und Vehikel für die statistischen Inhalte.

---

## 3 · Gesamt-Workshop-Outline (Tag 1 → 3)

### Tag 1 — 21.05.2026 — Technisches Setup & Datenerhebung *(in dieser Iteration umzusetzen)*

Ziel: Studierende haben den BFI-K ausgefüllt und rekodiert, eine GitHub-Workflow durchlaufen, einen vorgenerierten Datensatz heruntergeladen, mit ihren eigenen Daten kombiniert, in JASP bereinigt und wieder hochgeladen. Am Ende existiert ein gemeinsamer, sauberer Datensatz im Repo.

→ **Vollständig spezifiziert in §5.**

### Tag 2 — 28.05.2026 — Datensatz verstehen & deskriptiv analysieren *(später)*

Outline:
- Combined-Datensatz öffnen und Datenstruktur verstehen
- Deskriptive Statistik (Verteilungen, Boxplots, Histogramme) in JASP
- Reliabilitätsanalyse (Cronbach's α, McDonald's ω) für die fünf BFI-K-Skalen
- Subskalen-Mittelwerte berechnen (Computed Columns)
- Korrelationsmatrix der Big-Five-Faktoren
- t-Tests und ANOVA mit Effektgrößen (Cohen's d, η²)
- Konzeptuelle Einführung in Faktorenanalyse + EFA durchführen
- Stichprobenbias und Umgang mit fehlenden Werten

### Tag 3 — 04.06.2026 — Multivariate Verfahren *(später)*

Outline:
- Multiple Regression (Voraussetzungen, Interpretation, Modellvergleich)
- Moderation und Mediation
- Bootstrap als Resampling-Verfahren
- Logistische Regression
- Bayesianische Inferenz im Vergleich zum Frequentismus
- Demos: Strukturgleichungsmodelle (CFA), Hierarchische Modelle, Clusteranalyse, Metaanalyse
- Klausurvorbereitung

---

## 4 · Datenmodell

### 4.1 Instrument: BFI-K (Big Five Inventory — Kurzversion)

Quelle: Rammstedt, B. & John, O. P. (2005). Kurzversion des Big Five Inventory (BFI-K). *Diagnostica*, 51(4), 195–206.

- **21 Items**, 5-Punkt-Likert-Skala (1 = trifft gar nicht zu, 5 = trifft voll zu)
- 5 Faktoren:
  - **Extraversion (E)** — 4 Items, davon 2 Reverse
  - **Verträglichkeit (V)** — 4 Items, davon 1 Reverse
  - **Gewissenhaftigkeit (G)** — 4 Items, davon 2 Reverse
  - **Neurotizismus (N)** — 4 Items, davon 1 Reverse
  - **Offenheit (O)** — 5 Items, davon 1 Reverse

Itemtexte werden vom Lehrenden bzw. Agent aus der Originalpublikation übernommen.

### 4.2 Variablenliste (37 Spalten → reduziert: 25 Spalten)

| Variable | Typ | Skalenniveau | Kodierung |
|---|---|---|---|
| `probandennummer` | int | nominal | 1, 2, 3, … (innerhalb des Datensatzes eindeutig) |
| `geschlecht` | int | nominal | 1 = weiblich, 2 = männlich, 3 = divers |
| `alter` | int | metrisch | Jahre (18–99) |
| `bildungsgrad` | int | ordinal | 1 = ohne Abschluss, 2 = Hauptschulabschluss, 3 = Realschulabschluss, 4 = (Fach-)Abitur, 5 = Bachelor, 6 = Master/Diplom/Staatsexamen, 7 = Promotion |
| `BFI_E1` … `BFI_E4` | int | ordinal | 1–5 (bereits rekodiert) |
| `BFI_V1` … `BFI_V4` | int | ordinal | 1–5 (bereits rekodiert) |
| `BFI_G1` … `BFI_G4` | int | ordinal | 1–5 (bereits rekodiert) |
| `BFI_N1` … `BFI_N4` | int | ordinal | 1–5 (bereits rekodiert) |
| `BFI_O1` … `BFI_O5` | int | ordinal | 1–5 (bereits rekodiert) |

**Gesamt: 4 Demografie-Spalten + 21 BFI-K-Items = 25 Spalten.**

**Wichtig:** In den 10 vorgenerierten Datensätzen sind alle BFI-K-Items **bereits rekodiert** (Reverse-Items umgepolt). Studierende rekodieren nur ihre eigenen, von Hand ausgefüllten Antworten. Erklärt wird das im Datensatz-Codebook.

### 4.3 CSV-Format

- UTF-8
- Komma-getrennt
- Punkt als Dezimaltrenner
- Header-Zeile mit Spaltennamen
- Fehlende Werte = leeres Feld (nicht „NA", nicht „-99")

### 4.4 Die 10 vorgenerierten Datensätze

| Dataset | Datei | n | Fehler-Anzahl |
|---|---|---|---|
| 1 | `data/generated/datensatz-01.csv` | 9 | 2 |
| 2 | `data/generated/datensatz-02.csv` | 9 | 2 |
| … | … | 9 | 2 |
| 10 | `data/generated/datensatz-10.csv` | 9 | 2 |

**Fehler-Typen (über die 10 Datensätze rotieren lassen, jeder Datensatz hat 2 Fehler aus unterschiedlichen Kategorien):**

| # | Fehlertyp | Beispiel |
|---|---|---|
| F1 | Doppelte Probandennummer | zwei Zeilen haben `probandennummer = 4` |
| F2 | Fehlender Wert | leeres Feld in einer BFI-Spalte |
| F3 | Wert außerhalb des erlaubten Bereichs | `BFI_E2 = 7` (max ist 5) |
| F4 | Unmögliches Alter | `alter = 200` oder `alter = 5` |
| F5 | Ungültiger Code | `geschlecht = 9` (nur 1–3 erlaubt) |
| F6 | Fehlender Demografie-Wert | `bildungsgrad` leer |

Der Agent verteilt die Fehler so, dass über alle 10 Datensätze jede Fehlerart mehrfach vorkommt. **Eine Übersichtstabelle, welcher Datensatz welche Fehler enthält, gehört NICHT in die Studierenden-Materialien** — das Finden ist Teil der Übung.

Stattdessen wird in einer **internen Lehrenden-Datei** (`scripts/error-overview.md`, NICHT in der Site verlinkt) dokumentiert, wo die Fehler stecken — damit der Lehrende sie kennt.

---

## 5 · Tag-1-Detailplan

### 5.1 Tagesablauf

| Zeit | Modus | Inhalt | Kapitel |
|---|---|---|---|
| 09:45–10:30 | **Plenum** | Begrüßung, Workshop-Vorstellung, Site live demonstrieren, Einverständnis austeilen, JASP-Installation und GitHub-Account klären | — |
| 10:30–12:00 | Self-Study | Open Science & Reproduzierbarkeit; Forschungsethik & Einverständnis | Kap. 1, 2 |
| 12:00–12:30 | **Plenum** | Übergang: warum die BFI-K, was bedeutet Testkonstruktion? | — |
| 12:30–13:30 | Mittag | | |
| 13:30–14:30 | Self-Study | Messtheorie & BFI-K verstehen; Fragebogen selbst ausfüllen und rekodieren | Kap. 3, 4 |
| 14:30–15:30 | Self-Study | GitHub-Einstieg; Datensatz herunterladen | Kap. 5, 6 |
| 15:30–15:45 | **Plenum** | Kurze Demo: JASP-Oberfläche, Datei öffnen | — |
| 15:45–16:45 | Self-Study | Datenbereinigung in JASP (Fehler finden); eigenen Datensatz kombinieren und hochladen | Kap. 7, 8 |
| 16:45–17:00 | **Plenum** | Tagesabschluss, Reflexion, Ausblick auf Tag 2 | Kap. 9 |

Lehrender ist während Self-Study-Phasen ansprechbar.

### 5.2 Kapitelstruktur Tag 1

```
tag1/
├── 00-uebersicht.qmd
├── 01-open-science.qmd
├── 02-forschungsethik.qmd
├── 03-messtheorie-bfi-k.qmd
├── 04-fragebogen-ausfuellen.qmd
├── 05-github-einstieg.qmd
├── 06-datensatz-laden.qmd
├── 07-datenbereinigung-jasp.qmd
├── 08-hochladen-kombinieren.qmd
└── 09-tagesabschluss.qmd
```

### 5.3 Inhalt pro Kapitel

Jedes Kapitel folgt dieser **schlanken** Anatomie:

```markdown
# Kapitel-Titel

::: {.callout-note title="Lernziele"}
Nach diesem Kapitel kannst Du …
- (3–4 Bullets)
:::

## Worum es geht
[Kurzer Motivationstext, 3–5 Sätze]

## [Hauptinhalt — Theorie/Anleitung]
[Klarer Erklärtext, gerne mit Mini-Beispielen]

::: {.callout-tip title="Hinweis"}
[Kurzer Hinweis, falls nötig]
:::

## [Aufgabe oder Anwendung]
[Konkrete Schritte oder Übung]

## Selbstcheck
- (2–3 Fragen)
```

**Keine** „Wenn-Du-hängst"-Boxen, **keine** Mini-Übungen mit Lösungs-Toggles. Bei Bedarf fragen Studierende den Lehrenden.

#### Kapitel 00 — Tagesübersicht *(kurz, ½ Seite)*

- Was passiert heute (Tagesablauf-Tabelle)
- Was Du am Ende des Tages erreicht hast
- Was Du dafür brauchst (Laptop, JASP, GitHub-Account, Stift, Drucker für Einverständnis)

#### Kapitel 01 — Open Science & Reproduzierbarkeit

- **Worum es geht:** Warum wir den ganzen Workshop in einem öffentlichen GitHub-Repo machen. Replikationskrise in der Psychologie (Open Science Collaboration 2015). Was bedeutet Reproduzierbarkeit — und was unterscheidet sie von Replizierbarkeit?
- **Inhalte:**
  - Replikationskrise — kurze Einordnung (max. 1 Absatz)
  - Reproduzierbarkeit vs. Replizierbarkeit vs. Robustheit
  - FAIR-Prinzipien (Findable, Accessible, Interoperable, Reusable) als Faustregel
  - Open Science Framework (OSF), Preregistration — was ist das, kurz
  - Warum **GitHub für Lehre und Forschung** Sinn ergibt
- **Bezug zum Workshop:** Im Workshop praktizieren wir Reproduzierbarkeit selbst — jeder Schritt wird versioniert, jeder Datensatz ist nachvollziehbar, jeder Code ist offen.
- **Selbstcheck:** Was unterscheidet Reproduzierbarkeit von Replizierbarkeit? Warum ist Versionierung wichtig?

#### Kapitel 02 — Forschungsethik & Dein Einverständnis

- **Worum es geht:** Du wirst gleich selbst Daten beisteuern. Bevor das passiert, brauchst Du eine informierte Einverständniserklärung — genau wie in einer echten Studie.
- **Inhalte:**
  - Grundprinzipien der Forschungsethik (Helsinki, Belmont Report — kurz)
  - Informierte Einwilligung: was muss drin sein?
  - DSGVO-Basics für Forschungsdaten
  - Pseudonymisierung vs. Anonymisierung
  - **Konkret für diesen Workshop:** Deine Daten landen pseudonymisiert in einem öffentlichen Repo. Was bedeutet das? Was sind Deine Rechte?
- **Aufgabe:** Lies das Einverständnisformular (`materials/einverstaendnis.pdf`), unterschreibe es, gib es ab. Wer nicht teilnehmen möchte: nur die vorgenerierten Daten nutzen, eigene Antworten weglassen — die Alternative ist immer offen.
- **Selbstcheck:** Welche Angaben in Deinem Einverständnis sind verpflichtend? Was geschieht, wenn Du Deine Teilnahme zurückziehen willst?

#### Kapitel 03 — Messtheorie & der BFI-K

- **Worum es geht:** Was misst eigentlich ein psychometrischer Test? Warum vertrauen wir 21 Items, dass sie „Persönlichkeit" abbilden? Hier lernst Du das absolute Minimum an Testtheorie, das Du brauchst, um den BFI-K kompetent zu verwenden.
- **Inhalte:**
  - Klassische Testtheorie in zwei Sätzen: beobachteter Wert = wahrer Wert + Messfehler
  - Item, Skala, Subskala — Begriffe sauber unterscheiden
  - Reliabilität, Validität, Objektivität — Kurz-Definitionen (Vertiefung an Tag 2)
  - **Reverse-Items:** warum gibt es sie, was bedeuten sie?
  - Das Big-Five-Modell: kurze Einordnung (OCEAN, Geschichte, Verbreitung)
  - Der BFI-K: Aufbau, Quelle, Einsatzbereiche
- **Bezug zur Klausur:** Begriffe „Reliabilität", „Validität", „Reverse-Item" sollten sitzen.
- **Selbstcheck:** Was misst Reliabilität? Warum wird ein Reverse-Item *vor* der Aggregation umgepolt?

#### Kapitel 04 — Fragebogen ausfüllen & rekodieren

- **Worum es geht:** Jetzt füllst Du den BFI-K selbst aus — auf Papier. Anschließend rekodierst Du die Reverse-Items von Hand. Das macht den Mechanismus greifbar.
- **Inhalte / Aufgabe:**
  1. Hol Dir das ausgedruckte Fragebogen-Formular (`materials/bfi-k-fragebogen.pdf`) am Pult.
  2. Fülle alle 21 Items aus (1–5 Likert).
  3. Markiere im Formular die Reverse-Items (sind im Formular gekennzeichnet).
  4. Rekodiere die Reverse-Items von Hand mit der Formel: **rekodiert = 6 − Originalwert** (also 1↔5, 2↔4, 3↔3).
  5. Trage Deine rekodierten Werte zusammen mit Deiner Demografie in `materials/csv-template-eigene-daten.csv` ein.
- **Inhaltlich:** Erklärung der Rekodierungsformel mit ein, zwei Beispielen. Hinweis, dass alle Items dieselbe Polung haben müssen, bevor man Subskalen-Mittelwerte bildet.
- **Selbstcheck:** Wie lautet die Formel zur Rekodierung bei einer 5er-Likert-Skala? Warum kann man Items nicht einfach mitteln, ohne sie vorher umzupolen?

#### Kapitel 05 — GitHub-Einstieg

- **Worum es geht:** GitHub ist die Plattform, auf der wir unser Repo, unsere Daten und unsere Site verwalten. In diesem Kapitel verschaffst Du Dir einen Überblick und stellst sicher, dass Du Zugriff hast.
- **Inhalte:**
  - Was ist Git? Was ist GitHub? — Kurz und ohne Mystifizierung.
  - Begriffe in einer Übersichtstabelle: Repository, Branch, Commit, Pull Request, Merge.
  - **Drei Wege, mit dem Repo zu arbeiten** (eine Empfehlung pro Person — der Lehrende hilft bei der Wahl):
    1. **github.com im Browser** — am einfachsten, reicht für diesen Workshop.
    2. **GitHub Desktop** — für alle, die später öfter mit Git arbeiten wollen.
    3. **Kommandozeile** — nur, wenn Du das schon kennst.
  - Repo-Tour: was findest Du wo? (Verweis auf README)
- **Aufgabe:**
  1. Stelle sicher, dass Du einen GitHub-Account hast.
  2. Bitte den Lehrenden, Dich als Collaborator zum Repo hinzuzufügen.
  3. Öffne das Repo im Browser und finde den Ordner `data/generated/`.
- **Selbstcheck:** Was ist der Unterschied zwischen einem Commit und einem Pull Request? Wozu dient ein Branch?

#### Kapitel 06 — Deinen Datensatz herunterladen

- **Worum es geht:** Du bekommst einen der 10 vorgenerierten Datensätze zugewiesen, lädst ihn herunter und kombinierst ihn mit Deiner eigenen Zeile.
- **Inhalte:**
  - **Zuweisung:** Die Studierenden werden alphabetisch nach Nachnamen sortiert. Die erste Person bekommt Datensatz 1, die zweite Datensatz 2, …, die zehnte Datensatz 10, die elfte wieder Datensatz 1 etc. (Modulo-10-Zuweisung). Eine Liste mit der Zuweisung wird vom Lehrenden zu Tagesbeginn an der Tafel oder im Repo bereitgestellt.
  - **Hinweis zu Doppelzuweisungen:** Bei mehr als 10 Teilnehmenden wird derselbe Datensatz mehrfach bearbeitet. Das ist Absicht — am Ende kann man die Ergebnisse der Bereinigung vergleichen.
- **Aufgabe:**
  1. Schau Dir die Zuweisungsliste an und merke Dir Deine Datensatz-Nummer.
  2. Gehe im Repo zu `data/generated/datensatz-<XX>.csv` und lade die Datei herunter (Browser: „Download raw file").
  3. Speichere sie lokal auf Deinem Laptop.
- **Selbstcheck:** Was passiert, wenn mehrere Studierende denselben Datensatz bearbeiten? Wo liegen die vorgenerierten Datensätze im Repo?

#### Kapitel 07 — Datenbereinigung in JASP

- **Worum es geht:** Jeder Datensatz enthält genau **2 absichtliche Fehler**. Du sollst sie in JASP finden und entscheiden, wie Du damit umgehst.
- **Inhalte:**
  - **JASP öffnen und Datensatz laden:** File → Open → wähle Deine `datensatz-XX.csv`. JASP zeigt eine Tabellenansicht oben und Analyse-Optionen darunter.
  - **Variable-Typen prüfen:** Klick auf den Spaltenkopf. Geschlecht und Bildungsgrad sind nominal/ordinal (in JASP: „Nominal" bzw. „Ordinal"), Alter ist „Scale", BFI-Items sind „Scale" (kontinuierlich behandeln ist üblich).
  - **Deskriptive Statistik:** Analyses → Descriptives → Descriptive Statistics. Alle Variablen reinziehen. In den Optionen aktivieren: Distribution Plots, Frequency Tables für nominale Variablen.
  - **Fehler-Suche — drei Strategien:**
    1. **Wertebereich prüfen:** Min/Max in der Descriptives-Tabelle. Liegt ein BFI-Wert über 5 oder unter 1? Ist `alter` plausibel?
    2. **Frequenzen prüfen:** Frequency Tables für `geschlecht`, `bildungsgrad`, `probandennummer`. Doppelte Nummern fallen hier auf.
    3. **Missings prüfen:** In der Descriptives-Tabelle wird die Anzahl fehlender Werte angezeigt.
  - **Umgang mit gefundenen Fehlern:** Drei legitime Optionen, je nach Fehlertyp:
    - **Korrigieren**, wenn der wahre Wert klar erkennbar wäre (selten)
    - **Auf fehlend setzen** (Zelle leeren), wenn der Wert nicht rekonstruierbar ist
    - **Zeile entfernen**, wenn der Fall insgesamt unbrauchbar ist
  - Bei diesem Datensatz: Wir setzen Fehler auf fehlend (leere Zelle) und behalten die Zeilen.
- **Aufgabe:**
  1. Öffne Deinen Datensatz in JASP.
  2. Setze Variable-Typen korrekt.
  3. Führe Descriptives für alle Variablen durch.
  4. Finde die 2 Fehler.
  5. Korrigiere sie in JASP (Doppelklick auf die Zelle, Wert ändern oder löschen).
  6. Speichere die bereinigte Version als `datensatz-XX-bereinigt.csv` (File → Export Data).
- **Hinweis:** JASP speichert Analysen in `.jasp`-Dateien. Für unseren Workflow brauchen wir aber das **CSV** mit den bereinigten Daten. Daher unbedingt mit „Export Data" als CSV speichern.
- **Selbstcheck:** Welche Fehler hast Du gefunden? Warum ist „Wert auf fehlend setzen" oft besser, als die ganze Zeile zu löschen?

#### Kapitel 08 — Eigene Daten ergänzen, hochladen, alles kombinieren

- **Worum es geht:** Du fügst Deine eigene Zeile zum bereinigten Datensatz hinzu und lädst das Ergebnis ins Repo. Dann läuft ein GitHub-Workflow, der alle Studierenden-Uploads zu einem großen Gesamt-Datensatz zusammenfügt.
- **Inhalte:**
  - **Eigene Zeile anhängen:** Öffne `datensatz-XX-bereinigt.csv` in JASP (oder Excel/LibreOffice, falls Du lieber dort tippst — JASP ist nicht zum Tippen optimiert). Füge eine neue Zeile am Ende ein:
    - `probandennummer`: 10 (also die nächste freie Nummer in Deinem Datensatz)
    - `geschlecht`, `alter`, `bildungsgrad`: Deine Angaben (kodiert!)
    - BFI-Items: Deine **rekodierten** Werte aus Kapitel 4
  - **Speichern** als `datensatz-XX-kombiniert.csv`.
  - **Hochladen:**
    1. Im Repo zum Ordner `data/student-uploads/` navigieren.
    2. „Add file" → „Upload files" wählen.
    3. Deine Datei hochladen, unten als Dateinamen `datensatz-XX-<deinKuerzel>.csv` setzen (z. B. `datensatz-03-mm.csv`).
    4. Commit-Message: kurz, z. B. „upload datensatz 03 von mm".
    5. „Commit changes" — entweder direkt auf `main` (falls erlaubt) oder via Pull Request.
  - **Was dann passiert:** Eine GitHub Action wird automatisch ausgelöst, sobald Dateien in `data/student-uploads/` landen. Sie kombiniert alle hochgeladenen Datensätze (und entfernt Duplikate aus der vorgenerierten Stichprobe — der Algorithmus ist in `scripts/merge-uploads.R` dokumentiert) zu einem Master-Datensatz unter `data/combined/m15-gesamt.csv`.
- **Aufgabe:**
  1. Eigene Zeile hinzufügen.
  2. Datei korrekt benannt im Repo hochladen.
  3. Prüfen, dass die Action erfolgreich gelaufen ist (Repo-Reiter „Actions").
  4. `data/combined/m15-gesamt.csv` ansehen und prüfen, dass Deine Daten drin sind.
- **Selbstcheck:** Warum legt der Workflow den Gesamtdatensatz in einem separaten Ordner ab? Was passiert, wenn zwei Studierende denselben vorgenerierten Datensatz hochladen — gibt es dann doppelte Zeilen?

#### Kapitel 09 — Tagesabschluss

- **Worum es geht:** Reflexion, Sammlung offener Fragen, Ausblick auf Tag 2.
- **Inhalte:**
  - Was wir heute erreicht haben (Stichpunkte)
  - Was Tag 2 bringt (Stichpunkte)
- **Aufgabe (5 Minuten):** Notiere Dir kurz:
  - Eine Sache, die heute klar wurde.
  - Eine Sache, die noch unklar ist.
- Diese Notizen sind privat — nichts wird hochgeladen.

---

## 6 · Repository-Struktur (Stand nach Tag-1-Implementierung)

```
m15-quant-workshop/
├── CLAUDE.md
├── README.md
├── _quarto.yml                       # ANPASSEN
├── requirements.txt                  # ANPASSEN
├── index.qmd                         # Landing Page
│
├── tag1/                             # alle 10 Kapitel (s. §5.2)
│   └── 00 … 09.qmd
│
├── tag2/                             # leer / nur 00-uebersicht.qmd als Platzhalter
├── tag3/                             # leer / nur 00-uebersicht.qmd als Platzhalter
│
├── materials/
│   ├── bfi-k-fragebogen.qmd          # → bfi-k-fragebogen.pdf
│   ├── bfi-k-fragebogen.pdf
│   ├── codebook.csv                  # 25-Spalten-Wahrheit
│   ├── codebook.qmd                  # human-readable
│   ├── csv-template-eigene-daten.csv # leere Vorlage für eigene Werte
│   ├── einverstaendnis.qmd           # → einverstaendnis.pdf
│   └── einverstaendnis.pdf
│
├── data/
│   ├── generated/                    # 10 Datensätze vom Workflow erzeugt
│   │   └── .gitkeep                  # Inhalte werden via Action befüllt
│   ├── student-uploads/              # Studierenden-Uploads
│   │   └── .gitkeep
│   └── combined/                     # Gesamtdatensatz vom Workflow
│       └── .gitkeep
│
├── scripts/
│   ├── generate-datasets.R           # erzeugt die 10 Datensätze deterministisch
│   ├── merge-uploads.R               # kombiniert Studierenden-Uploads
│   ├── validate-upload.R             # CSV-Schema-Check für PRs (optional)
│   └── error-overview.md             # INTERN: welcher Datensatz hat welchen Fehler
│
└── .github/                          # EXISTIERT — ANPASSEN, ERWEITERN
    └── workflows/
        ├── build-site.yml            # existiert ggf. — anpassen
        ├── generate-datasets.yml     # NEU — baut die 10 Datensätze
        └── merge-uploads.yml         # NEU — baut den Gesamtdatensatz
```

---

## 7 · GitHub Actions

### 7.1 Site-Build *(existiert vermutlich schon — anpassen)*

Auf Push in `main`: Quarto rendern, Output auf `gh-pages` deployen. Bestehender Workflow als Vorlage übernehmen.

### 7.2 Datensätze generieren *(neu)*

`.github/workflows/generate-datasets.yml`:

```yaml
name: Generate seed datasets
on:
  push:
    paths:
      - 'scripts/generate-datasets.R'
      - '.github/workflows/generate-datasets.yml'
  workflow_dispatch:

jobs:
  generate:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - uses: r-lib/actions/setup-r@v2
      - run: |
          Rscript -e "install.packages(c('readr','dplyr','MASS'))"
          Rscript scripts/generate-datasets.R
      - uses: stefanzweifel/git-auto-commit-action@v5
        with:
          commit_message: "chore(data): regenerate seed datasets"
          file_pattern: data/generated/*.csv
```

### 7.3 Studierenden-Uploads kombinieren *(neu)*

`.github/workflows/merge-uploads.yml`:

```yaml
name: Merge student uploads
on:
  push:
    paths:
      - 'data/student-uploads/**.csv'

jobs:
  merge:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - uses: r-lib/actions/setup-r@v2
      - run: |
          Rscript -e "install.packages(c('readr','dplyr'))"
          Rscript scripts/merge-uploads.R
      - uses: stefanzweifel/git-auto-commit-action@v5
        with:
          commit_message: "chore(data): refresh combined dataset"
          file_pattern: data/combined/*.csv
```

---

## 8 · Skripte

### 8.1 `scripts/generate-datasets.R`

**Anforderungen:**
- Deterministisch (fester Seed pro Datensatz, z. B. `seed = 1000 + dataset_nr`)
- Erzeugt 10 CSV-Dateien `data/generated/datensatz-01.csv` … `datensatz-10.csv`
- Jede Datei: 9 Zeilen, 25 Spalten gemäß §4.2
- **BFI-K bereits rekodiert** (alle Items in gleicher Richtung)
- Plausible Verteilungen:
  - Alter: zwischen 18 und 65, leicht jüngere Schiefe (Master-Stichprobe-typisch)
  - Geschlecht: ca. 65 % weiblich, 30 % männlich, 5 % divers (realistisch für Psychologie-Studis)
  - Bildungsgrad: überwiegend 4–6 (Abitur/Bachelor/Master)
  - BFI-K: moderate Korrelationen innerhalb von Faktoren, niedrige zwischen Faktoren — via `MASS::mvrnorm` mit definierter Korrelationsmatrix, dann auf 1–5 diskretisieren
- **Pro Datensatz genau 2 absichtliche Fehler** aus der Liste in §4.4. Die Fehler werden nach festem Schema platziert (deterministisch — der Lehrende muss reproduzieren können, wo sie sind).
- Schreibt am Ende `scripts/error-overview.md` mit einer Tabelle: für jeden Datensatz, welche zwei Fehler in welcher Zeile/Spalte.

**Skript-Struktur:**

```r
# scripts/generate-datasets.R
# Erzeugt 10 Datensätze mit je 9 Probanden + 2 absichtlichen Fehlern.

library(MASS)
library(readr)
library(dplyr)

# --- Konfiguration -----------------------------------------------------------
N_DATASETS <- 10
N_PER_DATASET <- 9
OUT_DIR <- "data/generated"
ERROR_DOC <- "scripts/error-overview.md"

# Korrelationsmatrix der 21 BFI-K-Items (vereinfacht):
# - innerhalb eines Faktors: r ≈ .4
# - zwischen Faktoren: r ≈ .1
# (Die exakten Werte stehen im Skript-Kommentar.)

# --- Funktionen --------------------------------------------------------------
generate_one_dataset <- function(seed, dataset_nr) {
  set.seed(seed)
  # 1. Demografie generieren (geschlecht, alter, bildungsgrad)
  # 2. BFI-K-Items via mvrnorm + Diskretisierung auf 1–5 generieren
  # 3. Datenrahmen zusammenbauen
  # 4. 2 Fehler nach Schema einbauen
  # 5. CSV schreiben
  # 6. Fehler-Info zurückgeben (für error-overview.md)
}

# --- Hauptloop ---------------------------------------------------------------
error_log <- list()
for (i in 1:N_DATASETS) {
  error_log[[i]] <- generate_one_dataset(seed = 1000 + i, dataset_nr = i)
}

# --- Fehler-Übersicht schreiben ----------------------------------------------
# (Markdown-Tabelle mit Datensatz, Zeile, Spalte, Fehlertyp)
```

Vollständige Implementation gehört zum Agent-Auftrag.

### 8.2 `scripts/merge-uploads.R`

**Anforderungen:**
- Liest alle CSVs in `data/student-uploads/`
- Validiert Schema (25 Spalten, korrekte Spaltennamen)
- Fügt eine Spalte `quelle_datei` hinzu (Dateiname)
- Konkateniert alle zu einem Gesamtdatensatz
- Entfernt exakte Duplikate (gleiche Probandennummer + gleicher Datensatz-Herkunft) — sodass die vorgenerierten Probanden nicht mehrfach gezählt werden, wenn mehrere Studierende denselben Datensatz hochgeladen haben. Studierenden-eigene Zeilen (Probandennummer 10 oder höher) bleiben alle erhalten.
- Schreibt `data/combined/m15-gesamt.csv`

### 8.3 `scripts/validate-upload.R` *(optional, niedrige Priorität)*

Wenn Zeit bleibt: PR-Check, der die Spaltenzahl und Spaltennamen einer hochgeladenen CSV gegen das Codebook prüft.

### 8.4 `scripts/error-overview.md` *(intern, nicht in der Site)*

Wird von `generate-datasets.R` automatisch geschrieben. Markdown-Tabelle:

```
| Datensatz | Zeile | Spalte           | Fehlertyp                       |
|-----------|-------|------------------|---------------------------------|
| 01        | 3     | probandennummer  | F1: doppelte Nummer (mit Z. 5)  |
| 01        | 7     | BFI_E2           | F2: fehlender Wert              |
| 02        | …     | …                | …                               |
```

Diese Datei wird **nicht** in `_quarto.yml` als Kapitel eingebunden und ist nicht in der gerenderten Site sichtbar. Der Lehrende hat sie aber im Repo zur Hand.

---

## 9 · Anpassung der vorhandenen Konfigurationen

### 9.1 `_quarto.yml`

Anpassungen im `book:`-Block:
- `title: "M15 — Quantitative Forschungsmethoden"`
- `subtitle: "Workshop HMU Potsdam — Mai/Juni 2026"`
- `author: <Lehrender>`
- `date: "2026-05"`
- `chapters:` neu setzen — Tag-1-Kapitel vollständig, Tag 2 und Tag 3 jeweils nur `00-uebersicht.qmd` als Platzhalter, am Ende `materials/codebook.qmd`

Format-Block (Theme, TOC, Code-Copy) möglichst übernehmen.

### 9.2 `.github/workflows/`

Vorhandenen Site-Build-Workflow anschauen, ggf. Pfade/Branches anpassen. Die zwei neuen Workflows aus §7.2 und §7.3 hinzufügen.

### 9.3 `requirements.txt`

Anschauen, was schon drin ist. R-Pakete liefen typischerweise über `r-lib/actions/setup-r` — dort ggf. zusätzliche Pakete nachziehen. `requirements.txt` ist Python; für R-Workflows nicht zwingend relevant. Wenn das Quarto-Render Python-Bibliotheken braucht (eher unwahrscheinlich bei reinem Markdown), entsprechend ergänzen.

---

## 10 · Materialien (Details)

### 10.1 `materials/bfi-k-fragebogen.qmd` → PDF

Ein druckbares A4-Formular mit:
- Kurzem Intro-Text (1 Absatz: was ist das, wie lange dauert es, anonym/pseudonym)
- 21 Items mit Likert-Skala (1–5)
- **Markierung der Reverse-Items** (z. B. mit `(R)` neben der Item-Nummer)
- Demografie-Felder am Ende (Probandennummer wird vom System / Datensatz vergeben — leer lassen oder ans Pult zeigen)
- Eine kleine Hilfsspalte „rekodierter Wert" rechts neben jedem Item, in die Studierende per Hand den umgepolten Wert eintragen können

Itemtexte aus Rammstedt & John (2005) — der Lehrende fügt sie ein.

### 10.2 `materials/codebook.csv` und `materials/codebook.qmd`

`codebook.csv` (maschinenlesbar, Pflichtspalten):

```
variable,label,type,scale,min,max,values,instrument,reverse_coded
probandennummer,Probandennummer,int,nominal,1,99,,demo,
geschlecht,Geschlecht,int,nominal,1,3,"1=weiblich,2=männlich,3=divers",demo,
alter,Alter,int,metrisch,18,99,,demo,
bildungsgrad,Bildungsgrad,int,ordinal,1,7,"1=ohne ...,7=Promotion",demo,
BFI_E1,Extraversion Item 1,int,ordinal,1,5,,BFI-K,JA
BFI_E2,Extraversion Item 2,int,ordinal,1,5,,BFI-K,NEIN
...
```

`codebook.qmd` ist die human-readable Variante: Erklärung der Spalten, der Skalen-Kodierungen, der Rekodierung, der Datensatzherkunft. Wird als eigenes Kapitel in die Site eingebunden, damit Studierende immer nachschlagen können.

### 10.3 `materials/csv-template-eigene-daten.csv`

Leere CSV mit 25-Spalten-Header + einer Beispiel-Zeile (zum Tippen für Studierende beim ersten Mal, wenn sie ihre eigenen Daten erfassen).

### 10.4 `materials/einverstaendnis.qmd` → PDF

Standard-Einwilligungsformular mit:
- Studienkontext (Lehrveranstaltung, Verantwortlicher: Prof. Schad)
- Beschreibung der erhobenen Daten (BFI-K + Demografie)
- Speicherung in öffentlichem GitHub-Repo
- Pseudonymisierung (keine Namen, nur Probandennummern)
- Recht auf Rückzug bis Workshop-Ende Tag 1
- Alternative: nur vorgenerierte Daten bearbeiten, eigene Antworten weglassen
- Unterschriftsfeld + Datum

---

## 11 · Aufbau-Reihenfolge für den Agent (Tag-1-Implementierung)

1. **Lesen, nicht ersetzen:** existierende `.github/`, `_quarto.yml`, `requirements.txt` ansehen
2. `README.md` aufsetzen (Workshop-Überblick, Site-Link, Quick-Start)
3. `materials/codebook.csv` und `materials/codebook.qmd` als Erstes — Single Source of Truth für Spalten
4. `materials/bfi-k-fragebogen.qmd` (Itemtexte als Platzhalter `[ITEM-TEXT EINFÜGEN — Rammstedt & John 2005, Item E1 etc.]` — der Lehrende füllt das aus)
5. `materials/csv-template-eigene-daten.csv`
6. `materials/einverstaendnis.qmd` + PDF
7. `scripts/generate-datasets.R` (vollständig implementieren, lokal testen)
8. `scripts/merge-uploads.R` (vollständig, lokal testen mit Dummy-Uploads)
9. `.github/workflows/generate-datasets.yml` und `merge-uploads.yml` anlegen
10. Existierenden Site-Build-Workflow anpassen
11. Tag-1-Kapitel (`tag1/00…09.qmd`) — Inhalte vollständig ausschreiben gemäß §5.3
12. Tag 2 und Tag 3: nur `00-uebersicht.qmd` als Platzhalter mit Outline aus §3 anlegen
13. `_quarto.yml` finalisieren (Kapitel-Liste, Metadaten)
14. Lokal `quarto render` ausführen, prüfen
15. Push, Workflows triggern lassen, Site auf gh-pages prüfen

---

## 12 · Was der Agent NICHT tut

- Andere Instrumente als BFI-K einbauen — keine SWLS, kein PHQ, kein GAD
- Existierende Configs (`.github/`, `_quarto.yml`, `requirements.txt`) komplett ersetzen
- Tag 2 oder Tag 3 inhaltlich ausarbeiten — nur Platzhalter
- Screenshots erzeugen, außer im äußersten Ausnahmefall (z. B. erste JASP-Orientierung)
- Eigene Probandendaten erfinden, die echte Personen darstellen sollen — alle generierten Daten sind als synthetisch markiert
- Die Datenstruktur ändern (25 Spalten, BFI-K rekodiert, 9+1 Zeilen)
- „Wenn-Du-hängst"-Boxen oder Lösungs-Toggles einbauen — Studierende fragen den Lehrenden

---

## 13 · Stil-Vorgaben

- Anrede: **„Du"**
- Kurze Absätze
- Callouts sparsam: `note` für Lernziele, `tip` für Hinweise. Kein `warning` oder `important` außer bei wirklich relevanten Hinweisen.
- Code-Blöcke immer mit Sprachangabe
- Kapitel-Anatomie aus §5.3 einhalten
- Lizenz: Inhalte CC BY 4.0 (`LICENSE.md`), Code MIT (`LICENSE-CODE.md`)
- Commit-Messages: Conventional Commits auf Deutsch (`feat(tag1):`, `chore(data):`, `docs:`)

---

## 14 · Abnahmekriterien Tag 1

Der Tag-1-Build gilt als fertig, wenn:

- [ ] `_quarto.yml` enthält alle Tag-1-Kapitel + Platzhalter Tag 2/3 + Codebook
- [ ] `quarto render` läuft lokal ohne Fehler
- [ ] Site baut grün, GitHub Pages ist erreichbar
- [ ] Alle 10 Tag-1-Kapitel sind inhaltlich vollständig (gemäß §5.3)
- [ ] `materials/bfi-k-fragebogen.pdf` ist druckbar (auch wenn Itemtexte Platzhalter sind)
- [ ] `materials/codebook.csv` enthält alle 25 Spalten
- [ ] `materials/einverstaendnis.pdf` ist druckbar
- [ ] `scripts/generate-datasets.R` läuft lokal: erzeugt 10 valide CSV-Dateien à 9 Zeilen mit je 2 Fehlern
- [ ] `scripts/error-overview.md` wird automatisch geschrieben und enthält die korrekte Fehler-Übersicht
- [ ] `scripts/merge-uploads.R` läuft mit Dummy-Daten und erzeugt einen plausiblen Gesamtdatensatz
- [ ] GitHub Action `generate-datasets.yml` baut die 10 Datensätze im Repo
- [ ] GitHub Action `merge-uploads.yml` triggert bei Upload und erzeugt `data/combined/m15-gesamt.csv`
- [ ] README enthält Quick-Start für Studierende und für Lehrenden
- [ ] Lizenz-Dateien vorhanden

---

*Ende CLAUDE.md*