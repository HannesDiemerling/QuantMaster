# Merge-Report

Automatisch erzeugt von `scripts/merge-uploads.R` am 04.11.2026 12:38.

**Gesamtdatensatz:** `data/combined/m15-gesamt.csv` mit **91 Zeilen** (90 vorgeneriert aus 10 Datensätzen, 1 fiktive Personen).

Vorgenerierte Zeilen gehen in der Referenz-Bereinigung ein (Fehlerzellen leer, doppelte Probandennummer korrigiert), unabhängig davon, wie sie in den Uploads bereinigt wurden.

## Dateien

Suche Deine Datei über Deine **Kennziffer**. Handeln musst Du nur bei Status IGNORIERT, PRÜFEN oder DOPPELT oder wenn bei „Fehler gefunden“ nicht 2/2 steht. Hinweise zu Dateiname oder Trennzeichen sind unkritisch.

| Datei | Kennziffer | Status | Datensatz | fiktive Personen | Fehler gefunden | Abweichungen von Referenz | Hinweise |
|---|---|---|---|---:|---|---:|---|
| `data/student-uploads/datensatz-03-31005.csv` | 261831005 | ok | 03 | 1 | 2/2 | 0 | – |
| `data/student-uploads/datensatz-08 – 31015.csv` | – | IGNORIERT | – | 0 | – | – | Dateiname weicht vom Schema datensatz-XX-<Kennziffer>.csv ab (für den Merge unkritisch); Spalten fehlen: probandennummer, geschlecht, alter, bildungsgrad, BFI_01 (Kopfzeile fehlt oder Trennzeichen gemischt? Datei im Texteditor prüfen) |

## Weitere Hinweise

- Nicht hochgeladene Seed-Datensätze: 01, 02, 04, 05, 06, 07, 08, 09, 10 (trotzdem im Gesamtdatensatz enthalten)
