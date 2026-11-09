# Merge-Report

Automatisch erzeugt von `scripts/merge-uploads.R` am 09.11.2026 15:57.

**Gesamtdatensatz:** `data/combined/eingefroren/test2/m15-gesamt.csv` mit **99 Zeilen** (90 vorgeneriert aus 10 Datensätzen, 9 fiktive Personen).

Vorgenerierte Zeilen gehen in der Referenz-Bereinigung ein (Fehlerzellen leer, doppelte Probandennummer korrigiert), unabhängig davon, wie sie in den Uploads bereinigt wurden.

## Dateien

Suche Deine Datei über Deine **Kennziffer** (die letzten 6 Ziffern Deiner Matrikelnummer; andere Zahlen sind auch ok, wichtig ist, dass Du bei einer bleibst). Handeln musst Du nur bei Status IGNORIERT, PRÜFEN oder DOPPELT oder wenn bei „Fehler gefunden“ nicht 2/2 steht. Hinweise zu Dateiname oder Trennzeichen sind unkritisch.

| Datei | Kennziffer | Status | Datensatz | fiktive Personen | Fehler gefunden | Abweichungen von Referenz | Hinweise | Konto |
|---|---|---|---|---:|---|---:|---|---|
| `data/student-uploads/datensatz-01-261831001.csv` | 261831001 | ok | 01 | 1 | 2/2 | 0 | – | HannesDiemerling (für andere hochgeladen) |
| `data/student-uploads/datensatz-02-31009.csv` | 231831009 | ok | 02 | 1 | 2/2 | 0 | – | HannesDiemerling (für andere hochgeladen) |
| `data/student-uploads/datensatz-03-31005.csv` | 261831005 | ok | 03 | 1 | 2/2 | 0 | – | claudiabirkle |
| `data/student-uploads/datensatz-04-31006.csv` | 261831006 | ok | 04 | 1 | 2/2 | 0 | – | MariaZimara |
| `data/student-uploads/datensatz-05-26183.numbers` | 261831012 | ok | 05 | 1 | 2/2 | 0 | Dateiname weicht vom Schema datensatz-XX-<Kennziffer>.csv ab (für den Merge unkritisch) | kemismudzinski |
| `data/student-uploads/datensatz-07-31014.numbers` | 261831014 | ok | 07 | 1 | 1/2 | 1 | Dateiname weicht vom Schema datensatz-XX-<Kennziffer>.csv ab (für den Merge unkritisch) | la03-m |
| `data/student-uploads/datensatz-08 – 31015.csv` | 31015 | ok | 08 | 1 | 2/2 | 4 | Dateiname weicht vom Schema datensatz-XX-<Kennziffer>.csv ab (für den Merge unkritisch); nur 7 von 9 Seed-Zeilen | Mrmcmg |
| `data/student-uploads/datensatz-08-817.jasp` | 261831017 | ok | 08 | 1 | 2/2 | 0 | Dateiname weicht vom Schema datensatz-XX-<Kennziffer>.csv ab (für den Merge unkritisch) | svenjaschmidt1312 |
| `data/student-uploads/datensatz-10-261831036.xlsx` | 261831036 | ok | 10 | 1 | 2/2 | 0 | Dateiname weicht vom Schema datensatz-XX-<Kennziffer>.csv ab (für den Merge unkritisch) | HannesDiemerling (für andere hochgeladen) |

## Weitere Hinweise

- Nicht hochgeladene Seed-Datensätze: 06, 09 (trotzdem im Gesamtdatensatz enthalten)
