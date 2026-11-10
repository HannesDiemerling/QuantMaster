# Merge-Report

Automatisch erzeugt von `scripts/merge-uploads.R` am 10.11.2026 12:54.

**Gesamtdatensatz:** `data/combined/m15-gesamt.csv` mit **96 Zeilen** (90 vorgeneriert aus 10 Datensätzen, 6 fiktive Personen).

Vorgenerierte Zeilen gehen in der Referenz-Bereinigung ein (Fehlerzellen leer, doppelte Probandennummer korrigiert), unabhängig davon, wie sie in den Uploads bereinigt wurden.

## Dateien

Suche Deine Datei über Deine **Kennziffer** (die letzten 6 Ziffern Deiner Matrikelnummer; andere Zahlen sind auch ok, wichtig ist, dass Du bei einer bleibst). Handeln musst Du nur bei Status IGNORIERT, PRÜFEN oder DOPPELT oder wenn bei „Fehler gefunden“ nicht 2/2 steht. Hinweise zu Dateiname oder Trennzeichen sind unkritisch.

| Datei | Kennziffer | Status | Datensatz | fiktive Personen | Fehler gefunden | Abweichungen von Referenz | Hinweise | Konto |
|---|---|---|---|---:|---|---:|---|---|
| `data/student-uploads/Datensatz-10-261831036.csv` | 261831036 | ok | 10 | 1 | 2/2 | 0 | Dateiname weicht vom Schema datensatz-XX-<Kennziffer>.csv ab (für den Merge unkritisch); Trennzeichen ';' statt Komma | luciamexeses-cyber |
| `data/student-uploads/datensatz-01-261831001.csv` | 261831001 | ok | 01 | 1 | 2/2 | 0 | – | HannesDiemerling (für andere hochgeladen) |
| `data/student-uploads/datensatz-01-831011.csv` | 831011 | ok | 01 | 1 | 2/2 | 0 | – | amelieawhmu |
| `data/student-uploads/datensatz-02-31009.csv` | 231831009 | ok | 02 | 1 | 2/2 | 0 | – | HannesDiemerling (für andere hochgeladen) |
| `data/student-uploads/datensatz-03-31005.csv` | 261831005 | ok | 03 | 1 | 2/2 | 0 | – | claudiabirkle |
| `data/student-uploads/datensatz-04-31006.csv` | 261831006 | ok | 04 | 1 | 2/2 | 0 | – | MariaZimara |
| `data/student-uploads/datensatz-08 – 31015.csv` | – | IGNORIERT | – | 0 | – | – | Dateiname weicht vom Schema datensatz-XX-<Kennziffer>.csv ab (für den Merge unkritisch); Spalten fehlen: probandennummer, geschlecht, alter, bildungsgrad, BFI_01 (Kopfzeile fehlt oder Trennzeichen gemischt? Datei im Texteditor prüfen) | Mrmcmg |

## Weitere Hinweise

- Nicht hochgeladene Seed-Datensätze: 05, 06, 07, 08, 09 (trotzdem im Gesamtdatensatz enthalten)
- Ignoriert (kein CSV): `data/student-uploads/datensatz-04-1002.jasp` (Konto: Clara Rummler)
- Ignoriert (kein CSV): `data/student-uploads/datensatz-05-26183.numbers` (Konto: kemismudzinski)
- Ignoriert (kein CSV): `data/student-uploads/datensatz-07-31014.numbers` (Konto: la03-m)
- Ignoriert (kein CSV): `data/student-uploads/datensatz-08-817.jasp` (Konto: svenjaschmidt1312)
- Ignoriert (kein CSV): `data/student-uploads/datensatz-10-261831036.xlsx` (Konto: luciamexeses-cyber)
- Ignoriert (kein CSV): `data/student-uploads/Datensatz-10-261831036.xlsx` (Konto: luciamexeses-cyber)
- Ignoriert (kein CSV): `data/student-uploads/datensatz07_261831031.jasp` (Konto: janinakilian)
