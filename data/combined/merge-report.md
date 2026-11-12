# Merge-Report

Automatisch erzeugt von `scripts/merge-uploads.R` am 12.11.2026 17:51.

**Stand der Uploads:** 112 Zeilen (90 vorgeneriert aus 10 Datensätzen, 22 fiktive Personen). Den Datensatz zum Rechnen findest Du im Ordner Deiner Gruppe unter [`data/combined/eingefroren/`](eingefroren/). Der Ordner entsteht, sobald der Stand Deiner Gruppe eingefroren ist.

Vorgenerierte Zeilen gehen in der Referenz-Bereinigung ein (Fehlerzellen leer, doppelte Probandennummer korrigiert), unabhängig davon, wie sie in den Uploads bereinigt wurden.

## Dateien

Suche Deine Datei über Deine **Kennziffer** (die letzten 6 Ziffern Deiner Matrikelnummer; andere Zahlen sind auch ok, wichtig ist, dass Du bei einer bleibst). Handeln musst Du nur bei Status IGNORIERT, PRÜFEN oder DOPPELT oder wenn bei „Fehler gefunden“ nicht 2/2 steht. Bei Status „umgewandelt“ ist nichts zu tun. Hinweise zu Dateiname oder Trennzeichen sind unkritisch.

| Datei | Kennziffer | Status | Datensatz | fiktive Personen | Fehler gefunden | Abweichungen von Referenz | Hinweise | Konto |
|---|---|---|---|---:|---|---:|---|---|
| `data/student-uploads/Datensatz-10-261831036.csv` | 261831036 | ok | 10 | 1 | 2/2 | 0 | Dateiname weicht vom Schema datensatz-XX-<Kennziffer>.csv ab (für den Merge unkritisch); Trennzeichen ';' statt Komma | luciamexeses-cyber |
| `data/student-uploads/datensatz-01-261831001.csv` | 261831001 | ok | 01 | 1 | 2/2 | 0 | – | HannesDiemerling (für andere hochgeladen) |
| `data/student-uploads/datensatz-01-831004.csv` | 831004 | ok | 01 | 1 | 2/2 | 0 | Trennzeichen ';' statt Komma | nele-ui |
| `data/student-uploads/datensatz-01-831011.csv` | 831011 | ok | 01 | 1 | 2/2 | 0 | – | amelieawhmu |
| `data/student-uploads/datensatz-02-261831037.csv` | – | PRÜFEN | 02 | 0 | 2/2 | 0 | Trennzeichen ';' statt Komma; nur 7 von 9 Seed-Zeilen; keine Zeile einer fiktiven Person gefunden | lisasophierabe |
| `data/student-uploads/datensatz-02-31009.csv` | 231831009 | ok | 02 | 1 | 2/2 | 0 | – | HannesDiemerling (für andere hochgeladen) |
| `data/student-uploads/datensatz-03-261831038.csv` | 261831038 | ok | 03 | 1 | 2/2 | 0 | – | b9fcy2z6m8-creator |
| `data/student-uploads/datensatz-03-31005.csv` | 261831005 | ok | 03 | 1 | 2/2 | 0 | – | claudiabirkle |
| `data/student-uploads/datensatz-03-831023.csv` | 831023 | ok | 03 | 1 | 2/2 | 0 | – | Lene26 |
| `data/student-uploads/datensatz-03-831033.csv` | 831033 | ok | 03 | 1 | 2/2 | 0 | Zusatzspalten ignoriert: column 26, column 27, column 28, column 29, column 30, column 31, column 32; nur 8 von 9 Seed-Zeilen | ampfeil |
| `data/student-uploads/datensatz-04-31006.csv` | 261831006 | ok | 04 | 1 | 2/2 | 0 | – | MariaZimara |
| `data/student-uploads/datensatz-04-831024.csv` | 831024 | ok | 04 | 1 | 2/2 | 0 | – | larapff |
| `data/student-uploads/datensatz-04-831034.csv` | 831034 | ok | 04 | 1 | 2/2 | 0 | – | BenjaminKaempfer |
| `data/student-uploads/datensatz-04-831040.csv` | 831040 | ok | 04 | 1 | 2/2 | 0 | – | Tobiaseckelmann |
| `data/student-uploads/datensatz-05-831025.csv` | 831025 | ok | 05 | 1 | 2/2 | 0 | nur 8 von 9 Seed-Zeilen | luise2205 |
| `data/student-uploads/datensatz-06-31013.csv` | – | IGNORIERT | – | 0 | – | – | Spalten fehlen: probandennummer, geschlecht, alter, bildungsgrad, BFI_01 (Kopfzeile fehlt oder Trennzeichen gemischt? Datei im Texteditor prüfen) | HannesDiemerling (für andere hochgeladen) |
| `data/student-uploads/datensatz-06-831026.csv` | 1831026 | ok | 06 | 1 | 2/2 | 0 | – | lenemarieweinert |
| `data/student-uploads/datensatz-07-831007.csv` | 831007 | ok | 07 | 1 | 2/2 | 1 | – | chiaralorenzhmu07 |
| `data/student-uploads/datensatz-07-831027.csv` | 831027 | ok | 07 | 1 | 1/2 | 1 | – | annarcw |
| `data/student-uploads/datensatz-08 – 31015.csv` | – | IGNORIERT | – | 0 | – | – | Dateiname weicht vom Schema datensatz-XX-<Kennziffer>.csv ab (für den Merge unkritisch); Spalten fehlen: probandennummer, geschlecht, alter, bildungsgrad, BFI_01 (Kopfzeile fehlt oder Trennzeichen gemischt? Datei im Texteditor prüfen) | Mrmcmg |
| `data/student-uploads/datensatz-08-831008.csv` | 831008 | ok | 08 | 1 | 2/2 | 0 | – | HB0412 |
| `data/student-uploads/datensatz-08-831028.csv` | 10 | ok | 08 | 1 | 1/2 | 1 | – | PaulinaSchaubert |
| `data/student-uploads/datensatz-09-31030 bereinigt.csv` | 31030 | ok | 09 | 1 | 2/2 | 0 | Dateiname weicht vom Schema datensatz-XX-<Kennziffer>.csv ab (für den Merge unkritisch) | Siahmardy |
| `data/student-uploads/datensatz-09-831029.csv` | 831029 | PRÜFEN | 09 | 1 | 2/2 | 0 | Zusatzspalten ignoriert: column 26, column 27, column 28, column 29, column 30, column 31, column 32; nur 8 von 9 Seed-Zeilen; 1 Wert(e) der fiktiven Person fehlen oder sind keine Zahl | melinaahke |
| `data/student-uploads/datensatz-10-831020.csv` | 831020 | ok | 10 | 1 | 2/2 | 0 | – | michellekhl |
| `data/student-uploads/Datensatz-10-261831036-2.xlsx` | 261831036 | umgewandelt | – | 0 | – | – | keine CSV-Datei, ist in einem eingefrorenen Stand enthalten und wird beim Einfrieren automatisch umgewandelt. Nichts zu tun | luciamexeses-cyber |
| `data/student-uploads/datensatz-04-1002.jasp` | 261831002 | umgewandelt | – | 0 | – | – | keine CSV-Datei, ist in einem eingefrorenen Stand enthalten und wird beim Einfrieren automatisch umgewandelt. Nichts zu tun | Clara Rummler |
| `data/student-uploads/datensatz-05-1003.jasp` | 261831003 | umgewandelt | – | 0 | – | – | keine CSV-Datei, ist in einem eingefrorenen Stand enthalten und wird beim Einfrieren automatisch umgewandelt. Nichts zu tun | theresaschwanebeck |
| `data/student-uploads/datensatz-05-26183.numbers` | 261831012 | umgewandelt | – | 0 | – | – | keine CSV-Datei, ist in einem eingefrorenen Stand enthalten und wird beim Einfrieren automatisch umgewandelt. Nichts zu tun | kemismudzinski |
| `data/student-uploads/datensatz-06-831016.csv.jasp` | 831016 | IGNORIERT | – | 0 | – | – | keine CSV-Datei. In JASP: File → Export Data als CSV und als `datensatz-06-831016.csv` hochladen | nigrin170 |
| `data/student-uploads/datensatz-06_1018.numbers` | 261831018 | umgewandelt | – | 0 | – | – | keine CSV-Datei, ist in einem eingefrorenen Stand enthalten und wird beim Einfrieren automatisch umgewandelt. Nichts zu tun | Tommytrn02 |
| `data/student-uploads/datensatz-07-31014.numbers` | 261831014 | umgewandelt | – | 0 | – | – | keine CSV-Datei, ist in einem eingefrorenen Stand enthalten und wird beim Einfrieren automatisch umgewandelt. Nichts zu tun | la03-m |
| `data/student-uploads/datensatz-08-817.jasp` | 261831017 | umgewandelt | – | 0 | – | – | keine CSV-Datei, ist in einem eingefrorenen Stand enthalten und wird beim Einfrieren automatisch umgewandelt. Nichts zu tun | svenjaschmidt1312 |
| `data/student-uploads/datensatz-10-261831036.xlsx` | 261831036 | umgewandelt | – | 0 | – | – | keine CSV-Datei, ist in einem eingefrorenen Stand enthalten und wird beim Einfrieren automatisch umgewandelt. Nichts zu tun | luciamexeses-cyber |
| `data/student-uploads/datensatz07_261831031.jasp` | 261831031 | umgewandelt | – | 0 | – | – | keine CSV-Datei, ist in einem eingefrorenen Stand enthalten und wird beim Einfrieren automatisch umgewandelt. Nichts zu tun | janinakilian |
