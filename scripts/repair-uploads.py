#!/usr/bin/env python3
"""Repariert Studierenden-Uploads beim Einfrieren, damit möglichst niemand verloren geht.

Aufruf:  python scripts/repair-uploads.py <quelle> <ziel>
Ausgabe: <ziel>/uploads/...         reparierte Kopien, unveränderte Dateien bytegleich
         <ziel>/reparatur-log.tsv   Datei <Tab> was gemacht wurde

Die Originale bleiben unangetastet, der Live-Merge sieht diese Reparaturen nicht
(dort sollen Studierende ihre Datei weiterhin selbst korrigieren können).
Das Skript bricht nie wegen einer einzelnen Datei ab: Was nicht rettbar ist, wird
unverändert kopiert und vom Merge wie gewohnt gemeldet.

Gerettet werden andere Formate (.numbers, .jasp, .xlsx, auch falsch benannt),
Titelzeilen über dem Kopf, fehlende oder abweichend benannte Kopfzeilen, gemischte
Trennzeichen, wiederholte Kopfzeilen, beschädigte Übungszeilen und mehrere fiktive
Personen in einer Datei. Nur stdlib; numbers-parser und openpyxl sind optional.
"""
import csv
import io
import math
import os
import re
import shutil
import sqlite3
import sys
import tempfile
import zipfile

COLS = ["probandennummer", "geschlecht", "alter", "bildungsgrad"] + [f"BFI_{i:02d}" for i in range(1, 22)]
BFI = COLS[4:]
SYN = {
    "probandennummer": ["proband", "probandnr", "probandennr", "proband_nr", "probanden_nr", "probanden-nummer", "probandnummer",
                        "kennziffer", "vp", "vpn", "pn", "nr", "nummer", "matrikelnummer", "id"],
    "geschlecht": ["gender", "sex"],
    "alter": ["age", "alter_jahre"],
    "bildungsgrad": ["bildung", "bildungsstand", "bildungsabschluss", "education", "abschluss"],
}
MIN_MATCH = 19
MIN_NEAR = 14    # so viele BFI-Werte stimmen bei einer beschädigten oder umgepolten Übungszeile noch
MAX_BYTES = 20 * 1024 * 1024
MAX_ZEILEN = 1000             # ein normaler Upload hat 11 Zeilen; riesige Texte würden den Merge lahmlegen
MIN_NEAR_OHNE = 18            # strenger, wenn die Datei gar keine erkennbare Übungszeile enthält
FUSS = re.compile(r"^(mittelwert|mean|summe|sum|anzahl|n|sd|standardabweichung|hinweis|anmerkung|gesamt|total)\b", re.I)
REVERSE = [0, 1, 7, 8, 10, 11, 16, 20]   # Items 1, 2, 8, 9, 11, 12, 17, 21 (Index ab 0)
REF_DIR = os.path.join(os.path.dirname(os.path.abspath(__file__)), "referenz")
NUM = re.compile(r"^-?\d+([.,]\d+)?$")


class Unrettbar(Exception):
    pass


class ZuGross(Exception):
    pass


def canon(name):
    x = str(name).replace("﻿", "").replace(" ", " ").strip().strip('"\'').strip().lower()
    m = re.match(r"^bfi[\s_.\-]*0*(\d{1,2})[\s_]*r?$", x)
    if m and 1 <= int(m.group(1)) <= 21:
        return f"BFI_{int(m.group(1)):02d}"
    return x


def header_map(cells):
    """Spaltenname -> Index, oder None, wenn das keine Kopfzeile ist."""
    names = [canon(c) for c in cells]
    out = {}
    for col in COLS:
        if col in names:
            out[col] = names.index(col)
    for col, syns in SYN.items():
        if col not in out:
            for s in syns:
                if s in names:
                    out[col] = names.index(s)
                    break
    return out if sum(c in out for c in BFI) >= 18 else None


def text_cell(v):
    if v is None:
        return ""
    if isinstance(v, bool):
        return str(int(v))
    if isinstance(v, (int, float)):
        if isinstance(v, float) and (math.isnan(v) or math.isinf(v)):
            return ""
        return str(int(round(v))) if abs(v - round(v)) < 1e-6 else str(v)
    return str(v).strip()


# --- Einlesen je Format ---------------------------------------------------------------

def has_header(rows):
    return any(header_map(r) for r in rows[:15])


def better(rows, best):
    """Ein Blatt mit erkennbarer Kopfzeile schlägt ein größeres ohne (Notizen, Codebook)."""
    if best is None:
        return True
    if has_header(rows) != has_header(best):
        return has_header(rows)
    return len(rows) > len(best)


def read_numbers(path):
    try:
        from numbers_parser import Document
    except ImportError:
        raise Unrettbar("numbers-parser nicht installiert, .numbers nicht lesbar")
    best = None
    with tempfile.TemporaryDirectory(ignore_cleanup_errors=True) as d:   # numbers-parser verlangt die Endung .numbers
        kopie = os.path.join(d, "datei.numbers")
        shutil.copyfile(path, kopie)
        for sheet in Document(kopie).sheets:
            for table in sheet.tables:
                rows = [[text_cell(c) for c in r] for r in table.rows(values_only=True)]
                if better(rows, best):
                    best = rows
    if not best:
        raise Unrettbar(".numbers-Datei enthält keine Tabelle")
    return best


def read_xlsx(path):
    try:
        import openpyxl
    except ImportError:
        raise Unrettbar("openpyxl nicht installiert, .xlsx nicht lesbar")
    with open(path, "rb") as f:                       # über Bytes, damit auch umbenannte Dateien gehen
        wb = openpyxl.load_workbook(io.BytesIO(f.read()), read_only=True, data_only=True)
    best = None
    for ws in wb.worksheets:
        rows = [[text_cell(c) for c in r] for r in ws.iter_rows(values_only=True)]
        rows = [r for r in rows if any(r)]
        if better(rows, best):
            best = rows
    if not best:
        raise Unrettbar(".xlsx-Datei ist leer")
    return best


def read_jasp(path, z):
    with tempfile.TemporaryDirectory() as d:
        z.extract("internal.sqlite", d)
        con = sqlite3.connect(os.path.join(d, "internal.sqlite"))
        try:
            cols = con.execute("select id, name from Columns order by colIdx").fetchall()
            labels = {(c, v): l for c, v, l in con.execute("select columnId, value, label from Labels")}
            names = [r[1] for r in con.execute("pragma table_info(DataSet_1)")]
            data = con.execute("select * from DataSet_1 order by rowNumber").fetchall()
        finally:
            con.close()
    rows = [[n for _, n in cols]]
    for rec in data:
        row = dict(zip(names, rec))
        out = []
        for cid, _ in cols:
            dbl, val = row.get(f"Column_{cid}_DBL"), row.get(f"Column_{cid}_INT")
            if isinstance(dbl, float) and not math.isnan(dbl):
                out.append(text_cell(dbl))
            elif val is not None and val >= 0:
                out.append(labels.get((cid, val), text_cell(val)))
            else:
                out.append("")
        rows.append(out)
    return rows


def decode(raw):
    if raw[:2] == b"\xff\xfe":
        return raw[2:].decode("utf-16-le", "replace")
    if raw[:2] == b"\xfe\xff":
        return raw[2:].decode("utf-16-be", "replace")
    if b"\0" in raw:
        raise Unrettbar("keine Textdatei")
    if raw[:3] == b"\xef\xbb\xbf":
        raw = raw[3:]
    try:
        return raw.decode("utf-8")
    except UnicodeDecodeError:
        return raw.decode("latin-1")


def split(line, sep):
    return next(csv.reader([line], delimiter=sep, quotechar='"', skipinitialspace=True), [])


def strip_outer(line):
    """Ganze Zeile in Anführungszeichen, wie Excel sie bei falschem Import speichert."""
    s = line.strip()
    if len(s) > 1 and s[0] == '"' and s[-1] == '"' and s.count('"') == 2:
        return s[1:-1]
    return line


def parse_text(text):
    lines = [l for l in re.split(r"\r\n|\n|\r", text)]
    if lines and re.match(r'^\s*"?sep=', lines[0], re.I):
        lines = lines[1:]
    roh = [l for l in lines if re.sub(r'[,;\t"\s]', "", l)]
    lines = [strip_outer(l) for l in roh]
    if not lines:
        raise Unrettbar("Datei ist leer")
    zitiert = sum(a != b for a, b in zip(roh, lines))
    sep = None
    head = None
    for i, l in enumerate(lines[:15]):
        for s in [",", ";", "\t"]:
            if header_map(split(l, s)):
                sep, head = s, i
                break
        if sep:
            break
    notes = [f"{zitiert} Zeile(n) standen ganz in Anführungszeichen"] if zitiert else []
    if sep is None and len(lines) > 1:
        for s in [",", ";", "\t"]:
            kopf, zeile = split(lines[0], s), split(lines[1], s)
            if len(kopf) == 25 and len(zeile) == 25 and sum(bool(NUM.match(x.strip())) for x in kopf) < 5 \
                    and sum(bool(NUM.match(x.strip())) for x in zeile) >= 20:
                sep, head = s, 0
                break
    if sep is None:
        counts = {s: max(l.count(s) for l in lines) for s in [",", ";", "\t"]}
        sep = max(counts, key=counts.get)
        return [split(l, sep) for l in lines], lines, sep, None, notes
    if head > 0:
        notes.append(f"{head} Zeile(n) über der Kopfzeile entfernt")
    return [split(l, sep) for l in lines[head:]], lines[head:], sep, 0, notes


# --- Prüfung und Zuordnung -------------------------------------------------------------

def load_refs():
    refs = []
    for i in range(1, 11):
        with open(os.path.join(REF_DIR, f"datensatz-{i:02d}.csv"), encoding="utf-8", newline="") as f:
            r = list(csv.DictReader(f))
        refs.append([[row[b].strip() for b in BFI] for row in r])
    return refs


def norm(v):
    v = str(v).strip().replace(",", ".")
    try:
        x = float(v)
    except ValueError:
        return ""
    return str(int(x)) if x.is_integer() else str(x)


def match_seed(cells, refs):
    """(Datensatz, Zeile) der besten Übungszeile oder None, Regel wie in merge-uploads.R."""
    best, treffer = -1, None
    vals = [norm(c) for c in cells]
    for d, rows in enumerate(refs, 1):
        for k, r in enumerate(rows):
            n = sum(norm(a) == b for a, b in zip(r, vals))
            if n > best:
                best, treffer = n, (d, k)
    return treffer if best >= MIN_MATCH else None


def near_row(cells, refs, kandidaten, grenze):
    """(Datensatz, Zeile) der ähnlichsten Übungszeile, auch umgepolt, oder None."""
    vals = [norm(c) for c in cells]
    umgepolt = [str(6 - int(v)) if i in REVERSE and v.isdigit() else v for i, v in enumerate(vals)]
    best, treffer = 0, None
    for d in kandidaten:
        for k, r in enumerate(refs[d - 1]):
            n = max(sum(norm(a) == b for a, b in zip(r, v)) for v in (vals, umgepolt))
            if n > best:
                best, treffer = n, (d, k)
    return treffer if best >= grenze else None


def valid_kz(v):
    v = str(v).strip()
    return v.isdigit() and 1000 <= int(v) < 10**12


def fix_kz(v, kz_name):
    s = re.sub(r"[\s '’]", "", str(v))
    if re.fullmatch(r"\d{1,3}(\.\d{3})+", s):
        return s.replace(".", "")
    if re.fullmatch(r"\d+[.,]0+", s):
        return re.split(r"[.,]", s)[0]
    if re.fullmatch(r"\d(\.\d+)?[eE]\+?\d+", s.replace(",", ".")):
        return kz_name   # Excel hat Ziffern abgeschnitten: ohne Dateinamen lieber leer als erfunden
    return s


def fix_value(col, v):
    s = str(v).replace(" ", " ").strip()
    if not s:
        return s
    m = re.fullmatch(r"(\d{2})\s*(jahre?|j\.?)", s, re.I) if col == "alter" else None
    if m:
        return m.group(1)
    m = re.match(r"^(\d)(\s*[=:\u2013]|\s+-)\s*\D", s) if col != "alter" else None   # 3-Mai bleibt: Excel-Datum
    return m.group(1) if m else s


def repair_table(cells, raw, sep, head, name, refs, log):
    """cells: Zeilen als Listen, raw: Originalzeilen (oder None bei Tabellenformaten)."""
    changed = False
    if head is None:
        # keine Kopfzeile: nur nehmen, wenn die erste Zeile wirklich 25 Zahlen/Leerfelder hat
        first = next((c for c in cells if any(x.strip() for x in c)), [])
        if len(first) != 25 or not all(NUM.match(x.strip()) or not x.strip() for x in first):
            raise Unrettbar("Spalten nicht erkennbar")
        hm = {c: i for i, c in enumerate(COLS)}
        body = list(range(len(cells)))
        log("Kopfzeile fehlte, Spalten nach Reihenfolge zugeordnet")
        changed = True
    else:
        hm = header_map(cells[head])
        if not hm or any(c not in hm for c in COLS):
            if len(cells[head]) == 25:
                hm = hm or {}
                fehlend = [c for c in COLS if c not in hm]
                frei = [i for i in range(25) if i not in hm.values()]
                hm.update(zip(fehlend, frei))
                log(f"{len(fehlend)} unbekannte Spaltenname(n) nach Reihenfolge zugeordnet")
                changed = True
            else:
                fehlt = [c for c in COLS if not hm or c not in hm]
                raise Unrettbar("Spalten fehlen: " + ", ".join(fehlt[:5]))
        elif any(not merge_kennt(cells[head][i], c) for c, i in hm.items()):
            log("abweichende Spaltennamen zugeordnet")
            changed = True
        body = list(range(head + 1, len(cells)))

    n_cols = len(cells[head]) if head is not None else 25
    rows, wiederholt, gemischt, leer, schluss, komma, unlesbar, fuss = [], 0, 0, 0, 0, 0, 0, 0
    if n_cols > 25:
        log(f"{n_cols - 25} Zusatzspalte(n) nicht übernommen")
    for i in body:
        c = cells[i]
        if head is not None and header_map(c):
            wiederholt += 1
            continue
        if len(c) > n_cols and not any(x.strip() for x in c[n_cols:]):
            c = c[:n_cols]
            schluss += 1
        if len(c) != n_cols and raw is not None:
            for s in [",", ";", "\t", None]:
                if s != sep:
                    alt = re.split(r"\s+", raw[i].strip()) if s is None else split(raw[i], s)
                    while len(alt) > n_cols and not alt[-1].strip():
                        alt = alt[:-1]
                    if len(alt) == n_cols:
                        c = alt
                        gemischt += 1
                        break
        if len(c) > n_cols:
            # Dezimalkomma in einer Kommadatei: Eine 0 als eigenes Feld gibt es sonst nicht
            neu = []
            for x in c:
                if neu and x.strip() == "0" and re.fullmatch(r"\d+", neu[-1].strip()):
                    neu[-1] = neu[-1].strip()
                else:
                    neu.append(x)
            if len(neu) == n_cols:
                c = neu
                komma += 1
            else:
                unlesbar += 1          # lieber keine Zeile als verschobene Spalten
                continue
        row = {col: (c[j].strip() if j < len(c) else "") for col, j in hm.items()}
        if FUSS.match(row["probandennummer"]):
            fuss += 1
            continue
        if sum(bool(v) for v in row.values()) <= 1:
            leer += 1
            continue
        rows.append(row)
    if wiederholt:
        log(f"{wiederholt} wiederholte Kopfzeile(n) entfernt")
        changed = True
    if gemischt:
        log(f"{gemischt} Zeile(n) mit anderem Trennzeichen repariert")
        changed = True
    if leer:
        log(f"{leer} leere Zeile(n) entfernt")
        changed = True
    if fuss:
        log(f"{fuss} Fußzeile(n) wie „Mittelwert“ entfernt")
        changed = True
    if schluss:
        log(f"{schluss} Zeile(n) mit überzähligem Trennzeichen am Ende repariert")
        changed = True
    if komma:
        log(f"{komma} Zeile(n) mit Dezimalkomma repariert")
        changed = True
    if unlesbar:
        log(f"{unlesbar} Zeile(n) mit zu vielen Feldern verworfen (Dezimalkomma in einer Kommadatei?)")
        changed = True

    m = re.match(r"^datensatz[\s\-_\u2013\u2014]*(\d{1,2})[\s\-_\u2013\u2014]+(\d{4,12})(?=\s*(\(\d+\))?\.[A-Za-z])", name, re.I)
    kz_name = str(int(m.group(2))) if m else ""
    ds_name = int(m.group(1)) if m else None
    for row in rows:
        for col in COLS:
            alt = row[col]
            neu = fix_kz(alt, kz_name) if col == "probandennummer" else fix_value(col, alt)
            if neu != alt:
                log(f"Wert in {col}: „{alt}“ zu „{neu}“")
                row[col] = neu
                changed = True

    # Übungszeilen von fiktiven Personen trennen
    treffer = [match_seed([r[b] for b in BFI], refs) for r in rows]
    ds_rows = [t[0] if t else None for t in treffer]
    gefunden = sorted({d for d in ds_rows if d})
    if sum(1 for d in gefunden if ds_rows.count(d) >= 5) > 1:
        log("kombinierte Datei (mehrere Übungsdatensätze), keine Reparatur")
        return rows, [], changed
    # Fremde Übungszeilen raus: Es zählt der Datensatz mit den meisten Zeilen, bei Gleichstand der Dateiname
    if len(gefunden) > 1:
        top = max(ds_rows.count(d) for d in gefunden)
        kand = [d for d in gefunden if ds_rows.count(d) == top]
        ziel_ds = ds_name if ds_name in kand else kand[0]
        # Eine fremde Übungszeile mit gültiger Kennziffer ist eine abgeschriebene Person
        for i, d in enumerate(ds_rows):
            if d and d != ziel_ds and valid_kz(rows[i]["probandennummer"]):
                ds_rows[i] = None
                treffer[i] = None
        weg = [i for i, d in enumerate(ds_rows) if d and d != ziel_ds]
        log(f"{len(weg)} Übungszeile(n) aus anderem Datensatz entfernt, Datensatz {ziel_ds:02d} bleibt")
        rows = [r for i, r in enumerate(rows) if i not in weg]
        treffer = [t for i, t in enumerate(treffer) if i not in weg]
        ds_rows = [d for i, d in enumerate(ds_rows) if i not in weg]
        gefunden = [ziel_ds]
        changed = True
    # Zwei Zeilen treffen dieselbe Übungszeile, eine mit Kennziffer: abgeschriebene fiktive Person
    for t in set(x for x in treffer if x):
        gruppe = [i for i, x in enumerate(treffer) if x == t]
        if len(gruppe) > 1 and any(not valid_kz(rows[i]["probandennummer"]) for i in gruppe):
            for i in gruppe:
                if valid_kz(rows[i]["probandennummer"]):
                    ds_rows[i] = None
    andere = [i for i, d in enumerate(ds_rows) if d is None]
    # Eine beschädigte oder umgepolte Übungszeile ist keine Person. Ist die Übungszeile aber
    # schon in der Datei vorhanden, ist die ähnliche Zeile eine abgeschriebene Person.
    haupt = [max(gefunden, key=ds_rows.count)] if gefunden else list(range(1, 11))
    belegt = {t for t in treffer if t}
    beschaedigt = []
    if len(rows) > 1:
        kzs = [rows[i]["probandennummer"] for i in andere]
        for i in andere:
            pn = rows[i]["probandennummer"]
            if pn and pn == kz_name and kzs.count(pn) == 1:
                continue                # die eine Zeile mit der Kennziffer aus dem Dateinamen ist die Person
            nah = near_row([rows[i][b] for b in BFI], refs, haupt, MIN_NEAR if gefunden else MIN_NEAR_OHNE)
            # Nummer 1 bis 9: immer Übungszeile. Sonst nur, wenn die Referenzzeile noch fehlt
            if nah and (pn in list("123456789") or nah not in belegt):
                beschaedigt.append(i)
    if beschaedigt:
        log(f"{len(beschaedigt)} beschädigte oder umgepolte Übungszeile(n) mit Nummer {', '.join(rows[i]['probandennummer'] for i in beschaedigt)} nicht als Person gewertet. Waren sie umgepolt, kann auch die Person umgepolt sein")
        rows = [r for i, r in enumerate(rows) if i not in beschaedigt]
        ds_rows = [d for i, d in enumerate(ds_rows) if i not in beschaedigt]
        andere = [i for i, d in enumerate(ds_rows) if d is None]
        changed = True
    personen = []
    if len(andere) > 1:
        # Personen sind alle Zeilen mit einer anderen Nummer als 1 bis 9, auch ungültige wie 817
        klein = list("123456789") + [""]
        voll = [i for i in andere if valid_kz(rows[i]["probandennummer"])
                or sum(bool(rows[i][b]) for b in BFI) >= 15] or andere   # fast leere Zeilen zählen nicht
        kandidaten = [i for i in voll if rows[i]["probandennummer"] not in klein]
        if kandidaten:
            personen = kandidaten
        else:
            personen = [voll[-1]]
            log(f"letzte Zeile als fiktive Person genommen (Kennziffer „{rows[voll[-1]]['probandennummer']}“)")
        if len(andere) > len(personen):
            log(f"{len(andere) - len(personen)} Zeile(n) nicht als Person gewertet (Fußzeile oder beschädigte Übungszeile)")
        changed = True
    einzel = personen[0] if len(personen) == 1 else andere[0] if len(andere) == 1 else None
    if einzel is not None and not valid_kz(rows[einzel]["probandennummer"]) and valid_kz(kz_name):
        log(f"Kennziffer „{rows[einzel]['probandennummer']}“ durch {kz_name} aus dem Dateinamen ersetzt")
        rows[einzel]["probandennummer"] = kz_name
        changed = True
    seed_idx = [i for i, d in enumerate(ds_rows) if d]
    sets = []
    if personen:
        for p in personen:
            sets.append([rows[i] for i in seed_idx] + [rows[p]])
        if len(personen) > 1:
            log(f"{len(personen)} fiktive Personen in einer Datei, einzeln übernommen")
        return sets[0], sets[1:], changed
    return rows, [], changed


def merge_kennt(cell, col):
    """Versteht merge-uploads.R diesen Spaltennamen ohne Reparatur?"""
    if "\u00a0" in str(cell):     # geschütztes Leerzeichen versteht der Merge nicht
        return False
    x = str(cell).replace('"', "").strip().lower()
    if col in BFI:
        m = re.fullmatch(r"bfi_?0*(\d+)", x)
        return bool(m) and int(m.group(1)) == int(col[4:])
    return x == col


def out_name(rel):
    return rel if re.search(r"\.(csv|txt|tsv)$", rel, re.I) else rel + ".csv"


def write_csv(path, rows):
    os.makedirs(os.path.dirname(path), exist_ok=True)
    with open(path, "w", encoding="utf-8", newline="") as f:
        w = csv.writer(f, lineterminator="\n")
        w.writerow(COLS)
        for r in rows:
            w.writerow([r[c] for c in COLS])


def repair_file(src, rel, dst_dir, refs, log):
    """Gibt True zurück, wenn eine reparierte Fassung geschrieben wurde."""
    raw = open(src, "rb").read()
    binaer = raw[:4] in (b"PK\x03\x04", b"\xd0\xcf\x11\xe0") or raw[:5] == b"%PDF-"
    if len(raw) > MAX_BYTES:
        raise ZuGross(f"Datei zu groß ({len(raw) // 1024} KB), nicht verwendet")
    if raw[:4] == b"PK\x03\x04":
        z = zipfile.ZipFile(src)
        names = z.namelist()
        if "xl/workbook.xml" in names:
            table, kind = read_xlsx(src), "Excel"
        elif any(n.startswith("Index/") and n.endswith(".iwa") for n in names):
            table, kind = read_numbers(src), "Numbers"
        elif "internal.sqlite" in names:
            table, kind = read_jasp(src, z), "JASP"
        else:
            raise Unrettbar("unbekanntes ZIP-Format")
        log(f"{kind}-Datei in CSV umgewandelt")
        # Tabellen wie Text weiterverarbeiten, auch alles in einer Spalte
        if all(sum(bool(c) for c in r) <= 1 for r in table[:3]):
            text = "\n".join(next((c for c in r if c), "") for r in table)
            cells, lines, sep, head, notes = parse_text(text)
        else:
            head = next((i for i, r in enumerate(table[:15]) if header_map(r)), None)
            cells, lines, sep, notes = table, None, ",", []
            if head:
                notes.append(f"{head} Zeile(n) über der Kopfzeile entfernt")
        forced = True
    elif raw[:4] == b"\xd0\xcf\x11\xe0":
        raise Unrettbar("altes Excel-Format (.xls)")
    elif raw[:5] == b"%PDF-":
        raise Unrettbar("PDF-Datei")
    else:
        cells, lines, sep, head, notes = parse_text(decode(raw))
        if len(lines) > MAX_ZEILEN:
            raise ZuGross(f"Datei zu groß ({len(lines)} Zeilen), nicht verwendet")
        forced = bool(notes)
        if sum(l.count('"') % 2 for l in lines):
            log("einzelne Anführungszeichen entfernt")
            forced = True
        if out_name(rel) != rel:
            log("falsche Dateiendung, als CSV übernommen")
            forced = True
    for n in notes:
        log(n)
    rows, extra, changed = repair_table(cells, lines, sep, head, os.path.basename(rel), refs, log)
    if not (forced or changed):
        return False
    target = os.path.join(dst_dir, out_name(rel))
    write_csv(target, rows)
    stem, ext = os.path.splitext(target)
    for k, ex in enumerate(extra, 2):
        while os.path.exists(f"{stem}__person{k}{ext}"):
            k += 100
        write_csv(f"{stem}__person{k}{ext}", ex)
    return True


def main():
    if len(sys.argv) != 3:
        print("Aufruf: repair-uploads.py <quelle> <ziel>", file=sys.stderr)
        return 2
    src_root, out_root = sys.argv[1], sys.argv[2]
    dst_dir = os.path.join(out_root, "uploads")
    os.makedirs(dst_dir, exist_ok=True)
    refs = load_refs()
    eintraege = []
    for root, _, files in os.walk(src_root):
        for fn in sorted(files):
            if fn in (".gitkeep", "README.md"):
                continue
            src = os.path.join(root, fn)
            rel = os.path.relpath(src, src_root).replace("\\", "/")
            log_rel = []
            ok = False
            try:
                ok = repair_file(src, rel, dst_dir, refs, log_rel.append)
            except Unrettbar as e:
                log_rel = [f"nicht rettbar: {e}"]
            except ZuGross as e:
                eintraege.append((rel, str(e)))
                continue                      # nicht kopieren, der Merge bliebe sonst minutenlang hängen
            except Exception as e:  # eine einzelne Datei darf den Lauf nie stoppen
                log_rel = [f"Reparatur fehlgeschlagen ({type(e).__name__}: {e}), Original verwendet"]
            if not ok:
                target = os.path.join(dst_dir, rel)
                try:
                    os.makedirs(os.path.dirname(target), exist_ok=True)
                    shutil.copyfile(src, target)
                except OSError as e:          # etwa ein toter Symlink
                    log_rel.append(f"nicht kopierbar: {e}")
            eintraege += [(rel, m) for m in log_rel]
    with open(os.path.join(out_root, "reparatur-log.tsv"), "w", encoding="utf-8", errors="replace", newline="") as f:
        for rel, m in eintraege:
            f.write(re.sub(r"[\t\r\n]+", " ", rel) + "\t" + re.sub(r"[\t\r\n]+", " ", m) + "\n")
    print(f"{len(eintraege)} Reparatur-Hinweise")
    return 0


if __name__ == "__main__":
    sys.exit(main())
