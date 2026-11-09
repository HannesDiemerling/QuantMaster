#!/usr/bin/env python3
"""Prüft die Abgaben unter abgaben/ gegen gehashte Sollwerte und schreibt Rückmeldungen.

Zustandslos und idempotent: Jeder Lauf liest alle Abgaben und schreibt nur, was sich
gegenüber dem Vorhandenen ändert. Nur Python-Standardbibliothek, nur Lesen aus git.

Aufruf aus dem Repo-Root:
  python3 scripts/pruefe-abgaben.py                         alle Abgaben prüfen
  python3 scripts/pruefe-abgaben.py --baue-sollwerte SPEC GRUPPE [--korrektur]
                                                             sollwerte.json aus der R-Spec bauen

Ablage: abgaben/<gruppe>/sitzungNN/<Kennziffer>.txt  ->  <Kennziffer>-rueckmeldung.md, UEBERSICHT.md
Sollwerte: data/combined/eingefroren/<gruppe>/sollwerte.json (nur Hashes)
Rückmeldungen nennen nie einen Sollwert, und auch dieses Skript kennt keinen Klartext.
"""
import argparse
import csv
import hashlib
import json
import os
import re
import subprocess
import sys
import urllib.parse
from datetime import datetime, timezone
from decimal import Decimal, InvalidOperation, ROUND_HALF_UP
from pathlib import Path

PRUEFER_VERSION = 1
MAX_GROESSE = 100_000            # Bytes, größere Dateien werden nicht gelesen
MAX_HASHES = 15000               # je Feld, darüber werden Dezimalstellen vergröbert
ZIFFERN_MIN = 2                  # 1 bis 9 sind Übungszeilen, keine Kennziffern
SITZUNGEN = range(2, 11)         # Abgaben ab Sitzung 02
GRUPPE_RE = re.compile(r"^[a-z0-9-]+$")
SYSTEMDATEIEN = {"readme.md", "uebersicht.md", "termine.json", ".gitkeep", "nicht-zugeordnet.md", "pruefung-angehalten.md"}

KORRIGIEREN = "Zum Korrigieren öffnest Du Deine Datei im Ordner, klickst oben rechts auf das Stift-Symbol, änderst die Werte und klickst auf *Commit changes*."
STATUS_TEXT = {"richtig": "✓ richtig", "falsch": "✗ falsch", "fehlt": "– fehlt", "nicht_lesbar": "? nicht lesbar",
               "fehlt_frei": "– fehlt", "text": "liest Deine Lehrperson", "plausibel": "~ plausibel", "unplausibel": "~ unplausibel",
               "ohne_sollwert": "ohne Sollwert"}
KURZ = {"richtig": "✓", "falsch": "✗", "fehlt": "–", "nicht_lesbar": "?", "fehlt_frei": "–", "text": "T", "plausibel": "~", "unplausibel": "~", "ohne_sollwert": "·"}
ZAEHLT = {"richtig", "falsch", "fehlt", "nicht_lesbar"}


def log(msg):
    print(msg, flush=True)


# ---------------------------------------------------------------------------------------------
# Zahlen und Antworten verstehen
# ---------------------------------------------------------------------------------------------
_NUM = re.compile(r"(?<![A-Za-zÄÖÜäöüß0-9_.])([-+−–—]?)(\d+(?:[.,]\d+)*|[.,]\d+)((?:[eE][-+]?\d+)?)")
_DF_KLAMMER = re.compile(r"[A-Za-zχ²]+\s*\(\s*\d+(?:[.,]\d+)?\s*(?:[,;]\s*\d+(?:[.,]\d+)?\s*)?\)\s*(?=[=:])")


def _zu_decimal(z, ganz=False):
    """Eine Zahl als Text (ohne Vorzeichen) nach Decimal. Tausenderpunkte werden erkannt."""
    z = z.strip()
    if "." in z and "," in z:
        if re.fullmatch(r"\d{1,3}(\.\d{3})+,\d+", z):        # 1.234,56
            z = z.replace(".", "").replace(",", ".")
        elif re.fullmatch(r"\d{1,3}(,\d{3})+\.\d+", z):      # 1,234.56
            z = z.replace(",", "")
        else:
            return None
    elif z.count(",") == 1:
        z = z.replace(",", ".")
    elif z.count(",") > 1 or z.count(".") > 1:
        if re.fullmatch(r"\d{1,3}(?:[.,]\d{3})+", z):
            z = z.replace(",", "").replace(".", "")
        else:
            return None
    elif ganz and re.fullmatch(r"\d{1,3}\.\d{3}", z):
        z = z.replace(".", "")
    try:
        return Decimal(z)
    except InvalidOperation:
        return None


def zahlen_in(text, ganz=False):
    """Alle Zahlen im Text als Decimal, Dezimalkomma und -punkt gemischt."""
    werte = []
    text = _DF_KLAMMER.sub(" ", text)      # r(90) = -0.26: Freiheitsgrade sind nicht die Antwort
    for m in _NUM.finditer(text):
        vz, z, exp = m.group(1), m.group(2), m.group(3)
        d = _zu_decimal(z, ganz)
        if d is not None and exp:
            d = d.scaleb(int(exp[1:]))
        if d is None:                     # 0,12,0,45 oder 0.12,0.45: Paare trennen
            teile = re.split(r",(?=\d+\.)|(?<=\.\d),", z) if "." in z and "," in z else None
            if teile and len(teile) > 1:
                gemischt = [_zu_decimal(t) for t in teile]
            else:
                stuecke = z.replace(";", ",").split(",")
                if len(stuecke) % 2 == 0:
                    gemischt = [_zu_decimal(stuecke[i] + "." + stuecke[i + 1]) for i in range(0, len(stuecke), 2)]
                else:
                    gemischt = [_zu_decimal(t) for t in stuecke]
            gemischt = [x for x in gemischt if x is not None]
            if gemischt and vz in ("-", "−", "–", "—"):
                gemischt[0] = -gemischt[0]
            werte.extend(gemischt)
            continue
        if vz in ("-", "−", "–", "—"):
            d = -d
        werte.append(d)
    return werte


def auf_k(d, k):
    return d.quantize(Decimal(1).scaleb(-k), rounding=ROUND_HALF_UP)


def text_token(d, k):
    t = auf_k(d, k)
    if t == 0:
        t = abs(t)
    return f"{t:.{k}f}"


def hash_wert(gruppe, feld, teil, wert):
    return hashlib.sha256(f"{gruppe}|{feld}|{teil}|{wert}".encode("utf-8")).hexdigest()[:24]


def einheiten(s, k):
    return int((Decimal(str(s)).scaleb(k)).to_integral_value(rounding=ROUND_HALF_UP))


def bereich_tokens(bereiche, k):
    vals = set()
    for b in bereiche:
        lo, hi = sorted((einheiten(b["von"], k), einheiten(b["bis"], k)))
        for n in range(lo, hi + 1):
            vals.add(f"{Decimal(n).scaleb(-k):.{k}f}")
    return vals


def passendes_k(bereiche, k):
    while k > 0 and sum(abs(einheiten(b["bis"], k) - einheiten(b["von"], k)) + 1 for b in bereiche) > MAX_HASHES:
        k -= 1
    return k


def baue_feld(gruppe, sf):
    feld = sf["feld"]
    typ = sf["typ"]
    h = {}
    aus = {k: sf[k] for k in ("feld", "sitzung", "nr", "frage", "typ", "tipp") if k in sf}
    H = lambda teil, ws: sorted(hash_wert(gruppe, feld, teil, w) for w in ws)
    if typ in ("text", "nicht_berechnet"):
        return aus
    aus["abs"] = bool(sf.get("abs", False))
    if typ == "zahl":
        k = int(sf.get("k", 2))
        if "pro_datensatz" in sf:
            kh = k
            aus["k"] = kh
            aus["pd"] = {str(d): H(f"wert.d{int(d)}", bereich_tokens([r], kh)) for d, r in sf["pro_datensatz"].items()}
        else:
            kh = passendes_k(sf["bereiche"], k)
            aus["k"] = kh
            h["wert"] = H("wert", bereich_tokens(sf["bereiche"], kh))
    elif typ == "ganzzahl":
        aus["k"] = 0
        aus["plausibel"] = bool(sf.get("plausibel", False))
        if "erlaubt" in sf:
            werte = {str(int(x)) for x in sf["erlaubt"]}
        else:
            werte = {str(int(t.split(".")[0])) for t in bereich_tokens(sf["bereiche"], 0)}
        h["wert"] = H("wert", werte)
    elif typ == "p":
        aus["k"] = 3
        werte = bereich_tokens(sf["bereiche"], 3)
        if any(Decimal(str(b["von"])) == 0 for b in sf["bereiche"]):
            werte.add("<.001")
        h["wert"] = H("wert", werte)
    elif typ == "intervall":
        k = int(sf.get("k", 2))
        aus["k"] = k
        h["unten"] = H("unten", bereich_tokens(sf["unten"], k))
        h["oben"] = H("oben", bereich_tokens(sf["oben"], k))
    elif typ == "label_zahl":
        k = int(sf.get("k", 2))
        aus["k"] = k
        aus["kandidaten"] = sf["kandidaten"]
        h["label"] = H("label", {str(x) for x in sf["labels"]})
        h["wert"] = H("wert", bereich_tokens(sf["bereiche"], k))
    elif typ == "f_df":
        k = int(sf.get("k", 2))
        aus["k"] = k
        h["df1"] = H("df1", {str(int(sf["df1"]))})
        h["df2"] = H("df2", {str(int(sf["df2"]))})
        h["wert"] = H("wert", bereich_tokens(sf["bereiche"], k))
    elif typ == "wahl":
        aus["kandidaten"] = sf["kandidaten"]
        h["wert"] = H("wert", {str(x) for x in sf["ok"]})
    else:
        raise ValueError(f"unbekannter Feldtyp {typ}")
    if h:
        aus["h"] = h
    return aus


def sha_datensatz(pfad):
    return hashlib.sha256(Path(pfad).read_bytes().replace(b"\r\n", b"\n")).hexdigest()


def baue_sollwerte(spec_pfad, gruppe, korrektur, wurzel):
    if not GRUPPE_RE.fullmatch(gruppe):
        sys.exit(f"Ungültiger Gruppenname: {gruppe}")
    ziel_dir = wurzel / "data/combined/eingefroren" / gruppe
    daten = ziel_dir / "m15-gesamt.csv"
    if not daten.is_file():
        sys.exit(f"Kein eingefrorener Stand für {gruppe}: {daten}")
    ziel = ziel_dir / "sollwerte.json"
    version = 1
    if ziel.exists():
        if not korrektur:
            log(f"sollwerte.json für {gruppe} existiert bereits, nichts geändert. Korrektur nur mit --korrektur.")
            return 0
        try:
            version = int(json.loads(ziel.read_text(encoding="utf-8")).get("version", 1)) + 1
        except Exception:
            version = 2
    spec = json.loads(Path(spec_pfad).read_text(encoding="utf-8"))
    felder = {}
    fehlend = []
    for sf in spec["felder"]:
        fb = baue_feld(gruppe, sf)
        felder[sf["feld"]] = fb
        if sf["typ"] == "nicht_berechnet":
            fehlend.append(sf["feld"])
    aus = {
        "format": 1, "gruppe": gruppe, "version": version,
        "sha256_datensatz": sha_datensatz(daten),
        "erzeugt": datetime.now(timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ"),
        "erzeugt_mit": {"R": spec.get("r_version"), "pakete": spec.get("pakete"), "seed": spec.get("seed")},
        "nicht_berechnet": fehlend,
        "felder": felder,
    }
    tmp = ziel.with_suffix(".tmp")
    tmp.write_text(json.dumps(aus, ensure_ascii=False, separators=(",", ":")), encoding="utf-8", newline="\n")
    os.replace(tmp, ziel)
    log(f"sollwerte.json für {gruppe} geschrieben (Version {version}, {len(felder)} Felder, {len(fehlend)} nicht berechnet).")
    return 0


# ---------------------------------------------------------------------------------------------
# Datei lesen und Felder finden
# ---------------------------------------------------------------------------------------------
_FELD_RE = re.compile(r"^[\s>*`\"'„“”|#_-]*(?:\d{1,2}[.)]\s*)?(?:Feld\s+)?[\s*`\"'„“”]*S\s*0*(\d{1,2})\s*[-‑–—_.,:/\s]\s*0*(\d{1,3})(?!\d)(.*)$", re.I)


def lies_text(pfad):
    """Gibt (text, problem) zurück, problem ist None oder ein Grund."""
    try:
        groesse = pfad.stat().st_size
        if groesse == 0:
            return "", "leer"
        if groesse > MAX_GROESSE:
            return None, "zu_gross"
        b = pfad.read_bytes()
    except OSError:
        return None, "nicht_lesbar"
    if b[:4] == b"%PDF" or b[:4] == b"PK\x03\x04" or b[:4] == b"\xd0\xcf\x11\xe0":
        return None, "binaer"
    if b[:2] in (b"\xff\xfe", b"\xfe\xff"):
        try:
            return b.decode("utf-16"), None
        except UnicodeError:
            return None, "binaer"
    if b"\x00" in b:
        return None, "binaer"
    try:
        t = b.decode("utf-8-sig")
    except UnicodeDecodeError:
        t = b.decode("cp1252", errors="replace")
    steuer = sum(1 for c in t if ord(c) < 32 and c not in "\r\n\t\f")
    if steuer > max(5, len(t) * 0.02):
        return None, "binaer"
    return t.replace("\r\n", "\n").replace("\r", "\n"), None


def feldname(s, n):
    return f"S{int(s):02d}-{int(n)}"


def _nennung_leer(zeilen):
    """Eine Nennung ohne Antwort: nur die Frage aus der Vorlage (endet auf : oder ?) oder gar nichts."""
    erste = _normal(zeilen[0]).strip(" \t*`\"'„“”|")
    if any(z.strip(" \t>*`\"'„“”|") for z in zeilen[1:]):
        return False
    return not erste or erste.endswith((":", "?"))


def parse_abgabe(text):
    """Felder und Antworten. Gibt (antworten, doppelt) zurück."""
    nennungen, aktuell = {}, None
    for zeile in text.split("\n"):
        m = _FELD_RE.match(zeile)
        if m and 1 <= int(m.group(2)) <= 40 and int(m.group(1)) >= 1:
            aktuell = feldname(m.group(1), m.group(2))
            nennungen.setdefault(aktuell, []).append([m.group(3)])
        elif aktuell is not None:
            nennungen[aktuell][-1].append(zeile)
    antworten, doppelt = {}, []
    for fid, liste in nennungen.items():
        gefuellt = [n for n in liste if not _nennung_leer(n)]
        if len(gefuellt) > 1:
            doppelt.append(fid)
        antworten[fid] = gefuellt[-1] if gefuellt else liste[-1]    # die letzte ausgefüllte Nennung zählt
    return antworten, doppelt


def _normal(t):
    return re.sub(r"\s+", " ", (t or "").replace(" ", " ")).strip()


def antwort_aus(zeilen, frage):
    """Trennt die Frage (falls mitkopiert) von der Antwort, liefert die Antwort als Text."""
    erste = _normal(zeilen[0]).lstrip("`*\"'„“”|: \t")
    q = _normal(frage)
    rest = ""
    if q and erste.lower().startswith(q.lower()):
        rest = erste[len(q):]
    elif re.match(r"[-+−–—]?[.,]?\d", erste):              # Antwort steht vorn, zum Beispiel 29,94 (SD: 8,41)
        rest = erste
    elif ":" in erste:
        # Erster Doppelpunkt, vor dem noch keine Antwortzahl steht, sonst der letzte
        teile = erste.split(":")
        rest = teile[-1]
        for i in range(1, len(teile)):
            if not zahlen_in(":".join(teile[:i])):
                rest = ":".join(teile[i:])
                break
    else:
        fw = {w.strip("():?,.;„“\"'").lower() for w in q.split()}
        worte = erste.split()
        i = 0
        while i < len(worte) and worte[i].strip("():?,.;„“\"'").lower() in fw:
            i += 1
        rest = " ".join(worte[i:])
    rest = re.sub(r"^\s*(?:->|=>|→)\s*", "", rest).strip(" \t:?=*`\"'„“”|")
    if rest:
        return rest
    for z in zeilen[1:]:
        z = z.strip(" \t>*`\"'„“”|")
        if z:
            return z
    return ""


# ---------------------------------------------------------------------------------------------
# Einzelnes Feld prüfen
# ---------------------------------------------------------------------------------------------
def _in(gruppe, fid, teil, token, h):
    return hash_wert(gruppe, fid, teil, token) in set(h.get(teil, []))


def pruefe_feld(gruppe, fid, sf, ant, datensatz=None):
    """Gibt (status, verstanden, notiz) zurück."""
    typ = sf["typ"]
    if not ant or not ant.strip():
        # Freitext und Plausibilität zählen nicht in die Trefferzahl, auch wenn sie fehlen
        frei = typ in ("text", "nicht_berechnet") or sf.get("plausibel")
        return ("fehlt_frei" if frei else "fehlt", "", "")
    if typ == "text":
        return ("text", ant, "")
    if typ == "nicht_berechnet":
        return ("ohne_sollwert", ant, "")
    h = sf.get("h", {})
    k = int(sf.get("k", 2))
    if typ == "zahl":
        hset = None
        teil = "wert"
        if "pd" in sf:
            if datensatz is None or str(datensatz) not in sf["pd"]:
                return ("ohne_sollwert", ant, "Dein Datensatz ist nicht bekannt.")
            teil = f"wert.d{int(datensatz)}"
            hset = set(sf["pd"][str(datensatz)])
        zs = zahlen_in(ant)
        if not zs:
            return ("nicht_lesbar", ant, "Hier fehlt eine Zahl.")
        z = abs(zs[0]) if sf.get("abs") else zs[0]
        token = text_token(z, k)
        treffer = hash_wert(gruppe, fid, teil, token) in (hset if hset is not None else set(h.get("wert", [])))
        return ("richtig" if treffer else "falsch", token, "")
    if typ == "ganzzahl":
        zs = zahlen_in(ant, ganz=True)
        if not zs:
            return ("nicht_lesbar", ant, "Hier fehlt eine Zahl.")
        z = zs[0]
        if z != z.to_integral_value():
            return ("falsch", str(z), "Hier ist eine ganze Zahl gefragt.")
        token = str(int(z))
        treffer = _in(gruppe, fid, "wert", token, h)
        if sf.get("plausibel"):
            return ("plausibel" if treffer else "unplausibel", token, "")
        return ("richtig" if treffer else "falsch", token, "")
    if typ == "p":
        s = _normal(ant).lower().replace("kleiner als", "<").replace("kleiner", "<").replace(" ", "")
        s = re.sub(r"^p(-?wert)?[=:≈]?", "", s)
        m = re.search(r"([<>≤≥])=?([.,0-9]+)", s)
        if m:
            zs = zahlen_in(m.group(2))
            if m.group(1) in "<≤" and zs and zs[0] <= Decimal("0.001"):
                token = "<.001"
            else:
                return ("nicht_lesbar", ant, "Gib bitte den genauen p-Wert an, zum Beispiel .045.")
        else:
            zs = zahlen_in(s)
            if not zs:
                return ("nicht_lesbar", ant, "Hier fehlt ein p-Wert.")
            z = abs(zs[0])
            token = text_token(z, k)
            if token == f"{0:.{k}f}":
                token = "<.001"
        return ("richtig" if _in(gruppe, fid, "wert", token, h) else "falsch", token.replace("<.001", "< .001"), "")
    if typ == "intervall":
        zs = zahlen_in(ant)
        if len(zs) < 2:
            return ("nicht_lesbar", ant, "Hier fehlen die beiden Grenzen, zum Beispiel [0.10, 0.35].")
        a, b = zs[0], zs[1]
        lo_ok = lambda v: _in(gruppe, fid, "unten", text_token(v, k), h)
        hi_ok = lambda v: _in(gruppe, fid, "oben", text_token(v, k), h)
        kandidaten = [(a, b), (b, a)]
        if a > 0 > b and re.search(r"\d\s*[-–—−]\s*[.,]?\d", ant):       # 0.12 - 0.45 oder 0.12-0.45
            kandidaten.append((a, abs(b)))
        if sf.get("abs"):
            kandidaten += [(-b, -a), (-a, -b)]
        for lo, hi in kandidaten:
            if lo_ok(lo) and hi_ok(hi):
                return ("richtig", f"[{text_token(min(lo, hi), k)}, {text_token(max(lo, hi), k)}]", "")
        return ("falsch", f"[{text_token(a, k)}, {text_token(b, k)}]", "")
    if typ == "label_zahl":
        kand = sf.get("kandidaten", {})
        beste = None
        for alias, canon in sorted(kand.items(), key=lambda kv: -len(kv[0])):
            m = re.search(r"(?<![A-Za-zÄÖÜäöüß0-9_])" + re.escape(alias).replace(r"\ ", r"\s*") + r"(?![A-Za-zÄÖÜäöüß0-9_])", ant, re.I)
            if m and (beste is None or m.start() < beste[0]):
                beste = (m.start(), m.end(), canon)
        zs = zahlen_in(ant[:beste[0]] + " " + ant[beste[1]:]) if beste else zahlen_in(ant)
        if beste is None and not zs:
            return ("nicht_lesbar", ant, "Hier fehlen Name und Zahl.")
        if beste is None or not zs:
            return ("nicht_lesbar", ant, "Hier braucht es beides: den Namen und die Zahl.")
        label_ok = _in(gruppe, fid, "label", beste[2], h)
        wert_token = text_token(zs[0], k)
        wert_ok = _in(gruppe, fid, "wert", wert_token, h)
        status = "richtig" if label_ok and wert_ok else "falsch"
        notiz = "" if status == "richtig" else ("Der Name passt nicht." if not label_ok else "Die Zahl passt nicht.") if label_ok != wert_ok else ""
        return (status, f"{beste[2]} {wert_token}", notiz)
    if typ == "f_df":
        ant2 = ant
        df1 = df2 = None
        m = re.search(r"[Ff]?\s*\(\s*(\d+)\s*[,;/]\s*(\d+)\s*\)", ant2)
        if m:
            df1, df2 = int(m.group(1)), int(m.group(2))
            ant2 = ant2[:m.start()] + " " + ant2[m.end():]
            zs = zahlen_in(ant2)
            f = zs[0] if zs else None
        else:
            zs = zahlen_in(ant2)
            if len(zs) >= 3:
                ints = [z for z in zs if z == z.to_integral_value()]
                dez = [z for z in zs if z != z.to_integral_value()]
                if len(ints) >= 2 and dez:
                    df1, df2, f = int(ints[0]), int(ints[1]), dez[0]
                else:
                    df1, df2, f = int(zs[0]), int(zs[1]), zs[2]
            else:
                f = None
        if f is None or df1 is None:
            return ("nicht_lesbar", ant, "Hier fehlt etwas, schreib es wie im Beispiel: F(4, 85) = 1.23.")
        token = text_token(f, k)
        ok = _in(gruppe, fid, "df1", str(df1), h) and _in(gruppe, fid, "df2", str(df2), h) and _in(gruppe, fid, "wert", token, h)
        return ("richtig" if ok else "falsch", f"F({df1}, {df2}) = {token}", "")
    if typ == "wahl":
        low = _normal(ant).lower()
        treffer = None
        syn = {"ja": "ja", "j": "ja", "yes": "ja", "nein": "nein", "n": "nein", "no": "nein"}
        for w in re.findall(r"[a-zäöüß]+", low):
            if w in syn and syn[w] in sf.get("kandidaten", ["ja", "nein"]):
                treffer = syn[w]
                break
        if treffer is None:
            for w in sf.get("kandidaten", []):
                if re.search(r"(?<![a-zäöüß0-9])" + re.escape(str(w).lower()) + r"(?![a-zäöüß0-9])", low):
                    treffer = str(w)
                    break
        if treffer is None:
            return ("nicht_lesbar", ant, "Schreib bitte ja oder nein.")
        return ("richtig" if _in(gruppe, fid, "wert", treffer, h) else "falsch", treffer, "")
    return ("ohne_sollwert", ant, "")


# ---------------------------------------------------------------------------------------------
# Git-Historie und Konten
# ---------------------------------------------------------------------------------------------
def konto_von(name, email):
    """GitHub-Login aus der noreply-Adresse, sonst der Name. E-Mail-Adressen werden nie ausgegeben."""
    m = re.fullmatch(r"(?:\d+\+)?([A-Za-z0-9-]+(?:\[bot\])?)@users\.noreply\.github\.com", email or "", re.I)
    if m:
        return m.group(1)
    n = re.sub(r"[^\w .-]", "", (name or "").split("@")[0], flags=re.UNICODE).strip()[:60]
    return n or "unbekannt"


def lese_git(wurzel):
    """Pfad -> Liste der Commits (alt nach neu) [{zeit, name, email, konto}], Bot-Commits weggelassen."""
    try:
        aus = subprocess.run(["git", "-c", "core.quotepath=off", "log", "--no-merges", "--no-renames",
                              "--format=%x00%H%x1f%at%x1f%an%x1f%ae", "--name-only", "--", "abgaben/"],
                             cwd=str(wurzel), capture_output=True, encoding="utf-8", errors="replace", check=True).stdout
    except (OSError, subprocess.CalledProcessError) as e:
        log(f"Hinweis: git-Historie nicht lesbar ({e})")
        return {}
    pfade = {}
    for block in aus.split("\x00"):
        if not block.strip():
            continue
        kopf, _, dateien = block.partition("\n")
        teile = kopf.split("\x1f")
        if len(teile) < 4:
            continue
        name, email = teile[2], teile[3]
        if "[bot]" in name or "[bot]" in email:
            continue
        konto = konto_von(name, email)
        c = {"zeit": int(teile[1]), "name": name, "konto": konto}
        for d in dateien.split("\n"):
            d = d.strip()
            if d:
                pfade.setdefault(d, []).append(c)
    for v in pfade.values():
        v.reverse()
    return pfade


def lade_lehrpersonen(wurzel):
    try:
        zeilen = (wurzel / "scripts/lehrpersonen.txt").read_text(encoding="utf-8").splitlines()
    except OSError:
        return set()
    return {z.strip().lower() for z in zeilen if z.strip() and not z.strip().startswith("#")}


def ist_lehrperson(c, lehr):
    return bool(c) and (c["konto"].lower() in lehr or c["name"].lower() in lehr)


def _konto_key(c):
    return c["konto"].lower() if c else ""


def zuordnung_von(pfad):
    """(konto -> set(kennziffern), kennziffer -> konto, uploads: kennziffer -> datensatz) aus zuordnung.csv.
    Zeilen von Lehrpersonen zählen nur über die Kennziffer (kein Konto-Schluss)."""
    k2kz, kz2k, uploads = {}, {}, {}
    try:
        with open(pfad, newline="", encoding="utf-8") as fh:
            for r in csv.DictReader(fh):
                kz = (r.get("kennziffer") or "").strip()
                if not kz.isdigit() or int(kz) < 10:
                    continue
                kz = str(int(kz))
                m = re.search(r"datensatz-(\d{1,2})", r.get("datei") or "", re.I)
                if m:
                    uploads.setdefault(kz, int(m.group(1)))
                konto = (r.get("konto") or "").strip().lower()
                if not konto or (r.get("lehrperson") or "").strip().lower() == "ja":
                    continue
                k2kz.setdefault(konto, set()).add(kz)
                kz2k.setdefault(kz, konto)
    except (OSError, UnicodeDecodeError, csv.Error) as e:
        log(f"::warning::{pfad}: nicht lesbar ({e})")
    return k2kz, kz2k, uploads


def gleiche_nummer(a, b):
    """Können a und b dieselbe Kennziffer in anderer Länge sein?
    Ja, wenn die kürzere das Ende der längeren ist (mindestens 4 Ziffern) oder die letzten 6 Ziffern gleich sind."""
    if a == b:
        return True
    gemeinsam = 0
    for x, y in zip(reversed(a), reversed(b)):
        if x != y:
            break
        gemeinsam += 1
    kuerzere = min(len(a), len(b))
    return (gemeinsam == kuerzere and kuerzere >= 4) or gemeinsam >= 6


def datensatz_von(kz, uploads):
    """Datensatznummer: aus dem Upload-Dateinamen, sonst letzte Ziffer, 0 = 10."""
    if kz in uploads:
        return uploads[kz]
    if kz and kz[-1].isdigit():
        d = int(kz[-1])
        return 10 if d == 0 else d
    return None


def lade_uploads(wurzel):
    uploads = {}
    p = wurzel / "data/student-uploads"
    if p.is_dir():
        for f in p.iterdir():
            m = re.match(r"datensatz-(\d{1,2})\s*[-_–—]?\s*(\d+)\.", f.name, re.I)
            if m:
                uploads[str(int(m.group(2)))] = int(m.group(1))
    return uploads


# ---------------------------------------------------------------------------------------------
# Lauf
# ---------------------------------------------------------------------------------------------
def ist_abgabe_datei(p):
    return (not p.name.startswith(".") and p.name.lower() not in SYSTEMDATEIEN
            and not p.name.endswith("-rueckmeldung.md"))


def kennziffer_aus_name(name):
    stamm = re.sub(r"\.[^.]*$", "", name)
    laeufe = re.findall(r"\d+", stamm)
    if not laeufe:
        return None
    # Bei mehreren Ziffernfolgen zählt die längste (datensatz-02-123456), bei Gleichstand die letzte
    kz = max(enumerate(laeufe), key=lambda t: (len(t[1]), t[0]))[1]
    if int(kz) < 10 or len(str(int(kz))) > 15:     # 1 bis 9 sind Übungszeilen, längere Zahlen sind keine Kennziffern
        return None
    return kz


def lade_gruppen(wurzel):
    gruppen = {}
    root = wurzel / "data/combined/eingefroren"
    if root.is_dir():
        for d in sorted(x for x in root.iterdir() if x.is_dir()):
            if not GRUPPE_RE.fullmatch(d.name):
                continue
            info = {"dir": d, "status": "ok", "sw": None, "stand": set()}
            info["k2kz"], info["kz2k"], info["uploads_z"] = zuordnung_von(d / "zuordnung.csv") if (d / "zuordnung.csv").is_file() else ({}, {}, {})
            daten = d / "m15-gesamt.csv"
            sw_pfad = d / "sollwerte.json"
            if not daten.is_file():
                info["status"] = "kein_stand"
            else:
                try:
                    with open(daten, newline="", encoding="utf-8") as fh:
                        for r in csv.DictReader(fh):
                            if str(r.get("quelle", "")).strip().lower() == "studierende":
                                try:
                                    nr = str(int(float(r["probandennummer"])))
                                    info["stand"].add(nr)
                                    d_nr = int(float(r.get("quelle_datensatz") or 0))
                                    if 1 <= d_nr <= 10:
                                        info["uploads_z"].setdefault(nr, d_nr)
                                except (KeyError, TypeError, ValueError):
                                    pass
                except OSError:
                    pass
                if not sw_pfad.is_file():
                    info["status"] = "keine_sollwerte"
                else:
                    try:
                        sw = json.loads(sw_pfad.read_text(encoding="utf-8"))
                        felder = sw.get("felder") if isinstance(sw, dict) else None
                        if not isinstance(felder, dict) or not felder or not all(isinstance(v, dict) for v in felder.values()):
                            info["status"] = "gesperrt"
                        elif sw.get("sha256_datensatz") and sw["sha256_datensatz"] != sha_datensatz(daten):
                            info["status"] = "gesperrt"
                        else:
                            info["sw"] = sw
                    except (OSError, ValueError):
                        info["status"] = "gesperrt"
            gruppen[d.name] = info
    return gruppen


def bekannte_gruppen(wurzel, gruppen):
    namen = set(gruppen)
    abg = wurzel / "abgaben"
    if abg.is_dir():
        for d in abg.iterdir():
            if d.is_dir() and (d / "README.md").is_file() and GRUPPE_RE.fullmatch(d.name):
                namen.add(d.name)
    return sorted(namen)


def repo_url(wurzel):
    try:
        t = (wurzel / "_quarto.yml").read_text(encoding="utf-8")
        m = re.search(r'^\s*repo:\s*"?(https://github\.com/[^"\s]+)', t, re.M)
        if m:
            return m.group(1)
    except OSError:
        pass
    return "https://github.com/HannesDiemerling/QuantMaster"


def vorlage_von(wurzel, n):
    try:
        t = (wurzel / f"sitzung{n:02d}/_abgabe.md").read_text(encoding="utf-8")
    except OSError:
        return None
    m = re.search(r"```default\n(.*?)```", t, re.S)
    return m.group(1) if m else None


def schreibe(p, inhalt, stats):
    """Schreibt nur, wenn sich der Inhalt ändert. Text immer mit LF."""
    try:
        if p.read_text(encoding="utf-8").replace("\r\n", "\n") == inhalt:
            return False
    except (OSError, UnicodeDecodeError):
        pass
    p.parent.mkdir(parents=True, exist_ok=True)
    tmp = p.with_name(p.name + ".tmp")
    tmp.write_text(inhalt, encoding="utf-8", newline="\n")
    os.replace(tmp, p)
    stats["geschrieben"] += 1
    return True


def schreibe_readmes(wurzel, gruppen_namen, stats, geschrieben_pfade):
    repo = repo_url(wurzel)
    abg = wurzel / "abgaben"
    start = ["# Abgaben", "",
             "Hier liegen die Abgaben aller Gruppen, je Sitzung eine Textdatei pro Person. "
             "Öffne den Ordner Deiner Gruppe. Den Namen findest Du in der Tabelle „Deine Lehrperson“ auf der Übersichtsseite von Sitzung 01.", "",
             "| Gruppe | Ordner |", "|---|---|"]
    for g in gruppen_namen:
        start.append(f"| {g} | [abgaben/{g}](./{g}/) |")
    start += ["", "Lege nur Deine eigene Datei an, also `<Deine Kennziffer>.txt` im Ordner Deiner Gruppe und Sitzung. "
              "Dateien anderer Personen bleiben unberührt. Rückmeldungen und Übersichten erzeugt das Repo automatisch, "
              "ändern musst Du dort nichts.", ""]
    p = abg / "README.md"
    schreibe(p, "\n".join(start), stats)
    geschrieben_pfade.add(p)
    for g in gruppen_namen:
        z = [f"# Abgaben der Gruppe {g}", "",
             "Für jede Sitzung gibt es einen eigenen Ordner. Der Link öffnet eine neue Datei mit der fertigen Vorlage. "
             "Nenne sie `<Deine Kennziffer>.txt`, trag hinter jeder Frage Deine Antwort ein und klicke auf *Commit changes*.", "",
             KORRIGIEREN, "",
             "Ohne GitHub-Account schickst Du denselben Text per Mail an Deine Lehrperson, Betreff `M15 Abgabe SNN <Kennziffer>` mit der Nummer der Sitzung statt NN. Sie stellt die Datei für Dich ein. Deine Rückmeldung siehst Du ohne Anmeldung im Browser neben Deiner Datei im Ordner Deiner Gruppe und Sitzung. Für eine Korrektur schickst Du eine neue Mail.", "",
             "| Sitzung | Datei anlegen | Ordner |", "|---|---|---|"]
        for n in SITZUNGEN:
            vor = vorlage_von(wurzel, n)
            if vor is None:
                continue
            link = f"{repo}/new/main/abgaben/{g}/sitzung{n:02d}?value={urllib.parse.quote(vor)}"
            z.append(f"| {n:02d} | [Datei anlegen]({link}) | [sitzung{n:02d}](./sitzung{n:02d}/) |")
            r = [f"# Gruppe {g}, Abgabe Sitzung {n:02d}", "",
                 f"[Datei anlegen]({link}) öffnet eine neue Datei, in der die Vorlage schon steht. Nenne die Datei `<Deine Kennziffer>.txt`, zum Beispiel `234567.txt`, "
                 "trag hinter jeder Frage Deine Antwort ein und klicke auf *Commit changes*.", "",
                 "Klappt der Link nicht? Klicke oben auf *Add file*, dann auf *Create new file*, gib `<Deine Kennziffer>.txt` als Namen ein und kopiere diese Vorlage hinein:", "",
                 "```default", vor.rstrip("\n"), "```", "",
                 "Kurz darauf liegt hier auch `<Deine Kennziffer>-rueckmeldung.md` mit der Rückmeldung je Feld.", "",
                 KORRIGIEREN, "",
                 f"Ohne GitHub-Account schickst Du denselben Text per Mail an Deine Lehrperson, Betreff `M15 Abgabe S{n:02d} <Kennziffer>`. Sie stellt die Datei für Dich ein. "
                 "Deine Rückmeldung siehst Du ohne Anmeldung im Browser neben Deiner Datei im Ordner Deiner Gruppe und Sitzung. Für eine Korrektur schickst Du eine neue Mail.", ""]
            p = abg / g / f"sitzung{n:02d}" / "README.md"
            schreibe(p, "\n".join(r), stats)
            geschrieben_pfade.add(p)
        z.append("")
        p = abg / g / "README.md"
        schreibe(p, "\n".join(z), stats)
        geschrieben_pfade.add(p)


_MD_ERSATZ = {"&": "&amp;", "<": "&lt;", ">": "&gt;", "|": "&#124;", "`": "&#96;", "[": "&#91;", "]": "&#93;",
              "*": "&#42;", "_": "&#95;", "!": "&#33;", "~": "&#126;", "\\": "&#92;"}


def md_zelle(s, n=80):
    """Text für Markdown: Links, Bilder, HTML und Tabellen-Trenner werden zu reinem Text."""
    s = re.sub(r"\s+", " ", (s or "").replace("\n", " ")).strip()
    if len(s) > n:
        s = s[:n - 1] + "…"
    s = "".join(_MD_ERSATZ.get(c, c) for c in s)
    return s.replace("://", "&#58;//")


def zeit_text(ts):
    if not ts:
        return "unbekannt"
    try:
        from zoneinfo import ZoneInfo
        d = datetime.fromtimestamp(ts, ZoneInfo("Europe/Berlin"))
    except Exception:
        d = datetime.fromtimestamp(ts)
    return d.strftime("%d.%m.%Y, %H:%M")


# Wertet eine Abgabe gegen eine Gruppe aus. Rückgabe: dict feld -> (status, verstanden, notiz)
def bewerte(antworten, sw, gruppe, sitzung_felder, datensatz):
    ergebnis = {}
    for fid, sf in sitzung_felder.items():
        zeilen = antworten.get(fid)
        ant = antwort_aus(zeilen, sf.get("frage", "")) if zeilen else ""
        ergebnis[fid] = pruefe_feld(gruppe, fid, sf, ant, datensatz)
    return ergebnis


def treffer(ergebnis):
    r = sum(1 for s, _, _ in ergebnis.values() if s == "richtig")
    g = sum(1 for s, _, _ in ergebnis.values() if s in ZAEHLT)
    return r, g


def sitzung_aus_felder(antworten):
    zaehl = {}
    for fid in antworten:
        m = re.match(r"S(\d+)-", fid)
        if m and int(m.group(1)) in SITZUNGEN:             # S01-1 oder S99-1 legen keine Ordner an
            zaehl[int(m.group(1))] = zaehl.get(int(m.group(1)), 0) + 1
    if not zaehl:
        return None
    return max(zaehl.items(), key=lambda kv: kv[1])[0]


def rueckmeldung_text(a, gruppe_info):
    k = md_zelle(a["kz"] or a["name"], 80)
    aussen = ["# Rückmeldung zu Deiner Abgabe", ""]
    if a.get("gruppe"):
        kopf = [f"**Gruppe:** {a['gruppe']}  "]
    else:
        kopf = ["**Gruppe:** noch nicht erkannt  "]
    kopf += [f"**Sitzung:** {a['sitzung']:02d}  " if a.get("sitzung") else "**Sitzung:** noch nicht erkannt  ",
             f"**Kennziffer:** {k}  ",
             f"**Eingegangen:** {zeit_text(a['zeit'])}  ",
             f"**Fassung:** {a['fassungen'] or '1'}"]
    if a.get("eingestellt"):
        kopf[-1] += "  "
        kopf.append(f"**Eingestellt von:** {md_zelle(a['eingestellt'], 60)}")
    if a.get("sw_version"):
        kopf[-1] += "  "
        kopf.append(f"**Prüfstand:** Version {a['sw_version']}")
    aussen += kopf + [""]
    if a["problem"]:
        hinweis = {"leer": "Deine Datei ist leer. Füge die Vorlage ein, trag Deine Antworten ein und speichere noch einmal.",
                   "zu_gross": "Deine Datei ist zu groß. Bitte speichere nur den Text der Vorlage als `.txt`.",
                   "binaer": "Deine Datei lässt sich nicht als Text lesen. Bitte speichere sie als einfache Textdatei `.txt` mit der Vorlage aus dem Abschluss-Kapitel.",
                   "nicht_lesbar": "Deine Datei lässt sich gerade nicht lesen. Bitte speichere sie noch einmal als `.txt`."}[a["problem"]]
        aussen += [hinweis, ""]
    if a.get("ergebnis") is not None:
        r, g = treffer(a["ergebnis"])
        kopfsatz = f"**{r} von {g} prüfbaren Feldern sind richtig.**"
        if g and r == g:
            kopfsatz += " Sehr schön, alles passt."
        aussen += [kopfsatz + " " + KORRIGIEREN + " Die Rückmeldung wird dann neu erzeugt.", "",
                   "| Feld | Frage | Deine Antwort (so verstanden) | Ergebnis |", "|---|---|---|---|"]
        fragen = a["fragen"]
        texte = []
        for fid, (status, vst, notiz) in a["ergebnis"].items():
            if status == "text":
                texte.append((fid, fragen.get(fid, ""), vst))
                continue
            ergebnis = STATUS_TEXT.get(status, status)
            tipp = a["tipps"].get(fid, "") if status in ("falsch", "nicht_lesbar") else ""
            extra = " ".join(x for x in (notiz, tipp) if x)
            if status == "ohne_sollwert":
                ergebnis = "liest Deine Lehrperson"
            aussen.append(f"| {fid} | {md_zelle(fragen.get(fid, ''))} | {md_zelle(vst, 60) or ' '} | {ergebnis}{(' · ' + md_zelle(extra, 120)) if extra else ''} |")
        aussen += ["", "~ zählt nicht in die Trefferzahl, ? heißt, dass ich die Antwort nicht als Zahl lesen konnte.", ""]
        leere_texte = [fid for fid, (s, _, _) in a["ergebnis"].items() if s == "fehlt_frei" and a["sf_typ"].get(fid) == "text"]
        if texte or leere_texte:
            aussen += ["## Das liest Deine Lehrperson", ""]
            for fid, frage, antwort in texte:
                aussen += [f"**{fid}** {md_zelle(frage, 200)}", "", "> " + md_zelle(antwort, 1500), ""]
            if leere_texte:
                aussen += ["Noch offen: " + ", ".join(leere_texte), ""]
    elif not a["problem"] and a.get("gruppe") and a.get("sitzung") and not gruppe_info.get("sw") and gruppe_info.get("status") != "gesperrt":
        aussen += ["Für Deine Gruppe liegen noch keine Sollwerte vor. Deine Abgabe ist gespeichert und wird automatisch nachgeprüft.", ""]
    hinweise = a.get("hinweise", [])
    if hinweise:
        aussen += ["## Hinweise", ""] + [f"- {h}" for h in hinweise] + [""]
    return "\n".join(aussen)


def uebersicht_text(gruppe, sitzung, eintraege, info, termine):
    z = [f"# Übersicht Gruppe {gruppe}, Sitzung {sitzung:02d}", ""]
    sw = info.get("sw")
    if sw:
        z += [f"Prüfstand: Version {sw.get('version')} der Sollwerte. Abgaben: {len(eintraege)}.", ""]
    else:
        z += [f"Abgaben: {len(eintraege)}. {info.get('hinweis', 'Für diese Gruppe liegen noch keine Sollwerte vor, die Abgaben werden nachgeprüft.')}", ""]
    felder_ids = None
    if sw:
        felder_ids = [fid for fid, sf in sw["felder"].items() if sf.get("sitzung") == sitzung]
        z += ["| Kennziffer | Richtig | Falsch | Fehlt | Nicht lesbar | Freitext | Eingang | Fassung | Konto | Hinweise |",
              "|---|---:|---:|---:|---:|---|---|---:|---|---|"]
    else:
        z += ["| Kennziffer | Eingang | Fassung | Konto | Hinweise |", "|---|---|---:|---|---|"]
    for a in sorted(eintraege, key=lambda x: x["kz"] or x["name"]):
        spaet = ""
        if termine and a["zeit"]:
            t = termine.get(f"{sitzung:02d}") or termine.get(str(sitzung))
            if t:
                try:
                    if datetime.fromisoformat(t).date() < datetime.fromtimestamp(a["zeit"]).date():
                        spaet = " (nach dem Sitzungstag)"
                except ValueError:
                    pass
        hinw = "; ".join(md_zelle(h, 200) for h in a.get("kurz_hinweise", [])) or " "
        kto = md_zelle(a.get("konto_text") or "unbekannt", 40)
        if sw and a.get("ergebnis") is not None:
            c = {s: 0 for s in ("richtig", "falsch", "fehlt", "nicht_lesbar")}
            texts = [fid for fid in a["ergebnis"] if a["sf_typ"].get(fid) == "text"]
            for fid, (s, _, _) in a["ergebnis"].items():
                if s in c and a["sf_typ"].get(fid) != "text":
                    c[s] += 1
            frei = f"{sum(1 for f in texts if a['ergebnis'][f][0] == 'text')}/{len(texts)}"
            z.append(f"| {md_zelle(a['kz'] or a['name'])} | {c['richtig']} | {c['falsch']} | {c['fehlt']} | {c['nicht_lesbar']} | {frei} | {zeit_text(a['zeit'])}{spaet} | {a['fassungen'] or '1'} | {kto} | {hinw} |")
        elif sw:
            z.append(f"| {md_zelle(a['kz'] or a['name'])} |   |   |   |   |   | {zeit_text(a['zeit'])}{spaet} | {a['fassungen'] or '1'} | {kto} | {hinw} |")
        else:
            z.append(f"| {md_zelle(a['kz'] or a['name'])} | {zeit_text(a['zeit'])}{spaet} | {a['fassungen'] or '1'} | {kto} | {hinw} |")
    z.append("")
    if sw and eintraege:
        z += ["## Felder im Überblick", "", "✓ richtig, ✗ falsch, – fehlt, ? nicht lesbar, T Freitext, ~ Plausibilität, · ohne Sollwert. Felder mit weniger als 30 Prozent Treffern sind markiert, dort lohnt ein Blick auf die Sollwerte oder die Formel.", ""]
        kopf = "| Kennziffer | " + " | ".join(f.split("-")[1] for f in felder_ids) + " |"
        z += [kopf, "|---|" + ":-:|" * len(felder_ids)]
        for a in sorted(eintraege, key=lambda x: x["kz"] or x["name"]):
            if a.get("ergebnis") is None:
                continue
            sp = [KURZ.get(a["ergebnis"].get(f, ("fehlt", "", ""))[0], "?") for f in felder_ids]
            z.append(f"| {md_zelle(a['kz'] or a['name'])} | " + " | ".join(sp) + " |")
        z.append("")
        quoten = []
        for f in felder_ids:
            if sw["felder"][f].get("typ") in ("text",):
                continue
            ws = [a["ergebnis"].get(f, ("fehlt",))[0] for a in eintraege if a.get("ergebnis") is not None]
            ws = [s for s in ws if s in ZAEHLT]
            if ws:
                q = sum(1 for s in ws if s == "richtig") / len(ws)
                mark = " ⚠" if q < .3 and len(ws) >= 4 else ""
                quoten.append(f"| {f} | {md_zelle(sw['felder'][f].get('frage', ''), 70)} | {sum(1 for s in ws if s == 'richtig')} von {len(ws)}{mark} |")
        if quoten:
            z += ["| Feld | Frage | Treffer |", "|---|---|---|"] + quoten + [""]
        z += ["## Details je Kennziffer", "", "Abgegebene Werte je Feld, so wie sie verstanden wurden. Ein Klick klappt die Zeile auf.", ""]
        for a in sorted(eintraege, key=lambda x: x["kz"] or x["name"]):
            r, g = treffer(a.get("ergebnis") or {}) if a.get("ergebnis") is not None else (0, 0)
            z.append(f"<details><summary><b>{md_zelle(a['kz'] or a['name'])}</b>: {r} von {g} richtig</summary>")
            z.append("")
            if a.get("ergebnis") is not None:
                z += ["| Feld | Frage | Abgabe | Ergebnis |", "|---|---|---|---|"]
                for fid, (s, v, _) in a["ergebnis"].items():
                    z.append(f"| {fid} | {md_zelle(a['fragen'].get(fid, ''), 60)} | {md_zelle(v, 200) or ' '} | {STATUS_TEXT.get(s, s)} |")
            if a.get("hinweise"):
                z += [""] + [f"- {h}" for h in a["hinweise"]]
            z += ["", "</details>", ""]
    return "\n".join(z)


def lauf(wurzel):
    abg = wurzel / "abgaben"
    stats = {"geschrieben": 0, "abgaben": 0, "fehler": 0}
    gruppen = lade_gruppen(wurzel)
    gruppen_namen = bekannte_gruppen(wurzel, gruppen)
    hinweise_gruppe = {}
    for g, info in gruppen.items():
        if info["status"] == "gesperrt":
            log(f"::error::{g}: sollwerte.json passt nicht zum eingefrorenen Stand. Die Prüfung dieser Gruppe ist angehalten.")
            hinweise_gruppe[g] = True
    bewahrt = set()                     # Pfade, die erhalten bleiben
    schreibe_readmes(wurzel, gruppen_namen, stats, bewahrt)
    git = lese_git(wurzel)
    lehr = lade_lehrpersonen(wurzel)
    uploads = lade_uploads(wurzel)
    abgaben = []
    if abg.is_dir():
        for p in sorted(abg.rglob("*")):
            if not p.is_file() or ".git" in p.parts or not ist_abgabe_datei(p):
                continue
            abgaben.append(p)
    eintraege = []
    for p in abgaben:
        rel = p.relative_to(wurzel).as_posix()
        teile = p.relative_to(abg).parts
        gruppe = sitzung_pfad = None
        if len(teile) >= 2 and not re.fullmatch(r"sitzung\s*\d+", teile[0], re.I):
            kand = teile[0].lower()
            gruppe = kand if kand in gruppen_namen or GRUPPE_RE.fullmatch(kand) else None
            if gruppe not in gruppen_namen:
                gruppe = None
        for t in teile[:-1]:
            m = re.fullmatch(r"sitzung\s*0*(\d{1,2})", t, re.I)
            if m:
                sitzung_pfad = int(m.group(1))
                break
        try:
            text, problem = lies_text(p)
        except Exception as e:
            text, problem = None, "nicht_lesbar"
            log(f"::warning::{rel}: {e}")
        name = p.name
        commits = git.get(rel, [])
        kz = kennziffer_aus_name(name)
        kz_int = str(int(kz)) if kz else None
        a = {"pfad": p, "rel": rel, "name": name, "kz": kz, "kz_int": kz_int, "gruppe": gruppe, "sitzung_pfad": sitzung_pfad,
             "problem": problem, "text": text, "hinweise": [], "kurz_hinweise": [], "ergebnis": None, "fragen": {}, "tipps": {}, "sf_typ": {},
             "zeit": commits[-1]["zeit"] if commits else 0, "commits": commits, "fassungen": len(commits),
             "eingestellt": commits[-1]["konto"] if commits else "", "antworten": {}, "doppelt": []}
        try:
            if text is not None and not problem:
                a["antworten"], a["doppelt"] = parse_abgabe(text)
        except Exception as e:
            log(f"::warning::{rel} nicht verarbeitbar: {e}")
            a["problem"] = "nicht_lesbar"
        eintraege.append(a)
    stats["abgaben"] = len(eintraege)

    for a in eintraege:
        try:
            auswerten(a, gruppen, gruppen_namen, lehr, uploads)
        except Exception as e:
            stats["fehler"] += 1
            log(f"::warning::{a['rel']}: Prüfung fehlgeschlagen ({e})")
            a["problem"] = a["problem"] or "nicht_lesbar"
            a["kurz_hinweise"].append("Prüfung fehlgeschlagen, bitte von Hand ansehen")

    # Doppelte Abgaben derselben Kennziffer in Gruppe und Sitzung: die jüngste zählt
    nach_kz = {}
    for a in eintraege:
        if a["kz_int"] and a["gruppe"] and a["sitzung"]:
            nach_kz.setdefault((a["gruppe"], a["sitzung"], a["kz_int"]), []).append(a)
    for key, liste in nach_kz.items():
        if len(liste) < 2:
            continue
        # Eine lesbare Datei geht vor, ein später hochgeladenes Bild verdrängt sie nicht
        liste.sort(key=lambda x: (not x["problem"], x["zeit"], not x.get("ordner_passt", False), x["name"] == f"{x['kz']}.txt"))
        gewinner = liste[-1]
        for a in liste[:-1]:
            a["zaehlt_nicht"] = True
            a["hinweise"].append(f"Für Deine Kennziffer gibt es mehrere Dateien. Gewertet ist {md_zelle(gewinner['name'], 100)}, diese Datei zählt nicht.")
            a["kurz_hinweise"].append(f"zählt nicht, gewertet ist {gewinner['name']}")
        gewinner["hinweise"].append("Für Deine Kennziffer gibt es mehrere Dateien: " + ", ".join(md_zelle(x['rel'], 120) for x in liste) + ". Gewertet ist die neueste lesbare Datei.")
        gewinner["kurz_hinweise"].append("mehrere Dateien für diese Kennziffer")

    # Rückmeldungen, je Pfad höchstens eine Datei
    geschrieben_rm = {}
    for a in sorted(eintraege, key=lambda x: (not x.get("zaehlt_nicht", False), not x["problem"], x["zeit"])):
        ziel = a["pfad"].parent / ((f"{a['kz']}" if a["kz"] else re.sub(r"[^\w.-]+", "-", re.sub(r"\.[^.]*$", "", a["name"]))[:60]) + "-rueckmeldung.md")
        geschrieben_rm[ziel] = a
    for ziel, a in geschrieben_rm.items():
        info = gruppen.get(a["gruppe"], {}) if a["gruppe"] else {}
        if a["gruppe"] and gruppen.get(a["gruppe"], {}).get("status") == "gesperrt":
            bewahrt.add(ziel)
            continue
        try:
            schreibe(ziel, rueckmeldung_text(a, info), stats)
            bewahrt.add(ziel)
        except Exception as e:
            stats["fehler"] += 1
            log(f"::warning::{ziel}: {e}")

    # Übersichten je Gruppe und Sitzung
    uebersichten = {}
    for a in eintraege:
        if not a.get("zaehlt_nicht") and a["gruppe"] and a["sitzung"]:
            uebersichten.setdefault((a["gruppe"], a["sitzung"]), []).append(a)
    termine_cache = {}
    for (g, s), liste in uebersichten.items():
        info = gruppen.get(g, {})
        if info.get("status") == "gesperrt":
            continue
        if g not in termine_cache:
            try:
                termine_cache[g] = json.loads((abg / g / "termine.json").read_text(encoding="utf-8"))
            except (OSError, ValueError):
                termine_cache[g] = {}
        ziel = abg / g / f"sitzung{s:02d}" / "UEBERSICHT.md"
        try:
            schreibe(ziel, uebersicht_text(g, s, liste, info, termine_cache[g]), stats)
        except Exception as e:
            stats["fehler"] += 1
            log(f"::warning::{ziel}: {e}")
        bewahrt.add(ziel)

    # Angehaltene Gruppen und nicht zugeordnete Dateien
    for g in gruppen:
        marker = abg / g / "PRUEFUNG-ANGEHALTEN.md"
        if gruppen[g]["status"] == "gesperrt":
            schreibe(marker, "# Prüfung angehalten\n\nDie Datei `sollwerte.json` passt nicht zum eingefrorenen Stand dieser Gruppe. Rückmeldungen werden erst wieder erzeugt, wenn beides zusammenpasst. Stelle den eingefrorenen Stand wieder her, zum Beispiel über den Workflow „Eingefrorene Stände schützen“, oder prüfe die letzten Commits in `data/combined/eingefroren/" + g + "/`.\n", stats)
            bewahrt.add(marker)
        elif marker.exists():
            marker.unlink()
            stats["geschrieben"] += 1
    offen = [a for a in eintraege if not (a["gruppe"] and a["sitzung"]) or not a["kz"]]
    n_p = abg / "NICHT-ZUGEORDNET.md"
    if offen:
        z = ["# Nicht zugeordnete Dateien", "", "Diese Dateien liegen nicht dort, wo sie hingehören, oder haben keine Kennziffer im Namen. Sie stehen in keiner Übersicht, bis sie richtig liegen.", "",
             "| Datei | Grund | Eingang | Konto |", "|---|---|---|---|"]
        for a in offen:
            grund = "keine Kennziffer im Namen" if not a["kz"] else ("Gruppe nicht erkannt" if not a["gruppe"] else "Sitzung nicht erkannt")
            z.append(f"| {md_zelle(a['rel'], 120)} | {grund} | {zeit_text(a['zeit'])} | {md_zelle(a.get('konto_text') or 'unbekannt', 40)} |")
        z.append("")
        schreibe(n_p, "\n".join(z), stats)
        bewahrt.add(n_p)
    elif n_p.exists():
        n_p.unlink()
        stats["geschrieben"] += 1
    # Veraltete Rückmeldungen und Übersichten entfernen
    if abg.is_dir():
        for p in abg.rglob("*"):
            if p.is_file() and p not in bewahrt and (p.name.endswith("-rueckmeldung.md") or p.name == "UEBERSICHT.md"):
                a_g = p.relative_to(abg).parts[0] if len(p.relative_to(abg).parts) > 1 else ""
                if gruppen.get(a_g, {}).get("status") == "gesperrt":
                    continue
                p.unlink()
                stats["geschrieben"] += 1
    log(f"Abgaben: {stats['abgaben']}, Dateien geändert: {stats['geschrieben']}, Probleme: {stats['fehler']}")
    return 0


def auswerten(a, gruppen, gruppen_namen, lehr, uploads):
    antworten = a["antworten"]
    sitz_felder = sitzung_aus_felder(antworten)
    s_pfad = a["sitzung_pfad"]
    # Zielsitzung: Felder zählen, wenn sie vom Pfad abweichen; ohne Felder der Pfad
    a["sitzung"] = sitz_felder or (s_pfad if s_pfad in SITZUNGEN else None)
    a["ordner_passt"] = bool(a["gruppe"] and s_pfad and a["rel"].count("/") == 3 and (not sitz_felder or sitz_felder == s_pfad))
    commits = a["commits"]
    letzte = commits[-1] if commits else None
    erste = commits[0] if commits else None
    lehr_letzte = ist_lehrperson(letzte, lehr) if letzte else False
    a["konto_text"] = "Lehrperson" if lehr_letzte else (letzte["konto"] if letzte else "")
    if not a["kz"]:
        a["hinweise"].append(f"Der Dateiname {md_zelle(a['name'], 100)} enthält keine Kennziffer. Benenne die Datei bitte `<Deine Kennziffer>.txt`, dann wird sie zugeordnet.")
        a["kurz_hinweise"].append("keine Kennziffer im Dateinamen")
    if not a["gruppe"]:
        a["hinweise"].append("Ich erkenne Deine Gruppe nicht. Lege die Datei im Ordner Deiner Gruppe ab, siehe `abgaben/README.md`.")
        a["kurz_hinweise"].append("Gruppe nicht erkannt")
    if sitz_felder and s_pfad and sitz_felder != s_pfad:
        a["hinweise"].append(f"Die Datei liegt im Ordner von Sitzung {s_pfad:02d}, die Felder gehören zu Sitzung {sitz_felder:02d}. Geprüft wurde gegen Sitzung {sitz_felder:02d}. Lege sie bitte in den passenden Ordner.")
        a["kurz_hinweise"].append(f"liegt in sitzung{s_pfad:02d}, Felder aus sitzung{sitz_felder:02d}")
    elif not s_pfad and a["sitzung"]:
        a["hinweise"].append(f"Der Ordner für die Sitzung fehlt. Anhand der Felder wurde Sitzung {a['sitzung']:02d} geprüft. Lege die Datei bitte in `sitzung{a['sitzung']:02d}`.")
        a["kurz_hinweise"].append("Sitzungsordner fehlt")
    if not antworten and not a["problem"]:
        a["hinweise"].append("In Deiner Datei finde ich keine Feldnummern wie S02-5. Füge bitte die Vorlage aus dem Abschluss-Kapitel ein und trag dort Deine Antworten ein.")
        a["kurz_hinweise"].append("keine Feldnummern gefunden")
    if a["doppelt"]:
        a["hinweise"].append("Diese Felder stehen mehrfach in Deiner Datei, gewertet ist die letzte Nennung: " + ", ".join(sorted(set(a["doppelt"]))) + ".")
        a["kurz_hinweise"].append("Felder doppelt: " + ", ".join(sorted(set(a["doppelt"]))))

    # Konto und Kennziffer
    info_gr = gruppen.get(a["gruppe"], {}) if a["gruppe"] else {}
    k2kz, kz2k = info_gr.get("k2kz", {}), info_gr.get("kz2k", {})
    if letzte and not lehr_letzte and a["kz_int"]:
        konto = _konto_key(letzte)
        eigene = k2kz.get(konto, set())
        besitzer = kz2k.get(a["kz_int"])
        if besitzer and besitzer != konto and not (erste and besitzer == _konto_key(erste)):
            a["hinweise"].append(f"Die Kennziffer {a['kz']} ist einem anderen GitHub-Konto zugeordnet. Prüfe bitte Deine Kennziffer. Deine Lehrperson sieht das in der Übersicht.")
            a["kurz_hinweise"].append(f"Kennziffer gehört zu einem anderen Konto ({besitzer})")
        stand = info_gr.get("stand") or set()
        if stand and a["kz_int"] not in stand and not besitzer and eigene:
            passend = [z for z in eigene if z in stand]
            if len(passend) == 1:
                a["kz_zugeordnet"] = passend[0]
                a["hinweise"].append(f"Die Kennziffer im Dateinamen steht nicht im Stand Deiner Gruppe. Über Dein GitHub-Konto gehört zu Dir die Kennziffer {passend[0]}, darüber wurde Deine Abgabe zugeordnet. Du kannst bei Deiner Nummer bleiben.")
                a["kurz_hinweise"].append(f"über Konto zugeordnet zu {passend[0]}")
    if erste and letzte and _konto_key(erste) != _konto_key(letzte) and not ist_lehrperson(erste, lehr) and not lehr_letzte:
        a["hinweise"].append(f"Diese Datei wurde zuletzt von einem anderen GitHub-Konto geändert ({letzte['konto']}, zuerst {erste['konto']}). Die frühere Fassung bleibt in der Historie erhalten.")
        a["kurz_hinweise"].append(f"zuletzt von anderem Konto ({letzte['konto']}, zuerst {erste['konto']})")

    # Dieselbe Nummer in anderer Länge (z. B. nur die letzten 6 Ziffern), auch ohne Konto-Treffer
    bekannt = set(info_gr.get("stand") or set()) | set(info_gr.get("uploads_z", {}))
    if a["kz_int"] and bekannt and a["kz_int"] not in bekannt and a["kz_int"] not in uploads and not a.get("kz_zugeordnet"):
        passend = sorted(z for z in bekannt if gleiche_nummer(a["kz_int"], z))
        if len(passend) == 1:
            a["kz_zugeordnet"] = passend[0]
            a["hinweise"].append(f"Die Kennziffer im Dateinamen steht so nicht im Stand Deiner Gruppe, passt aber eindeutig zu {passend[0]}. Darüber wurde Deine Abgabe zugeordnet. Du kannst bei Deiner Nummer bleiben.")
            a["kurz_hinweise"].append(f"über Nummer zugeordnet zu {passend[0]}")
            besitzer = kz2k.get(passend[0])
            if besitzer and letzte and not lehr_letzte and besitzer not in (_konto_key(letzte), _konto_key(erste)):
                a["kurz_hinweise"].append(f"Nummer laut Upload von anderem Konto ({besitzer})")
        elif passend:
            a["hinweise"].append(f"Die Kennziffer im Dateinamen passt zu mehreren Personen im Stand ({', '.join(passend)}). Bitte nimm Deine vollständige Kennziffer aus dem Upload.")
            a["kurz_hinweise"].append("Kennziffer mehrdeutig: " + ", ".join(passend))
        else:
            a["kurz_hinweise"].append("Kennziffer nicht im Stand")

    if a["problem"]:
        a["kurz_hinweise"].append({"leer": "Datei ist leer", "zu_gross": "Datei ist zu groß", "binaer": "Datei ist kein Text (z. B. Word oder PDF)", "nicht_lesbar": "Datei nicht lesbar"}.get(a["problem"], a["problem"]))
    if not a["gruppe"] or not a["sitzung"] or a["problem"]:
        return
    info = gruppen.get(a["gruppe"])
    if not info or not info.get("sw"):
        gi = gruppen.setdefault(a["gruppe"], {"status": "keine_sollwerte", "stand": set(), "dir": None, "sw": None})
        gi["hinweis"] = "Für Deine Gruppe liegen noch keine Sollwerte vor. Deine Abgabe ist gespeichert und wird automatisch nachgeprüft."
        return
    sw = info["sw"]
    a["sw_version"] = sw.get("version")
    if a["sitzung"] < 2:
        return
    felder_s = {fid: sf for fid, sf in sw["felder"].items() if sf.get("sitzung") == a["sitzung"]}
    if not felder_s:
        a["hinweise"].append(f"Für Sitzung {a['sitzung']:02d} gibt es keine prüfbaren Felder.")
        return
    a["fragen"] = {fid: sf.get("frage", "") for fid, sf in felder_s.items()}
    a["tipps"] = {fid: sf.get("tipp", "") for fid, sf in felder_s.items()}
    a["sf_typ"] = {fid: sf.get("typ") for fid, sf in felder_s.items()}
    kz_d = a.get("kz_zugeordnet") or a["kz_int"]
    zu_datensatz = {**uploads, **info.get("uploads_z", {})}
    datensatz = datensatz_von(kz_d, zu_datensatz) if kz_d else None
    if datensatz and kz_d not in zu_datensatz and any("pd" in sf for sf in felder_s.values()):
        a["hinweise"].append(f"Deine Kennziffer finde ich nicht im Stand Deiner Gruppe. Für die Aufgabe zu Deinem eigenen Datensatz gehe ich deshalb von Datensatz {datensatz} aus (letzte Ziffer Deiner Kennziffer). Hast Du einen anderen bearbeitet, sag Deiner Lehrperson Bescheid.")
    a["ergebnis"] = bewerte(antworten, sw, a["gruppe"], felder_s, datensatz)
    unbekannt = [f for f in antworten if re.match(r"S\d+-", f) and f not in felder_s]
    if unbekannt:
        a["hinweise"].append("Diese Zeilen gehören nicht zu dieser Sitzung und wurden nicht gewertet: " + ", ".join(sorted(unbekannt)) + ".")
    r, g = treffer(a["ergebnis"])
    if g >= 3 and r / g < 0.5:
        besser = []
        for og, oi in gruppen.items():
            if og == a["gruppe"] or not oi.get("sw"):
                continue
            ofelder = {fid: sf for fid, sf in oi["sw"]["felder"].items() if sf.get("sitzung") == a["sitzung"]}
            or_ = bewerte(antworten, oi["sw"], og, ofelder, datensatz)
            ro, _ = treffer(or_)
            if ro >= r + 3:
                besser.append(og)
        if besser:
            a["hinweise"].append(f"Deine Werte passen besser zum Stand der Gruppe {', '.join(besser)}. Liegt Deine Datei im richtigen Ordner?")
            a["kurz_hinweise"].append("passt besser zu Gruppe " + ", ".join(besser))


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--wurzel", default=".")
    ap.add_argument("--baue-sollwerte", nargs=2, metavar=("SPEC", "GRUPPE"))
    ap.add_argument("--korrektur", action="store_true")
    ap.add_argument("--nur-readmes", action="store_true")
    args = ap.parse_args()
    wurzel = Path(args.wurzel).resolve()
    if args.baue_sollwerte:
        return baue_sollwerte(args.baue_sollwerte[0], args.baue_sollwerte[1], args.korrektur, wurzel)
    if args.nur_readmes:
        stats = {"geschrieben": 0}
        schreibe_readmes(wurzel, bekannte_gruppen(wurzel, lade_gruppen(wurzel)), stats, set())
        log(f"READMEs: {stats['geschrieben']} geändert")
        return 0
    return lauf(wurzel)


if __name__ == "__main__":
    sys.exit(main())
