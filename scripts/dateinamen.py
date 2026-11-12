"""Bereinigt Dateinamen, die unter Windows nicht auszuchecken wären.

Studierende laden im Browser hoch, dort sind Namen wie `datensatz-08-<831008>.csv` oder
zwei Dateien, die sich nur in Groß- und Kleinschreibung unterscheiden, möglich. Unter
Windows lässt sich das Repo dann nicht mehr vollständig klonen.

Regeln je Pfadteil: `< > : " \\ | ? *` und Steuerzeichen entfernen, Punkte und Leerzeichen am
Ende entfernen, reservierte Namen (CON, NUL, COM1, ...) mit `_` davor. Beginnt ein Name erst dadurch
mit einem Punkt (aus `<>.csv` würde die versteckte Datei `.csv`), kommt `datei` davor. Kollidieren zwei Pfade
(gleich ohne Rücksicht auf Groß/Klein und Unicode-Form), behält ein schon sauberer Name den
Vorrang, darunter der ganz klein geschriebene (wie im Schema `datensatz-10-123456.xlsx`), sonst
der nach Zeichencode erste. Die weiteren bekommen -2, -3, ... vor der Endung, in derselben
Reihenfolge. Nie wird etwas überschrieben oder gelöscht, sonst wird nichts geändert.
data/combined/eingefroren/ und die Pfade in `fest` behalten immer ihren Namen. Beim Aufruf als Skript sind
README.md und .gitkeep in jedem Unterordner fest, ein Upload `readme.md` bekommt dann die Nummer.

Aufruf aus dem Repo-Root (nur Standardbibliothek):
  python3 scripts/dateinamen.py <ordner> [<ordner> ...]              Plan zeigen, nichts ändern
  python3 scripts/dateinamen.py --anwenden <ordner> [<ordner> ...]   mit `git mv` umbenennen
Ausgabe je Umbenennung eine Zeile `alt -> neu`. Die Umbenennungen sind danach gestaged,
committen muss der Aufrufer. Als Modul: plan(pfade, ordner, fest) gibt die Liste (alt, neu) zurück.
"""

import os
import posixpath
import re
import subprocess
import sys
import unicodedata

GESCHUETZT = "data/combined/eingefroren/"
VERBOTEN = re.compile(r'[<>:"\\|?*\x00-\x1f\x7f]')
FEST_NAMEN = {"README.md", ".gitkeep"}   # beim Aufruf als Skript auf jeder Ebene fest
RESERVIERT = {"CON", "PRN", "AUX", "NUL"} | {f"{p}{i}" for p in ("COM", "LPT") for i in range(1, 10)}


def teil_bereinigen(teil):
    t = VERBOTEN.sub("", teil).rstrip(". ")
    if not t:
        return "datei"
    if t.startswith(".") and not teil.startswith("."):
        t = "datei" + t                       # sonst entstünde eine versteckte Datei
    if t.split(".")[0].rstrip(" ").upper() in RESERVIERT:
        return "_" + t
    return t


def bereinigen(pfad, ordner="", ordner_klein=False):
    """Bereinigt alle Pfadteile nach dem Ordner-Präfix (der Präfix selbst bleibt).
    Mit ordner_klein werden die Unterordner nach dem Präfix klein geschrieben, der Dateiname nicht."""
    rest = pfad[len(ordner):] if ordner and pfad.startswith(ordner) else pfad
    teile = [teil_bereinigen(t) for t in rest.split("/")]
    if ordner_klein:
        teile = [t.lower() for t in teile[:-1]] + teile[-1:]
    return pfad[: len(pfad) - len(rest)] + "/".join(teile)


def schluessel(pfad):
    return unicodedata.normalize("NFC", pfad).casefold()


def mit_nummer(pfad, n):
    kopf, name = posixpath.split(pfad)
    stamm, endung = posixpath.splitext(name)
    return posixpath.join(kopf, f"{stamm}-{n}{endung}")


def plan(pfade, ordner="", fest=(), ordner_klein=False):
    """Reine Funktion: Pfadliste -> Liste (alt, neu), sortiert nach alt. Pfade mit / als Trenner.
    Pfade in `fest` (und unter data/combined/eingefroren/) werden nie umbenannt und belegen ihren Namen.
    ordner_klein: Unterordner klein schreiben (abgaben/Hannes/Sitzung02/x.txt -> abgaben/hannes/sitzung02/x.txt)."""
    fest = set(fest)
    pfade = set(pfade) | fest
    bleibt = {p for p in pfade if p in fest or p.startswith(GESCHUETZT)}
    belegt = {schluessel(p) for p in bleibt}
    neu = {p: bereinigen(p, ordner, ordner_klein) for p in pfade - bleibt}
    # Vorrang: schon sauber, dann ganz klein geschrieben, dann Zeichencode
    reihe = sorted(neu, key=lambda p: (neu[p] != p, neu[p] != neu[p].lower(), p))
    offen = []
    for p in reihe:                           # saubere Namen, jeweils der vorderste behält ihn
        k = schluessel(p)
        if neu[p] == p and k not in belegt:
            belegt.add(k)
        else:
            offen.append(p)
    umbenennen = []
    for p in offen:                           # alle übrigen bekommen einen freien Namen
        kandidat, n = neu[p], 1
        while schluessel(kandidat) in belegt:
            n += 1
            kandidat = mit_nummer(neu[p], n)
        belegt.add(schluessel(kandidat))
        umbenennen.append((p, kandidat))
    return sorted(umbenennen)


def git_dateien(ordner):
    aus = subprocess.run(["git", "ls-files", "-z", "--", ordner], check=True, capture_output=True).stdout
    return [p for p in aus.decode("utf-8", "surrogateescape").split("\0") if p]


def main(argv):
    anwenden = "--anwenden" in argv
    ordner_liste = [a for a in argv if a != "--anwenden"]
    if not ordner_liste:
        sys.exit(__doc__)
    sys.stdout.reconfigure(errors="backslashreplace")
    for ordner in ordner_liste:
        ordner = ordner.rstrip("/") + "/"
        pfade = git_dateien(ordner)
        fest = [p for p in pfade if posixpath.basename(p) in FEST_NAMEN]
        for alt, neu in plan(pfade, ordner, fest):
            print(f"{alt} -> {neu}")
            if anwenden:
                os.makedirs(os.path.dirname(neu) or ".", exist_ok=True)
                subprocess.run(["git", "mv", "--", alt, neu], check=True)


if __name__ == "__main__":
    main(sys.argv[1:])
