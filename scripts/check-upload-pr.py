"""Prüft, ob ein Pull Request nur einen Studierenden-Upload enthält (für die automatische Übernahme).

Aufruf: python3 scripts/check-upload-pr.py <files.json> <pr-autor> <owner/repo>
  files.json: Ausgabe von `gh api repos/<repo>/pulls/<nr>/files`
Ausgabe: erste Zeile OK oder ABGELEHNT, danach Gründe. Exit-Code 0 = übernehmen.

Erlaubt sind nur neue oder geänderte Dateien direkt in data/student-uploads/ mit Endung
.csv/.txt/.tsv und Abgaben abgaben/<gruppe>/sitzungNN/<Kennziffer>.txt. Eine bestehende Datei
darf nur überschreiben, wer sie selbst hochgeladen hat. Commits von Bots (Login endet auf [bot])
zählen dabei nicht. Hat der Bot die Datei nur umbenannt (scripts/dateinamen.py, Inhalt
unverändert), zählt die Historie unter dem alten Namen, denn die API folgt Umbenennungen nicht.
Zum Testen ohne GitHub-API: Umgebungsvariable HISTORY_JSON = {"pfad": ["login", ...]}; eine
Bot-Umbenennung als Eintrag {"login": "github-actions[bot]", "von": "alter/pfad"}.
"""
import json, os, re, subprocess, sys, urllib.parse

ALLOWED = re.compile(r"^data/student-uploads/[^/]+\.(csv|txt|tsv)$", re.IGNORECASE)
ABGABE = re.compile(r"^abgaben/[a-z0-9-]+/sitzung\d{2}/\d{1,20}\.txt$")
MAX_FILES = 5
MAX_LINES = 500
MAX_UMBENENNUNGEN = 5


def ist_bot(login):
    return login.endswith("[bot]")


def gh_json(pfad):
    out = subprocess.run(["gh", "api", "--paginate", "--slurp", pfad], capture_output=True, text=True, check=True).stdout
    return [x for seite in json.loads(out) for x in (seite if isinstance(seite, list) else [seite])]


def commits(path, repo):
    """[(login, alter Pfad oder None)] je Commit auf main, der die Datei berührt. Der alte Pfad
    steht nur bei einer Umbenennung durch einen Bot ohne Inhaltsänderung."""
    if "HISTORY_JSON" in os.environ:
        return [(e, None) if isinstance(e, str) else (e["login"], e.get("von"))
                for e in json.loads(os.environ["HISTORY_JSON"]).get(path, [])]
    liste = []
    for c in gh_json(f"repos/{repo}/commits?path={urllib.parse.quote(path)}&sha=main"):
        login = (c.get("author") or {}).get("login") or "null"
        von = None
        if ist_bot(login):
            for seite in gh_json(f"repos/{repo}/commits/{c['sha']}"):
                for f in seite.get("files", []):
                    if f.get("filename") == path and f.get("status") == "renamed" and f.get("changes") == 0:
                        von = f.get("previous_filename")
        liste.append((login, von))
    return liste


def history(path, repo, tiefe=0):
    """GitHub-Logins aller Commits, die die Datei auf main berührt haben, Bot-Umbenennungen zurückverfolgt."""
    logins = []
    for login, von in commits(path, repo):
        logins.append(login)
        if von and tiefe < MAX_UMBENENNUNGEN:
            logins += history(von, repo, tiefe + 1)
    return logins


def check(files, author, repo):
    reasons = []
    if not files:
        reasons.append("keine Dateien im Pull Request")
    if len(files) > MAX_FILES:
        reasons.append(f"{len(files)} Dateien, erlaubt sind höchstens {MAX_FILES}")
    for f in files:
        name, status = f.get("filename", ""), f.get("status", "")
        if not (ALLOWED.fullmatch(name) or ABGABE.fullmatch(name)) or re.search(r"[\x00-\x1f\x7f]", name):
            reasons.append(f"{name}: nur CSV-Dateien direkt in data/student-uploads/ oder Abgaben unter abgaben/<Gruppe>/sitzungNN/<Kennziffer>.txt sind erlaubt")
            continue
        if status not in ("added", "modified"):
            reasons.append(f"{name}: Status „{status}“ (nur neue oder geänderte Dateien)")
            continue
        if f.get("additions", 0) > MAX_LINES:
            reasons.append(f"{name}: {f.get('additions')} Zeilen, das ist kein einzelner Upload")
        if status == "modified":
            owners = [o for o in history(name, repo) if not ist_bot(o)]
            if not owners or any(o != author for o in owners):
                reasons.append(f"{name}: Datei existiert schon und stammt nicht von {author}. Bitte unter eigenem Namen hochladen")
    return reasons


if __name__ == "__main__":
    files = json.load(open(sys.argv[1], encoding="utf-8"))
    if files and isinstance(files[0], list):          # gh --paginate kann Seiten als Listen liefern
        files = [x for page in files for x in page]
    reasons = check(files, sys.argv[2], sys.argv[3])
    print("ABGELEHNT" if reasons else "OK")
    for r in reasons:
        print("- " + r)
    sys.exit(1 if reasons else 0)
