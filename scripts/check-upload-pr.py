"""Prüft, ob ein Pull Request nur einen Studierenden-Upload enthält (für die automatische Übernahme).

Aufruf: python3 scripts/check-upload-pr.py <files.json> <pr-autor> <owner/repo>
  files.json: Ausgabe von `gh api repos/<repo>/pulls/<nr>/files`
Ausgabe: erste Zeile OK oder ABGELEHNT, danach Gründe. Exit-Code 0 = übernehmen.

Erlaubt sind nur neue oder geänderte Dateien direkt in data/student-uploads/ mit Endung
.csv/.txt/.tsv. Eine bestehende Datei darf nur überschreiben, wer sie selbst hochgeladen hat.
Zum Testen ohne GitHub-API: Umgebungsvariable HISTORY_JSON = {"pfad": ["login", ...]}.
"""
import json, os, re, subprocess, sys, urllib.parse

ALLOWED = re.compile(r"^data/student-uploads/[^/]+\.(csv|txt|tsv)$", re.IGNORECASE)
MAX_FILES = 5
MAX_LINES = 500


def history(path, repo):
    """GitHub-Logins aller Commits, die die Datei auf main berührt haben."""
    if "HISTORY_JSON" in os.environ:
        return json.loads(os.environ["HISTORY_JSON"]).get(path, [])
    out = subprocess.run(["gh", "api", "--paginate", f"repos/{repo}/commits?path={urllib.parse.quote(path)}&sha=main",
                          "--jq", ".[].author.login"], capture_output=True, text=True, check=True).stdout
    return [l for l in out.split("\n") if l]


def check(files, author, repo):
    reasons = []
    if not files:
        reasons.append("keine Dateien im Pull Request")
    if len(files) > MAX_FILES:
        reasons.append(f"{len(files)} Dateien, erlaubt sind höchstens {MAX_FILES}")
    for f in files:
        name, status = f.get("filename", ""), f.get("status", "")
        if not ALLOWED.fullmatch(name) or re.search(r"[\x00-\x1f\x7f]", name):
            reasons.append(f"{name}: nur CSV-Dateien direkt in data/student-uploads/ sind erlaubt")
            continue
        if status not in ("added", "modified"):
            reasons.append(f"{name}: Status „{status}“ (nur neue oder geänderte Dateien)")
            continue
        if f.get("additions", 0) > MAX_LINES:
            reasons.append(f"{name}: {f.get('additions')} Zeilen, das ist kein einzelner Upload")
        if status == "modified":
            owners = history(name, repo)
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
