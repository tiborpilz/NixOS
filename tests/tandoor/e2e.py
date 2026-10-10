#!/usr/bin/env python3
"""End-to-end checks for a Tandoor Recipes podman pod (stdlib only).

Intended to run as root inside a NixOS test VM (``podman`` on PATH) to verify
an upgrade from one Tandoor version to another.

    e2e.py [--url URL] [--state DIR] [--container NAME] [--http-timeout S] CMD

Commands:
    wait-ready [--timeout S] [--db-container NAME]
                                   poll /accounts/login/ until the web app is up
    seed                           create superuser + a canary recipe (via API)
    verify --expect-image REF      version / migrations / web / login / data
    control-pending --image REF    one-off run of REF must report pending migrations

Every failed check prints exactly one line ``<E2E>-FAIL[<tag>] <reason>`` and
exits 1. Success prints ``E2E-OK <command>``.
"""

import argparse
import http.cookiejar
import json
import os
import re
import secrets
import subprocess
import sys
import time
import urllib.error
import urllib.parse
import urllib.request
import uuid

# The failure marker is assembled at runtime on purpose.
FAIL_MARKER = "E2E-" + "FAIL"
OK_MARKER = "E2E-OK"

DEFAULT_URL = "http://localhost:8285"
DEFAULT_STATE = "/var/lib/tandoor-e2e"
DEFAULT_CONTAINER = "tandoor"
SEED_USERNAME = "e2e"
MANAGE_DIR = "/opt/recipes"
PYTHON_BIN = "/opt/recipes/venv/bin/python"


class CheckFailed(Exception):
    def __init__(self, tag, reason):
        super().__init__(reason)
        self.tag = tag
        self.reason = reason


def log(msg):
    print(msg, flush=True)


def fail(tag, reason):
    raise CheckFailed(tag, reason)


def oneline(text, limit=600):
    text = " ".join(str(text).split())
    return text if len(text) <= limit else text[:limit] + "..."


def tail_text(out, limit=400):
    """Last `limit` chars of command output, without known startup noise."""
    noise = ("django-vite", "LiteLLM")
    keep = [l for l in out.splitlines() if not any(n in l for n in noise)]
    return oneline("\n".join(keep)[-limit:], limit + 50)


# --------------------------------------------------------------------------
# HTTP
# --------------------------------------------------------------------------

class _NoRedirect(urllib.request.HTTPRedirectHandler):
    def redirect_request(self, req, fp, code, msg, headers, newurl):
        return None


class Response:
    def __init__(self, status, headers, body):
        self.status = status
        self.headers = headers
        self.body = body

    @property
    def text(self):
        return self.body.decode("utf-8", "replace")

    @property
    def content_type(self):
        return self.headers.get("Content-Type", "") if self.headers else ""

    def json(self):
        return json.loads(self.text)


class Http:
    """Tiny HTTP client: cookie jar, never follows redirects, always has a timeout."""

    def __init__(self, base_url, timeout):
        self.base = base_url.rstrip("/")
        self.timeout = timeout
        self.jar = http.cookiejar.CookieJar()
        # ProxyHandler({}) -> ignore any *_proxy environment variables.
        self.opener = urllib.request.build_opener(
            urllib.request.ProxyHandler({}),
            urllib.request.HTTPCookieProcessor(self.jar),
            _NoRedirect(),
        )

    def request(self, method, path, data=None, headers=None, timeout=None):
        url = path if path.startswith("http") else self.base + path
        hdrs = {"User-Agent": "tandoor-e2e/1.0", "Accept": "*/*"}
        hdrs.update(headers or {})
        req = urllib.request.Request(url, data=data, headers=hdrs, method=method)
        try:
            with self.opener.open(req, timeout=timeout or self.timeout) as resp:
                return Response(resp.status, resp.headers, resp.read())
        except urllib.error.HTTPError as e:  # 3xx (no redirect following), 4xx, 5xx
            try:
                body = e.read()
            except Exception:
                body = b""
            return Response(e.code, e.headers, body)

    def get(self, path, **kw):
        return self.request("GET", path, **kw)

    def post_form(self, path, fields, headers=None, **kw):
        hdrs = {"Content-Type": "application/x-www-form-urlencoded"}
        hdrs.update(headers or {})
        body = urllib.parse.urlencode(fields).encode()
        return self.request("POST", path, data=body, headers=hdrs, **kw)

    def post_json(self, path, obj, headers=None, **kw):
        hdrs = {"Content-Type": "application/json", "Accept": "application/json"}
        hdrs.update(headers or {})
        return self.request("POST", path, data=json.dumps(obj).encode(), headers=hdrs, **kw)

    def cookie(self, name):
        for c in self.jar:
            if c.name == name:
                return c.value
        return None


def request_or_fail(tag, what, fn, *args, **kwargs):
    try:
        return fn(*args, **kwargs)
    except (urllib.error.URLError, OSError, ValueError) as e:
        fail(tag, "%s: request failed: %s" % (what, oneline(e)))


# --------------------------------------------------------------------------
# subprocess helpers
# --------------------------------------------------------------------------

def run(cmd, tag, timeout=3600, check=True, desc=None):
    """Run a command, return (rc, combined output). Fails with `tag` on timeout/launch error."""
    shown = desc or " ".join(cmd)
    log("+ " + (shown if len(shown) < 300 else shown[:300] + "..."))
    try:
        p = subprocess.run(cmd, stdout=subprocess.PIPE, stderr=subprocess.STDOUT,
                           text=True, errors="replace", timeout=timeout)
    except subprocess.TimeoutExpired as e:
        fail(tag, "command timed out after %ss: %s" % (timeout, oneline(shown, 200)))
    except OSError as e:
        fail(tag, "cannot run %s: %s" % (cmd[0], e))
    if check and p.returncode != 0:
        fail(tag, "command exited %d: %s -> %s" % (p.returncode, oneline(shown, 200), tail_text(p.stdout, 600)))
    return p.returncode, p.stdout


def manage(args, script_args, tag, timeout=3600, check=True, desc=None):
    """Run `manage.py <script_args>` inside the tandoor container."""
    cmd = ["podman", "exec", "-w", MANAGE_DIR, args.container, PYTHON_BIN, "manage.py"] + script_args
    return run(cmd, tag, timeout=timeout, check=check, desc=desc)


# --------------------------------------------------------------------------
# state
# --------------------------------------------------------------------------

def state_path(args):
    return os.path.join(args.state, "state.json")


def load_state(args, tag):
    try:
        with open(state_path(args)) as f:
            return json.load(f)
    except (OSError, ValueError) as e:
        fail(tag, "cannot read state file %s (run 'seed' first): %s" % (state_path(args), e))


def save_state(args, state, tag):
    try:
        os.makedirs(args.state, exist_ok=True)
        tmp = state_path(args) + ".tmp"
        with open(tmp, "w") as f:
            json.dump(state, f, indent=2)
        os.chmod(tmp, 0o600)
        os.replace(tmp, state_path(args))
    except OSError as e:
        fail(tag, "cannot write state file %s: %s" % (state_path(args), e))


# --------------------------------------------------------------------------
# common building blocks
# --------------------------------------------------------------------------

def get_token(http, username, password, tag):
    """POST /api-token-auth/ exactly once (throttled to 10/day per IP -> never retry)."""
    r = request_or_fail(tag, "api-token-auth", http.post_form, "/api-token-auth/",
                        {"username": username, "password": password},
                        headers={"Accept": "application/json"})
    if r.status != 200:
        fail(tag, "/api-token-auth/ returned HTTP %d: %s" % (r.status, oneline(r.text, 300)))
    try:
        token = r.json()["token"]
    except (ValueError, KeyError, TypeError):
        fail(tag, "/api-token-auth/ returned no token: %s" % oneline(r.text, 300))
    if not token:
        fail(tag, "/api-token-auth/ returned an empty token")
    return token


def auth_headers(token):
    return {"Authorization": "Bearer " + token, "Accept": "application/json"}


def extract_assets(html):
    """Return ordered unique (css_urls, js_urls) for /static/... assets referenced by the HTML."""
    found = re.findall(r"""(?:src|href)\s*=\s*["'](/static/[^"'#\s]+)["']""", html)
    css, js, seen = [], [], set()
    for u in found:
        if u in seen:
            continue
        seen.add(u)
        p = urllib.parse.urlsplit(u).path
        if p.endswith(".css"):
            css.append(u)
        elif p.endswith(".js") or p.endswith(".mjs"):
            js.append(u)
    return css, js


def check_assets(http, urls, tag, what):
    for u in urls:
        r = request_or_fail(tag, "%s asset %s" % (what, u), http.get, u)
        if r.status != 200:
            fail(tag, "%s asset %s returned HTTP %d" % (what, u, r.status))
        if not r.body:
            fail(tag, "%s asset %s has an empty body" % (what, u))
    log("  %s: %d assets OK" % (what, len(urls)))


def results_of(payload):
    """Recipe list payload: paginated {'results': [...]} or a bare list."""
    if isinstance(payload, dict) and isinstance(payload.get("results"), list):
        return payload["results"]
    if isinstance(payload, list):
        return payload
    return None


# --------------------------------------------------------------------------
# commands
# --------------------------------------------------------------------------

def db_running(name):
    try:
        p = subprocess.run(["podman", "container", "inspect", name, "--format", "{{.State.Running}}"],
                           capture_output=True, text=True, timeout=120)
    except (OSError, subprocess.TimeoutExpired):
        return False
    return p.returncode == 0 and p.stdout.strip() == "true"


def check_db_running(args, down_since):
    """A database that stays down will never let the app come up; say so instead of timing out."""
    if down_since is None or time.monotonic() - down_since < args.db_grace:
        return
    try:
        logs = subprocess.run(["podman", "logs", "--tail", "5", args.db_container],
                              capture_output=True, text=True, errors="replace", timeout=120)
        tail = tail_text(logs.stdout + logs.stderr)
    except (OSError, subprocess.TimeoutExpired) as e:
        tail = "(no logs: %s)" % e
    fail("db", "database container %s has not been running for %ds: %s" % (args.db_container, args.db_grace, tail))


def cmd_wait_ready(args):
    http = Http(args.url, args.http_timeout)
    deadline = time.monotonic() + args.timeout
    log("waiting for %s/accounts/login/ (timeout %ds)" % (http.base, args.timeout))
    last = "no attempt yet"
    attempt = 0
    db_down_since = None
    while True:
        attempt += 1
        if args.db_container:
            check_db_running(args, db_down_since)
            running = db_running(args.db_container)
            db_down_since = None if running else (db_down_since or time.monotonic())
        try:
            r = http.get("/accounts/login/")
            if r.status == 200:
                log("ready after %d attempt(s): HTTP 200" % attempt)
                return
            loc = r.headers.get("Location", "") if r.headers else ""
            if r.status in (301, 302, 303, 307, 308) and "error" not in loc.lower():
                log("ready after %d attempt(s): HTTP %d -> %s" % (attempt, r.status, loc))
                return
            last = "HTTP %d%s" % (r.status, (" -> " + loc) if loc else "")
        except (urllib.error.URLError, OSError, ValueError) as e:
            last = "connection error: %s" % oneline(e, 200)
        remaining = deadline - time.monotonic()
        log("not ready yet (attempt %d, %s), %ds left" % (attempt, last, max(0, int(remaining))))
        if remaining <= 0:
            fail("web", "web app not ready within %ds at %s/accounts/login/ (last: %s)" % (args.timeout, http.base, last))
        time.sleep(min(10, max(0.0, remaining)))
        if time.monotonic() >= deadline:
            fail("web", "web app not ready within %ds at %s/accounts/login/ (last: %s)" % (args.timeout, http.base, last))


SEED_SHELL = """\
from django.contrib.auth import get_user_model
from django_scopes import scopes_disabled
from cookbook.helper.permission_helper import create_space_for_user
User = get_user_model()
with scopes_disabled():
    if User.objects.filter(username={username!r}).exists():
        raise SystemExit('SEED-ERROR user already exists')
    user = User.objects.create_superuser(username={username!r}, email='e2e@example.invalid', password={password!r})
    create_space_for_user(user, 'E2E Space')
    print('SEED-USER-CREATED', user.pk)
"""


def cmd_seed(args):
    tag = "seed"
    password = secrets.token_hex(16)
    hexid = uuid.uuid4().hex[:12]
    name = "E2E Canary " + hexid

    log("creating superuser %r with a random password and a space" % SEED_USERNAME)
    _, out = manage(args, ["shell", "-c", SEED_SHELL.format(username=SEED_USERNAME, password=password)], tag, timeout=3600,
                    desc="podman exec %s manage.py shell -c <create superuser script>" % args.container)
    if "SEED-USER-CREATED" not in out:
        fail(tag, "user creation did not report success: %s" % tail_text(out))
    log("user created")

    http = Http(args.url, args.http_timeout)
    token = get_token(http, SEED_USERNAME, password, tag)
    log("obtained API token")

    # Minimal payload accepted by RecipeSerializer on both 2.4.2 and 2.6.15:
    # name + steps (each step needs `ingredients`, may be empty). Everything else has defaults.
    payload = {
        "name": name,
        "description": "Canary recipe created by the tandoor e2e test (%s)." % hexid,
        "steps": [{"instruction": "Mix everything and check that the data survives the upgrade.", "ingredients": []}],
    }
    r = request_or_fail(tag, "create recipe", http.post_json, "/api/recipe/", payload, headers=auth_headers(token))
    if r.status not in (200, 201):
        fail(tag, "POST /api/recipe/ returned HTTP %d: %s" % (r.status, oneline(r.text, 400)))
    try:
        recipe_id = r.json()["id"]
    except (ValueError, KeyError, TypeError):
        fail(tag, "POST /api/recipe/ returned no id: %s" % oneline(r.text, 300))
    log("created recipe id=%s name=%r" % (recipe_id, name))

    r = request_or_fail(tag, "read back recipe", http.get, "/api/recipe/%s/" % recipe_id, headers=auth_headers(token))
    if r.status != 200:
        fail(tag, "GET /api/recipe/%s/ returned HTTP %d: %s" % (recipe_id, r.status, oneline(r.text, 300)))
    try:
        got = r.json().get("name")
    except (ValueError, AttributeError):
        fail(tag, "GET /api/recipe/%s/ returned non-JSON: %s" % (recipe_id, oneline(r.text, 300)))
    if got != name:
        fail(tag, "recipe name mismatch: expected %r got %r" % (name, got))

    save_state(args, {"username": SEED_USERNAME, "password": password,
                      "recipe_id": recipe_id, "recipe_name": name, "recipe_hex": hexid}, tag)
    log("state saved to %s" % state_path(args))


def check_version(args):
    tag = "version"
    ref = args.expect_image
    _, out = run(["podman", "inspect", args.container, "--format", "{{.ImageName}}"], tag, timeout=120)
    image = out.strip()
    log("container image: %s" % image)
    if image != ref:
        fail(tag, "container %s runs image %r, expected %r" % (args.container, image, ref))

    last = ref.rsplit("/", 1)[-1]
    if ":" not in last:
        fail(tag, "expected image %r has no tag" % ref)
    want = last.rsplit(":", 1)[1]
    _, out = run(["podman", "exec", args.container, "cat", MANAGE_DIR + "/cookbook/version_info.py"], tag, timeout=120)
    m = re.search(r"""TANDOOR_VERSION\s*=\s*["']([^"']+)["']""", out)
    if not m:
        fail(tag, "TANDOOR_VERSION not found in version_info.py: %s" % oneline(out, 200))
    log("TANDOOR_VERSION in container: %s" % m.group(1))
    if m.group(1) != want:
        fail(tag, "TANDOOR_VERSION is %r, expected %r" % (m.group(1), want))


def check_migrations(args):
    tag = "migrations"
    rc, out = manage(args, ["migrate", "--check"], tag, timeout=3600, check=False)
    log("migrate --check exit code: %d" % rc)
    if rc != 0:
        detail = tail_text(out)
        _, plan = manage(args, ["showmigrations", "--plan"], tag, timeout=3600, check=False)
        pending = [l.strip() for l in plan.splitlines() if "[ ]" in l]
        if pending:
            detail = "%d unapplied migration(s), first: %s; %s" % (len(pending), pending[0], detail)
        fail(tag, "migrate --check exited %d: %s" % (rc, detail))
    _, plan = manage(args, ["showmigrations", "--plan"], tag, timeout=3600)
    lines = [l for l in plan.splitlines() if re.match(r"\s*\[.\]", l)]
    unapplied = [l.strip() for l in plan.splitlines() if "[ ]" in l]
    if unapplied:
        fail(tag, "%d unapplied migration(s), first: %s" % (len(unapplied), unapplied[0]))
    if not any("[X]" in l for l in lines):
        fail(tag, "showmigrations --plan lists no applied migrations: %s" % tail_text(plan, 300))
    log("all %d migrations applied" % len(lines))


def check_web(args, http):
    tag = "web"
    r = request_or_fail(tag, "login page", http.get, "/accounts/login/")
    if r.status != 200:
        fail(tag, "GET /accounts/login/ returned HTTP %d" % r.status)
    if "text/html" not in r.content_type:
        fail(tag, "login page content-type is %r" % r.content_type)
    html = r.text
    for needle in ("csrfmiddlewaretoken", 'name="login"', 'name="password"', "Tandoor"):
        if needle not in html:
            fail(tag, "login page lacks %r" % needle)
    css, js = extract_assets(html)
    log("login page references %d css and %d js assets" % (len(css), len(js)))
    if not css:
        fail(tag, "login page references no /static/ css")
    if not js:
        fail(tag, "login page references no /static/ js")
    check_assets(http, css + js, tag, "login page")


def check_login(args, http, state):
    tag = "login"
    r = request_or_fail(tag, "login page", http.get, "/accounts/login/")
    if r.status != 200:
        fail(tag, "GET /accounts/login/ returned HTTP %d" % r.status)
    token = http.cookie("csrftoken")
    m = re.search(r"""name=["']csrfmiddlewaretoken["']\s+value=["']([^"']+)["']""", r.text) \
        or re.search(r"""value=["']([^"']+)["']\s+name=["']csrfmiddlewaretoken["']""", r.text)
    csrf = m.group(1) if m else token
    if not csrf:
        fail(tag, "no CSRF token found on login page")
    # Single attempt: allauth rate-limits logins (5/min/ip), never retry.
    r = request_or_fail(tag, "login POST", http.post_form, "/accounts/login/",
                        {"login": state["username"], "password": state["password"], "csrfmiddlewaretoken": csrf},
                        headers={"Referer": http.base + "/accounts/login/"})
    log("login POST -> HTTP %d%s" % (r.status, (" Location: " + r.headers.get("Location", "")) if r.status in (301, 302, 303) else ""))
    if r.status not in (301, 302, 303):
        fail(tag, "login POST returned HTTP %d (expected redirect): %s" % (r.status, oneline(re.sub(r"<[^>]+>", " ", r.text), 300)))
    loc = r.headers.get("Location", "")
    if "/accounts/login" in loc:
        fail(tag, "login redirected back to login page: %s" % loc)
    if not http.cookie("sessionid"):
        fail(tag, "login did not set a session cookie")

    r = request_or_fail(tag, "index page", http.get, "/")
    if r.status != 200:
        fail(tag, "authenticated GET / returned HTTP %d (Location: %s)" % (r.status, r.headers.get("Location", "")))
    if "text/html" not in r.content_type:
        fail(tag, "authenticated / content-type is %r" % r.content_type)
    if 'id="app"' not in r.text:
        fail(tag, 'authenticated / does not contain id="app"')
    css, js = extract_assets(r.text)
    log("app page references %d css and %d js assets" % (len(css), len(js)))
    if not js:
        fail(tag, "authenticated / references no /static/ js")
    check_assets(http, css + js, tag, "app page")


def check_data(args, state):
    tag = "data"
    # Fresh client without cookies: with a session cookie DRF would enforce CSRF on /api-token-auth/.
    http = Http(args.url, args.http_timeout)
    token = get_token(http, state["username"], state["password"], tag)
    hdr = auth_headers(token)
    rid, name, hexid = state["recipe_id"], state["recipe_name"], state["recipe_hex"]

    r = request_or_fail(tag, "get recipe", http.get, "/api/recipe/%s/" % rid, headers=hdr)
    if r.status != 200:
        fail(tag, "GET /api/recipe/%s/ returned HTTP %d: %s" % (rid, r.status, oneline(r.text, 300)))
    try:
        got = r.json().get("name")
    except (ValueError, AttributeError):
        fail(tag, "GET /api/recipe/%s/ returned non-JSON" % rid)
    if got != name:
        fail(tag, "recipe %s name is %r, expected %r" % (rid, got, name))
    log("recipe %s present with expected name" % rid)

    r = request_or_fail(tag, "search recipe", http.get, "/api/recipe/?" + urllib.parse.urlencode({"query": hexid}), headers=hdr)
    if r.status != 200:
        fail(tag, "recipe search returned HTTP %d: %s" % (r.status, oneline(r.text, 300)))
    try:
        res = results_of(r.json())
    except ValueError:
        fail(tag, "recipe search returned non-JSON")
    if res is None:
        fail(tag, "unrecognised recipe search payload: %s" % oneline(r.text, 300))
    ids = [x.get("id") for x in res if isinstance(x, dict)]
    if rid not in ids:
        fail(tag, "search for %r did not return recipe %s (got ids %s)" % (hexid, rid, ids))
    log("search for %r returns the recipe (%d result(s))" % (hexid, len(res)))

    # Controls: make sure the checks above are not vacuous.
    r = request_or_fail(tag, "control get", http.get, "/api/recipe/2147483000/", headers=hdr)
    if r.status != 404:
        fail(tag, "control: GET /api/recipe/2147483000/ returned HTTP %d, expected 404" % r.status)
    r = request_or_fail(tag, "control search", http.get, "/api/recipe/?" + urllib.parse.urlencode({"query": uuid.uuid4().hex}), headers=hdr)
    if r.status != 200:
        fail(tag, "control search returned HTTP %d" % r.status)
    try:
        res = results_of(r.json())
    except ValueError:
        res = None
    if res is None:
        fail(tag, "control search: unrecognised payload: %s" % oneline(r.text, 300))
    if len(res) != 0:
        fail(tag, "control: search for a random uuid returned %d result(s), expected 0" % len(res))
    log("controls OK (404 for unknown id, 0 results for random query)")


def cmd_verify(args):
    state = load_state(args, "state")
    http = Http(args.url, args.http_timeout)
    steps = [
        ("version", lambda: check_version(args)),
        ("migrations", lambda: check_migrations(args)),
        ("web", lambda: check_web(args, http)),
        ("login", lambda: check_login(args, http, state)),
        ("data", lambda: check_data(args, state)),
    ]
    for name, fn in steps:
        log("== check: %s" % name)
        fn()
        log("check %s OK" % name)


def cmd_control_pending(args):
    tag = "control"
    _, out = run(["podman", "inspect", args.container, "--format", "{{.Pod}}"], tag, timeout=120)
    pod = out.strip()
    if not pod:
        fail(tag, "container %s is not in a pod" % args.container)
    _, out = run(["podman", "inspect", args.container, "--format", "{{json .Config.Env}}"], tag, timeout=120)
    cont_env = json.loads(out)
    _, out = run(["podman", "inspect", args.container, "--format", "{{.Image}}"], tag, timeout=120)
    cur_image = out.strip()
    # Only forward variables that were set on the container itself, not the old image's defaults.
    _, out = run(["podman", "image", "inspect", cur_image, "--format", "{{json .Config.Env}}"], tag, timeout=120)
    image_env = set(json.loads(out) or [])
    skip = ("HOSTNAME=", "container=")
    env = [e for e in cont_env if e not in image_env and not e.startswith(skip)]
    log("pod %s, forwarding %d env var(s)" % (pod[:12], len(env)))

    script = ("cd /opt/recipes && venv/bin/python manage.py migrate --check; rc=$?; "
              "echo MIGRATE_CHECK_EXIT=$rc; "
              "venv/bin/python manage.py showmigrations --plan; exit $rc")
    cmd = ["podman", "run", "--rm", "--pod", pod, "--pull=never", "--entrypoint", "sh"]
    for e in env:
        cmd += ["-e", e]
    cmd += [args.image, "-c", script]
    rc, out = run(cmd, tag, timeout=3600, check=False)
    log("---- one-off container output (exit %d) ----" % rc)
    shown = [l for l in out.splitlines() if not re.match(r"\s*\[X\]", l)]
    log("\n".join(shown))
    log("(%d applied '[X]' migration lines omitted)" % (len(out.splitlines()) - len(shown)))
    log("---- end ----")
    if "MIGRATE_CHECK_EXIT=" not in out:
        fail(tag, "one-off container exited %d before running migrate --check (not a migration result): %s" % (rc, tail_text(out)))
    plan = [l for l in out.splitlines() if re.match(r"\s*\[.\]", l)]
    if not any("[X]" in l for l in plan):
        fail(tag, "could not read applied migrations from the live DB (DB unreachable?): %s" % tail_text(out))
    pending = sum(1 for l in plan if "[ ]" in l)
    if rc == 0 and pending == 0:
        # Legitimate for releases that ship no migrations; the restart still runs.
        log("no new migrations between the baseline database and %s" % args.image)
        return
    if rc == 0 or pending == 0:
        fail(tag, "migrate --check exited %d but showmigrations lists %d pending migration(s): %s" % (rc, pending, tail_text(out)))
    log("%d migration(s) pending, as expected for an upgrade" % pending)


# --------------------------------------------------------------------------

def build_parser():
    p = argparse.ArgumentParser(description="Tandoor end-to-end upgrade checks")
    p.add_argument("--url", default=DEFAULT_URL, help="base URL (default %(default)s)")
    p.add_argument("--state", default=DEFAULT_STATE, help="state directory (default %(default)s)")
    p.add_argument("--container", default=DEFAULT_CONTAINER, help="tandoor container name (default %(default)s)")
    p.add_argument("--http-timeout", type=float, default=120.0, help="timeout per HTTP request in seconds (default %(default)s)")
    sub = p.add_subparsers(dest="command", required=True)

    s = sub.add_parser("wait-ready", help="wait until /accounts/login/ answers")
    s.add_argument("--timeout", type=int, default=3600)
    s.add_argument("--db-container", default="", metavar="NAME",
                   help="fail early if this database container stays down")
    s.add_argument("--db-grace", type=int, default=300, metavar="S",
                   help="how long the database container may be down (default %(default)s)")
    s.set_defaults(fn=cmd_wait_ready)

    s = sub.add_parser("seed", help="create user and canary recipe")
    s.set_defaults(fn=cmd_seed)

    s = sub.add_parser("verify", help="run all post-upgrade checks")
    s.add_argument("--expect-image", required=True, metavar="REF")
    s.set_defaults(fn=cmd_verify)

    s = sub.add_parser("control-pending", help="new image must see pending migrations on the old DB")
    s.add_argument("--image", required=True, metavar="REF")
    s.set_defaults(fn=cmd_control_pending)
    return p


def main(argv=None):
    args = build_parser().parse_args(argv)
    try:
        args.fn(args)
    except CheckFailed as e:
        print("%s[%s] %s" % (FAIL_MARKER, e.tag, oneline(e.reason, 1500)), flush=True)
        return 1
    except KeyboardInterrupt:
        print("%s[%s] interrupted" % (FAIL_MARKER, args.command), flush=True)
        return 1
    except Exception as e:  # unexpected bug: still report in the standard format
        print("%s[%s] unexpected error: %s: %s" % (FAIL_MARKER, args.command, type(e).__name__, oneline(e, 600)), flush=True)
        return 1
    print("%s %s" % (OK_MARKER, args.command), flush=True)
    return 0


if __name__ == "__main__":
    sys.exit(main())
