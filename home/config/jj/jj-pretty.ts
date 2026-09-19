/**
 * jj-pretty: re-render `jj log` output with icons, day headers, diff bars and PR status.
 *
 *   jj-pretty [log] [JJ LOG ARGS...]   graph view (jj draws the DAG, we draw the rows)
 *   jj-pretty stack [-r REVSET]        stack cards for trunk()..@, one card per bookmark
 *
 * jj emits one JSON record per commit behind a \x1f marker plus a \x1d continuation line;
 * jj's own graph prefix is kept verbatim, everything right of it is ours. Colors are ANSI
 * palette indices so the output follows the terminal theme.
 */

// ---------------------------------------------------------------- styling
const sgr = (...c: number[]) => `\x1b[${c.join(";")}m`;
const R = sgr(0), B = sgr(1), DIM = sgr(2), IT = sgr(3);
const fg = (n: number) => (n < 8 ? sgr(30 + n) : sgr(90 + n - 8));

const LINKS = Deno.env.get("JJ_PRETTY_LINKS") !== "0";
const link = (url: string | null, text: string) =>
  url && LINKS ? `\x1b]8;;${url}\x1b\\${text}\x1b]8;;\x1b\\` : text;

type Icon = [glyph: string, color: number];

// conventional-commit type -> nerd font glyph and ansi color
const TYPES: Record<string, Icon> = {
  feat: ["\uf0eb", 2], // lightbulb
  fix: ["\uf188", 1], // bug
  chore: ["\uf0ad", 8], // wrench
  docs: ["\uf02d", 4], // book
  refactor: ["\uf1b8", 5], // recycle
  test: ["\uf0c3", 3], // flask
  perf: ["\uf0e4", 11], // gauge
  ci: ["\uf013", 6], // gear
  build: ["\uf1b3", 6], // cubes
  style: ["\uf1fc", 13], // brush
  revert: ["\uf0e2", 9], // undo
};
const ICON_NODESC: Icon = ["\uf128", 3]; // question
const ICON_OTHER: Icon = ["\uf444", 7]; // dot
const CC = /^(?<type>[a-zA-Z]+)(?:\((?<scope>[^)]*)\))?(?<bang>!)?:\s*(?<subj>.*)$/;

const ANSI = /\x1b\[[0-9;]*m|\x1b\]8;;.*?\x1b\\/g;
const vis = (s: string) => s.replace(ANSI, "");

// Terminal cells per code point: East Asian wide and emoji take two, combining marks none.
// Nerd Font glyphs are private-use and one cell wide in a Mono font.
const WIDE: [number, number][] = [
  [0x1100, 0x115f], [0x2e80, 0xa4cf], [0xac00, 0xd7a3], [0xf900, 0xfaff], [0xfe30, 0xfe4f],
  [0xff00, 0xff60], [0xffe0, 0xffe6], [0x1f300, 0x1f64f], [0x1f900, 0x1f9ff], [0x20000, 0x3fffd],
];
const ZERO: [number, number][] = [[0x0300, 0x036f], [0x200b, 0x200f], [0xfe00, 0xfe0f]];
const inRanges = (cp: number, rs: [number, number][]) => rs.some(([a, b]) => cp >= a && cp <= b);
function width(s: string): number {
  let w = 0;
  for (const ch of vis(s)) {
    const cp = ch.codePointAt(0)!;
    w += inRanges(cp, ZERO) ? 0 : inRanges(cp, WIDE) ? 2 : 1;
  }
  return w;
}
function trunc(s: string, n: number): string {
  if (n <= 0) return "";
  const cps = [...s];
  return cps.length <= n ? s : cps.slice(0, Math.max(0, n - 1)).join("") + "…";
}

// ---------------------------------------------------------------- jj data
const FIELDS: [string, string][] = [
  ["cp", "change_id.shortest(8).prefix()"], ["cr", "change_id.shortest(8).rest()"],
  ["cid", "commit_id"], ["subj", "description.first_line()"], ["empty", "empty"],
  ["conflict", "conflict"], ["immutable", "immutable"], ["wc", "current_working_copy"],
  ["divergent", "divergent"], ["lb", "local_bookmarks"], ["rb", "remote_bookmarks"],
  ["tags", "tags"], ["ts", "committer.timestamp()"], ["email", "author.email()"],
  ["add", "self.diff().stat().total_added()"], ["rem", "self.diff().stat().total_removed()"],
  ["files", "self.diff().files().len()"], ["pushed", 'self.contained_in("::remote_bookmarks()")'],
  ["parents", "parents.len()"],
];
const TEMPLATE = '"\\x1f[" ++ ' + FIELDS.map(([, e]) => `json(${e})`).join(' ++ "," ++ ') +
  ' ++ "]\\n\\x1d\\n"';

interface Ref { name: string; remote?: string }
interface Commit {
  cp: string; cr: string; cid: string; subj: string; empty: boolean; conflict: boolean;
  immutable: boolean; wc: boolean; divergent: boolean; lb: Ref[]; rb: Ref[]; tags: Ref[];
  ts: string; email: string; add: number; rem: number; files: number; pushed: boolean;
  parents: number;
}
function decode(raw: string): Commit {
  const vals = JSON.parse(raw) as unknown[];
  return Object.fromEntries(FIELDS.map(([k], i) => [k, vals[i]])) as unknown as Commit;
}

class JjError extends Error {
  constructor(readonly stderr: string, readonly code: number) {
    super(stderr);
  }
}
function jj(args: string[], color = false): string {
  const out = new Deno.Command("jj", {
    args: ["--ignore-working-copy", "--no-pager", "--color", color ? "always" : "never", ...args],
    stdout: "piped",
    stderr: "piped",
  }).outputSync();
  if (!out.success) throw new JjError(new TextDecoder().decode(out.stderr), out.code);
  return new TextDecoder().decode(out.stdout);
}

function githubSlug(): string | null {
  let out: string;
  try {
    out = jj(["git", "remote", "list"]);
  } catch {
    return null;
  }
  for (const line of out.split("\n")) {
    const m = line.match(/github\.com[:/]([^/\s]+\/[^/\s]+?)(?:\.git)?$/);
    if (m && (line.startsWith("origin") || line.startsWith("upstream"))) return m[1];
  }
  return null;
}

// ---------------------------------------------------------------- PR cache (stale-while-revalidate)
interface Check { conclusion?: string; state?: string; status?: string }
interface PR {
  number: number; headRefName: string; state: string; isDraft?: boolean; url: string;
  statusCheckRollup?: Check[];
}

function onPath(cmd: string): boolean {
  return (Deno.env.get("PATH") ?? "").split(":").some((dir) => {
    try {
      return Deno.statSync(`${dir}/${cmd}`).isFile;
    } catch {
      return false;
    }
  });
}

function prMap(slug: string | null): Map<string, PR> {
  const out = new Map<string, PR>();
  if (!slug || Deno.env.get("JJ_PRETTY_PRS") === "0" || !onPath("gh")) return out;
  const cacheDir = `${Deno.env.get("XDG_CACHE_HOME") ?? `${Deno.env.get("HOME")}/.cache`}/jj-pretty`;
  Deno.mkdirSync(cacheDir, { recursive: true });
  const path = `${cacheDir}/${slug.replace("/", "__")}.json`;
  let age = Infinity;
  try {
    age = (Date.now() - Deno.statSync(path).mtime!.getTime()) / 1000;
  } catch { /* no cache yet */ }
  if (age > 300) { // refresh in the background; never block rendering on the network
    const cmd = `gh pr list -R ${slug} --state all --limit 100 --json ` +
      `number,headRefName,state,isDraft,url,statusCheckRollup > ${path}.tmp && mv ${path}.tmp ${path}`;
    new Deno.Command("sh", { args: ["-c", cmd], stdin: "null", stdout: "null", stderr: "null" })
      .spawn().unref();
  }
  let prs: PR[];
  try {
    prs = JSON.parse(Deno.readTextFileSync(path));
  } catch {
    return out;
  }
  for (const pr of prs.sort((a, b) => a.number - b.number)) out.set(pr.headRefName, pr); // newest wins
  return out;
}

function checks(pr: PR): string {
  const roll = pr.statusCheckRollup ?? [];
  const states = roll.map((c) => (c.conclusion || c.state || c.status || "").toUpperCase());
  if (states.some((s) => ["FAILURE", "ERROR", "TIMED_OUT", "CANCELLED", "ACTION_REQUIRED"].includes(s))) {
    return fg(1) + "\uf00d" + R; // x
  }
  if (roll.length && states.some((s) => ["PENDING", "IN_PROGRESS", "QUEUED", "EXPECTED", ""].includes(s))) {
    return fg(3) + "\uf192" + R; // dot-circle
  }
  return roll.length ? fg(2) + "\uf00c" + R : "";
}

function prBadge(pr: PR): string {
  const [glyph, c] = pr.state === "MERGED"
    ? ["\uf419", 5] // git-merge
    : pr.state === "CLOSED"
    ? ["\uf4dc", 1]
    : pr.isDraft
    ? ["\uf4dd", 8]
    : ["\uf407", 2]; // git-pull-request
  return link(pr.url, `${fg(c)}${glyph} #${pr.number}${R}`) + (pr.state === "OPEN" ? " " + checks(pr) : "");
}

// ---------------------------------------------------------------- row pieces
function rel(dt: Date, now: Date): string {
  const s = (now.getTime() - dt.getTime()) / 1000;
  const units: [string, number][] = [["y", 31536000], ["mo", 2592000], ["w", 604800], ["d", 86400], ["h", 3600], ["m", 60]];
  for (const [unit, n] of units) if (s >= n) return `${Math.floor(s / n)}${unit}`;
  return "now";
}

const dayKey = (d: Date) => `${d.getFullYear()}-${d.getMonth()}-${d.getDate()}`;
const midnight = (d: Date) => new Date(d.getFullYear(), d.getMonth(), d.getDate()).getTime();

const WEEKDAYS = ["Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"];
const MONTHS = ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"];

function dayLabel(dt: Date, now: Date): string {
  const d = Math.round((midnight(now) - midnight(dt)) / 86400000);
  const name = ({ 0: "Today", 1: "Yesterday" } as Record<number, string>)[d];
  const year = dt.getFullYear() !== now.getFullYear() ? ` ${dt.getFullYear()}` : "";
  const pretty = `${WEEKDAYS[dt.getDay()]} ${String(dt.getDate()).padStart(2, "0")} ${MONTHS[dt.getMonth()]}${year}`;
  return name ? `${name} · ${pretty}` : pretty;
}

function diffbar(add: number, rem: number, cells = 8): string {
  const tot = add + rem;
  if (tot === 0) return DIM + "·".repeat(cells) + R;
  const n = Math.min(cells, Math.max(1, Math.round(cells * Math.min(1, Math.sqrt(tot / 400))))); // 400 lines = full
  const a = Math.round((n * add) / tot), r = n - a;
  return fg(2) + "■".repeat(a) + fg(1) + "■".repeat(r) + DIM + "·".repeat(cells - n) + R;
}

function iconFor(c: Commit): [Icon, RegExpMatchArray | null] {
  const m = c.subj.match(CC);
  if (!c.subj) return [ICON_NODESC, null];
  const type = m?.groups!.type.toLowerCase();
  if (type && type in TYPES) return [TYPES[type], m];
  return [ICON_OTHER, m];
}

function subject(c: Commit, m: RegExpMatchArray | null): string {
  if (!c.subj) return `${IT}${fg(3)}(no description)${R}`;
  let scope = "", bang = "", text = c.subj;
  if (m) {
    const g = m.groups!;
    scope = g.scope ? `${DIM}${g.scope}${R} ` : "";
    bang = g.bang ? `${fg(1)}${B}! ${R}` : "";
    text = g.subj;
  }
  return scope + bang + (c.wc ? B : "") + text + R;
}

function idPart(c: Commit, slug: string | null): string {
  const s = `${B}${fg(5)}${c.cp}${R}${DIM}${c.cr.slice(0, Math.max(0, 4 - c.cp.length))}${R}`;
  return c.pushed && slug ? link(`https://github.com/${slug}/commit/${c.cid}`, s) : s;
}

function pills(c: Commit, prs: Map<string, PR>, slug: string | null): string {
  const out: string[] = [];
  const remotes = c.rb.filter((b) => b.remote !== "git");
  const local = new Set(c.lb.map((b) => b.name));
  for (const name of [...local].sort()) {
    const synced = remotes.some((b) => b.name === name && b.remote === "origin");
    const pr = prs.get(name);
    const url = slug && synced ? `https://github.com/${slug}/tree/${name}` : null;
    const mark = synced ? "" : `${fg(3)}*${R}`;
    let label = link(url, `${fg(6)}\ue0a0 ${name}${R}`) + mark; // powerline branch glyph
    if (pr && name !== "main" && name !== "master") label += " " + prBadge(pr);
    out.push(label);
  }
  const sorted = [...remotes].sort((a, b) => `${a.name}@${a.remote}`.localeCompare(`${b.name}@${b.remote}`));
  for (const { name, remote } of sorted) {
    if (local.has(name) || name.startsWith("renovate/")) continue;
    out.push(`${DIM}${fg(6)}\ue0a0 ${name}@${remote}${R}`);
  }
  for (const t of c.tags) out.push(`${fg(3)}\uf02b ${t.name}${R}`);
  return out.join("  ");
}

function markers(c: Commit): string {
  const m: string[] = [];
  if (c.conflict) m.push(`${fg(1)}${B}\uf071 conflict${R}`);
  if (c.divergent) m.push(`${fg(1)}\uf126 divergent${R}`);
  if (c.empty && c.parents < 2) m.push(`${DIM}∅${R}`);
  return m.join(" ");
}

const stat = (c: Commit) =>
  `${fg(2)}${`+${c.add}`.padEnd(6)}${R}${fg(1)}${`-${c.rem}`.padEnd(6)}${R}${diffbar(c.add, c.rem)}`;

function renderRow(prefix: string, c: Commit, prs: Map<string, PR>, slug: string | null, now: Date, cols: number): string {
  const [[glyph, col], m] = iconFor(c);
  const left = `${fg(col)}${glyph}${R} ${idPart(c, slug)} `;
  const rightParts = [pills(c, prs, slug), markers(c)].filter(Boolean);
  const age = `${DIM}${rel(new Date(c.ts), now).padStart(4)}${R}`;
  let right = [...rightParts, c.add || c.rem ? stat(c) : " ".repeat(20), age].join("  ");
  let subj = subject(c, m);
  let room = cols - width(prefix) - width(left) - width(right) - 2;
  if (room < 12) { // narrow terminal: drop the diff stat first
    right = [...rightParts, age].join("  ");
    room = cols - width(prefix) - width(left) - width(right) - 2;
  }
  const plain = vis(subj);
  if ([...plain].length > room) subj = (c.wc ? B : "") + trunc(plain, room) + R;
  const pad = Math.max(1, cols - width(prefix) - width(left) - width(subj) - width(right));
  return prefix + left + subj + " ".repeat(pad) + right;
}

function dayHeader(prefix: string, dt: Date, now: Date, cols: number): string {
  const label = ` ${dayLabel(dt, now)} `;
  const rule = "─".repeat(Math.max(0, cols - width(prefix) - width(label) - 3));
  return `${prefix}${DIM}──${R}${B}${fg(4)}${label}${R}${DIM}${rule}${R}`;
}

// ---------------------------------------------------------------- graph mode
type Item =
  | { kind: "node"; prefix: string; c: Commit }
  | { kind: "cont"; prefix: string }
  | { kind: "raw"; line: string };

function graphMode(args: string[], cols: number): string[] {
  const slug = githubSlug(), prs = prMap(slug), now = new Date();
  const raw = jj(["log", "-T", TEMPLATE, ...args], true);
  const items: Item[] = raw.replace(/\n+$/, "").split("\n").map((line): Item => {
    if (line.includes("\x1f")) {
      const i = line.indexOf("\x1f");
      return { kind: "node", prefix: line.slice(0, i), c: decode(line.slice(i + 1)) };
    }
    if (line.includes("\x1d")) return { kind: "cont", prefix: line.slice(0, line.indexOf("\x1d")) };
    return { kind: "raw", line };
  });

  const nodes = items.flatMap((it, i) => (it.kind === "node" ? [i] : []));
  const nextNode = new Map(nodes.slice(0, -1).map((a, i) => [a, nodes[i + 1]]));
  const straighten: Record<string, string> = { "├": "│", "┤": "│", "┼": "│", "─": " ", "╯": " ", "╮": " ", "╭": " ", "╰": " " };
  const out: string[] = [];
  let lastDay: string | null = null, lastNode = -1;

  items.forEach((it, i) => {
    if (it.kind === "node") {
      const dt = new Date(it.c.ts);
      if (lastDay === null) out.push(dayHeader("", dt, now, cols));
      lastDay = dayKey(dt);
      lastNode = i;
      out.push(renderRow(it.prefix, it.c, prs, slug, now, cols));
    } else if (it.kind === "cont") {
      let prefix = it.prefix;
      const nxt = nextNode.get(lastNode);
      if (nxt !== undefined) {
        const ndt = new Date((items[nxt] as { c: Commit }).c.ts);
        if (dayKey(ndt) !== lastDay) {
          const edges = vis(prefix);
          if (edges.replace(/[ │]/g, "")) { // fork/merge line: keep it, header goes below
            out.push(prefix);
            prefix = DIM + [...edges].map((ch) => straighten[ch] ?? ch).join("").trimEnd() + " " + R;
          }
          out.push(dayHeader(prefix, ndt, now, cols));
          return;
        }
      }
      if (vis(prefix).replace(/[ │]/g, "")) out.push(prefix); // keep fork/merge edges, drop plain verticals
    } else {
      out.push(it.line);
    }
  });
  return out;
}

// ---------------------------------------------------------------- stack mode
function stackMode(args: string[], cols: number): string[] {
  const ri = args.indexOf("-r");
  const rev = ri >= 0 ? args[ri + 1] : "trunk()..@";
  const slug = githubSlug(), prs = prMap(slug), now = new Date();
  const records = (revset: string) =>
    jj(["log", "--no-graph", "-r", revset, "-T", TEMPLATE]).split("\n")
      .filter((l) => l.startsWith("\x1f")).map((l) => decode(l.slice(1)));
  const commits = records(rev), base = records("trunk()");

  // split the newest-first list into cards; a card ends (going down) where a bookmark sits
  const cards: Commit[][] = [];
  let cur: Commit[] = [];
  for (const c of commits) {
    if (c.lb.length && cur.length) {
      cards.push(cur);
      cur = [];
    }
    cur.push(c);
  }
  if (cur.length) cards.push(cur);

  const w = Math.min(cols, 110);
  const out: string[] = [];
  for (const card of cards) {
    const owner = card.find((c) => c.lb.length);
    let title: string;
    if (owner) {
      const name = owner.lb[0].name;
      title = `${fg(6)}${B}\ue0a0 ${name}${R}`;
      const pr = prs.get(name);
      if (pr) title += "  " + prBadge(pr);
      else if (!owner.rb.some((b) => b.remote === "origin" && b.name === name)) {
        title += `  ${DIM}not pushed · jj push -b ${name}${R}`;
      }
    } else {
      title = `${fg(3)}\uf4a5 unbookmarked${R}  ${DIM}jj pr NAME${R}`;
    }
    const totA = card.reduce((s, c) => s + c.add, 0), totR = card.reduce((s, c) => s + c.rem, 0);
    const summary = `${DIM}${card.length} change${card.length !== 1 ? "s" : ""}${R} ${fg(2)}+${totA}${R} ${fg(1)}-${totR}${R}`;
    const head = `${DIM}╭─${R} ${title} `, tail = ` ${summary} ${DIM}─╮${R}`;
    out.push(head + DIM + "─".repeat(Math.max(1, w - width(head) - width(tail))) + R + tail);
    for (const c of card) {
      const [[glyph, col], m] = iconFor(c);
      const node = c.wc ? `${fg(2)}${B}@${R}` : `${DIM}○${R}`;
      const left = `${DIM}│${R} ${node} ${fg(col)}${glyph}${R} ${idPart(c, slug)} `;
      const right = `${stat(c)} ${DIM}${rel(new Date(c.ts), now).padStart(4)}${R} ${DIM}│${R}`;
      const mk = markers(c);
      let subj = subject(c, m) + (mk ? " " + mk : "");
      const room = w - width(left) - width(right) - 1;
      if (width(subj) > room) subj = trunc(vis(subj), room);
      out.push(left + subj + " ".repeat(Math.max(1, w - width(left) - width(subj) - width(right))) + right);
    }
    out.push(`${DIM}╰${"─".repeat(w - 2)}╯${R}`);
    out.push(`${DIM}  ┊${R}`);
  }
  for (const c of base) {
    out.push(`  ${fg(6)}◆${R} ${DIM}trunk${R} ${pills(c, prs, slug)}  ${idPart(c, slug)} ` +
      `${DIM}${trunc(c.subj, 50)} · ${rel(new Date(c.ts), now)}${R}`);
  }
  return out;
}

// ---------------------------------------------------------------- main
function termSize(): { columns: number; rows: number } {
  try {
    return Deno.consoleSize();
  } catch {
    return { columns: 120, rows: 40 };
  }
}

async function main() {
  let argv = [...Deno.args];
  let mode = "log";
  if (argv[0] === "log" || argv[0] === "stack") mode = argv.shift()!;
  const tty = Deno.stdout.isTerminal();
  const color = argv.includes("--color=always") || (tty && !argv.includes("--color=never"));
  argv = argv.filter((a) => !a.startsWith("--color"));
  const size = termSize();
  const cols = Number(Deno.env.get("JJ_PRETTY_COLS")) || size.columns;

  let lines: string[];
  try {
    lines = (mode === "stack" ? stackMode : graphMode)(argv, cols);
  } catch (e) {
    if (!(e instanceof JjError)) throw e;
    await Deno.stderr.write(new TextEncoder().encode(e.stderr));
    Deno.exit(e.code);
  }
  let text = lines.join("\n") + "\n";
  if (!color) text = vis(text);
  const data = new TextEncoder().encode(text);
  if (tty && lines.length > size.rows) {
    const less = new Deno.Command("less", { args: ["-RFXS"], stdin: "piped" }).spawn();
    const w = less.stdin.getWriter();
    await w.write(data).catch(() => {}); // the pager may quit before reading everything
    await w.close().catch(() => {});
    await less.status;
  } else {
    await Deno.stdout.write(data);
  }
}

await main();
