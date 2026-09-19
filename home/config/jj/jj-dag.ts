/**
 * jj-dag: draw a jj revset as a Graphviz picture in the terminal's own colors.
 *
 *   jj-dag [-r REVSET] [-o out.svg|out.png|out.dot] [--show]
 *
 * The default revset mirrors the `jj l` alias plus every mutable head of mine. Commits whose
 * parents fall outside the revset get a dashed edge to their nearest included ancestor
 * (what jj's graph prints as "~"). SVG nodes link to the commit on GitHub. With --show
 * the PNG goes to the terminal through `kitten icat` (inside tmux this needs
 * `set -g allow-passthrough on`).
 */

const REVSET = "trunk() | (mutable() & mine()) | (mutable() & mine())- | bookmarks() & mutable()";

function jj(...args: string[]): string {
  const out = new Deno.Command("jj", {
    args: ["--ignore-working-copy", "--no-pager", "--color=never", ...args],
    stdout: "piped",
    stderr: "inherit",
  }).outputSync();
  if (!out.success) Deno.exit(out.code);
  return new TextDecoder().decode(out.stdout);
}

// ---------------------------------------------------------------- palette
type Palette = Record<"bg" | "fg" | "c1" | "c2" | "c3" | "c4" | "c5" | "c6" | "c8", string>;

// Used when the terminal doesn't answer, e.g. output redirected or no tty.
const FALLBACK: Palette = {
  bg: "#1c1c1c", fg: "#d0d0d0", c1: "#d7875f", c2: "#87af87", c3: "#d7af5f",
  c4: "#87afd7", c5: "#af87af", c6: "#5fafaf", c8: "#6c6c6c",
};

/**
 * Ask the terminal for its palette (OSC 4), foreground (OSC 10) and background (OSC 11).
 * A trailing device-attributes query (DA1) is answered by every terminal, so its reply
 * marks the end of the answers without waiting on a timeout.
 */
async function queryPalette(): Promise<Partial<Palette>> {
  if (!Deno.stdin.isTerminal() || !Deno.stdout.isTerminal()) return {};
  const slots = [1, 2, 3, 4, 5, 6, 8];
  const query = slots.map((i) => `\x1b]4;${i};?\x1b\\`).join("") + "\x1b]10;?\x1b\\\x1b]11;?\x1b\\\x1b[c";

  Deno.stdin.setRaw(true);
  const reader = Deno.stdin.readable.getReader();
  let buf = "";
  try {
    await Deno.stdout.write(new TextEncoder().encode(query));
    const decoder = new TextDecoder();
    const deadline = Date.now() + 500;
    while (!/\x1b\[\?[\d;]*c/.test(buf)) {
      const left = deadline - Date.now();
      if (left <= 0) break;
      let timer: number | undefined;
      const timeout = new Promise<null>((resolve) => (timer = setTimeout(() => resolve(null), left)));
      const chunk = await Promise.race([reader.read(), timeout]);
      clearTimeout(timer);
      if (!chunk || chunk.done) break;
      buf += decoder.decode(chunk.value, { stream: true });
    }
  } finally {
    await reader.cancel().catch(() => {});
    Deno.stdin.setRaw(false);
  }

  // Replies look like ESC ]4;1;rgb:RRRR/GGGG/BBBB ST, with 1-4 hex digits per channel.
  const hex = (h: string) =>
    Math.round((parseInt(h, 16) / (16 ** h.length - 1)) * 255).toString(16).padStart(2, "0");
  const found: Partial<Palette> = {};
  for (const m of buf.matchAll(/\x1b\](\d+);(?:(\d+);)?rgb:([0-9a-f]+)\/([0-9a-f]+)\/([0-9a-f]+)/gi)) {
    const color = `#${hex(m[3])}${hex(m[4])}${hex(m[5])}`;
    const key = m[1] === "10" ? "fg" : m[1] === "11" ? "bg" : m[1] === "4" ? `c${m[2]}` : null;
    if (key && key in FALLBACK) found[key as keyof Palette] = color;
  }
  return found;
}

// ---------------------------------------------------------------- graph
const TYPE_COLOR: Record<string, keyof Palette> = {
  feat: "c2", fix: "c1", docs: "c4", refactor: "c5", test: "c3", chore: "c8",
  ci: "c6", build: "c6", perf: "c3", style: "c5", revert: "c1",
};
const CC = /^(\w+)(?:\(([^)]*)\))?!?:\s*(.*)$/;

const T = [
  "commit_id", "change_id.shortest(4)", "description.first_line()", "parents", "local_bookmarks",
  "immutable", "current_working_copy", "empty", "self.diff().stat().total_added()",
  "self.diff().stat().total_removed()", "author.timestamp()",
].map((e) => `json(${e})`).join(' ++ "\\t" ++ ') + ' ++ "\\n"';

interface Commit {
  cid: string; chg: { prefix: string; rest: string }; subj: string;
  parents: { commit_id: string }[]; lb: { name: string }[]; imm: boolean; wc: boolean;
  empty: boolean; add: number; rem: number; ts: string;
}
const KEYS = ["cid", "chg", "subj", "parents", "lb", "imm", "wc", "empty", "add", "rem", "ts"];

const esc = (s: string) =>
  s.replace(/&/g, "&amp;").replace(/</g, "&lt;").replace(/>/g, "&gt;").replace(/"/g, "&quot;");

function slug(): string | null {
  const m = jj("git", "remote", "list").match(/github\.com[:/]([^/\s]+\/[^/\s]+?)(?:\.git)?\s*$/m);
  return m ? m[1] : null;
}

function parseArgs(argv: string[]) {
  const a = { r: REVSET, o: null as string | null, show: false };
  for (let i = 0; i < argv.length; i++) {
    if (argv[i] === "-r") a.r = argv[++i];
    else if (argv[i] === "-o") a.o = argv[++i];
    else if (argv[i] === "--show") a.show = true;
    else {
      console.error("usage: jj-dag [-r REVSET] [-o out.svg|out.png|out.dot] [--show]");
      Deno.exit(2);
    }
  }
  return a;
}

async function main() {
  const a = parseArgs(Deno.args);
  const rows = jj("log", "--no-graph", "-r", a.r, "-T", T).split("\n").filter(Boolean)
    .map((l) => Object.fromEntries(l.split("\t").map((v, i) => [KEYS[i], JSON.parse(v)])) as Commit);
  const commits = new Map(rows.map((c) => [c.cid, c]));
  const p: Palette = { ...FALLBACK, ...(await queryPalette()) };
  const gh = slug();

  const edges: [string, string, boolean][] = [];
  for (const [cid, c] of commits) {
    const pids = c.parents.map((x) => x.commit_id);
    for (const x of pids) if (commits.has(x)) edges.push([cid, x, false]);
    if (pids.some((x) => !commits.has(x))) {
      // nearest included ancestor for the parents we lost
      const anc = jj("log", "--no-graph", "-r", `heads(::${cid}- & (${a.r}))`, "-T", 'commit_id ++ "\\n"').split(/\s+/);
      for (const x of anc) if (x && !pids.includes(x)) edges.push([cid, x, true]);
    }
  }

  const out = [
    `digraph jj { bgcolor="${p.bg}" rankdir=TB nodesep=0.25 ranksep=0.35 pad=0.3`,
    `node [shape=plain fontname="FiraCode Nerd Font" fontsize=11 fontcolor="${p.fg}"]`,
    `edge [color="${p.c8}" arrowhead=none penwidth=1.6]`,
  ];
  for (const [cid, c] of commits) {
    const m = c.subj.match(CC);
    const kind = m ? m[1].toLowerCase() : null;
    const accent = c.subj ? p[TYPE_COLOR[kind ?? ""] ?? "c4"] : p.c3;
    const subj = esc((m ? m[3] : c.subj) || "(no description)").slice(0, 48);
    const scope = m?.[2] ? `<font color="${p.c8}">${esc(m[2])} </font>` : "";
    const tag = kind ? `<font color="${accent}"><b>${kind}</b></font> ` : "";
    const pills = c.lb.map((b) =>
      `<td bgcolor="${p.c6}" style="rounded"><font color="${p.bg}"> ${esc(b.name)} </font></td>`
    ).join("");
    const stat = c.add || c.rem
      ? `<font color="${p.c2}">+${c.add}</font> <font color="${p.c1}">-${c.rem}</font>`
      : `<font color="${p.c8}">empty</font>`;
    const border = c.wc ? p.c2 : c.imm ? p.c8 : accent;
    const chg = `${c.chg.prefix}<font color="${p.c8}">${c.chg.rest}</font>`;
    const label =
      `<table border="${c.wc ? 2 : 1}" color="${border}" cellborder="0" cellspacing="2" cellpadding="3" style="rounded">` +
      `<tr><td rowspan="2" bgcolor="${accent}" width="4"></td><td align="left"><font color="${p.c5}"><b>${c.wc ? "@ " : ""}${chg}</b></font> ${tag}${scope}${subj}</td>${pills}</tr>` +
      `<tr><td align="left"><font point-size="9">${stat} <font color="${p.c8}">${c.ts.slice(0, 16).replace("T", " ")}</font></font></td></tr></table>`;
    const href = gh && c.imm
      ? ` href="https://github.com/${gh}/commit/${cid}" target="_blank" tooltip="${esc(c.subj)}"`
      : "";
    out.push(`"${cid}" [label=<${label}>${href}]`);
  }
  for (const [s, d, dashed] of edges) out.push(`"${s}" -> "${d}"` + (dashed ? " [style=dashed]" : ""));
  out.push("}");
  const dot = out.join("\n");

  const target = a.o ?? `${Deno.env.get("TMPDIR") ?? "/tmp"}/jj-dag.png`;
  const fmt = target.match(/\.(\w+)$/)?.[1] ?? "png";
  if (fmt === "dot") {
    Deno.writeTextFileSync(target, dot);
  } else {
    const proc = new Deno.Command("dot", {
      args: [`-T${fmt}`, fmt === "png" ? "-Gdpi=110" : "-Gdpi=72", "-o", target],
      stdin: "piped",
    }).spawn();
    const w = proc.stdin.getWriter();
    await w.write(new TextEncoder().encode(dot));
    await w.close();
    const { success, code } = await proc.status;
    if (!success) Deno.exit(code);
  }
  if (a.show && fmt === "png") {
    try {
      const { code } = await new Deno.Command("kitten", { args: ["icat", "--align=left", target] }).spawn().status;
      Deno.exit(code);
    } catch { /* no kitten on PATH: fall through and print the path */ }
  }
  console.log(target);
}

await main();
