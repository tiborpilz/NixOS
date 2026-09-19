/**
 * jj-pulse: GitHub-style activity heatmap of your changes.
 *
 *   jj-pulse [WEEKS]     (default 26)
 *
 * One jj call over mine() within the window; colors are ANSI palette slots, so the
 * heatmap follows the terminal theme.
 */

const weeks = Number(Deno.args[0] ?? 26);
const iso = (d: Date) =>
  `${d.getFullYear()}-${String(d.getMonth() + 1).padStart(2, "0")}-${String(d.getDate()).padStart(2, "0")}`;
const now = new Date();
const today = new Date(now.getFullYear(), now.getMonth(), now.getDate());
const addDays = (d: Date, n: number) => new Date(d.getFullYear(), d.getMonth(), d.getDate() + n);
const start = addDays(today, -(((today.getDay() + 6) % 7) + 7 * (weeks - 1))); // Monday, weeks back

const out = new Deno.Command("jj", {
  args: [
    "--ignore-working-copy", "--no-pager", "--color=never", "log", "--no-graph",
    "-r", `mine() & committer_date(after:"${iso(start)}") & ~empty()`,
    "-T", 'author.timestamp().format("%Y-%m-%d") ++ "\\n"',
  ],
  stdout: "piped",
  stderr: "inherit",
}).outputSync();
if (!out.success) Deno.exit(out.code);

const days = new Map<string, number>();
for (const d of new TextDecoder().decode(out.stdout).split(/\s+/).filter(Boolean)) {
  days.set(d, (days.get(d) ?? 0) + 1);
}
const count = (d: Date) => days.get(iso(d)) ?? 0;

const E = "\x1b[", R = E + "0m", DIM = E + "2m";
const LEVELS = [DIM + "·" + R, E + "32m▪" + R, E + "32m■" + R, E + "92m■" + R, E + "1;92m■" + R];
const cell = (n: number) => LEVELS[n === 0 ? 0 : n === 1 ? 1 : n <= 3 ? 2 : n <= 6 ? 3 : 4];

const cols = Array.from({ length: weeks }, (_, w) => Array.from({ length: 7 }, (_, d) => addDays(start, 7 * w + d)));
const months = Array<string>(weeks * 2 + 3).fill(" ");
cols.forEach((col, w) => {
  if (col[0].getDate() <= 7) {
    months.splice(w * 2, 3, ...col[0].toLocaleDateString("en-GB", { month: "short" }).slice(0, 3));
  }
});
console.log("     " + DIM + months.join("").padEnd(weeks * 2).slice(0, weeks * 2) + R);
["Mon", "", "Wed", "", "Fri", "", "Sun"].forEach((name, d) => {
  const row = cols.map((c) => (c[d] <= today ? cell(count(c[d])) : " ")).join(" ");
  console.log(`${DIM}${name.padStart(4)}${R} ${row}`);
});

const total = [...days.values()].reduce((a, b) => a + b, 0);
let streak = 0;
while (count(addDays(today, -streak))) streak++;
console.log(`\n     ${total} changes on ${days.size} days · streak ${streak}d   ` +
  `${DIM}less${R} ${LEVELS.join(" ")} ${DIM}more${R}`);
