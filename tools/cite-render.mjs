// cite-render.mjs — called by R/studio_build.R:
//   node tools/cite-render.mjs in.json out.json
// in:  { bib, defaultLang, today: "YYYY-MM-DD", jobs: { de: { citations: [...], lang: "de" }, fr: {...} },
// out: { de: { cites: [html], refs: [{ key, html, lang }] }, …, warnings: [...] }
// All citations of a page (text, source lines under figures, tables) go in one list in
// reading order, so a/b suffixes and "first citation" rules match the page.
import fs from "fs";
import { makeCiter } from "./cite.mjs";

const [, , inFile, outFile] = process.argv;
const job = JSON.parse(fs.readFileSync(inFile, "utf8"));
const today = job.today ? new Date(job.today + "T12:00:00") : new Date();
const out = { warnings: [] };
for (const [lang, j] of Object.entries(job.jobs)) {
  const c = makeCiter(job.bib || "", { defaultLang: job.defaultLang || "de", today });
  const cites = c.cite(j.citations || []);
  out[lang] = { cites, refs: c.references() };
  for (const w of c.warnings) if (!out.warnings.includes(w)) out.warnings.push(w);
}
fs.writeFileSync(outFile, JSON.stringify(out));
