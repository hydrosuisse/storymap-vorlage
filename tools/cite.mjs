// cite.js — BibTeX parsing and APA 6 citations, shared by the Storymap-Studio
// (browser) and the storymap build (node, via tools/cite.mjs). No dependencies.
//
//   const c = makeCiter(bibText, { defaultLang: "de", today: new Date() });
//   c.cite([{ keys: ["aebi2026"], form: "p" }, ...])  -> ["(Aebi, 2026)", ...]  (HTML)
//   c.sourceLine(["bfe_produktion"], "de")              -> "Quelle: BFE (2026b)"
//   c.references()                                      -> [{ key, html, lang }]  (cited only)
//
// Language of the APA words ("und", "Hrsg.", "o. J.", "Abgerufen am …") follows each
// entry's BibTeX field `langid` or `language`, falling back to defaultLang. Nothing is
// translated when the story is shown in another language.

export const TERMS = {
  de: {
    and: "und", ed: "Hrsg.", eds: "Hrsg.", nd: "o. J.", page: "S.", pages: "S.",
    dataset: "Datensatz", in: "In", etal: "et al.", edition: "Aufl.", no: "Nr.",
    thesis: "Dissertation", source: "Quelle", sources: "Quellen", data: "Daten",
    months: ["Januar", "Februar", "März", "April", "Mai", "Juni", "Juli", "August",
             "September", "Oktober", "November", "Dezember"],
    date: (d, m, y) => `${d}. ${m} ${y}`,
    refDate: (y, m, d) => (d ? `${y}, ${d}. ${m}` : `${y}, ${m}`),
    retrieved: (date, url) => `Abgerufen am ${date} von ${url}`,
  },
  en: {
    and: "and", ed: "Ed.", eds: "Eds.", nd: "n.d.", page: "p.", pages: "pp.",
    dataset: "Data set", in: "In", etal: "et al.", edition: "ed.", no: "No.",
    thesis: "Doctoral dissertation", source: "Source", sources: "Sources", data: "Data",
    months: ["January", "February", "March", "April", "May", "June", "July", "August",
             "September", "October", "November", "December"],
    date: (d, m, y) => `${m} ${d}, ${y}`,
    refDate: (y, m, d) => (d ? `${y}, ${m} ${d}` : `${y}, ${m}`),
    retrieved: (date, url) => `Retrieved ${date}, from ${url}`,
  },
  fr: {
    and: "et", ed: "Éd.", eds: "Éds", nd: "s. d.", page: "p.", pages: "p.",
    dataset: "Jeu de données", in: "Dans", etal: "et al.", edition: "éd.", no: "no",
    thesis: "Thèse de doctorat", source: "Source", sources: "Sources", data: "Données",
    months: ["janvier", "février", "mars", "avril", "mai", "juin", "juillet", "août",
             "septembre", "octobre", "novembre", "décembre"],
    date: (d, m, y) => `${d} ${m} ${y}`,
    refDate: (y, m, d) => (d ? `${y}, ${d} ${m}` : `${y}, ${m}`),
    retrieved: (date, url) => `Consulté le ${date} sur ${url}`,
  },
};

const LANG_ALIASES = {
  de: "de", deu: "de", ger: "de", german: "de", ngerman: "de", deutsch: "de", swissgerman: "de", nswissgerman: "de",
  en: "en", eng: "en", english: "en", american: "en", british: "en", usenglish: "en", ukenglish: "en", englisch: "en",
  fr: "fr", fra: "fr", fre: "fr", french: "fr", francais: "fr", "français": "fr", swissfrench: "fr", franzoesisch: "fr", "französisch": "fr",
};

// ---------------------------------------------------------------- BibTeX parser
const ACCENTS = { '"': "̈", "'": "́", "`": "̀", "^": "̂", "~": "̃", "c": "̧", "=": "̄" };

export function latexToText(s) {
  if (!s) return "";
  return s
    .replace(/\\(["'`^~=c])\s*\{?\\?([A-Za-z])\}?/g, (_, a, ch) => (ch + ACCENTS[a]).normalize("NFC"))
    .replace(/\{\\ss\}|\\ss\b\s?/g, "ß")
    .replace(/\\&/g, "&").replace(/\\%/g, "%").replace(/\\\$/g, "$").replace(/\\_/g, "_").replace(/\\#/g, "#")
    .replace(/---/g, "—").replace(/--/g, "–")
    .replace(/~/g, " ")
    .replace(/\\textit\{([^{}]*)\}|\\emph\{([^{}]*)\}/g, (_, a, b) => a || b)
    .replace(/\\url\{([^{}]*)\}/g, "$1")
    .replace(/[{}]/g, "")
    .replace(/\s+/g, " ")
    .trim();
}

function readValue(src, i) {
  // returns [rawValue, nextIndex]; value delimited by {..}, ".." or bare word; joined by #
  let out = "";
  for (;;) {
    while (/\s/.test(src[i])) i++;
    if (src[i] === "{") {
      let depth = 0, j = i;
      for (; j < src.length; j++) {
        if (src[j] === "{" && src[j - 1] !== "\\") depth++;
        else if (src[j] === "}" && src[j - 1] !== "\\") { depth--; if (depth === 0) break; }
      }
      out += src.slice(i + 1, j); i = j + 1;
    } else if (src[i] === '"') {
      let depth = 0, j = i + 1;
      for (; j < src.length; j++) {
        if (src[j] === "{") depth++;
        else if (src[j] === "}") depth--;
        else if (src[j] === '"' && depth === 0 && src[j - 1] !== "\\") break;
      }
      out += src.slice(i + 1, j); i = j + 1;
    } else {
      const m = /^[^,}\s#]+/.exec(src.slice(i));
      if (m) { out += m[0]; i += m[0].length; }
    }
    while (/\s/.test(src[i])) i++;
    if (src[i] === "#") { i++; continue; }
    return [out, i];
  }
}

export function parseBib(text) {
  const entries = new Map();
  const src = String(text || "");
  const re = /@(\w+)\s*[{(]/g;
  let m;
  while ((m = re.exec(src))) {
    const type = m[1].toLowerCase();
    let i = re.lastIndex;
    if (type === "comment" || type === "preamble" || type === "string") continue;
    const keyEnd = src.indexOf(",", i);
    if (keyEnd < 0) break;
    const key = src.slice(i, keyEnd).trim();
    i = keyEnd + 1;
    const fields = {}, raw = {};
    for (;;) {
      while (/[\s,]/.test(src[i])) i++;
      if (src[i] === "}" || src[i] === ")" || i >= src.length) { i++; break; }
      const nm = /^([\w:.-]+)\s*=\s*/.exec(src.slice(i));
      if (!nm) { i++; continue; }
      i += nm[0].length;
      const [val, next] = readValue(src, i);
      i = next;
      const name = nm[1].toLowerCase();
      raw[name] = val;
      fields[name] = latexToText(val);
    }
    re.lastIndex = i;
    entries.set(key, { key, type, fields, raw });
  }
  return entries;
}

// ---------------------------------------------------------------- names
function splitTopLevel(s, sep) {
  const parts = []; let depth = 0, cur = "";
  const words = s.split(/(\s+)/);
  for (const w of words) {
    for (const ch of w) { if (ch === "{") depth++; else if (ch === "}") depth--; }
    if (depth === 0 && w.trim().toLowerCase() === sep) { parts.push(cur.trim()); cur = ""; }
    else cur += w;
  }
  if (cur.trim()) parts.push(cur.trim());
  return parts;
}

export function parseNames(raw) {
  if (!raw) return [];
  return splitTopLevel(raw, "and").map((n) => {
    const t = n.trim();
    if (/^\{.*\}$/.test(t) && !/\}\s*,/.test(t)) return { corporate: latexToText(t) };
    if (t.toLowerCase() === "others") return { others: true };
    let last, first;
    if (t.includes(",")) {
      const [l, ...f] = t.split(",");
      last = latexToText(l); first = latexToText(f.join(","));
    } else {
      const ws = latexToText(t).split(" ");
      // particles (von, van, de, di, du, le, la, der) belong to the last name
      let k = ws.length - 1;
      while (k > 0 && /^(von|van|de|di|du|da|le|la|der|den|del|des|zu)$/.test(ws[k - 1])) k--;
      last = ws.slice(k).join(" "); first = ws.slice(0, k).join(" ");
    }
    return { last, first };
  });
}

function initials(first) {
  if (!first) return "";
  return first.split(/\s+/).filter(Boolean).map((w) =>
    w.split("-").map((p) => (/^[A-ZÄÖÜÀ-Þ]\.?$/.test(p) ? p.replace(/\.?$/, ".") : p[0] + ".")).join("-")
  ).join(" ");
}

function nameRef(n) {
  if (n.corporate) return n.corporate;
  const i = initials(n.first);
  return i ? `${n.last}, ${i}` : n.last;
}
function nameEditor(n) {
  if (n.corporate) return n.corporate;
  const i = initials(n.first);
  return i ? `${i} ${n.last}` : n.last;
}

function joinRefNames(list) {
  const names = list.filter((n) => !n.others);
  if (names.length === 0) return "";
  const f = names.map(nameRef);
  if (f.length === 1) return f[0];
  if (f.length <= 7) return f.slice(0, -1).join(", ") + ", & " + f[f.length - 1];
  return f.slice(0, 6).join(", ") + ", … " + f[f.length - 1];
}

// ---------------------------------------------------------------- helpers
const esc = (s) => String(s).replace(/&/g, "&amp;").replace(/</g, "&lt;").replace(/>/g, "&gt;");
const link = (u) => `<a href="${esc(u)}" target="_blank" rel="noopener">${esc(u)}</a>`;
const endDot = (s) => (/[.?!]$/.test(s.trim()) ? s.trim() : s.trim() + ".");

export function entryLang(entry, fallback = "de") {
  const v = (entry.fields.langid || entry.fields.language || "").toLowerCase().trim().split(/[\s,;]+/)[0];
  return LANG_ALIASES[v] || (TERMS[v] ? v : fallback);
}

function parseDate(s) {
  if (!s) return null;
  const m = /^(\d{4})(?:-(\d{1,2}))?(?:-(\d{1,2}))?/.exec(s.trim());
  return m ? { y: +m[1], m: m[2] ? +m[2] : null, d: m[3] ? +m[3] : null } : null;
}

function fmtDate(d, lang) {
  const T = TERMS[lang];
  return T.date(d.getDate(), T.months[d.getMonth()], d.getFullYear());
}

// ---------------------------------------------------------------- citer
export function makeCiter(bibText, opts = {}) {
  const defaultLang = opts.defaultLang || "de";
  const today = opts.today || new Date();
  const entries = typeof bibText === "string" ? parseBib(bibText) : bibText;
  const warnings = [];
  const usedOrder = [];      // keys in order of first citation
  const firstSeen = new Set();

  const info = new Map();    // key -> computed bits
  function get(key) {
    if (info.has(key)) return info.get(key);
    const e = entries.get(key);
    if (!e) return null;
    const f = e.fields;
    const lang = entryLang(e, defaultLang);
    const authors = parseNames(e.raw.author);
    const editors = parseNames(e.raw.editor);
    const date = parseDate(f.date) || (f.year ? { y: f.year, m: null, d: null } : null);
    const yearText = date ? String(date.y) : TERMS[lang].nd;
    const persons = authors.length ? authors : editors;
    const it = { e, f, lang, authors, editors, persons, date, yearText, suffix: "" };
    info.set(key, it);
    return it;
  }

  function authorShort(it, narrative, first) {
    const T = TERMS[it.lang];
    if (it.f.shortauthor) return it.f.shortauthor;
    const p = it.persons.filter((n) => !n.others);
    if (!p.length) {
      const t = it.f.shorttitle || it.f.title || it.e.key;
      return /^(article|incollection|inproceedings|inbook)$/.test(it.e.type) ? `«${t}»` : `<i>${esc(t)}</i>`;
    }
    const L = p.map((n) => n.corporate || n.last);
    if (L.length === 1) return L[0];
    const amp = narrative ? ` ${T.and} ` : " & ";
    if (L.length === 2) return L[0] + amp + L[1];
    if (L.length <= 5 && first) return L.slice(0, -1).join(", ") + (narrative ? ` ${T.and} ` : ", & ") + L[L.length - 1];
    return `${L[0]} ${T.etal}`;
  }

  // a/b/c suffixes among the cited entries
  function assignSuffixes(keys) {
    const groups = new Map();
    for (const k of keys) {
      const it = get(k); if (!it) continue;
      const g = authorShort(it, false, false) + "|" + it.yearText;
      if (!groups.has(g)) groups.set(g, []);
      groups.get(g).push(it);
    }
    for (const list of groups.values()) {
      list.forEach((it) => (it.suffix = ""));
      if (list.length < 2) continue;
      list.sort((a, b) => sortKey(a.f.title).localeCompare(sortKey(b.f.title), "de"));
      list.forEach((it, i) => (it.suffix = (it.date ? "" : "-") + String.fromCharCode(97 + i)));
    }
  }

  const sortKey = (s) => String(s || "").normalize("NFD").replace(/[̀-ͯ]/g, "").replace(/<[^>]+>|[«»"'{}]/g, "").toLowerCase();

  function yearCite(it) { return it.yearText + it.suffix; }

  // citations: [{keys, form: "p"|"t", page}] in document order
  function cite(citations) {
    const all = [];
    for (const c of citations) for (const k of c.keys) if (!all.includes(k)) all.push(k);
    for (const k of all) if (!usedOrder.includes(k)) usedOrder.push(k);
    assignSuffixes(usedOrder);
    return citations.map((c) => {
      const items = c.keys.map((k) => {
        const it = get(k);
        if (!it) { warnings.push(`Unbekannte Quelle: ${k}`); return { k, missing: true }; }
        const first = !firstSeen.has(k); firstSeen.add(k);
        return { k, it, first };
      });
      const parts = [];
      if (c.form === "t") {
        // narrative: same author grouped -> BFE (2026c, 2026d); Aebi (2026, S. 5)
        const groups = [];
        for (const x of items) {
          if (x.missing) { groups.push({ missing: true, k: x.k }); continue; }
          const label = authorShort(x.it, true, x.first);
          let g = groups.find((y) => y.label === label);
          if (!g) groups.push((g = { label, list: [] }));
          g.list.push(x);
        }
        for (const g of groups) {
          if (g.missing) { parts.push(`<span class="sm-cite-missing">[?${esc(g.k)}]</span>`); continue; }
          g.list.sort((a, b) => yearCite(a.it).localeCompare(yearCite(b.it)));
          const ys = g.list.map((x) => {
            const pg = c.page && c.keys.length === 1 ? `, ${TERMS[x.it.lang].page}\u00A0${esc(c.page)}` : "";
            return `<a class="sm-cite" href="#ref-${esc(x.k)}">${yearCite(x.it)}${pg}</a>`;
          });
          parts.push(`${g.label} (${ys.join(", ")})`);
        }
        return parts.join("; ");
      }
      // parenthetical: group same author label, sort alphabetically
      const groups = [];
      for (const x of items) {
        if (x.missing) { groups.push({ label: `[?${x.k}]`, missing: true, list: [x] }); continue; }
        const label = authorShort(x.it, false, x.first);
        let g = groups.find((y) => y.label === label);
        if (!g) groups.push((g = { label, list: [] }));
        g.list.push(x);
      }
      groups.sort((a, b) => sortKey(a.label).localeCompare(sortKey(b.label), "de"));
      const inner = groups.map((g) => {
        if (g.missing) return `<span class="sm-cite-missing">${esc(g.label)}</span>`;
        g.list.sort((a, b) => yearCite(a.it).localeCompare(yearCite(b.it)));
        const ys = g.list.map((x) => {
          const pg = c.page && c.keys.length === 1 ? `, ${TERMS[x.it.lang].page} ${esc(c.page)}` : "";
          return `<a class="sm-cite" href="#ref-${esc(x.k)}">${yearCite(x.it)}${pg}</a>`;
        });
        return `${g.label}, ${ys.join(", ")}`;
      });
      return `(${inner.join("; ")})`;
    });
  }

  // "Quelle: BFE (2026b); Fraunhofer ISE (2026)" — prefix in the story's language
  function sourceLine(keys, storyLang = defaultLang, prefix) {
    if (!keys || !keys.length) return "";
    const T = TERMS[storyLang] || TERMS.de;
    const pre = prefix != null ? prefix : (keys.length > 1 ? T.sources : T.source) + (storyLang === "fr" ? " : " : ": ");
    const html = cite([{ keys, form: "t" }])[0];
    return pre + html;
  }

  function reference(key) {
    const it = get(key);
    if (!it) return null;
    const { e, f, lang } = it;
    const T = TERMS[lang];
    const type = e.type;
    const isPart = /^(article|incollection|inproceedings|inbook)$/.test(type) || (type === "online" && !!f.journaltitle);
    const isNewspaper = type === "article" && !f.volume;

    // author / editor part
    let who = joinRefNames(it.authors);
    let title = f.title || "";
    if (!who && it.editors.length) {
      who = joinRefNames(it.editors) + ` (${it.editors.length > 1 ? T.eds : T.ed}).`;
    }
    // date part
    const d = it.date;
    let dateTxt = yearCite(it);
    if (d && d.m && (type === "online" || isNewspaper || type === "misc")) {
      dateTxt = T.refDate(String(d.y) + it.suffix, T.months[d.m - 1], d.d);
    }
    const datePart = `(${dateTxt}).`;

    const titleHtml = (t) => (isPart ? esc(t) : `<i>${esc(t)}</i>`);
    let parts = [];
    if (who) parts.push(endDot(esc(who)).replace(/\.\.$/, "."), datePart);
    // title + description
    let desc = f.type || (type === "dataset" ? T.dataset : type === "phdthesis" ? T.thesis : "");
    let extra = [];
    if (f.edition && !isPart) extra.push(`${f.edition}. ${T.edition}`);
    if (f.number && /report/.test(type)) extra.push(`${T.no} ${f.number}`);
    let titleBlock = title ? titleHtml(title) : "";
    if (extra.length) titleBlock += ` (${extra.join(", ")})`;
    if (desc) titleBlock += ` [${esc(desc)}]`;
    if (!who) { parts.push(endDot(titleBlock), datePart); }
    else if (titleBlock) parts.push(endDot(titleBlock));

    // container
    if (type === "article") {
      const j = f.journaltitle || f.journal || "";
      if (isNewspaper) { if (j) parts.push(`<i>${esc(j)}</i>.`); }
      else {
        let c = `<i>${esc(j)}, ${esc(f.volume)}</i>`;
        if (f.number) c += `(${esc(f.number)})`;
        if (f.pages) c += `, ${esc(f.pages.replace(/-+/g, "–"))}`;
        parts.push(c + ".");
      }
    } else if (/^(incollection|inproceedings|inbook)$/.test(type)) {
      let c = `${T.in} `;
      if (it.editors.length && it.authors.length) {
        const ed = it.editors.filter((n) => !n.others).map(nameEditor);
        c += (ed.length <= 2 ? ed.join(" & ") : ed.slice(0, -1).join(", ") + ", & " + ed[ed.length - 1]) +
             ` (${it.editors.length > 1 ? T.eds : T.ed}), `;
      }
      c += `<i>${esc(f.booktitle || "")}</i>`;
      if (f.pages) c += ` (${T.pages} ${esc(f.pages.replace(/-+/g, "–"))})`;
      parts.push(c + ".");
      const pub = [f.address || f.location, f.publisher].filter(Boolean).join(": ");
      if (pub) parts.push(esc(pub) + ".");
    } else if (/^(book|report|techreport|phdthesis|mastersthesis|thesis)$/.test(type)) {
      const pub = [f.address || f.location, f.publisher || f.institution || f.school].filter(Boolean).join(": ");
      if (pub) parts.push(esc(pub) + ".");
    } else {
      const org = f.journaltitle || f.organization || f.howpublished;
      if (org && !/^https?:/.test(org)) parts.push(`<i>${esc(org)}</i>.`);
    }

    // DOI / URL
    if (f.doi) parts.push(link(/^https?:/.test(f.doi) ? f.doi : "https://doi.org/" + f.doi));
    else if (f.url) {
      if (f.urldate) {
        const ud = /^(heute|today|aujourd)/i.test(f.urldate) ? today : (() => {
          const p = parseDate(f.urldate); return p ? new Date(p.y, (p.m || 1) - 1, p.d || 1) : today;
        })();
        parts.push(T.retrieved(fmtDate(ud, lang), link(f.url)));
      } else parts.push(link(f.url));
    }
    if (f.addendum || f.note) parts.push(esc(f.addendum || f.note));
    return { key, lang, html: parts.join(" ").replace(/\s+/g, " ").trim(), sort: refSort(it) };
  }

  function refSort(it) {
    const first = it.persons.filter((n) => !n.others)[0];
    const a = first ? (first.corporate || first.last + " " + (first.first || "")) : it.f.title;
    return [sortKey(a), String(it.date ? it.date.y : "0000"), it.suffix, sortKey(it.f.title)];
  }

  function references(keys = usedOrder) {
    assignSuffixes(keys);
    const out = keys.map(reference).filter(Boolean);
    out.sort((x, y) => {
      for (let i = 0; i < 4; i++) {
        const c = x.sort[i].localeCompare(y.sort[i], "de");
        if (c) return c;
      }
      return 0;
    });
    return out.map(({ key, lang, html }) => ({ key, lang, html }));
  }

  // label for chips / autocomplete (no links)
  function label(key, form = "p") {
    const it = get(key);
    if (!it) return `[?${key}]`;
    const a = authorShort(it, form === "t", false).replace(/<[^>]+>/g, "");
    return form === "t" ? `${a} (${yearCite(it)})` : `(${a}, ${yearCite(it)})`;
  }

  function search(q, limit = 8) {
    const s = sortKey(q);
    const out = [];
    for (const [k, e] of entries) {
      const it = get(k);
      const hay = sortKey([k, e.fields.author, e.fields.editor, e.fields.title, e.fields.year, e.fields.date, e.fields.shortauthor].join(" "));
      if (!s || hay.includes(s)) out.push(k);
      if (out.length >= limit) break;
    }
    return out;
  }

  function reset() { usedOrder.length = 0; firstSeen.clear(); warnings.length = 0; }

  return { entries, cite, sourceLine, references, reference, label, search, warnings, usedOrder, reset, get };
}

// Citation markup used in the story text:
//   <cite data-keys="a,b" data-form="p|t" data-page="12">…</cite>
// Plain-text forms are converted too: [@a; @b], @a (narrative), \cite{a,b}, \citep{a}, \citet{a}
export function textCitationsToMarkup(html) {
  return String(html)
    .replace(/\\cite([pt]?)\{([^}]+)\}/g, (_, f, k) => `<cite data-keys="${k.split(",").map((x) => x.trim()).join(",")}" data-form="${f === "t" ? "t" : "p"}"></cite>`)
    .replace(/\[((?:\s*@[\w:.\-]+\s*;?)+)\]/g, (_, inner) => `<cite data-keys="${inner.match(/@([\w:.\-]+)/g).map((x) => x.slice(1)).join(",")}" data-form="p"></cite>`);
}
