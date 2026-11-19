# Storymap template (blank)

Starting point for a new storymap that is written with the **Storymap-Studio**
– no R code needed. GitHub builds the page and publishes it on GitHub Pages.

```
Storymap-Studio  ──save──▶  branch "entwurf"  ──publish──▶  branch "main"  ──GitHub Actions──▶  GitHub Pages
```

## Setting up a new story (once, about 10 minutes)

1. **Create a repository** on GitHub, e.g. `hydrosuisse/meine-storymap`, and upload all files from this folder
   (keep the folder structure; `.github/` matters too – on a Mac, hidden folders appear with Cmd+Shift+.).
2. **Enable Pages:** Settings → Pages → Build and deployment → Source: **GitHub Actions**.
3. **Allow Actions to write:** Settings → Actions → General → Workflow permissions → **Read and write permissions**.
4. **Open the studio** (e.g. `https://hydrosuisse.github.io/storymap-studio/`), sign in with
   `hydrosuisse/meine-storymap` and a token (Contents: Read and write on this repository; see the studio's README).
   The blank story appears: title, one chapter «Einleitung».
5. **Write, save, publish.** After «Veröffentlichen» the story is online after about 2–3 minutes at
   `https://hydrosuisse.github.io/meine-storymap/de/` (and `/fr/`).

The first build after uploading already publishes the blank story – that's fine and shows that everything works.

## What's where

```
content/story.json      the story (written by the studio – do not edit by hand)
content/quellen.bib     sources (BibTeX), only cited entries appear (APA 6)
media/                  images, videos, subtitles uploaded in the studio
data/geo/               layers imported from QGIS (created by the studio)
data/snapshot/          last downloaded data of live charts (fallback)
assets/                 look of the page (storymap.css, storymap.js, chart pages), placeholder cover cover.svg
R/                      build: studio_build.R (pages), studio_charts.R (own data sources),
                        bfe_charts.R (BFE templates), storymap.R (blocks)
tools/                  citations (cite.mjs – the same code as in the studio)
build.R                 builds everything into _site/
.github/workflows/      builds on every push to main (publish) and entwurf (preview) and daily
```

## Settings in the studio

- **Languages:** under «Titelbild & Einstellungen» (German and French by default; Italian, English and Romansh can be added)
- **Address:** under «Veröffentlichen → Adresse»: own domain, path, language codes, forwarding
- **Logo and footer:** under «Titelbild & Einstellungen»

## Optional

- **Preview page for drafts:** create an empty public repository `<name>-vorschau` (Pages: «Deploy from a branch»,
  `gh-pages`), a token with Contents: Read and write on it, and the secret `PREVIEW_TOKEN` in this repository
  (Settings → Secrets and variables → Actions). After that, every save in the studio builds a preview at
  `https://<owner>.github.io/<name>-vorschau/de/`.
- **Own domain:** enter it in the studio; at the domain provider add a CNAME record pointing to `<owner>.github.io`;
  in GitHub under Settings → Pages → Custom domain.
- **API keys for own data sources:** as a secret in the repository, add it under `env:` in the workflow,
  and in the studio write `{SECRET}` in the data address.

## Building locally (optional)

R ≥ 4.2 with the packages from `DESCRIPTION` and [Node.js](https://nodejs.org) ≥ 18:

```
Rscript build.R
python3 -m http.server -d _site
```
