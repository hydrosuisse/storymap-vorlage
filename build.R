# Build the story into _site/  (run locally or by GitHub Actions)
#   Rscript build.R
# Preview: python3 -m http.server -d _site   (then open http://localhost:8000)
#
# Content comes from the Storymap-Studio: content/story.json + content/quellen.bib
# Output: _site/de/, _site/fr/, … and a forwarding page _site/index.html

Sys.setlocale("LC_CTYPE", "C.UTF-8")
options(encoding = "UTF-8")
suppressPackageStartupMessages({ library(htmltools); library(mapgl); library(sf) })
source("R/storymap.R", encoding = "UTF-8")
source("R/bfe_charts.R", encoding = "UTF-8")
source("R/studio_charts.R", encoding = "UTF-8")
source("R/studio_build.R", encoding = "UTF-8")

unlink("_site", recursive = TRUE)
dir.create("_site/charts", recursive = TRUE)

# BFE live charts: only built if the story uses one of the BFE templates
story_text <- paste(readLines("content/story.json", encoding = "UTF-8", warn = FALSE), collapse = "\n")
uses <- function(t) grepl(sprintf('"template"\\s*:\\s*"%s"', t), story_text)
if (uses("speicherseen")) {
  speicher <- bfe_data("v2/fuellungsgrad-speicherseen")
  bfe_chart_page(bfe_speicherseen_chart(speicher), "_site/charts/speicherseen.html",
                 title = "Füllstände der Speicherseen", updated = speicher$totalCH$currentEntry$date)
}
if (uses("produktion_handel") || uses("produktion")) {
  produktion <- bfe_data("strom-produktion-import-verbrauch")
  bfe_chart_page(bfe_produktion_handel_chart(produktion, years = 5), "_site/charts/produktion.html",
                 title = "Landesproduktion, Landesverbrauch und Aussenhandel", updated = produktion$currentEntry$date)
  bfe_chart_page(bfe_produktion_chart(produktion, years = 5), "_site/charts/produktion_bfe.html",
                 title = "Stromproduktion nach Energieträger", updated = produktion$currentEntry$date)
}

studio_build("content/story.json", "content/quellen.bib", site = "_site")
for (d in c("assets", "media")) if (dir.exists(d)) invisible(file.copy(d, "_site", recursive = TRUE))
message("Done: _site/<Sprache>/index.html")
