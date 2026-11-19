# -----------------------------------------------------------------------------
# studio_build.R — builds the story pages from the Storymap-Studio files
#
#   content/story.json   chapters, blocks, translations, settings (written by the studio)
#   content/quellen.bib  sources (BibTeX); only cited entries are listed (APA 6)
#
# Output (in _site/):  <lang>/index.html for every language (e.g. de/, fr/),
#   index.html that forwards to a language, CNAME for a custom domain,
#   charts/chart-<id>.html for custom live charts.
# Citations are formatted by tools/cite.mjs (the same code the studio uses), via node.
# -----------------------------------------------------------------------------

library(htmltools)

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || identical(a, "")) b else a

.sb <- new.env()   # state while building one page

# ---- translatable values ------------------------------------------------------
# list(de = "...", fr = "...", `_h` = ...) -> list(v = text, l = language actually used)
tr_get <- function(x, lang, orig) {
  if (is.null(x)) return(list(v = "", l = lang))
  if (is.character(x)) return(list(v = x, l = lang))
  v <- x[[lang]]
  if (!is.null(v) && nzchar(gsub("<[^>]+>|\\s", "", v))) return(list(v = v, l = lang))
  v <- x[[orig]]
  if (!is.null(v) && nzchar(v)) return(list(v = v, l = orig))
  for (k in setdiff(names(x), "_h")) if (nzchar(x[[k]] %||% "")) return(list(v = x[[k]], l = k))
  list(v = "", l = lang)
}
tx <- function(x, lang, orig) tr_get(x, lang, orig)$v
# plain text as a tag; marks the language when the original is shown as fallback
ptext <- function(x, lang, orig, tag = tags$span) {
  t <- tr_get(x, lang, orig)
  if (t$l == lang) t$v else tag(lang = t$l, t$v)
}
# rich text (HTML from the studio) with citations; fallback paragraphs get lang=""
rich <- function(x, lang, orig) {
  t <- tr_get(x, lang, orig)
  h <- cite_html(t$v)
  if (t$l != lang) h <- gsub("<(p|ul|ol)>", sprintf("<\\1 lang=\"%s\">", t$l), h)
  HTML(h)
}

# ---- citations ----------------------------------------------------------------
cite_ph <- function(keys, form = "p", page = NULL) {
  .sb$cites[[length(.sb$cites) + 1]] <- list(keys = as.list(keys), form = form, page = page)
  n <- length(.sb$cites)
  if (!is.null(.sb$formatted)) .sb$formatted[[n]] else sprintf("[[C%d]]", n)
}
cite_html <- function(h) {
  if (!nzchar(h %||% "")) return("")
  re <- '<cite data-keys="([^"]+)" data-form="(p|t)"(?: data-page="([^"]*)")?></cite>'
  m <- gregexpr(re, h, perl = TRUE)
  hits <- regmatches(h, m)[[1]]
  if (!length(hits)) return(h)
  rep <- vapply(hits, function(x) {
    p <- regmatches(x, regexec(re, x, perl = TRUE))[[1]]
    cite_ph(strsplit(p[2], ",")[[1]], p[3], if (nzchar(p[4])) p[4] else NULL)
  }, "")
  regmatches(h, m) <- list(rep)
  h
}
TERMS <- list(
  de = list(source = "Quelle: ", sources = "Quellen: ", data = "Daten: ", table = "Tabelle", refs = "Quellen",
            images = "Bildnachweise", tables = "Tabellennachweise", open = "In neuem Fenster öffnen",
            loading = "Wird geladen …", video = "Video abspielen – wird von %s geladen", more = "Weiterlesen"),
  fr = list(source = "Source : ", sources = "Sources : ", data = "Données : ", table = "Tableau", refs = "Sources",
            images = "Crédits photographiques", tables = "Sources des tableaux", open = "Ouvrir dans une nouvelle fenêtre",
            loading = "Chargement …", video = "Lire la vidéo – chargée depuis %s", more = "Lire la suite"),
  en = list(source = "Source: ", sources = "Sources: ", data = "Data: ", table = "Table", refs = "References",
            images = "Image credits", tables = "Table sources", open = "Open in new window",
            loading = "Loading …", video = "Play video – loaded from %s", more = "Read more"),
  it = list(source = "Fonte: ", sources = "Fonti: ", data = "Dati: ", table = "Tabella", refs = "Fonti",
            images = "Crediti fotografici", tables = "Fonti delle tabelle", open = "Apri in una nuova finestra",
            loading = "Caricamento …", video = "Riproduci video – caricato da %s", more = "Continua")
)
term <- function(lang, k) (TERMS[[lang]] %||% TERMS$de)[[k]]
src_line <- function(keys, lang, prefix = NULL) {
  keys <- unlist(keys)
  if (!length(keys)) return(NULL)
  pre <- prefix %||% term(lang, if (length(keys) > 1) "sources" else "source")
  paste0(pre, cite_ph(keys, "t"))
}

run_citations <- function(bib, orig, jobs, today) {
  inf <- tempfile(fileext = ".json"); outf <- tempfile(fileext = ".json")
  jsonlite::write_json(list(bib = bib, defaultLang = orig, today = format(today), jobs = jobs),
                       inf, auto_unbox = TRUE, null = "null")
  status <- system2("node", c("tools/cite-render.mjs", shQuote(inf), shQuote(outf)))
  if (status != 0) stop("Zitierungen konnten nicht erstellt werden (node tools/cite-render.mjs).")
  jsonlite::read_json(outf, simplifyVector = FALSE)
}

# ---- paths --------------------------------------------------------------------
rel <- function(p) if (is.null(p) || !nzchar(p) || grepl("^(https?:|data:)", p)) p else paste0("../", p)
esc <- function(x) htmlEscape(x %||% "")

# ---- media credits (same rules as the studio) ---------------------------------
is_license <- function(a) grepl("\\b(CC|Public Domain|gemeinfrei|domaine public)\\b", a %||% "", ignore.case = TRUE)
credit_text <- function(cr) {
  a <- trimws(cr$author %||% ""); y <- trimws(cr$year %||% "")
  if (!nzchar(a)) return("")
  if (is_license(a) || !nzchar(y)) a else sprintf("%s (%s)", a, y)
}
strip_tags <- function(h) trimws(gsub("\\s+", " ", gsub("<[^>]+>", " ", h %||% "")))
all_images <- function(story) {
  out <- list()
  if (nzchar(story$cover$image %||% "")) out[[length(out) + 1]] <- list(id = "cover", kind = "cover", item = story$cover)
  for (ch in story$chapters) for (b in ch$blocks) {
    if (b$type %in% c("image", "video")) out[[length(out) + 1]] <- list(id = b$id, kind = b$type, item = b, orig = b$origLang)
    if (b$type == "tour") for (s in b$slides) for (im in s$images)
      out[[length(out) + 1]] <- list(id = im$id, kind = "slideimage", item = im, orig = s$origLang %||% b$origLang)
  }
  out
}
nachweis_rows <- function(story, lang, orig) {
  ov <- story$nachweise$overrides %||% list()
  rows <- list()
  for (e in all_images(story)) {
    o <- ov[[e$id]] %||% list()
    io <- e$orig %||% orig
    label <- if (!is.null(o$label)) tx(o$label, lang, orig) else if (e$kind == "cover") tx(e$item$creditLabel, lang, orig) else {
      cap <- strip_tags(tx(e$item$caption, lang, io))
      cap <- sub("\\s*(Foto|Bild|Grafik|Photo|Image|Video)\\s*:.*$", "", cap, ignore.case = TRUE)
      cap <- sub("[.\\s]+$", "", cap, perl = TRUE)
      if (!nzchar(cap)) cap <- strip_tags(tx(e$item$alt, lang, io))
      if (e$kind == "video") cap <- paste0(if (lang == "fr") "Vidéo : " else "Video: ", cap)
      cap
    }
    credit <- if (nzchar(o$credit %||% "")) o$credit else credit_text(e$item$credit)
    if (isTRUE(o$mergeWithPrevious) && length(rows)) next
    rows[[length(rows) + 1]] <- list(label = label, credit = credit)
  }
  rows
}

# ---- blocks -------------------------------------------------------------------
WIDTH <- c(standard = "standard", wide = "wide", full = "full")

block_tag <- function(b, ch, story, lang, orig, ctx) {
  o <- b$origLang %||% orig
  switch(b$type,
    text = rich(b$html, lang, o),
    subheading = tags$h3(ptext(b$text, lang, o)),
    abstract = tags$div(class = "sm-abstract",
      tags$span(class = paste("sm-abstract-titel", if (identical(b$titleSize, "small")) "is-small"), ptext(b$title, lang, o)),
      rich(b$html, lang, o)),
    image = if (nzchar(b$src %||% "")) sm_image(rel(b$src), caption = esc(tx(b$caption, lang, o)), alt = tx(b$alt, lang, o),
      width = WIDTH[[b$width %||% "standard"]], zoom = isTRUE(b$zoom), source = src_line(b$sources, lang)),
    table = table_tag(b, story, lang, o, ctx),
    chart = {
      page <- if (nzchar(b$template %||% "")) c(speicherseen = "speicherseen", produktion_handel = "produktion", produktion = "produktion_bfe")[[b$template]]
              else paste0("chart-", b$id)
      sm_frame(src = paste0("../charts/", page, ".html"), title = tx(b$title, lang, o) %||% "Grafik",
               height = "560px", auto_height = TRUE, caption = esc(tx(b$caption, lang, o)) %||% NULL,
               source = src_line(b$sources, lang), open_label = term(lang, "open"))
    },
    embed = if (nzchar(b$url %||% "")) sm_frame(src = b$url, title = tx(b$title, lang, o) %||% "Eingebettete Seite",
      height = b$heightDesktop %||% "600px", height_mobile = b$heightMobile %||% b$heightDesktop %||% "600px",
      load = if (identical(b$load, "after_page")) "after_page" else "lazy", delay_ms = b$delay %||% 2000,
      caption = esc(tx(b$caption, lang, o)) %||% NULL,
      source = src_line(b$sources, lang, if (identical(b$sourcePrefix, "data")) term(lang, "data")),
      loading_text = tx(b$loadingText, lang, o) %||% term(lang, "loading"), open_label = term(lang, "open")),
    video = video_tag(b, lang, o),
    map = {
      keys <- unique(c(unlist(b$sources), unlist(lapply(b$layers, function(l) l$sources))))
      sm_map(build_map(b, lang, o, NULL), caption = HTML(paste(c(esc(tx(b$caption, lang, o)) %||% NULL,
        if (length(keys)) sprintf('<span class="sm-source">%s</span>', src_line(keys, lang))), collapse = "<br>")),
      width = WIDTH[[b$width %||% "wide"]], height = b$height %||% "70vh",
      terrain = if (isTRUE(b$terrain)) "swisstopo", exaggeration = b$exaggeration %||% 1.3)
    },
    tour = {
      keys <- unique(unlist(lapply(b$layers, function(l) l$sources)))
      tagList(tour_tag(b, lang, o),
        if (length(keys)) tags$p(class = "sm-caption sm-tour-source", tags$span(class = "sm-source", HTML(src_line(keys, lang)))))
    },
    NULL)
}

table_tag <- function(b, story, lang, o, ctx) {
  ctx$tables <- ctx$tables + 1
  n <- ctx$tables
  al <- unlist(b$align) %||% rep("left", length(b$header))
  cell <- function(tag, x, i) tag(style = sprintf("text-align:%s", al[i] %||% "left"), ptext(x, lang, o))
  sl <- src_line(b$sources, lang, term(lang, "data"))
  ctx$table_rows[[length(ctx$table_rows) + 1]] <- list(n = n, title = tx(b$title, lang, o), sources = b$sources)
  tags$figure(class = "sm-block sm-standard sm-table",
    tags$figcaption(class = "sm-table-title", tags$b(sprintf("%s %d:", term(lang, "table"), n)), " ", ptext(b$title, lang, o)),
    tags$table(
      tags$thead(tags$tr(lapply(seq_along(b$header), function(i) cell(tags$th, b$header[[i]], i)))),
      tags$tbody(lapply(b$rows, function(r) tags$tr(lapply(seq_along(r), function(i) cell(tags$td, r[[i]], i)))))),
    if (!is.null(sl)) tags$figcaption(class = "sm-caption", tags$span(class = "sm-source", HTML(sl))))
}

video_embed <- function(provider, url) {
  url <- url %||% ""
  pick <- function(re) { m <- regmatches(url, regexec(re, url, perl = TRUE))[[1]]; if (length(m) > 1) m[2] else NULL }
  switch(provider,
    youtube = { id <- pick("(?:youtu\\.be/|v=|embed/|shorts/)([\\w-]{6,})"); if (!is.null(id)) sprintf("https://www.youtube-nocookie.com/embed/%s?autoplay=1&rel=0", id) },
    vimeo = { id <- pick("vimeo\\.com/(?:video/)?(\\d+)"); if (!is.null(id)) sprintf("https://player.vimeo.com/video/%s?autoplay=1&dnt=1", id) },
    switchtube = { id <- pick("tube\\.switch\\.ch/(?:videos|embed)/([\\w-]+)"); if (!is.null(id)) sprintf("https://tube.switch.ch/embed/%s", id) },
    NULL)
}
video_tag <- function(b, lang, o) {
  cap <- esc(tx(b$caption, lang, o))
  sl <- src_line(b$sources, lang)
  figcap <- if (nzchar(cap) || !is.null(sl)) tags$figcaption(class = "sm-caption", HTML(cap),
    if (nzchar(cap) && !is.null(sl)) tags$br(), if (!is.null(sl)) tags$span(class = "sm-source", HTML(sl)))
  w <- WIDTH[[b$width %||% "wide"]]
  if (identical(b$provider, "file")) {
    tracks <- lapply(names(b$subtitles %||% list()), function(l) tags$track(kind = "subtitles", srclang = l, label = toupper(l),
      src = rel(b$subtitles[[l]]), default = if (l == lang) NA))
    attrs <- list(controls = NA, preload = "metadata", src = rel(b$file), `aria-label` = tx(b$alt, lang, o),
                  poster = if (nzchar(b$poster %||% "")) rel(b$poster))
    if (isTRUE(b$autoplayLoop)) attrs <- c(attrs, list(autoplay = NA, muted = NA, loop = NA, playsinline = NA))
    return(tags$figure(class = paste0("sm-block sm-", w, " sm-video"), do.call(tags$video, c(attrs, tracks)), figcap))
  }
  name <- c(youtube = "YouTube", vimeo = "Vimeo", switchtube = "SWITCHtube")[[b$provider %||% "youtube"]]
  tags$figure(class = paste0("sm-block sm-", w, " sm-video"),
    tags$div(class = "sm-video-box", style = if (nzchar(b$poster %||% "")) sprintf("background-image:url('%s')", rel(b$poster)),
      tags$button(type = "button", class = "sm-video-play", `data-embed` = video_embed(b$provider, b$url),
        `aria-label` = paste(sprintf(term(lang, "video"), name), "–", tx(b$alt, lang, o)),
        tags$span(class = "sm-video-icon", `aria-hidden` = "true", HTML("&#9654;")),
        tags$span(class = "sm-video-note", sprintf(term(lang, "video"), name)))),
    figcap)
}

# ---- maps and tours (mapgl) ---------------------------------------------------
BASEMAP <- list(
  light = swisstopo_light, color = swisstopo_color,
  aerial = list(version = 8, glyphs = "https://vectortiles.geo.admin.ch/fonts/{fontstack}/{range}.pbf",
    sources = list(swissimage = list(type = "raster", tileSize = 256, attribution = "© swisstopo",
      tiles = list("https://wmts.geo.admin.ch/1.0.0/ch.swisstopo.swissimage/default/current/3857/{z}/{x}/{y}.jpeg"))),
    layers = list(list(id = "swissimage", type = "raster", source = "swissimage"))))

read_geo <- function(path) {
  if (is.null(.sb$geo)) .sb$geo <- list()
  if (!is.null(.sb$geo[[path]])) return(.sb$geo[[path]])
  g <- sf::st_read(path, quiet = TRUE)
  if (!is.na(sf::st_crs(g)) && sf::st_crs(g)$epsg %||% 4326 != 4326) g <- sf::st_transform(g, 4326)
  .sb$geo[[path]] <- g
  g
}
popup_col <- function(g, fields) {
  fields <- intersect(unlist(fields), names(g))
  if (!length(fields)) return(NULL)
  df <- sf::st_drop_geometry(g)
  apply(df[, fields, drop = FALSE], 1, function(r) paste(sprintf("<b>%s</b>: %s", esc(fields), esc(r)), collapse = "<br>"))
}

# map with all layers of a map/tour block; returns list(map, layer ids per studio layer, legend)
build_map <- function(b, lang, o, view) {
  v <- view %||% b$view %||% list(center = list(8.23, 46.8), zoom = 7)
  args <- list(style = BASEMAP[[b$basemap %||% "light"]], scrollZoom = isTRUE(b$scrollZoom), maxPitch = 85,
               pitch = v$pitch %||% 0, bearing = v$bearing %||% 0)
  if (!is.null(v$bounds)) args$bounds <- unlist(v$bounds) else { args$center <- unlist(v$center); args$zoom <- v$zoom %||% 7 }
  m <- do.call(maplibre, args) |> add_navigation_control(position = "top-right", visualize_pitch = TRUE)
  ids <- list(); leg_v <- character(); leg_c <- character()
  for (l in b$layers) {
    id <- paste0("L-", l$id); sub <- id
    nm <- tx(l$name, lang, o)
    if (l$type == "points") {
      pts <- Filter(function(p) is.numeric(p$lon) && is.numeric(p$lat), l$points)
      if (!length(pts)) next
      g <- sf::st_as_sf(data.frame(
        label = vapply(pts, function(p) tx(p$label, lang, o), ""),
        popup = vapply(pts, function(p) { t <- tx(p$popup, lang, o); if (nzchar(t)) sprintf("<b>%s</b><br>%s", esc(tx(p$label, lang, o)), esc(t)) else esc(tx(p$label, lang, o)) }, ""),
        color = vapply(pts, function(p) p$color %||% "#FC3B36", ""),
        lon = vapply(pts, function(p) p$lon, 0), lat = vapply(pts, function(p) p$lat, 0)),
        coords = c("lon", "lat"), crs = 4326)
      m <- m |> add_circle_layer(id = id, source = g, circle_color = get_column("color"), circle_radius = l$radius %||% 9,
        circle_stroke_color = "#ffffff", circle_stroke_width = 3, circle_pitch_alignment = "map",
        popup = "popup", tooltip = "label")
      if (isTRUE(l$labels)) {
        m <- m |> add_symbol_layer(id = paste0(id, "-lbl"), source = g, text_field = get_column("label"),
          text_font = c("Frutiger Neue Medium"), text_size = 14, text_color = "#1a1a1a",
          text_halo_color = "#ffffff", text_halo_width = 2, text_anchor = "left", text_offset = c(1.2, 0),
          text_allow_overlap = TRUE)
        sub <- c(sub, paste0(id, "-lbl"))
      }
      for (i in seq_len(nrow(g))) if (!(g$label[i] %in% leg_v)) { leg_v <- c(leg_v, g$label[i]); leg_c <- c(leg_c, g$color[i]) }
    } else if (l$type == "line") {
      if (length(l$coords) < 2) next
      xy <- do.call(rbind, lapply(l$coords, function(p) unlist(p)))
      g <- sf::st_sf(label = tx(l$label, lang, o), geometry = sf::st_sfc(sf::st_linestring(xy), crs = 4326))
      m <- m |> add_line_layer(id = id, source = g, line_color = l$color %||% "#333333", line_width = l$width %||% 3,
        line_dasharray = if (isTRUE(l$dashed)) c(2, 1), tooltip = "label")
      if (nzchar(g$label)) {
        m <- m |> add_symbol_layer(id = paste0(id, "-lbl"), source = g, text_field = get_column("label"),
          text_font = c("Frutiger Neue Medium"), text_size = 13, text_color = "#333333",
          text_halo_color = "#ffffff", text_halo_width = 2, symbol_placement = "line-center",
          text_keep_upright = TRUE, text_offset = c(0, -0.9))
        sub <- c(sub, paste0(id, "-lbl"))
      }
    } else if (l$type == "geojson") {
      if (!nzchar(l$file %||% "") || !file.exists(l$file)) { warning("GeoJSON fehlt: ", l$file); next }
      g <- read_geo(l$file)
      pc <- popup_col(g, l$popupFields); if (!is.null(pc)) g$popup <- pc
      pop <- if (!is.null(pc)) "popup"
      if (identical(l$geom, "fill")) {
        m <- m |> add_fill_layer(id = id, source = g, fill_color = l$fillColor, fill_opacity = l$fillOpacity, popup = pop) |>
          add_line_layer(id = paste0(id, "-line"), source = g, line_color = l$lineColor, line_width = l$lineWidth)
        sub <- c(sub, paste0(id, "-line")); col <- l$fillColor
      } else if (identical(l$geom, "line")) {
        m <- m |> add_line_layer(id = id, source = g, line_color = l$lineColor, line_width = l$lineWidth, popup = pop); col <- l$lineColor
      } else {
        m <- m |> add_circle_layer(id = id, source = g, circle_color = l$circleColor, circle_radius = l$circleRadius,
          circle_stroke_color = "#ffffff", circle_stroke_width = 1, popup = pop); col <- l$circleColor
      }
      if (nzchar(l$labelField %||% "") && l$labelField %in% names(g)) {
        m <- m |> add_symbol_layer(id = paste0(id, "-lbl"), source = g, text_field = get_column(l$labelField),
          text_font = c("Frutiger Neue Medium"), text_size = 13, text_color = "#1a1a1a", text_halo_color = "#ffffff", text_halo_width = 2)
        sub <- c(sub, paste0(id, "-lbl"))
      }
      leg_v <- c(leg_v, nm); leg_c <- c(leg_c, col)
    } else if (l$type == "mapgl") {
      r <- add_mapgl_layer(m, l, id); m <- r$map
      if (!is.null(r$color)) { leg_v <- c(leg_v, nm); leg_c <- c(leg_c, r$color) }
    }
    ids[[l$id]] <- sub
  }
  if (isTRUE(b$legend) && length(leg_v))
    m <- m |> add_categorical_legend(legend_title = "", values = leg_v, colors = leg_c, position = "bottom-left")
  attr(m, "layer_ids") <- ids
  m
}

add_mapgl_layer <- function(m, l, id) {
  s <- l$source; p <- l$paint; lt <- l$layerType
  src <- id; data <- NULL
  if (identical(s$kind, "geojson")) {
    path <- if (nzchar(s$url %||% "")) s$url else s$file
    data <- tryCatch(if (grepl("^https?:", path)) sf::st_read(path, quiet = TRUE) else read_geo(path),
                     error = function(e) { warning("mapgl-Ebene: Daten nicht lesbar: ", path); NULL })
    if (is.null(data)) return(list(map = m))
    pc <- popup_col(data, l$popupFields); if (!is.null(pc)) data$popup <- pc
    src <- data
  } else if (identical(s$kind, "vector")) {
    m <- m |> add_vector_source(id = id, tiles = s$tiles)
  } else {
    tiles <- if (identical(s$kind, "geoadmin")) sprintf("https://wmts.geo.admin.ch/1.0.0/%s/default/current/3857/{z}/{x}/{y}.png", s$geoadmin) else s$tiles
    m <- m |> add_raster_source(id = id, tiles = tiles, tileSize = s$tileSize %||% 256)
  }
  color <- p$color %||% "#1B84CB"
  if (!identical(p$colorMode %||% "single", "single") && nzchar(p$colorField %||% "")) {
    pairs <- strsplit(trimws(strsplit(p$colorStops %||% "", ",")[[1]]), ":")
    pairs <- Filter(function(x) length(x) == 2, lapply(pairs, trimws))
    if (length(pairs)) {
      vals <- vapply(pairs, `[`, "", 1); cols <- vapply(pairs, `[`, "", 2)
      color <- if (identical(p$colorMode, "ramp")) interpolate(column = p$colorField, values = as.numeric(vals), stops = cols)
               else match_expr(column = p$colorField, values = vals, stops = cols, default = p$color %||% "#cccccc")
    }
  }
  common <- list(map = m, id = paste0(id, "-layer"), source = src, min_zoom = l$minzoom %||% 0, max_zoom = l$maxzoom %||% 24)
  if (identical(s$kind, "vector") && nzchar(s$sourceLayer %||% "")) common$source_layer <- s$sourceLayer
  if (nzchar(l$filter %||% "")) common$filter <- jsonlite::fromJSON(l$filter, simplifyVector = FALSE)
  if (!is.null(data) && "popup" %in% names(data)) common$popup <- "popup"
  num <- function(x, d) as.numeric(x %||% d)
  m <- switch(lt,
    circle = do.call(add_circle_layer, c(common, list(circle_color = color, circle_radius = num(p$radius, 6), circle_opacity = num(p$opacity, 0.8),
      circle_stroke_color = p$strokeColor %||% "#ffffff", circle_stroke_width = num(p$strokeWidth, 1)))),
    line = do.call(add_line_layer, c(common, list(line_color = color, line_width = num(p$width, 2), line_opacity = num(p$opacity, 1)))),
    fill = do.call(add_fill_layer, c(common, list(fill_color = color, fill_opacity = num(p$opacity, 0.6), fill_outline_color = p$strokeColor %||% "#ffffff"))),
    `fill-extrusion` = do.call(add_fill_extrusion_layer, c(common, list(fill_extrusion_color = color, fill_extrusion_opacity = num(p$opacity, 0.8),
      fill_extrusion_height = if (nzchar(p$heightField %||% "")) get_column(p$heightField) else num(p$height, 100)))),
    heatmap = do.call(add_heatmap_layer, c(common, list(heatmap_radius = num(p$heatRadius, 20), heatmap_intensity = num(p$heatIntensity, 1),
      heatmap_opacity = num(p$opacity, 0.8)))),
    symbol = do.call(add_symbol_layer, c(common, list(text_field = get_column(p$textField %||% "name"), text_size = num(p$textSize, 13),
      text_color = p$color %||% "#1a1a1a", text_halo_color = "#ffffff", text_halo_width = 2, text_font = c("Frutiger Neue Medium")))),
    raster = do.call(add_raster_layer, c(common, list(raster_opacity = num(p$rasterOpacity, 0.7)))),
    m)
  list(map = m, color = if (lt %in% c("circle", "line", "fill", "fill-extrusion")) (if (is.character(color)) color else p$color))
}

tour_tag <- function(b, lang, o) {
  if (!length(b$slides)) return(NULL)
  first <- b$slides[[1]]$view
  m <- build_map(b, lang, o, first)
  ids <- attr(m, "layer_ids")
  all_ids <- unlist(ids, use.names = FALSE)
  slides <- lapply(b$slides, function(s) {
    so <- s$origLang %||% o
    show_ids <- if (length(s$show)) unlist(ids[unlist(s$show)], use.names = FALSE) else all_ids
    v <- s$view %||% list()
    imgs <- lapply(s$images, function(im) sm_slide_image(rel(im$src), caption = esc(tx(im$caption, lang, so)) %||% NULL,
      alt = tx(im$alt, lang, so)))
    args <- c(list(title = ptext(s$title, lang, so)), list(rich(s$html, lang, so)), imgs,
      list(show = show_ids, hide = setdiff(all_ids, show_ids), pitch = v$pitch %||% 0, bearing = v$bearing %||% 0))
    if (!is.null(v$bounds)) args$bounds <- unlist(v$bounds) else { args$center <- unlist(v$center); args$zoom <- v$zoom }
    do.call(sm_slide, args)
  })
  do.call(sm_sidecar, c(list(m), slides, list(panel = b$panel %||% "left",
    terrain = if (isTRUE(b$terrain)) "swisstopo", exaggeration = b$exaggeration %||% 1.3,
    narrative_width = paste0(b$narrativeWidth %||% 36, "%"))))
}

# ---- one page -----------------------------------------------------------------
page_tags <- function(story, lang, orig) {
  s <- story$settings
  ctx <- new.env(); ctx$tables <- 0; ctx$table_rows <- list()
  .sb$cites <- list()
  langs <- s$languages
  lang_nav <- if (length(langs) > 1) tags$nav(class = "sm-lang", `aria-label` = "Sprache / Langue",
    lapply(langs, function(l) tags$a(href = paste0("../", l$path, "/"), lang = l$code, hreflang = l$code,
      `aria-current` = if (l$code == lang) "true", toupper(l$code))))
  brand <- if (nzchar(s$logo %||% "")) tags$img(src = rel(s$logo), alt = s$logoAlt %||% "", class = "sm-logo") else
    tags$span(class = "sm-logo-text", s$logoAlt %||% "")
  if (nzchar(s$logoHref %||% "")) brand <- tags$a(href = s$logoHref, target = "_blank", rel = "noopener", brand)
  cv <- story$cover
  nav <- vapply(story$chapters, function(ch) tx(ch$nav, lang, orig), "")
  names(nav) <- vapply(story$chapters, function(ch) ch$id, "")
  nav <- c(nav, quellen = term(lang, "refs"))
  sections <- lapply(story$chapters, function(ch) {
    tags$div(id = ch$id, class = "section level2",
      tags$h2(ptext(ch$title, lang, orig)),
      lapply(ch$blocks, function(b) block_tag(b, ch, story, lang, orig, ctx)))
  })
  rows <- nachweis_rows(story, lang, orig)
  table_rows <- ctx$table_rows
  refs_ph <- "[[REFERENCES]]"
  tagList(
    tags$header(class = "sm-header", brand, lang_nav),
    sm_cover(title = ptext(cv$title, lang, orig), summary = if (nzchar(tx(cv$subtitle, lang, orig))) ptext(cv$subtitle, lang, orig),
             byline = cv$byline %||% NULL, image = rel(cv$image) %||% "../assets/cover.svg", image_alt = tx(cv$alt, lang, orig),
             credit = if (nzchar(tx(cv$creditLabel, lang, orig))) tx(cv$creditLabel, lang, orig)),
    sm_nav(nav),
    tags$div(class = "main-container", sections,
      tags$div(id = "quellen", class = "section level2",
        tags$h2(term(lang, "refs")),
        HTML(if (!is.null(.sb$refs_html)) .sb$refs_html else refs_ph),
        if (length(rows)) tagList(tags$h3(term(lang, "images")),
          sm_credits(lapply(rows, function(r) sm_credit(esc(r$label), esc(r$credit))))),
        if (length(table_rows)) tagList(tags$h3(term(lang, "tables")),
          sm_credits(lapply(table_rows, function(t) sm_credit(esc(sprintf("%s %d: %s", term(lang, "table"), t$n, t$title)),
            src_line(t$sources, lang, term(lang, "data")) %||% "")))))),
    if (nzchar(tx(s$footer, lang, orig))) sm_footer(esc(tx(s$footer, lang, orig)))
  )
}

head_tags <- function(story, lang, orig) {
  tags$head(
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    tags$title(tx(story$cover$title, lang, orig)),
    tags$meta(name = "description", content = tx(story$cover$subtitle, lang, orig)),
    lapply(story$settings$languages, function(l) tags$link(rel = "alternate", hreflang = l$code, href = paste0("../", l$path, "/"))),
    tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
    tags$link(rel = "preconnect", href = "https://fonts.gstatic.com", crossorigin = NA),
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Noto+Serif:ital,wght@0,400;0,700;1,400&family=Nunito+Sans:wght@400;500;600;700&display=swap"),
    tags$link(rel = "stylesheet", href = "../assets/storymap.css"),
    tags$script(src = "../assets/storymap.js")
  )
}

# ---- custom live charts --------------------------------------------------------
build_custom_charts <- function(story, site) {
  for (ch in story$chapters) for (b in ch$blocks) {
    if (b$type != "chart" || nzchar(b$template %||% "")) next
    f <- file.path(site, "charts", paste0("chart-", b$id, ".html"))
    tryCatch({
      p <- studio_custom_chart(b, snapshot_dir = "data/snapshot", orig = story$settings$originalLanguage %||% "de")
      url <- b$custom$url %||% ""
      bfe_chart_page(p$chart, f, title = tx(b$title, story$settings$originalLanguage %||% "de", "de"), updated = p$updated,
                     source_name = if (grepl("^https?:", url)) sub("^https?://(www\\.)?([^/]+).*", "\\2", url) else "eigene Daten",
                     source_url = if (grepl("^https?:", url)) sub("^(https?://[^/]+).*", "\\1", url) else "")
    }, error = function(e) {
      warning("Eigene Grafik ", b$id, " nicht erstellt: ", conditionMessage(e))
      writeLines(sprintf('<!doctype html><meta charset="utf-8"><p style="font-family:sans-serif">Grafik zurzeit nicht verfügbar.</p>'), f)
    })
  }
}

# ---- everything ---------------------------------------------------------------
studio_build <- function(story_file = "content/story.json", bib_file = "content/quellen.bib",
                         site = "_site", today = Sys.Date()) {
  story <- jsonlite::read_json(story_file, simplifyVector = FALSE)
  bib <- if (file.exists(bib_file)) paste(readLines(bib_file, encoding = "UTF-8", warn = FALSE), collapse = "\n") else ""
  s <- story$settings
  orig <- s$originalLanguage %||% s$languages[[1]]$code
  dir.create(file.path(site, "charts"), recursive = TRUE, showWarnings = FALSE)
  build_custom_charts(story, site)

  # pass 1: collect citations of every page in reading order
  jobs <- list()
  for (l in s$languages) {
    .sb$formatted <- NULL; .sb$refs_html <- NULL
    invisible(renderTags(page_tags(story, l$code, orig)))
    jobs[[l$code]] <- list(citations = .sb$cites)
  }
  res <- run_citations(bib, orig, jobs, today)
  for (w in res$warnings) warning(w)

  # pass 2: pages with formatted citations
  for (l in s$languages) {
    r <- res[[l$code]]
    .sb$formatted <- r$cites
    .sb$refs_html <- paste(vapply(r$refs, function(x) sprintf('<p id="ref-%s" class="sm-ref" lang="%s">%s</p>', x$key, x$lang, x$html), ""), collapse = "\n")
    dir <- file.path(site, l$path); dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    file <- file.path(dir, "index.html")
    save_html(tagList(head_tags(story, l$code, orig), page_tags(story, l$code, orig)), file = file, libdir = "libs", lang = l$code)
    # one shared libs/ folder for all languages
    dir.create(file.path(site, "libs"), showWarnings = FALSE)
    for (d in list.dirs(file.path(dir, "libs"), recursive = FALSE)) file.copy(d, file.path(site, "libs"), recursive = TRUE)
    unlink(file.path(dir, "libs"), recursive = TRUE)
    h <- readLines(file, encoding = "UTF-8", warn = FALSE)
    h <- gsub('(src|href)="libs/', '\\1="../libs/', h)
    if (l$code == "fr" && isTRUE(s$decimalCommaFr)) h <- sub('<html lang="fr">', '<html lang="fr" data-decimal="comma">', h, fixed = TRUE)
    writeLines(h, file, useBytes = TRUE)
    .sb$formatted <- NULL
  }

  # root: forward to a language (browser language or fixed)
  codes <- vapply(s$languages, function(l) l$code, ""); paths <- vapply(s$languages, function(l) l$path, "")
  fixed <- if (!identical(s$rootRedirect, "browser")) paths[match(s$rootRedirect, codes)] else NA
  default <- if (!is.na(fixed)) fixed else paths[1]
  map_js <- paste(sprintf('"%s":"%s"', codes, paths), collapse = ",")
  writeLines(c("<!doctype html>", '<html><head><meta charset="utf-8">',
    sprintf("<title>%s</title>", esc(tx(story$cover$title, orig, orig))),
    sprintf('<meta http-equiv="refresh" content="2; url=%s/">', default),
    sprintf('<link rel="canonical" href="%s/">', default),
    "<script>(function(){var m={", map_js, "};",
    if (is.na(fixed)) "var l=((navigator.languages&&navigator.languages[0])||navigator.language||'').slice(0,2).toLowerCase();" else "var l='';",
    sprintf("var p=m[l]||'%s';location.replace(p+'/'+location.hash);})();</script>", default),
    "</head><body>",
    paste(vapply(s$languages, function(l) sprintf('<p><a href="%s/">%s</a></p>', l$path, esc(l$label)), ""), collapse = ""),
    "</body></html>"), file.path(site, "index.html"), useBytes = TRUE)
  if (nzchar(s$domain %||% "")) writeLines(sub("^https?://", "", sub("/$", "", s$domain)), file.path(site, "CNAME"))
  invisible(TRUE)
}
