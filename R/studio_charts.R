# -----------------------------------------------------------------------------
# studio_charts.R — "Live-Grafik" blocks with an own data source (Storymap-Studio)
#
# The data is fetched at every build (daily). Supported:
#   * CSV (comma or semicolon) with a header row
#   * JSON: an array of records, an object of equal-length columns, or the
#     Energy-Charts shape { unix_seconds: [...], production_types: [{ name, data }] }
#   * a file in the repository (path instead of URL)
# A secret (API key) is never stored in the story: write {SECRET} in the URL and
# enter the name of the GitHub secret in the studio; the build replaces it.
# The last successful download is kept in data/snapshot/ and used if the source fails.
# -----------------------------------------------------------------------------

studio_fetch <- function(b, snapshot_dir = "data/snapshot") {
  cu <- b$custom
  url <- cu$url %||% ""
  if (!nzchar(url)) stop("keine Datenquelle")
  dir.create(snapshot_dir, showWarnings = FALSE, recursive = TRUE)
  snap <- file.path(snapshot_dir, paste0("chart-", b$id, ".dat"))
  if (!grepl("^https?:", url)) return(list(text = paste(readLines(url, encoding = "UTF-8", warn = FALSE), collapse = "\n"), fresh = TRUE))
  if (nzchar(cu$secret %||% "")) url <- gsub("{SECRET}", Sys.getenv(cu$secret), url, fixed = TRUE)
  tmp <- tempfile()
  ok <- tryCatch({
    utils::download.file(url, tmp, quiet = TRUE, mode = "wb", method = "libcurl",
                         headers = c(`User-Agent` = "Storymap-Studio build (GitHub Actions)"))
    txt <- paste(readLines(tmp, encoding = "UTF-8", warn = FALSE), collapse = "\n")
    studio_parse(txt)                        # validate before replacing the snapshot
    file.copy(tmp, snap, overwrite = TRUE)
    TRUE
  }, error = function(e) { message("Datenquelle nicht erreichbar (", conditionMessage(e), ") – verwende Kopie"); FALSE })
  if (!file.exists(snap)) stop("keine Daten und keine gespeicherte Kopie")
  list(text = paste(readLines(snap, encoding = "UTF-8", warn = FALSE), collapse = "\n"), fresh = ok)
}

# -> data.frame
studio_parse <- function(txt) {
  t <- trimws(txt)
  if (substr(t, 1, 1) %in% c("{", "[")) {
    j <- jsonlite::fromJSON(t, simplifyVector = TRUE)
    if (is.data.frame(j)) return(j)
    if (is.list(j) && !is.null(j$production_types) && is.data.frame(j$production_types)) {   # Energy-Charts
      xkey <- intersect(c("unix_seconds", "x", "time", "date"), names(j))[1]
      df <- data.frame(x = j[[xkey]])
      names(df) <- xkey
      for (i in seq_len(nrow(j$production_types))) df[[j$production_types$name[i]]] <- unlist(j$production_types$data[[i]])
      return(df)
    }
    cols <- Filter(function(v) is.atomic(v) && length(v) > 1, j)
    if (length(cols)) { n <- min(lengths(cols)); return(as.data.frame(lapply(cols, `[`, seq_len(n)), check.names = FALSE)) }
    stop("JSON-Form nicht erkannt")
  }
  sep <- if (grepl(";", strsplit(t, "\n")[[1]][1])) ";" else ","
  utils::read.csv(text = t, sep = sep, check.names = FALSE, stringsAsFactors = FALSE)
}

studio_x <- function(x) {
  if (is.numeric(x) && median(x, na.rm = TRUE) > 1e9) return(as.POSIXct(x, origin = "1970-01-01", tz = "Europe/Zurich"))
  if (is.numeric(x)) return(x)
  d <- suppressWarnings(as.POSIXct(x, tz = "Europe/Zurich", tryFormats = c("%Y-%m-%dT%H:%M:%OS", "%Y-%m-%d %H:%M:%OS", "%Y-%m-%d", "%d.%m.%Y")))
  if (all(is.na(d))) x else d
}

studio_custom_chart <- function(b, snapshot_dir = "data/snapshot", orig = "de", height = 460) {
  cu <- b$custom
  f <- studio_fetch(b, snapshot_dir)
  df <- studio_parse(f$text)
  xcol <- cu$x %||% names(df)[1]
  if (!xcol %in% names(df)) stop("Spalte für die Zeitachse fehlt: ", xcol)
  x <- studio_x(df[[xcol]])
  series <- Filter(function(s) (s$col %||% "") %in% names(df), cu$series)
  if (!length(series)) stop("keine der angegebenen Reihen gefunden")
  d <- data.frame(x = x)
  for (s in series) d[[s$col]] <- suppressWarnings(as.numeric(df[[s$col]]))
  agg <- cu$aggregate %||% "none"
  if (agg != "none" && inherits(d$x, c("POSIXct", "Date"))) {
    key <- switch(agg, day = as.Date(d$x, tz = "Europe/Zurich"),
                  week = as.Date(cut(as.Date(d$x, tz = "Europe/Zurich"), "week")),
                  month = as.Date(cut(as.Date(d$x, tz = "Europe/Zurich"), "month")))
    fun <- if (identical(cu$aggFun, "mean")) mean else sum
    d <- stats::aggregate(d[-1], by = list(x = key), FUN = function(v) fun(v, na.rm = TRUE))
  }
  kind <- cu$kind %||% "line"
  unit <- cu$unit %||% ""
  p <- plotly::plot_ly(height = height)
  for (s in series) {
    nm <- tx(s$label, orig, orig) %||% s$col
    col <- s$color %||% "#1B84CB"
    ht <- sprintf("%s: %%{y:,.1f} %s<extra></extra>", nm, unit)
    p <- switch(kind,
      area = plotly::add_trace(p, x = d$x, y = d[[s$col]], name = nm, type = "scatter", mode = "lines", stackgroup = "one",
                               line = list(width = 0.5, color = col), fillcolor = col, hovertemplate = ht),
      bar = plotly::add_trace(p, x = d$x, y = d[[s$col]], name = nm, type = "bar", marker = list(color = col), hovertemplate = ht),
      plotly::add_trace(p, x = d$x, y = d[[s$col]], name = nm, type = "scatter", mode = "lines", line = list(width = 2, color = col), hovertemplate = ht))
  }
  xa <- bfe_xaxis()
  if (isTRUE(cu$rangeButtons) && inherits(d$x, c("POSIXct", "Date"))) {
    xa$rangeselector <- list(buttons = list(
      list(count = 1, label = "1 Jahr", step = "year", stepmode = "backward"),
      list(count = 5, label = "5 Jahre", step = "year", stepmode = "backward"),
      list(step = "all", label = "Gesamt")))
  }
  p <- bfe_layout(p, xaxis = xa, yaxis = bfe_yaxis(ticksuffix = if (nzchar(unit)) paste0(" ", unit) else ""),
                  barmode = if (kind == "bar") "stack" else NULL, margin = list(l = 60, r = 10, t = 40, b = 40))
  list(chart = p, updated = if (inherits(d$x, c("POSIXct", "Date"))) format(max(as.Date(d$x), na.rm = TRUE)) else format(Sys.Date()))
}
