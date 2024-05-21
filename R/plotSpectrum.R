#' Spectral Plots
#' @param x an object of class [persephone].
#' @param tsType character (`"original"`,`"sa"`,`"irregular"`,`"residuals"`)
#' selecting the preferred type of time series to plot. A numeric
#' value (`1:4`) corresponding to one of these characters is also accepted.
#' @param plotType character (`"arSpec"`,`"arSpecBars"`,`"periodogram"`)
#' selecting the preferred type of plot, see Details. A numeric
#' value (`1:3`) corresponding to one of these characters is also accepted.
#' @param maxobs maximum number of observations of the time series. If `NULL`,
#' `maxobs` equals the length of the time series for plotTypes
#' `"arSpec"` and `"periodogram"` and `maxobs=96` for plotType `"arSpecBars"`.
#' @param main plot title
#' @param n.freq the number of frequencies, i.e. the number of points at which
#' to plot. If `NULL`, `n.freq=301` for plotTypes `"arSpec"` and `"periodogram"`
#' and `n.freq=61` for plotType `"arSpecBars"`.
#' @param order order of the AR model.
#' @examples
#' x <- perX13(AirPassengers)
#' x$run()
#' plotSpectrum(x)
#' plotSpectrum(x, plotType = "periodogram")
#' plotSpectrum(x, tsType = "irregular")
#' @export
plotSpectrum <- function(x,
                         tsType = c("original", "sa", "irregular", "residuals"),
                         plotType = c("arSpec", "arSpecBars", "periodogram"),
                         maxobs = NULL, main = NULL, n.freq = NULL, order = 30) {
  stopifnot(inherits(x, "persephone"))
  series <- plotSpectrum_series(x, tsType)
  freq <- switch(
    as.character(stats::frequency(series$ts)),
    `4` = list(td = c(0.044375, 0.08875, 0.294375, 0.33875, 0.38125),
               seas = 1 / 4,
               seas_lab = c("1/4"),
               minobs = 60),
    `12` = list(
      td = c(0.348, 0.432),
      seas =  c(1 / 12, 2 / 12, 3 / 12, 4 / 12, 5 / 12, 6 / 12),
      seas_lab = c("1/12", "2/12", "3/12", "4/12", "5/12", "6/12"),
      minobs = 80
    ),
    stop("invalid frequency")
  )

  if (length(series$ts) <= freq$minobs) {
    stop("The minimum number of observations needed to compute the spectrum",
         " is ", freq$minobs, ".")
  }
  plotType <- match.arg(plotType)
  if (is.null(maxobs)) {
    if (plotType %in% c("arSpec", "periodogram")) {
      # Default settings in JD+
      maxobs <- length(series$ts)
    } else if (plotType == "arSpecBars") {
      # Default settings in X-13ARIMA-SEATS
      maxobs <- 96
    } else {
      maxobs <- length(series$ts)
    }
  }
  if (length(series$ts) > maxobs) {
    series$d <- stats::window(series$d, start = stats::start(stats::lag(
      stats::ts(end = stats::end(series$d),
                frequency = stats::frequency(series$d)), maxobs - 2
    )))
  }
  plot_args <- list(series = series, maxobs = maxobs, freq = freq, main = main,
                    n.freq = n.freq, order = order)
  switch(
    match.arg(plotType),
    periodogram = plotSpectrum_periodogram(plot_args),
    arSpec = plotSpectrum_arSpec(plot_args),
    arSpecBars = plotSpectrum_arSpecBars(plot_args),
    stop("unsupported plot type")
  )
}

plotSpectrum_periodogram <- function(args) {
  main <- args$main %||%
    paste0("Periodogram of the ", args$series$type, " series")
  d1s <- stats::spec.pgram(args$series$d, plot = FALSE)
  d1s <- data.frame(freq = d1s$freq / stats::frequency(args$series$d),
                    spec = d1s$spec)
  create_spectral_plot(x = d1s$freq, y = d1s$spec, title = main) %>%
    highcharter::hc_xAxis(plotLines = c(
      lapply(args$freq$seas, function(x) { list(value = x) }),
      lapply(args$freq$td, function(x) { list(value = x, color = "#ff000044") }))
    )
}


plotSpectrum_arSpec <- function(args) {
  order <- args$order
  main <- args$main %||%
    paste0("Autoregressive spectrum of the ", args$series$type, " series")
  n.freq <- args$n.freq %||% length(args$series$ts)
  d2s <- stats::spec.ar(args$series$d, n.freq = n.freq, order = order, plot = FALSE)
  d2s <- data.frame(freq = d2s$freq / stats::frequency(args$series$d),
                    spec = 10 * log10(2 * d2s$spec))
  d1s <- d2s

  create_spectral_plot(x = d1s$freq, y = d1s$spec, title = main) %>%
    highcharter::hc_xAxis(plotLines = c(
      lapply(args$freq$seas, function(x) { list(value = x) }),
      lapply(args$freq$td, function(x) { list(value = x, color = "#ff000044") }))
    )
}

plotSpectrum_arSpecBars <- function(args) {
  main <- args$main %||%
    paste0("Autoregressive spectrum of the ", args$series$type, " series")
  n.freq <- args$n.freq %||% 61
  order <- args$order
  d1 <- args$series$d
  d3s <- stats::spec.ar(d1, n.freq = n.freq, order = order, plot = FALSE)
  d3s <- data.frame(freq = d3s$freq / stats::frequency(d1),
                    spec = 10 * log10(d3s$spec / sum(d3s$spec)))
  td_freq_approx <- sapply(args$freq$td, function(f) {
    d3s$freq[which(abs(d3s$freq - f) == min(abs(d3s$freq - f)))]
  })
  td_freq_approx
  d3s$spec_new1 <- rescale(d3s$spec, 1, max(d3s$spec - min(d3s$spec)))
  breaks_15 <- seq(1, max(d3s$spec_new1), length = 15)
  labels_15 <- as.character(round(
    rescale(breaks_15, min(d3s$spec), max(d3s$spec)),
    digits = 0
  ))
  breaks_15
  seas_td_col <- rep("grey", length(d3s$freq))
  seas_td_col[which(d3s$freq %in% args$freq$td)] <- "blue"
  seas_td_col[which(d3s$freq %in% td_freq_approx)] <- "red"
  seas_td_col
  d3s$color <- seas_td_col
  d3s
  highcharter::highchart() %>%
    highcharter::hc_add_series(data.frame(x = d3s$freq, y = abs(d3s$spec_new1),
                                          color = d3s$color), type = "column",
                               showInLegend = FALSE, findNearestPointBy = "x",
                               name = "AR spectrum") %>%
    highcharter::hc_xAxis(crosshair = list(width = 20, color = "#eee")) %>%
    highcharter::hc_title(text = main, align = "left") %>%
    highcharter::hc_yAxis(title = list(text = "Spectrum")) %>%
    highcharter::hc_xAxis(title = list(text = "Frequency"))
}

create_spectral_plot <- function(x, y, title = "spectral Plot", type = "line", name = "spectrum") {
  highcharter::highchart() %>%
    highcharter::hc_add_series(data.frame(x = x, y = y), showInLegend = FALSE,
                               type = type, name = name) %>%
    highcharter::hc_xAxis(title = list(text = "Frequency")) %>%
    highcharter::hc_yAxis(title = list(text = "Spectrum")) %>%
    highcharter::hc_title(text = title, align = "left")
}

plotSpectrum_series <- function(x, tsType = c("original", "sa", "irregular", "residuals")) {
  if (is.numeric(tsType)) {
    tsType <- c("original", "sa", "irregular", "residuals")[tsType]
  } else {
    tsType <- match.arg(tsType)
  }

  if (tsType == "original") {
    tsobj <- x$ts
    # First differences -> stationary ts
    d1 <- diff(tsobj, 1)
  } else if (tsType == "sa") {
    tsobj <- x$output$user_defined$sa
    # First differences -> stationary ts
    d1 <- diff(tsobj, 1)
  } else if (tsType == "irregular") {
    tsobj <- x$output$user_defined$i
    d1 <- tsobj
  } else if (tsType == "residuals") {
    tsobj <- x$output$regarima$residuals
    d1 <- tsobj
  }
  list(ts = tsobj, d = d1, type = tsType)
}
