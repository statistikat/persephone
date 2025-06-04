#' Spectral Plots
#'
#' Interactive plot of the periodogram or the autogregressive spectrum.
#' Produces a `ggplot`/`plotly` object for objects of class [persephone].
#'
#' @param x an object of class [persephone].
#' @param tsType character (`"original"`,`"sa"`,`"irregular"`,`"residuals"`)
#' selecting the preferred type of time series to plot. A numeric
#' value (`1:4`) corresponding to one of these characters is also accepted.
#' @param plotType character (`"arSpec"`,`"arSpecBars"`,`"periodogram"`)
#' selecting the preferred type of plot, see Details. A numeric
#' value (`1:3`) corresponding to one of these characters is also accepted.
#' @param maxobs maximum number of observations of the time series. If NULL,
#' `maxobs` equals the length of the time series for plotTypes
#' `"arSpec"` and `"periodogram"` and `maxobs=96` for plotType `"arSpecBars"`.
#' @param n.freq the number of frequencies, i.e. the number of points at which
#'   to plot. If NULL, `n.freq=301` for plotTypes `"arSpec"` and `"periodogram"`
#'   and `n.freq=61` for plotType `"arSpecBars"`.
#' @param order order of the AR model
#' @param main plot title
#' @param interactive If the return value would be a `ggplot` object, wrap it
#'   in [plotly::ggplotly] before returning.
#' @param ... other plotting parameters to affect the plot. Not currently used.
#'
#' @details
#'
#' The following options are available for the parameter `plotType`.
#'
#' * `arSpec`: The autoregressive spectrum similar to JD+ plots. The default
#'   settings for the number of frequencies, i.e. the number of points at which
#'   to plot, the order of the AR model to be fitted and the maximum number of
#'   observations of the time series are n.freq = 301, order = 30 and
#'   maxobs='length of the time series' in this case.
#' * `arSpecBars`: The autoregressive spectrum similar to X-13ARIMA-SEATS plots.
#'   The default settings for the number of frequencies, the order of the AR
#'   model to be fitted and the maximum number of observations are n.freq = 61,
#'   order = 30 and maxobs=96 in this case.
#' * `periodogram`: The raw periodogram similar to JD+ plots. The default
#'   settings for the number of frequencies and the maximum number of
#'   observations are n.freq = 301 and maxobs='length of the time series' in
#'   this case.
#'
#' @return Returns an object of class `ggplot` or `plotly`. The seasonal and
#'   trading day frequencies are indicated in blue and red. For monthly data,
#'   the seasonal frequencies are 1/12, 2/12, ..., 6/12 and the trading day
#'   frequencies are 0.348 and 0.432. For quarterly data, the seasonal
#'   frequency is 1/4 and the trading day frequencies are 0.044375, 0.08875,
#'   0.294375, 0.33875 and 0.38125.
#'
#' @examples
#' # Monthly Data Example
#' data(AirPassengers, package = "datasets")
#' # Generate a persephone object, in this case an x13Single object
#' obj <- perX13(AirPassengers, "rsa5c")
#' obj$run()
#'
#' # Plot the AR-Spectrum after run
#' # Autoregressive Spectrum of the original series
#' # (similar to JD+ plots)
#' # where n.freq = 301 and order = 30
#' plotSpectrum(obj)
#'
#' # Autoregressive Spectrum of the original series
#' # (similar to X-13ARIMA-SEATS plots)
#' # where n.freq = 61 and order = 30
#' plotSpectrum(obj, plotType="arSpecBars")
#'
#' # Periodogram
#' plotSpectrum(obj, plotType="periodogram")
#' # same as
#' plotSpectrum(obj, plotType=3)
#'
#' # Quarterly Data Example
#' data(UKgas, package = "datasets")
#' # Generate a persephone object, in this case a tramoseats object
#' obj2 <- perTramo(UKgas, "rsafull")
#' obj2$run()
#' plotSpectrum(obj2, tsType="original")
#' plotSpectrum(obj2, tsType="sa")
#' plotSpectrum(obj2, tsType="irregular")
#' plotSpectrum(obj2, tsType="residuals")
#'
#' @importFrom stats window ts spec.ar spec.pgram
#'
#' @export
plotSpectrum_old <- function(x,
                         tsType = c("original", "sa", "irregular", "residuals"),
                         plotType = c("arSpec", "arSpecBars", "periodogram"),
                         maxobs = NULL, n.freq = NULL, order = 30,
                         main = NULL, interactive = TRUE, ...) {

  freq <- spec <- spec_new1 <- NULL # nolint

  if (is.null(x$output$user_defined)) {
    stop("No results from run available.\n")
  }

  if (is.numeric(plotType)) {
    plotType <- c("arSpec", "arSpecBars", "periodogram")[plotType]
  }else{
    plotType <- match.arg(plotType)
  }
  if (is.numeric(tsType)) {
    tsType <- c("original", "sa", "irregular", "residuals")[tsType]
  }else{
    tsType <- match.arg(tsType)
  }
  if (is.null(n.freq)) {
    if (plotType %in% c("arSpec", "periodogram")) {
      n.freq <- 301
    } else if (plotType == "arSpecBars") {
      n.freq <- 61
    }
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
    tsobj <- x$output$user_defined$residuals.tsres
    d1 <- tsobj
  }

  # Seasonal and trading day frequencies
  if (frequency(tsobj) == 4) {
    td_freq <- c(0.044375, 0.08875, 0.294375, 0.33875, 0.38125)
    seas_freq <- 1 / 4
    seas_freq_lab <- c("1/4")
    minobs <- 60
  } else if (frequency(tsobj) == 12) {
    td_freq <- c(0.348, 0.432)
    seas_freq <- c(1 / 12, 2 / 12, 3 / 12, 4 / 12, 5 / 12, 6 / 12)
    seas_freq_lab <- c("1/12", "2/12", "3/12", "4/12", "5/12", "6/12")
    minobs <- 80
  }

  # minobs error (minobs=60 for quartlery data, minobs=80 for monthly data)
  if (length(tsobj) <= minobs) {
    stop("The minimum number of observations needed to compute the spectrum",
         " is ", minobs, ".")
  }

  # maxobs
  # The default starting date for the spectral plots is set to be 96
  # observations (8 years of monthly data)
  # from the end of the series for X-13ARIMA-SEATS
  # maxobs <- length(tsobj) # JD+
  # maxobs <- 96 # X-13ARIMA-SEATS
  if (is.null(maxobs)) {
    if (plotType %in% c("arSpec", "periodogram")) {
      # Default settings in JD+
      maxobs <- length(tsobj)
    } else if (plotType == "arSpecBars") {
      # Default settings in X-13ARIMA-SEATS
      maxobs <- 96
    }
  }
  if (length(tsobj) > maxobs) {
    d1 <- window(d1, start = start(lag(
      ts(end = end(d1), frequency = frequency(d1)), maxobs - 2
    )))
  }

  if (plotType == "periodogram") {
    ## Periodogram
    if (is.null(main)) {
      main <- paste0("Periodogram of the ", tsType, " series")
    }
    d1s <- spec.pgram(d1, plot = FALSE)
    d1s <- data.frame(freq = d1s$freq / frequency(d1), spec = d1s$spec)

    p <- ggplot(d1s, aes(x = freq, y = spec)) +
      geom_line() + geom_vline(xintercept = td_freq, col = "red", alpha = 0.3) +
      geom_vline(xintercept = seas_freq, col = "blue", alpha = 0.3) +
      labs(title = main) +
      theme_minimal() +
      ylab("Spectrum") +
      xlab("Frequency")

  } else if (plotType == "arSpec") {

    # Autoregressive Spectrum JD+

    if (is.null(main)) {
      main <- paste0("Autoregressive spectrum of the ", tsType, " series")
    }

    # Default settings in JD+
    # d2s <- spec.ar(d1, n.freq = 301, order = 30, plot = FALSE)
    d2s <- spec.ar(d1, n.freq = n.freq, order = order, plot = FALSE)
    d2s <- data.frame(freq = d2s$freq / frequency(d1),
                      spec = 10 * log10(2 * d2s$spec))

    p <- ggplot(d2s, aes(x = freq, y = spec)) +
      geom_line() +
      geom_vline(xintercept = td_freq, col = "red", alpha = 0.3) +
      geom_vline(xintercept = seas_freq, col = "blue", alpha = 0.3) +
      labs(title = main) +
      theme_minimal() +
      ylab("Spectrum") +
      xlab("Frequency")

  } else if (plotType == "arSpecBars") {

    # Autoregressive Spectrum X-13ARIMA-SEATS

    if (is.null(main)) {
      main <- paste0("Autoregressive spectrum of the ", tsType, " series")
    }

    # Default settings in X-13ARIMA-SEATS
    # d3s <- spec.ar(d1, n.freq = 61, order = 30, plot = FALSE)
    # Not 100% sure what X-13ARIMA-SEATS really does but
    # 10 * log10(d3s$spec/sum(d3s$spec)
    # comes closest to the scale they use
    d3s <- spec.ar(d1, n.freq = n.freq, order = order, plot = FALSE)
    d3s <- data.frame(freq = d3s$freq / frequency(d1),
                      spec = 10 * log10(d3s$spec / sum(d3s$spec)))

    ## Quick-Fix: use closest value of d3s$freq to td_freq
    ## (spec.ar() only allows for a parameter "n.freq=The number of points at
    ## which to plot."
    ## but not for a vector of frequencies at which to plot)
    td_freq_approx <- sapply(td_freq, function(f) {
      d3s$freq[which(abs(d3s$freq - f) == min(abs(d3s$freq - f)))]
    })

    # Workaround to recreate X-13ARIMA-SEATS spectral plots (as far as possible)
    # where the scale of the y-axis
    # ranges from a low negative value (e.g. -40) to a higher negative value
    # (e.g. -15)
    d3s$spec_new1 <- scales::rescale(d3s$spec, from = range(d3s$spec),
                                     to = c(1, max(d3s$spec - min(d3s$spec))))
    #d3s$spec_new2 <- d3s$spec - min(d3s$spec)
    #d3s$spec_rescaled <-  scales::rescale(d3s$spec_new1, to = c(min(d3s$spec),
    #max(d3s$spec)))
    #d3s[order(d3s$spec_new2),]
    breaks_15 <- seq(1, max(d3s$spec_new1), length = 15)
    labels_15 <- as.character(round(
      scales::rescale(breaks_15, to = c(min(d3s$spec), max(d3s$spec))),
      digits = 0
    ))

    seas_td_col <- rep("grey", length(d3s$freq))
    seas_td_col[which(d3s$freq %in% seas_freq)] <- "blue"
    seas_td_col[which(d3s$freq %in% td_freq_approx)] <- "red"

    p <- ggplot(d3s,  aes(x = freq, y = abs(spec_new1))) +
      geom_bar(stat = "identity", position = "dodge",
               fill = seas_td_col, width = 0.003) +
      scale_x_continuous(
        breaks = c(seas_freq, td_freq_approx),
        labels = c(seas_freq_lab,
                   as.character(round(td_freq_approx, digits = 3)))
      ) +
      scale_y_continuous(breaks = breaks_15, labels = labels_15) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1,
                                       size = rel(0.9))) +
      ggtitle(main) +
      ylab("Spectrum") +
      xlab("Frequency")

  }

  p <-  p + theme_bw()

  if (interactive) {
    p <- plotly::ggplotly(p)
  }

  p
}
