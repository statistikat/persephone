#' Several interactive plots in connection with residuals for a persephone
#' object
#'
#' Produces either a dygraph (see the
#' [online documentation](https://rstudio.github.io/dygraphs/) for more detail)
#' or a `ggplot`/`plotly` object for objects of class [persephone].
#'
#' @param x an object of class [persephone].
#' @param which character (`"res"`,`"acf"`,`"acf2"`, `"pacf"`,`"sreshist"`,
#'   `"nqq"`) selecting the preferred type of plot, see Details. A numeric
#'   value (`1:6`) corresponding to one of these characters is also accepted.
#'
#' @param main plot title
#' @param interactive If the return value would be a `ggplot` object, wrap it in
#'   [plotly::ggplotly] before returning.
#' @param ... other plotting parameters to affect the plot. Not currently used.
#'
#' @details
#'
#' The following options are available for the parameter `which`.
#'
#' * `res`: residuals (1)
#' * `acf`: autocorrelations of the residuals (2)
#' * `pacf`: partial autocorrelations of the residuals (3)
#' * `acf2`: autocorrelations of the squared residuals (4)
#' * `sreshist`: histogram of standardized residuals including normal curve (5)
#' * `nqq`: normal q-q plot of standardized residuals (6)
#'
#' @return Returns an object of class `dygraphs`, `ggplot` or `plotly`
#'
#' @examples
#' data(AirPassengers, package = "datasets")
#' # Generate a persephone object, in this case an x13Single object
#' obj <- perX13(AirPassengers, "RSA1")
#'
#' obj$run()
#' # Plot the residuals after run
#' plotResiduals(obj)
#' # Plot the autocorrelations of the residuals
#' plotResiduals(obj, which="acf")
#'
## TO DO:
# maybe other theme_bw()
# maybe one standard for colors (for dygraph and ggplots -> take dygraph
# colors for ggplots?)
# maybe adjust tooltip for ggplotly (e.g. when count is shown even though
# y=density)
#'
#' @export
plotResiduals <-   function(x, which = c("res", "acf", "acf2", "pacf",
                                         "sreshist", "nqq"),
                            main = NULL, interactive = TRUE, ...) {

  ..density.. <- y <- NULL # nolint

  if (is.null(x$output$regarima)) {
    stop("No results from run available.\n")
  }

  regarima <- x$output$regarima

  if (is.numeric(which)) {
    which <- c("res", "acf", "acf2", "pacf", "sreshist", "nqq")[which]
  }else{
    which <- match.arg(which)
  }

  if (which == "res") {
    if (is.null(main)) {
      main <- "Residuals"
    }

    dyBarChart <- function(dygraph) {
      dyPlotter(dygraph = dygraph,
                name = "BarChart",
                path = system.file("plotters/barchart.js",
                                   package = "dygraphs"))
    }
    p <- dygraph(x$output$regarima$residuals, main = main) %>%
      dySeries("V1", label = "Residual value") %>%
      dyBarChart()

    # if(rangeSelector){
    #   graphObj <- graphObj %>%
    #     dyRangeSelector(height = 20)
    # }
  }

  # Histogram (and/or Frequency Polygon?) of Standardized Residuals
  if (which == "sreshist") {
    if (is.null(main)) {
      main <- "Histogram of Standardized Residuals and Normal Curve"
    }

    result <- regarima$residuals / regarima$residuals.stat$st.error

    result <- data.frame(
      date = paste0(c(floor(time(result) + .01)), "-",
                    stringfix(c(cycle(result)), 2, "0"), "-01"),
      x = c(result))

    p <- ggplot(result,  aes(x = x)) + ## ,stat(density)
      geom_histogram(binwidth = 0.5, center = 0,
                     aes(y = ..density..) # nolint
      ) +
      # ,fill=..count..
      stat_function(fun = dnorm, color = "red", args = list(mean = 0, sd = 1)) +
      # geom_freqpoly(binwidth = 0.5, center=0, aes(y = ..density..)) +
      xlab("Standardized Residuals") +
      ylab("Density") +
      ggtitle(main) +
      theme_bw()
  }

  if (which == "nqq") {
    if (is.null(main)) {
      main <- "Normal Q-Q Plot"
    }

    result <- regarima$residuals / regarima$residuals.stat$st.error
    result <- data.frame(date = paste0(
      c(floor(time(result) + .01)), "-",
      stringfix(c(cycle(result)), 2, "0"), "-01"),
      y = c(result))

    p <- ggplot(result, aes(sample = y)) +
      stat_qq() +
      stat_qq_line(color = "red") +
      xlab("Theoretical Quantiles") +
      ylab("Standardized Residuals") +
      ggtitle(main) +
      theme_bw()
    #stat_qq_line(color="gray50", lty=3) +
  }

  if (which == "acf") {
    if (is.null(main)) {
      main <- "Autocorrelations of the Residuals"
    }

    result <- acf(regarima$residuals, plot = FALSE)

    # confidence interval as in R package forecast
    ci <- 0.95 #coverage probability for confidence interval
    ci <- qnorm((1 + ci) / 2) / sqrt(result$n.used)

    # start from lag1
    # to show whole numbers for lags
    result <- data.frame(lag = result$lag[-1] * frequency(x$ts),
                         acf = result$acf[-1, 1, 1])


    # require(forecast)
    # ggAcf(regarima$residuals, lag.max = NULL,
    #       type = c("correlation", "covariance", "partial"),
    #       plot = TRUE, na.action = na.contiguous, demean=TRUE)
    #

    p <- ggplot(result, aes(x = lag, y = acf)) +
      geom_bar(stat = "identity", width = 0.1) +
      geom_hline(
        yintercept = c(-ci, ci), colour = "blue",
        linetype = "dashed"
      ) +
      xlab("Lag") +
      ylab("ACF") +
      ggtitle(main) +
      theme_bw()
  }

  if (which == "acf2") {
    if (is.null(main)) {
      main <- "Autocorrelations of the Squared Residuals"
    }

    result <- acf(regarima$residuals ^ 2, plot = FALSE)

    # confidence interval as in R package forecast
    ci <- 0.95 #coverage probability for confidence interval
    ci <- qnorm((1 + ci) / 2) / sqrt(result$n.used)

    # start from lag1
    # to show whole numbers for lags
    result <- data.frame(lag = result$lag[-1] * frequency(x$ts),
                         acf = result$acf[-1, 1, 1])

    p <- ggplot(result, aes(x = lag, y = acf)) +
      geom_bar(stat = "identity", width = 0.1) +
      geom_hline(yintercept = c(-ci, ci), colour = "blue",
                 linetype = "dashed") +
      xlab("Lag") +
      ylab("ACF") +
      ggtitle(main) +
      theme_bw()
  }

  if (which == "pacf") {
    if (is.null(main)) {
      main <- "Partial Autocorrelations of the Residuals"
    }

    result <- pacf(regarima$residuals, plot = FALSE)

    # confidence interval as in R package forecast
    ci <- 0.95 #coverage probability for confidence interval
    ci <- qnorm((1 + ci) / 2) / sqrt(result$n.used)

    #to show whole numbers for lags
    result <- data.frame(lag = result$lag * frequency(x$ts),
                         acf = result$acf[, 1, 1])


    p <- ggplot(result, aes(x = lag, y = acf)) +
      geom_bar(stat = "identity", width = 0.1) +
      geom_hline(yintercept = c(-ci, ci), colour = "blue",
                 linetype = "dashed") +
      xlab("Lag") +
      ylab("PACF") +
      ggtitle(main) +
      theme_bw()
  }

  if (interactive & which %in% c("acf", "acf2", "pacf", "nqq", "sreshist")) {

    # p <- p + theme_bw()

    p <- plotly::ggplotly(p)
  }

  p

}
