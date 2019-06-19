#' Interactive time series plot for a persephone object
#'
#' Produces a dygraph (see the
#' [online documentation](https://rstudio.github.io/dygraphs/) for more detail)
#' for objects of class [persephone].
#' In case of ``persephoneSingle`` objects,
#' the function generates an interactive time series plot
#' of the original series, the seasonally adjusted (SA) series and the trend as
#' well as one year forecasts with prediction intervals of the original series.
#' For [hierarchicalTimeSeries] objects, the function plots the original
#' aggregate series
#' as well as the directly and indirectly seasonally adjusted aggregate series.
#'
#' If no run has been performed on the [persephone] object,
#' only the original (aggregate) time series is plotted.
#'
#' @name plot
#' @param x an object of class [persephone].
#' @param main plot title
#' @param forecasts logical flag indicating if forecasts should be plotted
#' @param showOutliers logical flag specifying if outliers should be highlighted
#'   in the plot
#' @param rangeSelector logical flag specifying if a range selector should be
#'   included in the plot
#' @param drawPoints logical flag indicating if a small dot should be drawn at
#'   each point, in
#'   addition to a line going through the point.
#' @param annualComparison integer corresponding to the month or quarter which
#'   should be highlighted in the plot for every year.
#' @param ... other plotting parameters to affect the plot. Not currently used.
#'
#' @return Returns an object of class `dygraphs`.
#'
#' @examples
#' # Plotting hierarchicalTimeSeries objects
#'
#' # Monthly data
#' data(AirPassengers, package = "datasets")
#' # Generate two persephoneSingle objects, in this case two tramoseatsSingle
#' # objects
#' tsAir1 <- tramoseatsSingle$new(AirPassengers, "RSA1")
#' tsAir2 <- tramoseatsSingle$new(AirPassengers, "RSA1")
#' # Generate a hierarchicalTimeSeries object
#' htAir <- hierarchicalTimeSeries$new(tsAir1=tsAir1, tsAir1=tsAir1)
#' # Plot before run of persephone object
#' plot(htAir, drawPoints = TRUE)
#' htAir$run()
#' # Plot after run
#' plot(htAir, drawPoints=TRUE)
#'
#' # Quarterly data
#' data(UKgas, package = "datasets")
#' tsUKgas1 <- tramoseatsSingle$new(UKgas, "RSA3")
#' tsUKgas2 <- tramoseatsSingle$new(UKgas, "RSA3")
#' htUKgas <- hierarchicalTimeSeries$new(tsUKgas1=tsUKgas1, tsUKgas2=tsUKgas2)
#' plot(htUKgas)
#' htUKgas$run()
#' plot(htUKgas)
#'
#' @importFrom stats time cycle dnorm frequency lag acf qnorm pacf
#' @importFrom dygraphs dyCSS dyLegend dyRangeSelector dySeries dygraph
#'   dyHighlight dyAnnotation dyPlotter dyEvent
#' @importFrom stringr str_pad
#' @import ggplot2
#' @export
plot.hierarchicalTimeSeries <- function(x, main=NULL,
                            rangeSelector=TRUE, drawPoints=FALSE,
                            annualComparison=NULL, ...){

  # Helper function for annual comparison
  annComp <- function(ts, annualComparison){

    annCompVec <- format(time(ts)[cycle(ts) == annualComparison])
    if (frequency(ts) == 12) {
      annCompLab <- month.abb[annualComparison]
      #annCompLab <- month.name[annualComparison]
      annCompVec <- paste0(substr(annCompVec, 1, 4), "-",
                           str_pad(annualComparison, 2, "left", "0"),
                           "-01")
    }else if (frequency(ts) == 4) {
      annCompLab <- paste0("Q", annualComparison)
      #annCompLab <- paste0(annualComparison,". Quarter")
      annCompVec <- paste0(substr(annCompVec, 1, 4), "-",
                           str_pad(c(1, 4, 7, 10)[annualComparison],
                                   2, "left", "0"), "-01")
    }
    return(list(annCompVec = annCompVec, annCompLab = annCompLab))

  }

  preRunPlot <- function(ts, rangeSelector=rangeSelector, drawPoints=drawPoints,
                         annualComparison=annualComparison){

    if (is.null(main)) {
      main <- "Original Aggregate Time Series"
    }

    graphObj <- dygraph(ts, main = main) %>%
      dySeries("V1", label = "Original", drawPoints = drawPoints)

    if (rangeSelector) {
      graphObj <- graphObj %>%
        dyRangeSelector(height = 20)
    }
    if (!is.null(annualComparison)) {
      annCompRes <- annComp(ts, annualComparison)
      for (i in 1:length(annCompRes[["annCompVec"]])) {
        graphObj <- graphObj %>%
          #dyEvent(annCompRes[["annCompVec"]][i],
          # paste(substr(annCompRes[["annCompVec"]][i],1,4),
          # annCompRes[["annCompLab"]]),labelLoc = "bottom",color="lightgrey")
          dyEvent(annCompRes[["annCompVec"]][i], annCompRes[["annCompLab"]],
                  labelLoc = "bottom", color = "lightgrey")
      }
    }

    graphObj

  }

  postRunPlot <- function(
    main = main, rangeSelector = rangeSelector, drawPoints = drawPoints,
    annualComparison = annualComparison) {

    if (is.null(main)) {
      main <- "Original, Directly and Indirectly Adjusted Aggregate Series"
    }

    y <- x$ts
    sa_dir <- x$adjusted
    sa_indir <- x$adjusted_indirect

    # Forecasts not implemented yet

    # ppm_y_f <- x$output$user_defined$preprocessing.model.y_f # nolint
    # ppm_y_ef <- x$output$user_defined$preprocessing.model.y_ef # nolint

    # Initialize Graph Object
    # # Back-/Forecasts
    # if (forecasts & !is.null(ppm_y_f) & !is.null(ppm_y_ef)) {
    #   lowerci <- ppm_y_f - 1.96 * ppm_y_ef
    #   upperci <- ppm_y_f + 1.96 * ppm_y_ef
    #   ts <- cbind(y, t, sa, ppm_y_f, lowerci, upperci)
    #
    #   graphObj <- dygraph(ts, main = main) %>%
    #     dySeries("y", label = "Original", drawPoints = drawPoints) %>%
    #     dySeries("sa", label = "Seasonally Adjusted") %>%
    #     dySeries("t", label = "Trend") %>%
    #     dySeries(c("lowerci", "ppm_y_f", "upperci"), label = "Forecasts",
    #              strokePattern = "dashed", drawPoints = drawPoints) %>%
    #     dyLegend(width = 400)
    #
    # }else{
      ts <- cbind(y, sa_dir, sa_indir)

      graphObj <- dygraph(ts, main = main) %>%
        dySeries("y", label = "Original", drawPoints = drawPoints) %>%
        dySeries("sa_dir", label = "Direct SA") %>%
        dySeries("sa_indir", label = "Indirect SA") %>%
        dyLegend(width = 290)
    # }

    # Outliers not implemented

    if (rangeSelector) {
      graphObj <- graphObj %>%
        dyRangeSelector(height = 20)
    }

    if (!is.null(annualComparison)) {
      annCompRes <- annComp(ts, annualComparison)
      for (i in 1:length(annCompRes[["annCompVec"]])) {
        graphObj <- graphObj %>%
          dyEvent(annCompRes[["annCompVec"]][i], annCompRes[["annCompLab"]],
                  labelLoc = "bottom", color = "lightgrey")
      }
    }

    graphObj <- graphObj %>%
      dyHighlight(highlightSeriesOpts = list(strokeWidth = 2),
                  highlightCircleSize = 4, highlightSeriesBackgroundAlpha = 0.5)

    graphObj
  }

  if (!is.null(x$output$user_defined)) {

    graphObj <- postRunPlot(
      main = main, rangeSelector = rangeSelector, drawPoints = drawPoints,
      annualComparison = annualComparison
    )
    graphObj

  }else{
    ts <- x$ts
    preRunPlot(
      ts = ts,
      rangeSelector = rangeSelector,
      drawPoints = drawPoints,
      annualComparison = annualComparison
    )
  }
}
