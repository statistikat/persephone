#' Interactive plot of the seasonal component, irregular component and calendar
#' effects for a persephone object
#'
#' Produces a dygraph (see the
#' [online documentation](https://rstudio.github.io/dygraphs/) for more detail).
#' The function generates an interactive time series plot of the seasonal
#' component, irregular component and calendar effects
#' for a [persephone] object together with one year forecasts
#'
#' @param x an object of class [persephone].
#' @param main plot title
#' @param forecasts logical flag indicating if forecasts should be plotted
#' @param rangeSelector logical flag specifying if a range selector should be
#'   included in the plot
#' @param drawPoints logical flag indicating if a small dot should be drawn at
#'   each point, in addition to a line going through the point.
#' @param annualComparison integer corresponding to the month or quarter which
#'   should be highlighted in the plot for every year.
#'
#' @return Returns an object of class `dygraphs`.
#'
#' @examples
#' data(AirPassengers, package = "datasets")
#' # Generate a persephone object, in this case an x13Single object
#' obj <- perX13(AirPassengers, "rsa1")
#'
#' obj$run()
#' # Plot after run
#' plotSeasIrrCal(obj)
#'
#' @export
plotSeasIrrCal_old <- function(x, main=NULL, forecasts=TRUE, rangeSelector=TRUE,
                    drawPoints=FALSE, annualComparison=NULL){

  if (is.null(x$output$user_defined)) {
    stop("No results from run available.\n")
  }

  # Helper function for annual comparison
  annComp <- function(ts, annualComparison){
    annCompVec <- format(time(ts)[cycle(ts) == annualComparison])
    if (frequency(ts) == 12) {
      annCompLab <- month.abb[annualComparison]
      #annCompLab <- month.name[annualComparison]
      annCompVec <- paste0(substr(annCompVec, 1, 4), "-",
                           stringfix(annualComparison, 2, "0"), "-01")
    } else if (frequency(ts) == 4) {
      annCompLab <- paste0("Q", annualComparison)
      #annCompLab <- paste0(annualComparison,". Quarter")
      annCompVec <- paste0(
        substr(annCompVec, 1, 4), "-",
        stringfix(c(1, 4, 7, 10)[annualComparison], 2, "0"), "-01")
    }
    return(list(annCompVec = annCompVec, annCompLab = annCompLab))
  }

  if (is.null(main)) {
    main <- "Seasonal Comp., Irregular Comp. and Calendar Eff."
  }

  s <- x$output$user_defined$s
  s_f <- x$output$user_defined$s_f
  i <- x$output$user_defined$i
  i_f <- window(x$output$decomposition$d13, start = start(x$output$final$d11a)) #i_f is not correctly implemented in userdefined output
  cal <- x$output$user_defined$cal
  cal_f <- x$output$user_defined$cal_f

  # Initialize Graph Object
  # Back-/Forecasts
  if (forecasts & !is.null(s_f) & !is.null(i_f)) {

    ts <- cbind(s, i, s_f, i_f, cal, cal_f)

    graphObj <- dygraph(ts, main = main) %>%
      dySeries("s", label = "Seasonal Component", drawPoints = drawPoints,
               color = "rgb(0,128,0)") %>%
      dySeries("i",
               label = "Irregular Component",
               drawPoints = drawPoints,
               color = "rgb(0,0,128)") %>%
      dySeries(
        "s_f",
        label = "Forecasts Seasonal Comp.",
        strokePattern = "dashed",
        drawPoints = drawPoints,
        color = "rgb(0,128,0)"
      ) %>%
      dySeries(
        "i_f",
        label = "Forecasts Irregular Comp.",
        strokePattern = "dashed",
        drawPoints = drawPoints,
        color = "rgb(0,0,128)"
      )
    if (!is.null(cal) & !is.null(cal_f)) {
      graphObj <- graphObj %>%
        dySeries("cal",
                 label = "Calendar Effects",
                 drawPoints = drawPoints,
                 color = "rgb(128,0,0)") %>%
        dySeries(
          "cal_f",
          label = "Forecasts Calendar Eff.",
          strokePattern = "dashed",
          drawPoints = drawPoints,
          color = "rgb(128,0,0)"
        )
    }
    #graphObj <- graphObj %>% dyOptions(rightGap=10)

  }else{
    ts <- cbind(s, i, cal)

    graphObj <- dygraph(ts, main = main) %>%
      dySeries("s",
               label = "Seasonal Componenet",
               drawPoints = drawPoints,
               color = "rgb(0,128,0)") %>%
      dySeries("i",
               label = "Irregular Component",
               drawPoints = drawPoints,
               color = "rgb(0,0,128)")
    if (!is.null(cal)) {
      graphObj <- graphObj %>%
        dySeries("cal",
                 label = "Calendar Effects",
                 drawPoints = drawPoints,
                 color = "rgb(128,0,0)")
    }
    #graphObj <- graphObj %>% dyLegend(width=400)

  }


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

  # graphObj <- graphObj %>%
  #   dyHighlight(highlightSeriesOpts = list(strokeWidth = 2),
  #               highlightCircleSize = 4,
  #               highlightSeriesBackgroundAlpha = 0.5)
  # graphObj <- graphObj %>%
  #   dyOptions(titleHeight=3)
  #
  cssString <- textConnection(".dygraph-title {
font-size: 14px;
}
.dygraph-legend > span {
font-size: 10px;
}")

  graphObj <- graphObj %>%
    dyCSS(cssString)

  graphObj

}
