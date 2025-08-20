#' @name plot
#' @examples
#' # Plotting persephoneSingle objects
#'
#' # Monthly data
#' data(AirPassengers, package = "datasets")
#' # Generate a persephoneSingle object, in this case an x13Single object
#' obj <- perX13(AirPassengers, "RSA1")
#' # Plot before run of persephoneSingle object
#' plot(obj, drawPoints = TRUE)
#' obj$run()
#' # Plot after run
#' plot(obj, drawPoints=TRUE)
#' # Update some parameters of the persephoneSingle object
#' # (randomly specify some outliers for illustrative purposes only)
#' obj$updateParams(usrdef.outliersEnabled = TRUE,
#'                 usrdef.outliersType = c("AO","LS","LS"),
#'                 usrdef.outliersDate=c("1950-01-01","1955-04-01",
#'                                       "1959-10-01"))
#' # Perform run to make updateParams take effect
#' obj$run()
#' plot(obj)
#'
#' # Quarterly data
#' data(UKgas, package = "datasets")
#' obj2 <- perX13(UKgas, "RSA3")
#' plot(obj2)
#' obj2$run()
#' plot(obj2)
#'
#' # Generate a persephoneSingle object, in this case a tramoseatsSingle object
#' obj3 <- perTramo(UKgas, "RSA3")
#' plot(obj3)
#' obj3$run()
#' plot(obj3)
#'
#' @importFrom stats time cycle dnorm frequency lag acf qnorm pacf
#' @importFrom dygraphs dyCSS dyLegend dyRangeSelector dySeries dygraph
#'   dyHighlight dyAnnotation dyPlotter dyEvent
#' @import ggplot2
#' @importFrom stats window<-
#' @export
plot.persephoneSingle <- function(
  x, main = NULL, forecasts = TRUE, showOutliers = TRUE, rangeSelector = TRUE,
  drawPoints = FALSE, annualComparison = NULL, ...) {
  # Helper function for annual comparison
  annComp <- function(ts, annualComparison){

    annCompVec <- format(time(ts)[cycle(ts) == annualComparison])
    if (frequency(ts) == 12) {
      annCompLab <- month.abb[annualComparison]
      #annCompLab <- month.name[annualComparison]
      annCompVec <- paste0(substr(annCompVec, 1, 4), "-",
                           stringfix(annualComparison, 2, "0"),
                           "-01")
    }else if (frequency(ts) == 4) {
      annCompLab <- paste0("Q", annualComparison)
      #annCompLab <- paste0(annualComparison,". Quarter")
      annCompVec <- paste0(substr(annCompVec, 1, 4), "-",
                           stringfix(c(1, 4, 7, 10)[annualComparison],
                                     2, "0"), "-01")
    }
    return(list(annCompVec = annCompVec, annCompLab = annCompLab))

  }

  preRunPlot <- function(ts, rangeSelector=rangeSelector, drawPoints=drawPoints,
                         annualComparison=annualComparison){

    if (is.null(main)) {
      main <- "Original Time Series"
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

  gettsout <- function(outliers, y) {

    names(outliers) <- sapply(outliers, function(x) x[1])

    if (frequency(x$ts) == 12) {
      #Date format für dyevent
      dateout <- lapply(sapply(outliers, function(x) strsplit(x[[2]], "-")),
                        function(z) paste0(
                          z[[2]], "-", stringfix(z[[1]], 2, "0"), "-01"))

      outliers <- lapply(sapply(outliers, function(x) strsplit(x[[2]], "-")),
                         function(z) as.numeric(c( z[[2]],z[[1]])))
    } else {
      dateout <- lapply(
        sapply(outliers,function(x) strsplit(x[[2]], "-")),
        function(z) paste0(
          z[[2]], "-",
          stringfix(c(1, 4, 7, 10)[as.numeric(utils::as.roman(z[[1]]))],
                    2, "0"), "-01"))
      outliers <- lapply(sapply(outliers, function(x) strsplit(x[[2]], "-")),
                         function(z) as.numeric(c( z[[2]], as.numeric(utils::as.roman(z[[1]])))))
    }

    outliersAO <- outliers[names(outliers) %in% "AO"]
    outliersLS <- outliers[names(outliers) %in% "LS"]
    outliersTC <- outliers[names(outliers) %in% "TC"]

    tsout <- list()
    if(length(outliersAO)>0){
      otlAO <- ts(start = start(y), end = end(y), frequency = frequency(y))
      for(i in seq_along(outliersAO)) {
        window(otlAO, start=outliersAO[[i]], end=outliersAO[[i]]) <- window(y, start=outliersAO[[i]], end=outliersAO[[i]])
      }
      tsout[[length(tsout)+1]] <- otlAO
      names(tsout)[length(tsout)] <- "otlAO"
    }

    if(length(outliersLS)>0){
      otlLS <- ts(start = start(y), end = end(y), frequency = frequency(y))
      for(i in seq_along(outliersLS)) {
        window(otlLS, start=outliersLS[[i]], end=outliersLS[[i]]) <- window(y, start=outliersLS[[i]], end=outliersLS[[i]])
      }
      tsout[[length(tsout)+1]] <- otlLS
      names(tsout)[length(tsout)] <- "otlLS"
    }

    if(length(outliersTC)>0){
      otlTC <- ts(start = start(y), end = end(y), frequency = frequency(y))
      for(i in seq_along(outliersTC)) {
        window(otlTC, start=outliersTC[[i]], end=outliersTC[[i]]) <- window(y, start=outliersTC[[i]], end=outliersTC[[i]])
      }
      tsout[[length(tsout)+1]] <- otlTC
      names(tsout)[length(tsout)] <- "otlTC"
    }

    tsout <- do.call(cbind, tsout)

    return(list(tsout, dateout))
  }

  includeOutGraphically <- function (otlType, otlColor = NULL, otl, graphObj) {

    if (any(grepl(otlType, names(otl[[2]])))) {

      if (is.null(otlColor)) {
        otlColors <- list(AO = "red", LS = "orange", TC = "dodgerblue")
        otlColor <- otlColors[[otlType]]
      }
      if (is.null(dim(otl[[1]]))) {
        seriesName <- "otl[[1]]"
      } else {
        seriesName <- paste0("otl[[1]].otl",otlType)
      }
      graphObj <- graphObj %>%
        dySeries(seriesName, label = otlType,
                 drawPoints =TRUE, color = otlColor,
                 pointShape = "dot", pointSize = 2.5)
      dateout <- otl[[2]][grepl(otlType, names(otl[[2]]))]
      for(i in seq_along(dateout)){
        graphObj <-  graphObj %>% dyEvent(dateout[[i]], label = NULL,
                                          labelLoc = "bottom",
                                          strokePattern = "dashed",color = otlColor)
      }
    }
    return(graphObj)
  }

  postRunPlot <- function(
    main=main, forecasts=forecasts, showOutliers=showOutliers,
    rangeSelector=rangeSelector, drawPoints=drawPoints,
    annualComparison=annualComparison) {

    if (is.null(main)) {
      main <- "Original, SA and Trend Series"
    }

    y <- x$output$user_defined$y
    t <- x$output$user_defined$t
    sa <- x$output$user_defined$sa
    ppm_y_f <- x$output$user_defined$preprocessing.model.y_f # nolint
    ppm_y_ef <- x$output$user_defined$preprocessing.model.y_ef # nolint
    # forecasts currently only plotted for original series, maybe allow t and
    # sa forecasts in some other setting??

    otlTF <- FALSE
    # Outliers
    if (showOutliers & !is.null(x$output$regarima$regression.coefficients)) {
      outliers <- rownames(x$output$regarima$regression.coefficients)
      outliers <- outliers[substr(outliers, 1, 2) %in% c("AO", "LS", "TC")]
      if (length(outliers) > 0) {
      outliersName <- outliers
      outliers <- gsub("(", "", outliers, fixed = TRUE)
      outliers <- gsub(")", "", outliers, fixed = TRUE)
      outliers <- strsplit(outliers, " ")
      # outliersType <- outliers
      # # Variante1 : Date format
      # if (frequency(x$ts) == 12) {
      #   outliers <- sapply(sapply(outliers, function(x) strsplit(x[[2]], "-")),
      #                     function(y) paste0(
      #                       y[[2]], "-", stringfix(y[[1]], 2, "0"), "-01")
      #   )
      # }else{
      #   outliers <- sapply(
      #     sapply(
      #       outliers,
      #       function(x) strsplit(x[[2]], "-")
      #     ),
      #     function(y) paste0(
      #       y[[2]], "-",
      #       stringfix(c(1, 4, 7, 10)[as.numeric(utils::as.roman(y[[1]]))],
      #               2, "0"), "-01"))
      # }
      # Variante2 : ts format und date format
      #
      otl <- gettsout(outliers, y)
      otlTF <- TRUE
      }
    }

    # Initialize Graph Object
    # Back-/Forecasts
    if (forecasts & !is.null(ppm_y_f) & !is.null(ppm_y_ef)) {
      lowerci <- ppm_y_f - 1.96 * ppm_y_ef
      upperci <- ppm_y_f + 1.96 * ppm_y_ef
      if (!otlTF) {
        ts <- cbind(y, t, sa, ppm_y_f, lowerci, upperci)
        graphObj <- dygraph(ts, main = main) %>%
          dySeries("y", label = "Original", drawPoints = drawPoints) %>%
          dySeries("sa", label = "Seasonally Adjusted") %>%
          dySeries("t", label = "Trend") %>%
          dySeries(c("lowerci", "ppm_y_f", "upperci"), label = "Forecasts",
                   strokePattern = "dashed", drawPoints = drawPoints) %>%
          dyLegend(width = 400)
      } else {
        ts <- cbind(y, t, sa, otl[[1]],ppm_y_f, lowerci, upperci)

        graphObj <- dygraph(ts, main = main) %>%
          dySeries("y", label = "Original", drawPoints = drawPoints) %>%
          dySeries("sa", label = "Seasonally Adjusted") %>%
          dySeries("t", label = "Trend")
        graphObj <- includeOutGraphically(otlType = "AO", otl=otl, graphObj=graphObj)
        graphObj <- includeOutGraphically(otlType = "LS", otl=otl, graphObj=graphObj)
        graphObj <- includeOutGraphically(otlType = "TC", otl=otl, graphObj=graphObj)
        graphObj <- graphObj %>%
          dySeries(c("lowerci", "ppm_y_f", "upperci"), label = "Forecasts",
                   strokePattern = "dashed", drawPoints = drawPoints) %>%
          dyLegend(width = 300)#300 bei nouttype=3
      }
    } else {
      if (!otlTF) {
        ts <- cbind(y, t, sa)
        graphObj <- dygraph(ts, main = main) %>%
          dySeries("y", label = "Original", drawPoints = drawPoints) %>%
          dySeries("sa", label = "Seasonally Adjusted") %>%
          dySeries("t", label = "Trend") %>%
          dyLegend(width = 290)
      } else {
        ts <- cbind(y, t, sa, otl[[1]])
        graphObj <- dygraph(ts, main = main) %>%
          dySeries("y", label = "Original", drawPoints = drawPoints) %>%
          dySeries("sa", label = "Seasonally Adjusted") %>%
          dySeries("t", label = "Trend")
        graphObj <- includeOutGraphically(otlType = "AO", otl=otl, graphObj=graphObj)
        graphObj <- includeOutGraphically(otlType = "LS", otl=otl, graphObj=graphObj)
        graphObj <- includeOutGraphically(otlType = "TC", otl=otl, graphObj=graphObj)
        graphObj <- graphObj %>%
          dyLegend(width = 290)
      }
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

  graphObj <- graphObj %>%
    dyHighlight(highlightSeriesOpts = list(strokeWidth = 2),
                highlightCircleSize = 4, highlightSeriesBackgroundAlpha = 0.5)

  graphObj
}

if (!is.null(x$output$user_defined)) {

  graphObj <- postRunPlot(
    main = main, forecasts = forecasts, showOutliers = showOutliers,
    rangeSelector = rangeSelector, drawPoints = drawPoints,
    annualComparison = annualComparison
  )
  graphObj

} else {
  ts <- x$ts
  preRunPlot(
    ts = ts,
    rangeSelector = rangeSelector,
    drawPoints = drawPoints,
    annualComparison = annualComparison
  )
}

}

stringfix <- function(x, l, fill = " ") {
  x <- sapply(x, function(x) {
    if (is.na(x)) {
      return("")
    }
    paste0(paste0(rep(fill, l - nchar(x)), collapse = ""),
           x)
  })
  return(x)
}
