#' Plot of SI-ratios by period
#'
#' Generate a `hchart` showing the si ratios and seasonal factors.
#' @returns An object of class `highchart`
#' @param x An object of class `persephone`
#' @examples
#' x <- perX13(AirPassengers)
#' x$run()
#' plotSiRatios(x)
#' @export
plotSiRatios <- function(x) {
  decomp <- x$output$result$decomposition
  d8 <- decomp$d8
  d9 <- decomp$d9
  d10 <- decomp$d10


  d10_by <- by(d10, cycleName(d10), mean)
  d10_mean <- data.frame(cycleName = names(d10_by),
                         d10Mean = as.vector(d10_by))
  dat <- merge(data.frame(year = floor(stats::time(d10)),
                          cycleName = cycleName(d10),
                          d8, d9, d10, stringsAsFactors = FALSE),
               d10_mean, by = "cycleName", sort = FALSE, all.x = TRUE)
  nyears <- length(unique(dat$year))
  colnames(dat) <- c("cycleName", "year", "SI-Ratio", "Replaced SI-Ratio",
                     "Seasonal Factor", "SF Mean")
  dat$x <- as.integer(dat$cycleName) + (dat$year - min(dat$year))/nyears
  dat <- dat[order(dat$x), ]

  hc <- highcharter::highchart()


  hc <- hc %>% highcharter::hc_add_series(
    data = list(),
    name = "Seasonal Factor",
    color = "#A59187",
    marker = FALSE,
    custom = list(month = dat$cycleName, year = dat$year))

  for (x2 in split(dat, dat$cycleName)) {
    hc <- hc %>% highcharter::hc_add_series(
      data.frame(x = x2$x, y = x2$`Seasonal Factor`),
      linkedTo = ":previous",
      name = "Seasonal Factor",
      color = "#A59187"
    )
  }

  hc <- hc %>% highcharter::hc_add_series(
    data = list(), name = "SF Mean", color = "#5A8CBE",
    marker = FALSE)

  for (x2 in split(dat, dat$cycleName)) {
    hc <- hc %>% highcharter::hc_add_series(
      data.frame(x = x2$x, y = x2$`SF Mean`),
      name = "SF Mean",
      marker = list(enabled = FALSE),
      linkedTo = ":previous",
      color = "#5A8CBE",
      states = list(hover = list(enabled = FALSE))
    )
  }

  hc <- hc %>% highcharter::hc_add_series(
    lineWidth = 0, marker = list(symbol = "circle"),
    data = list(), name = "SI-Ratio", color = "#00643C")

  for (x2 in split(dat, dat$cycleName)) {
    hc <- hc %>% highcharter::hc_add_series(
      data.frame(x = x2$x, y = x2$`SI-Ratio`),
      name = "SI-Ratio",
      linkedTo = ":previous",
      color = "#00643C",
      lineWidth = 0,

      #type = "scatter",
      #findNearestPointBy = "x",
      marker = list(
        enabled = TRUE,
        symbol = "circle"
      ),
      states = list(hover = list(
        lineWidthPlus = 0
      ))
    )
  }

  hc %>%
    highcharter::hc_xAxis(
      #breaks = list(
      #  list(breakSize = 0.3, from = 2, to = 2),
      #  list(breakSize = 0.5, from = 3, to = 3)
      #),
      breaks = lapply(1:11, function(i) {
        list(breakSize = 0.3, from = i + 1, to = i + 1)
      }),
      tickPositions = 1:12 + 0.5,
      labels = list(
        formatter = highcharter::JS('
          function() {
            return ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul",
              "Aug", "Sep", "Oct", "Nov", "Dec"][this.value-1.5];
          }
        ')
      )
    ) %>%
    highcharter::hc_tooltip(
      useHTML = TRUE,
      shared = TRUE,
      formatter = highcharter::JS(readLines(
        system.file("js/tooltip_si.js", package = "persephone3") %>%
          paste(collapse = "\n")
      ))) %>%
    highcharter::hc_xAxis(crosshair = list(width = 30, color = "#efefef"))
}

cycleName <- function(ts) {
  if (stats::frequency(ts) == 12) {
    res <-  month.abb[stats::cycle(ts)]
    res <- ordered(res, levels = c(month.abb))
  } else if (stats::frequency(ts) == 4) {
    res <-  c("Q1", "Q2", "Q3", "Q4")[stats::cycle(ts)]
    res <- ordered(res, levels = c("Q1", "Q2", "Q3", "Q4"))
  } else if (stats::frequency(ts) == 2) {
    res <-  c("H1", "H2")[stats::cycle(ts)]
    res <- ordered(res, levels = c("H1", "H2"))
  }
  return(res)
}
