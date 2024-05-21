#' irregular component and calendar effects for a persephone object
#'
#' The function generates an interactive time series plot of the seasonal
#' component, irregular component and calendar effects
#' for a [persephone] object together with one year forecasts
#' @param x an object of class [persephone].
#' @export
plotSeasIrrCal <- function(x) {
  ud <- x$output$user_defined
  stopifnot(!is.null(ud))
  #i_f <- x$output$user_defined$i_f
  highcharter::highchart(type = "stock") %>%
    add_ts(ud$s, name = "Seasonal Component", id = "s") %>%
    add_ts(ud$i, name = "Irregular Component") %>%
    add_ts(ud$cal, name = "Calendar Effects", id = "cal") %>%
    add_ts(ud$cal_f, name = "Calendar Effects2", linkedTo = "cal",
           dashStyle = "shortdash") %>%
    add_ts(ud$s_f, name = "Seasonal om. Forecast", linkedTo = "s",
           dashStyle = "shortdash") %>%
    highcharter::hc_legend(enabled = TRUE) %>%
    highcharter::hc_chart(zoomType = "x") %>%
    highcharter::hc_chart(events = list(load = highcharter::JS("function() { this.container.ondblclick = _ => this.zoomOut(); }"))) %>%
    highcharter::hc_rangeSelector(buttons = list(list(
      type = "all", text = "all"), list(text = "20y", count = 20, type = "year"),
      list(text = "10y", count = 10, type = "year"), list(text = "5y",
                                                          count = 5, type = "year")))
}
