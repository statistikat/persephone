add_ts <- function(hc, x, name, ..., type = "spline") {
  highcharter::hc_add_series(hc, x, name = name, ..., type = type)
}

#' @export
hchart.persephoneSingle <- function(object, ...) {
  output <- object$output
  stopifnot(!is.null(output))
  ud <- object$output$user_defined
  outliers <- getOutliers(object)$date

  highcharter::highchart(type = "stock") %>%
    highcharter::hc_yAxis_multiples(
      highcharter::create_axis(2, height = c(4, 1), turnopposite = TRUE)) %>%
    add_ts(ud$y, "original", id = "original") %>%
    add_ts(ud$t, "trend") %>%
    add_ts(ud$sa, "seasonally adjusted") %>%
    # add_ts(decomp$irr, "irregular (d13)", yAxis = 1, id = "irregular") %>%
    # add_ts(output$result$final$e3, "irregular (e3)", yAxis = 1, linkedTo = "irregular") %>%
    # positioner: The callback receives a point object that contains plotX and plotY positions.
    # Fix the y value to some number and return point.plotX as the x value.
    # highcharter::hc_tooltip(fixed = TRUE,
    #   # positioner =  JS("function (labelWidth, labelHeight, point) {
    #   # return { x: point.plotX, y: 15 };}"),
    #   positioner =  JS("function () {
    #   return { x: 80, y: 50 };}"),
    # shadow= FALSE,
    # borderWidth= 0,
    # backgroundColor= 'rgba(255,255,255,0.8)') %>%
    # highcharter::hc_tooltip(fixed = TRUE,
    #                         formatter = JS(
    #                           "function () {
    #     var tooltip = '<b>' + Highcharts.dateFormat('%Y-%m-%d', this.x) + '</b><br/>';
    #     $.each(this.points, function (i, point) {
    #       var color = point.series.color;
    #       var marker = point.series.symbol; // Get the marker symbol
    #       tooltip += '<span style=\"color:' + color + ';\"><b>' +
    #       marker + '</b>: '+ point.series.name + '</b>: ' +
    #       point.y.toFixed(2) + '</span><br/>';
    #     });
    #     return tooltip;
    #   }")) %>%
    # https://jsfiddle.net/BlackLabel/edmjkq91/
    # https://api.highcharts.com/class-reference/Highcharts.Time
    highcharter::hc_tooltip(fixed = TRUE,
    formatter = JS("function () {
        return `${Highcharts.dateFormat('%Y-%m', this.x)}`;
      }"),
    positioner =  JS("function (labelWidth, labelHeight, point) {
    return { x: point.plotX, y: -5 };}"))%>%
  # highcharter::hc_tooltip(enabled = FALSE) %>%
    highcharter::hc_legend(enabled = TRUE) %>%
    # highcharter::hc_rangeSelector(enabled = FALSE) %>%
    highcharter::hc_rangeSelector(buttons = list(
      list(type = "all", text = "all"),
      #list(text = "20y", count = 20, type = "year"),
      list(text = "10y", count = 10, type = "year"),
      list(text = "5y", count = 5, type = "year")
      #,list(text = "1y", count = 1, type = "year") # geht das nur für vollständige Jahre?
    )) %>%
    highcharter::hc_navigator(enabled = FALSE) %>%
    highcharter::hc_scrollbar(enabled = FALSE) %>%
    highcharter::hc_chart(zoomType = "x") %>%
    highcharter::hc_chart(events = list(load = highcharter::JS(
      "function() { this.container.ondblclick = _ => this.zoomOut(); }"))) %>% # zoomout on doubleclick
    highcharter::hc_xAxis(plotLines = lapply(outliers, function(o) {
      list(value = highcharter::datetime_to_timestamp(o),
           color = "#FF83FA", dashStyle = "dash")#color = "#aaa"
    })) %>%
    hc_add_forecast(output)
}
# https://www.highcharts.com/docs/chart-design-and-style/colors
# https://www.nceas.ucsb.edu/sites/default/files/2020-04/colorPaletteCheatsheet.pdf
# https://www.farb-tabelle.de/en/rgb2hex.htm?q=plum3
hc_add_forecast <- function(hc, x) {
  forecast <- x$user_defined$y_f
  forecast_sd <- x$user_defined$y_ef
  forecast_lower <- forecast - 1.96 * forecast_sd
  forecast_upper <- forecast + 1.96 * forecast_sd
  forecast_x <- zoo::as.Date(forecast) %>% highcharter::datetime_to_timestamp()
  hc %>% highcharter::hc_add_series(
    forecast, type = "spline",   name = "forecast",
    dashStyle = "shortdash", color = "grey", id = "forecast") %>%
    highcharter::hc_add_series(
      type = "areasplinerange", opacity = 0.15, color = "blue",
      linkedTo = "forecast", name = "confidence interval",
      data = lapply(seq_along(forecast_x), function(i) {
        list(x = forecast_x[i], low = forecast_lower[i],
             high = forecast_upper[i])
      }))
}
