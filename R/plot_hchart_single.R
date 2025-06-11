add_ts <- function(hc, x, name, ..., type = "spline") {
  highcharter::hc_add_series(hc, x, name = name, ..., type = type)
}

#' @export
hchart.persephoneSingle <- function(object, ...) {
  output <- object$output
  stopifnot(!is.null(output))
  decomp <- output$decomposition
  outliers <- object$outliers
  highcharter::highchart(type = "stock") %>%
    highcharter::hc_yAxis_multiples(
      highcharter::create_axis(2, height = c(4, 1), turnopposite = TRUE)) %>%
    add_ts(decomp$series, "series (a1)", id = "mainseries") %>%
    add_ts(decomp$trend, "trend (d12)") %>%
    add_ts(decomp$sa, "seasonally adjusted (d11)") %>%
    add_ts(decomp$irr, "irregular (d13)", yAxis = 1, id = "irregular") %>%
    #add_ts(output$result$final$e3, "irregular (e3)", yAxis = 1, linkedTo = "irregular") %>%
    highcharter::hc_legend(enabled = TRUE) %>%
    highcharter::hc_rangeSelector(buttons = list(
      list(type = "all", text = "all"),
      list(text = "20y", count = 20, type = "year"),
      list(text = "10y", count = 10, type = "year"),
      list(text = "5y", count = 5, type = "year")
    )) %>%
    highcharter::hc_chart(zoomType = "x") %>%
    highcharter::hc_chart(events = list(load = highcharter::JS(
      "function() { this.container.ondblclick = _ => this.zoomOut(); }"))) %>%
    highcharter::hc_xAxis(plotLines = lapply(outliers, function(o) {
      list(value = highcharter::datetime_to_timestamp(o$pos),
           color = "#aaa", dashStyle = "dash")
    })) %>%
    hc_add_forecast(output)
}

hc_add_forecast <- function(hc, x) {
  forecast <- x$user_defined$`y_f(?)`
  forecast_sd <- x$user_defined$`y_ef(?)`
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
