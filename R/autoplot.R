#' @export
ggplot2::autoplot

#' @export
autoplot.persephone <- function(object, ...) {
  autoplot_ts <- utils::getS3method("autoplot", "ts",
                                    envir = environment(forecast::forecast))
  autoplot_ts(object$ts) + theme_minimal() +
    ggtitle("Original time series") +
    ylab(deparse(substitute(object)))
}
