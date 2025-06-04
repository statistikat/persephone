#' Generate figures from the EUROSTAT quality report
#'
#' This function uses the method `iterate` to create figures for a quality
#' report for each component of a hierarchical `persephone` object
#'
#' @param x a persephone object
#'
#' @section Columns:
#' * __component__ is a column added by `iterate()` and informs where in the
#'   hierarchy a certain component is.
#' * __Method__ is `TS` for [tramoseats_fast()] and `x13` for [x13_fast()]
#' * __Period__ is `12` for yearly measurements and `4` for quarterly
#'   measurements
#' * __Start__ denotes the start of the time series
#' * __End__ denotes the end of the time series
#' * __Log.Transformation__
#' * __ARIMA.Model__
#' * __LeapYear__
#' * __MovingHoliday__
#' * __NbTF__
#' * __Noutliers__ Number of outliers
#' * __Outlier1__
#' * __Outlier2__
#' * __Outlier3__
#' * __CombinedTest_SI__
#' * __Residual.Seasonality__
#' * __Residual.TD.Effect__
#' * __Q.Stat__
#' * __Final.Henderson.Filter__
#' * __Stage.2.Henderson.Filter__
#' * __Seasonal.Filter__
#' * __Max.Adj__
#'
#' @examples
#' obj_x13 <- perX13(AirPassengers, "rsa3")
#' ht <- perHts(a = obj_x13, b = obj_x13, method = "x13")
#' ht$run()
#' generateQrTable(ht)
#'
#' @export
generateQrTable <- function(x) {
  stopifnot(is.persephone(x))
  x$generateQrTable()
}
