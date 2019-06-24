#' @importFrom stats start end
generateQrList <- function(x, tsName = "tsName"){
  x$ts
  x$output

  # Select 3 main (most significant) outliers
  outliers3 <- rep("", 3)
  if (!is.null(x$output$regarima$regression.coefficients)) {
    regcoeff <- as.data.frame(x$output$regarima$regression.coefficients,
                              stringsAsFactors = FALSE)
    regcoeff$regcoeff <- row.names(regcoeff)
    outliers <- regcoeff[substr(regcoeff$regcoeff, 1, 2) %in%
                           c("AO", "LS", "TC"), ]
    outliers <- outliers[order(abs(outliers[, "T-stat"]), decreasing = TRUE), ]
    outliers3[1:length(outliers$regcoeff)] <- outliers$regcoeff
    outliers3 <- outliers3[1:3]
  }

  # Residual Seasonality and TD Effects
  res_seas <- x$output$diagnostics$residuals_test[
    "f-test on sa (seasonal dummies)", ]
  res_seas <- ifelse(res_seas$P.value < 0.05, "Yes", "No")
  res_td <- x$output$diagnostics$residuals_test["f-test on sa (td)", ]
  res_td <- ifelse(res_td$P.value < 0.05, "Yes", "No")

  # Arima Model
  arma <- x$output$regarima$arma
  bpbdbq <- paste0("(", arma[["bp"]], " ", arma[["bd"]], " ", arma[["bq"]], ")")

  QrEntries <- list(tsName = tsName,
       Period = frequency(x$ts),
       Nobs = length(x$ts),
       Start = start(x$ts), # adjust date format
       End = end(x$ts), # adjust date format
       `Log-Transformation` =
         x$output$regarima$model$spec_rslt$`Log transformation`, #Yes/No/empty
       `ARIMA Model` = bpbdbq,
       # Calendar effects
       LeapYear = x$output$regarima$model$spec_rslt$`Leap year`,
       MovingHoliday = x$output$regarima$model$spec_rslt$Easter,
       NbTD = x$output$regarima$model$spec_rslt$`Trading days`, # numeric (>=0)
       Noutliers = x$output$regarima$model$spec_rslt$Outliers,
       # 3 main (most significant) outliers
       Outlier1 = outliers3[1],
       Outlier2	= outliers3[2],
       Outlier3 = outliers3[3],
       CombinedTest_SI =
         x$output$diagnostics$combined_test$combined_seasonality_test,
       `Residual Seasonality` = res_seas,
       `Residual TD Effect` = res_td,
       `Q-Stat` = x$output$decomposition$mstats["Q", ],
       # Filters used by X12-related methods (Not to be filled by countries
       # using Tramo-Seats)
       `Final Henderson Filter` = x$output$decomposition$t_filter,
       `Stage 2 Henderson Filter` = "?",
       `Seasonal Filter` = x$output$decomposition$s_filter,
       Quality = "?",
       `Max-Adj` = "?")

  return(QrEntries)
}
