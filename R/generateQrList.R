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
  bpbdbq <- paste0("(", arma[["p"]], " ", arma[["d"]], " ", arma[["q"]], ")",
                   "(", arma[["bp"]], " ", arma[["bd"]], " ", arma[["bq"]], ")")

  # Stage 2 Henderson Filter
  # Seasonal Adjustment with the X-11 Method - Ladiray, Quenneville 2001, p. 72
  # D6: preliminary seasonally adjusted series, D iteration
  # D7: preliminary trend-cycle, D iteration
  prelim_sa <- x$output$user_defined$decomposition.d6
  prelim_c <- x$output$user_defined$decomposition.d7
  if (x$output$user_defined$mode == "Additive") {
    prelim_i <- prelim_sa - prelim_c
    c_bar <- 1 / (length(prelim_c) - 1) * sum(abs(diff(prelim_c, 1) - mean(diff(prelim_c, 1))))
    i_bar <- 1 / (length(prelim_i) - 1) * sum(abs(diff(prelim_i, 1) - mean(diff(prelim_i, 1))))
  } else if (x$output$user_defined$mode == "Multiplicative") {
    prelim_i <- prelim_sa / prelim_c
    c_bar <- 1 / (length(prelim_c) - 1) * sum(abs(prelim_c / lag(prelim_c, k = 1) - mean(prelim_c / lag(prelim_c, k = 1))))
    i_bar <- 1 / (length(prelim_i) - 1) * sum(abs(prelim_i / lag(prelim_i, k = 1) - mean(prelim_i / lag(prelim_i, k = 1))))
  }
  ic_ratio <- i_bar / c_bar
  if (ic_ratio < 1) {
    stage2_henderson <- "H9"
  } else if (ic_ratio >= 1 & ic_ratio < 3.5) {
    stage2_henderson <- "H13"
  } else {
    stage2_henderson <- "H23"
  }

  # Indicator of the "size" of the seasonal and calendar adjustment
  max_adj <- 100 * max(abs((x$ts - x$adjusted) / x$ts))

  QrEntries <- list(tsName = tsName,
                    # Method =
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
                    NbTD = x$output$regarima$model$spec_rslt$`Trading days`, # including lpyear -> kontrollieren
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
                    `Final Henderson Filter` = paste0("H",unlist(strsplit(x$output$decomposition$t_filter, " " , fixed=TRUE))[1]),
                    `Stage 2 Henderson Filter` = stage2_henderson, # needs verification
                    `Seasonal Filter` = x$output$decomposition$s_filter,
                    # Quality = "?", # no real documentation for JD+ global quality indicator
                    `Max-Adj` = paste0(round(max_adj), "%"))

  return(QrEntries)
}
