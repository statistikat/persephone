#' @importFrom stats start end
generateQrList <- function(x, tsName = "tsName"){

  if (is.null(x$output$user_defined)) {
    stop("No results from run available.\n")
  }

  if (inherits(x$output$decomposition, "decomposition_X11")) {

    # Method
    method <- "x13"

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

    if(grepl("-",x$output$decomposition$t_filter)){
      final_henderson <- paste0("H",unlist(strsplit(x$output$decomposition$t_filter, "-" , fixed=TRUE))[1])
    }else{
      final_henderson <- paste0("H",unlist(strsplit(x$output$decomposition$t_filter, " " , fixed=TRUE))[1])
    }
    seasonal_filter <- x$output$decomposition$s_filter
    q_stat <- round(x$output$decomposition$mstats["Q", ],digits=2)
  } else {
    method <- "TS"
    stage2_henderson <- NA
    final_henderson <- NA
    seasonal_filter <- NA
    q_stat <- NA
  }

  if (frequency(x$ts) == 12) {
    start_ts = paste0(start(x$ts)[2],"-", start(x$ts)[1])
    end_ts = paste0(end(x$ts)[2],"-", end(x$ts)[1])
  } else if (frequency(x$ts) == 4) {
    start_ts = paste0(start(x$ts)[2],"-", start(x$ts)[1])
    end_ts = paste0(end(x$ts)[2],"-", end(x$ts)[1])
  }
  # Select 3 main (most significant) outliers
  outliers3 <- rep(NA, 3)
  if (!is.null(x$output$regarima$regression.coefficients)) {
    regcoeff <- as.data.frame(x$output$regarima$regression.coefficients,
                              stringsAsFactors = FALSE)
    regcoeff$regcoeff <- row.names(regcoeff)
    outliers <- regcoeff[substr(regcoeff$regcoeff, 1, 2) %in%
                           c("AO", "LS", "TC"), ]
    if (nrow(outliers) > 0) {
      outliers <- outliers[order(abs(outliers[, "T-stat"]), decreasing = TRUE), ]
      outliers3[1:length(outliers$regcoeff)] <- outliers$regcoeff
      outliers3 <- outliers3[1:3]
    }
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


  # Indicator of the "size" of the seasonal and calendar adjustment
  max_adj <- 100 * max(abs((x$ts - x$adjusted) / x$ts))

  QrEntries <- list(tsName = tsName,
                    Method = method,
                    Period = frequency(x$ts),
                    Nobs = length(x$ts),
                    Start = start_ts,
                    End = end_ts,
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
                    `Q-Stat` = q_stat,
                    # Filters used by X12-related methods (Not to be filled by countries
                    # using Tramo-Seats)
                    `Final Henderson Filter` = final_henderson,
                    `Stage 2 Henderson Filter` = stage2_henderson, # needs verification
                    `Seasonal Filter` = seasonal_filter,
                    # Quality = "?", # no real documentation for JD+ global quality indicator
                    `Max-Adj` = paste0(round(max_adj), "%"))

  return(QrEntries)
}
