#' @importFrom stats start end
generateQrList <- function(x){

  # TO DO wenn persephone obj neu, das funktioniert jetzt anders.
  # if (is.null(x$output$user_defined)) {
  #   stop("No results from run available.\n")
  # }
  if(is.null(x$output)) {
    stop("No results from run available.\n")
  }
  # tramoseats/x13 distinction
  if (inherits(x$output,"JD3_X13_RSLTS")) {
    # x13() and tramoseats() generate output of class "JD3_X13_OUTPUT" and "JD3_TRAMOSEATS_OUTPUT"
    # x13_fast() and tramoseats_fast() generate output of class "JD3_X13_RSLTS" and "JD3_TRAMOSEATS_RSLTS"

    # if(class(x$output) %in% c("JD3_X13_OUTPUT","JD3_TRAMOSEATS_OUTPUT")) {
    #   x$output <- x$output$result
    # }

    # Method
    method <- "x13"

    # Stage 2 Henderson Filter
    # Seasonal Adjustment with the X-11 Method - Ladiray, Quenneville 2001,
    # p. 72
    # D6: preliminary seasonally adjusted series, D iteration
    # D7: preliminary trend-cycle, D iteration
    prelim_sa <- x$output$decomposition$d6
    prelim_c <- x$output$decomposition$d7
    if (x$output$user_defined$decomposition.mode == "Additive") {
      prelim_i <- prelim_sa - prelim_c
      c_bar <- 1 / (length(prelim_c) - 1) *
        sum(abs(diff(prelim_c) - mean(diff(prelim_c))))
      i_bar <- 1 / (length(prelim_i) - 1) *
        sum(abs(diff(prelim_i) - mean(diff(prelim_i))))
    } else if (x$output$user_defined$decomposition.mode == "Multiplicative") {
      prelim_i <- prelim_sa / prelim_c
      c_bar <- 1 / (length(prelim_c) - 1) *
        sum(abs(prelim_c / lag(prelim_c) - mean(prelim_c / lag(prelim_c))))
      i_bar <- 1 / (length(prelim_i) - 1) *
        sum(abs(prelim_i / lag(prelim_i) - mean(prelim_i / lag(prelim_i))))
    }
    ic_ratio <- i_bar / c_bar
    if (ic_ratio < 1) {
      stage2_henderson <- "H9"
    } else if (ic_ratio >= 1 & ic_ratio < 3.5) {
      stage2_henderson <- "H13"
    } else {
      stage2_henderson <- "H23"
    }


    final_henderson <- paste0("H", x$output$decomposition$final_henderson)

    seasonal_filter <- x$output$decomposition$final_seasonal
    q_stat <- round(x$output$mstats$q, digits = 2)
  } else if (inherits(x$output,"JD3_TRAMOSEATS_RSLTS")){
    method <- "TS"
    stage2_henderson <- NA
    final_henderson <- NA
    seasonal_filter <- NA
    q_stat <- NA
  }

  if (frequency(x$ts) == 12) {
    start_ts <- paste0(start(x$ts)[2], "-", start(x$ts)[1])
    end_ts <- paste0(end(x$ts)[2], "-", end(x$ts)[1])
  } else if (frequency(x$ts) == 4) {
    start_ts <- paste0(start(x$ts)[2], "-", start(x$ts)[1])
    end_ts <- paste0(end(x$ts)[2], "-", end(x$ts)[1])
  }



  variables <- x$output$preprocessing$description$variables
  vartype <- sapply(variables, function(v) v$type )

  # Select 3 main (most significant) outliers
  outliers3 <- rep(NA, 3)

  nout <- length(which(vartype %in% c("AO","TC","LS")))
  if (nout > 0) {
    outliers <- variables[which(vartype %in% c("AO","TC","LS"))]
    outliers <- as.data.frame(do.call(rbind, lapply(outliers, function(x) unlist(x))))
    if (nrow(outliers) > 0) {
      outliers <- outliers[order(abs(as.numeric(outliers[, "coef.value"])),
                                 decreasing = TRUE), ]
      outliers3[1:nout] <- outliers$name
    }
  }

  # Residual Seasonality and TD Effects
  # res_seas <- x$output$diagnostics$residuals_test[
  #   "f-test on sa (seasonal dummies)", ]
  # res_seas <- ifelse(res_seas$P.value < 0.05, "Yes", "No")
  # res_td <- x$output$diagnostics$residuals_test["f-test on sa (td)", ]
  # res_td <- ifelse(res_td$P.value < 0.05, "Yes", "No")

  # bekomme hier auch anderes Ergebnis als bei RJDemetra
  res_seas <- ifelse(x$output$diagnostics$seas.ftest.sa$pvalue < 0.05, "Yes", "No")
  res_td <- ifelse(x$output$diagnostics$td.ftest.sa$pvalue < 0.05, "Yes", "No")

  # Arima Model
  # arma <- x$output$regarima$arma
  # bpbdbq <- paste0("(", arma[["p"]], " ", arma[["d"]], " ", arma[["q"]], ")",
  #                  "(", arma[["bp"]], " ", arma[["bd"]], " ", arma[["bq"]], ")")

  # x$output$preprocessing$description$arima nicht vollständig, arbeiten also lieber mit
  # user_defined output als hier im print output herumzustochern
  bpbdbq <- paste0("(", x$output$user_defined$arima.p, " ",
                   x$output$user_defined$arima.d, " ",
                   x$output$user_defined$arima.q, ")",
                   "(", x$output$user_defined$arima.bp, " ",
                   x$output$user_defined$arima.bd, " ",
                   x$output$user_defined$arima.bq, ")")

  # Indicator of the "size" of the seasonal and calendar adjustment
  max_adj <- 100 * max(abs((x$ts - x$adjustedDirect) / x$ts))

  QrEntries <- list(
    Method = method,
    Period = frequency(x$ts),
    Nobs = length(x$ts),
    Start = start_ts,
    End = end_ts,
    `Log-Transformation` =
      x$output$preprocessing$description$log, #TF
    `ARIMA Model` = bpbdbq,
    # Calendar effects
    # Achtung, LeapYear und MovingHoliday noch testen
    LeapYear = ifelse(is.null(x$output$user_defined$regression.lp),FALSE, TRUE), # muss ich hier noch auf Signifikanz checken oder werden die eh nur inkludiert wenn signifikant?
    MovingHoliday = ifelse(is.null(x$output$user_defined$regression.easter),FALSE,TRUE),# oder "regression.leaster"?
    NbTD = x$output$user_defined$regression.ntd ,# oder `regression.td(*)`?
    # including lpyear -> kontrollieren
    Noutliers = nout,
    # 3 main (most significant) outliers
    Outlier1 = outliers3[1],
    Outlier2	= outliers3[2],
    Outlier3 = outliers3[3],
    CombinedTest_SI =
      x$output$user_defined$`diagnostics.seas-si-combined`, # oder `diagnostics.seas-si-combined3`?
    `Residual Seasonality` = res_seas,
    `Residual TD Effect` = res_td,
    `Q-Stat` = q_stat,
    # Filters used by X12-related methods (Not to be filled by countries
    # using Tramo-Seats)
    `Final Henderson Filter` = final_henderson,
    `Stage 2 Henderson Filter` = stage2_henderson, # needs verification
    `Seasonal Filter` = seasonal_filter,
    # Quality = "?", # no real documentation for JD+ global quality indicator
    `Max-Adj` = paste0(round(max_adj), "%")
  )

  return(QrEntries)
}


