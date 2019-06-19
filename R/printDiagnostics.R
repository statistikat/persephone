printDiagnostics <- function(x) {
  if (is.null(x$output))
    return(data.frame(
      seasonality = NA, log_transform = NA, arima_mdl = NA,
      n_outliers = NA, q_stat = NA
    ))

  arma <- x$output$regarima$arma
  pdq <- paste0("(", arma[["p"]], " ", arma[["d"]], " ", arma[["q"]], ")")
  bpbdbq <- paste0("(", arma[["bp"]], " ", arma[["bd"]], " ", arma[["bq"]], ")")
  q_stat <- x$output$decomposition$mstats["Q", ]

  data.frame(
    # seasonality: placeholder (test for stable seasonality)
    seasonality = x$output$diagnostics$combined_test$combined_seasonality_test,
    log_transform = x$output$regarima$model$spec_rslt$`Log transformation`,
    arima_mdl = paste0(pdq, bpbdbq),
    n_outliers = x$output$regarima$model$spec_rslt$Outliers,
    q_stat = ifelse(is.null(q_stat), NA, q_stat)
  )
}
