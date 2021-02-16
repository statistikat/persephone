printDiagnostics <- function(x) {
  if (is.null(x$output))
    return(data.frame(
      run = !is.null(x$output), class = class(x)[1],
      seasonality = NA, logTransform = NA, arimaMdl = NA,
      nOutliers = NA, qStat = NA
    ))

  arma <- x$output$regarima$arma
  pdq <- paste0("(", arma[["p"]], " ", arma[["d"]], " ", arma[["q"]], ")")
  bpbdbq <- paste0("(", arma[["bp"]], " ", arma[["bd"]], " ", arma[["bq"]], ")")
  qStat <- x$output$decomposition$mstats["Q", ]

  data.frame(
    run = !is.null(x$output),
    class = class(x)[1],
    # seasonality: placeholder (test for stable seasonality)
    seasonality = x$output$diagnostics$combined_test$combined_seasonality_test,
    logTransform = x$output$regarima$model$spec_rslt$`Log transformation`,
    arimaMdl = paste0(pdq, bpbdbq),
    nOutliers = x$output$regarima$model$spec_rslt$Outliers,
    qStat = ifelse(is.null(qStat), NA, round(qStat, digits = 2))
  )
}
