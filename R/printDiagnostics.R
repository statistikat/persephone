persephone.printDiagnostics <- function(x){

  log_transform <- x$output$regarima$model$spec_rslt$`Log transformation`
  pdq <- paste0("(",x$output$regarima$arma[["p"]]," ",x$output$regarima$arma[["d"]]," ",x$output$regarima$arma[["q"]],")")
  bpbdbq <- paste0("(",x$output$regarima$arma[["bp"]]," ",x$output$regarima$arma[["bd"]]," ",x$output$regarima$arma[["bq"]],")")
  arima_mdl <- paste0(pdq,bpbdbq)
  n_outliers <- x$output$regarima$model$spec_rslt$Outliers
  seasonality <- x$output$diagnostics$combined_test$combined_seasonality_test
  diagnostics <- c(seasonality=seasonality,log_transform=log_transform,arima_mdl=arima_mdl,n_outliers=n_outliers)

  if (inherits(x$output$decomposition, "decomposition_X11")) {
    q_stat = x$output$decomposition$mstats["Q",]
    diagnostics <- c(diagnostics, q_stat)
  }

return(diagnostics)

}
