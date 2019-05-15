library(persephone)
library(RJDemetra)
#####################     EXAMPLE 1     #########################

# userdef <- user_defined_variables(sa_object = c("X13-ARIMA"))
#
# mysa <- x13_spec(AirPassengers, spec = "RSA1", usrdef.outliersEnabled = TRUE,
#          usrdef.outliersType = c("AO","LS","LS","AO"),
#          usrdef.outliersDate = c("1950-01-01","1955-04-01","1959-10-01","1959-11-01"))
# mysa <- x13(AirPassengers,x13_spec,userdefined=userdef)
#

obj <- x13Single$new(AirPassengers, "RSA1",usrdef.outliersEnabled = TRUE,
                     usrdef.outliersType = c("AO","LS","LS","AO"),
                     usrdef.outliersDate = c("1950-01-01","1955-04-01","1959-10-01","1959-11-01"))#,
                    # userdefined=user_defined_variables(sa_object = c("X13-ARIMA")))

obj$run()
x <- obj
bla <- lapply(x$output$user_defined,print)

univariateQR <- function(x, tsName = "tsName"){
  x$ts
  x$output
  # @Regressionskoeffizienten:
  # H0: coeff=0 -> coeff nicht signifikant wenn 0 in KI liegt (coeff +-2*sterr)
  # tval=coef/sterr
  # high t-val -> significant

  # Select 3 main (most significant) outliers
  outliers3 <- rep("", 3)
  if (!is.null(x$output$regarima$regression.coefficients)) {
    regcoeff <- as.data.frame(x$output$regarima$regression.coefficients, stringsAsFactors = FALSE)
    regcoeff$regcoeff <- row.names(regcoeff)
    outliers <- regcoeff[substr(regcoeff$regcoeff, 1, 2) %in% c("AO", "LS", "TC"), ]
    outliers <- outliers[order(abs(outliers[ , "T-stat"]), decreasing = TRUE),]
    outliers3[1 : length(outliers$regcoeff)] <- outliers$regcoeff
    outliers3 <- outliers3[1 : 3]
 }
  list(tsName = tsName,
       Period = frequency(x$ts),
       Nobs = length(x$ts),
       Start = start(x$ts), # adjust date format
       End = end(x$ts), # adjust date format
       `Log-Transformation` = x$output$regarima$model$spec_rslt$`Log transformation`, #Yes/No/empty
       P = x$output$regarima$arma[["p"]],
       D = x$output$regarima$arma[["d"]],
       Q = x$output$regarima$arma[["q"]],
       BP = x$output$regarima$arma[["bp"]],
       BD = x$output$regarima$arma[["bd"]],
       BQ = x$output$regarima$arma[["bq"]],
       # Calendar effects
       LeapYear = x$output$regarima$model$spec_rslt$`Leap year`,
       #MovingHoliday = x$output$regarima$model$spec_rslt$Easter, # Nicht nur Easter, was noch?
       Easter = x$output$regarima$model$spec_rslt$Easter, # eigentlich nicht im QR
       NbTD = x$output$regarima$model$spec_rslt$`Trading days`, # numeric (>=0)
       Noutliers = x$output$regarima$model$spec_rslt$Outliers,
       # 3 main (most significant) outliers
       Outlier1 = outliers3[1],
       Outlier2	= outliers3[2],
       Outlier3 = outliers3[3],
       CombinedTest_SI = x$output$diagnostics$combined_test$combined_seasonality_test,
       `Residual Seasonality` = ,
       `Residual TD Effect` = ,
       `Q-Stat` = x$output$decomposition$mstats["Q",],
       # Filters used by X12-related methods (Not to be filled by countries using Tramo-Seats)
       `Final Henderson Filter` = x$output$decomposition$t_filter,
       `Stage 2 Henderson Filter` = ,
       `Seasonal Filter` = x$output$decomposition$s_filter,
        Quality = ,
       `Max-Adj` = ,


       )
}


