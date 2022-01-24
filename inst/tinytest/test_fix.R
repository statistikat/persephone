message("Fix Arima")

# fix model single", {
  objX13 <- perX13(AirPassengers, "RSA3")
  expect_warning(objX13$fixModel())
  objX13$run()
  expect_message(objX13$fixModel(verbose= TRUE))
  objX13$run()
  expect_false(objX13$output$regarima$specification$arima$specification$enabled)
#


# fix model batch", {
  objX13 <- perX13(AirPassengers, "RSA3")
  bt <- perBatch(a=objX13,b=objX13)
  expect_warning(bt$fixModel())
  bt$run()
  expect_message(bt$fixModel(verbose= TRUE))
  bt$run()
  expect_false(bt$components[[1]]$output$regarima$specification$arima$specification$enabled)
  expect_false(bt$components[[2]]$output$regarima$specification$arima$specification$enabled)
#

# fix model hts", {
  objX13 <- perX13(AirPassengers, "RSA3")
  ht <- perHts(a=objX13,b=objX13, method = "x13")
  expect_warning(ht$fixModel())
  ht$run()
  expect_message(ht$fixModel(verbose= TRUE))
  ht$run()
  expect_false(ht$components[[1]]$output$regarima$specification$arima$specification$enabled)
  expect_false(ht$components[[2]]$output$regarima$specification$arima$specification$enabled)
  expect_false(ht$output$regarima$specification$arima$specification$enabled)
#

context("Fix Outlier")

# fix outlier single", {
  AirPassengersOut <- AirPassengers
  AirPassengersOut[10] <- AirPassengersOut[10] * 10
  AirPassengersOut[20:26] <- AirPassengersOut[20:26] * 3
  objX13 <- perX13(AirPassengersOut, "RSA3")
  objX13$updateParams(usrdef.outliersEnabled = TRUE,
                      usrdef.outliersType = c("AO"),
                      usrdef.outliersDate = c("1949-03-01"))
  expect_warning(objX13$fixOutlier())
  objX13$run()
  expect_message(objX13$fixOutlier(verbose= TRUE))
  objX13$run()
  expect_equal(nrow(objX13$output$regarima$specification$regression$userdef$outliers),4L)
#

# fix outlier single quarterly", {
  df <- data.frame(value=as.vector(AirPassengers), time=time(AirPassengers))
  df$year <- floor(df$time)
  df$quarter <- rep(rep(1:4,each=3),12)
  AirPassengersQuarterly <- ts(aggregate(df$value,by = df[,c("year","quarter")], FUN = sum)$x,
     start = c(1949,1), frequency = 4)

  AirPassengersOut <- AirPassengersQuarterly
  AirPassengersOut[10] <- AirPassengersOut[10] * 10
  AirPassengersOut[14:18] <- AirPassengersOut[14:18] * 3
  objX13 <- perX13(AirPassengersOut, "RSA3")
  objX13$updateParams(usrdef.outliersEnabled = TRUE,
                      usrdef.outliersType = c("AO"),
                      usrdef.outliersDate = c("1949-01-01"))
  expect_warning(objX13$fixOutlier())
  objX13$run()
  expect_message(objX13$fixOutlier(verbose= TRUE))
  objX13$run()
  expect_equal(nrow(objX13$output$regarima$specification$regression$userdef$outliers),6L)
#

# fix outlier batch", {
  AirPassengersOut <- AirPassengers
  AirPassengersOut[10] <- AirPassengersOut[10] * 10
  AirPassengersOut[20:26] <- AirPassengersOut[20:26] * 3
  objX13 <- perX13(AirPassengers, "RSA3")
  objX13out <- perX13(AirPassengersOut, "RSA3")
  bt <- perBatch(noout = objX13,
                 out = objX13out)
  expect_warning(bt$fixOutlier())
  bt$updateParams(usrdef.outliersEnabled = TRUE,
                      usrdef.outliersType = c("AO"),
                      usrdef.outliersDate = c("1949-03-01"))
  bt$run()
  expect_message(bt$fixOutlier(verbose= TRUE))
  bt$run()
  expect_equal(nrow(bt$components$noout$output$regarima$specification$regression$userdef$outliers),1L)
  expect_equal(nrow(bt$components$out$output$regarima$specification$regression$userdef$outliers),4L)
#

# fix outlier hts", {
  AirPassengersOut <- AirPassengers
  AirPassengersOut[10] <- AirPassengersOut[10] * 10
  AirPassengersOut[20:26] <- AirPassengersOut[20:26] * 3
  objX13 <- perX13(AirPassengers, "RSA3")
  objX13out <- perX13(AirPassengersOut, "RSA3")
  ht <- perHts(noout = objX13,
                 out = objX13out, method ="x13", spec=x13_spec("RSA3"))
  expect_warning(ht$fixOutlier())
  ht$run()
  ht$updateParams(usrdef.outliersEnabled = TRUE,
                  usrdef.outliersType = c("AO"),
                  usrdef.outliersDate = c("1949-03-01"))
  ht$run()
  expect_message(ht$fixOutlier(verbose= TRUE))
  ht$run()
  expect_equal(nrow(ht$components$noout$output$regarima$specification$regression$userdef$outliers),1L)
  expect_equal(nrow(ht$components$out$output$regarima$specification$regression$userdef$outliers),4L)
  expect_equal(nrow(ht$output$regarima$specification$regression$userdef$outliers),4L)
#
