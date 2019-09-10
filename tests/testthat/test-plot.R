context("plot.persephoneSingle")

test_that("plotting works", {
  data(AirPassengers, package = "datasets")
  obj <- per_x13(AirPassengers, "RSA1")
  expect_error(plot(obj, drawPoints = TRUE), NA)
  expect_error(plot(obj, annualComparison = 1), NA)

  obj$run()
  expect_error(obj$plot(drawPoints = TRUE), NA)

  obj$updateParams(
    usrdef.outliersEnabled = TRUE,
    usrdef.outliersType = c("AO", "LS", "LS"),
    usrdef.outliersDate = c("1950-01-01", "1955-04-01", "1959-10-01"))
  obj$run()
  expect_error(plot(obj), NA)

  expect_error(plot(obj, annualComparison = 1), NA)
  expect_error(plot(obj, showOutliers = FALSE, forecasts = FALSE), NA)

  ## tramoseats ----

  obj <- per_tramo(AirPassengers)
  expect_error(plot(obj, drawPoints = TRUE), NA)
  obj$updateParams(tradingdays.pftd = NA_integer_)
  obj$run()
  expect_error(plot(obj, drawPoints = TRUE), NA)
  obj$run(verbose = TRUE)
})

test_that("plotting quarterly works", {
  jj <- JohnsonJohnson
  jj[7] <- 100
  obj <- per_x13(jj, "RSA1")
  obj$run()
  expect_error(plot(obj, annualComparison = 1, showOutliers = TRUE), NA)
})

context("plotSeasIrrCal")

test_that("plotSeasIrrCal", {
  obj <- per_x13(AirPassengers, "RSA1")
  expect_error(obj$plotSeasIrrCal(), "No results from run available.\n")
  obj$run()
  expect_error(obj$plotSeasIrrCal(), NA)
  expect_error(plotSeasIrrCal(obj, annualComparison = 1), NA)
  expect_error(plotSeasIrrCal(obj, forecasts = FALSE), NA)
})

test_that("plotSeasIrrCal quarterly", {
  obj <- per_tramo(UKgas, "RSA3")
  obj$run()
  expect_error(plotSeasIrrCal(obj, annualComparison = 1), NA)
})

context("plotResiduals")

test_that("plotResiduals", {
  obj <- per_x13(AirPassengers, "RSA1")
  expect_error(obj$plotResiduals(), "No results from run available.\n")
  obj$run(verbose = TRUE)
  expect_error(obj$plotResiduals(), NA)

  for (i in 2:6)
    expect_error(plotResiduals(obj, which = i), NA)
})

context("other plots")

test_that("plotSIC", {
  obj <- per_x13(AirPassengers, "RSA1")
  expect_error(obj$plotSeasIrrCal(), "No results from run available.\n")
  obj$run()
  expect_error(obj$plotSeasIrrCal(), NA)
  expect_error(plotSeasIrrCal(obj, annualComparison = 1), NA)
  expect_error(plotSeasIrrCal(obj, forecasts = FALSE), NA)

})

test_that("SIRatios", {
  obj <- per_x13(AirPassengers, "RSA1")
  expect_error(plotSiRatios(obj), "No results from run available.\n")
  obj$run()
  expect_error(plotSiRatios(obj), NA)

  obj2 <- per_tramo(UKgas, "RSA3")
  obj2$run()
  expect_error(plotSiRatios(obj2), NA)
})

test_that("autoplot", {
  obj <- per_x13(AirPassengers, "RSA1")
  expect_true("ggplot" %in% class(autoplot(obj)))
})

test_that("plotSpectrum", {
  obj <- per_x13(AirPassengers, "RSA1")
  expect_error(plotSpectrum(obj), "No results from run available.\n")

  obj$run()
  plotSpectrum(obj)
  plotSpectrum(obj, plotType = "arSpecBars", tsType = "sa")

  obj2 <- per_tramo(UKgas, "RSA3")
  obj2$run()
  plotSpectrum(obj2, plotType = "periodogram", maxobs = 10,
               tsType = "irregular")
  plotSpectrum(obj2, tsType = "residuals", plotType = 1)

  obj3 <- per_tramo(window(UKgas, end = c(1972, 1)))
  obj3$run()
  expect_error(
    plotSpectrum(obj3, tsType = 1),
    "The minimum number of observations needed to compute the spectrum"
  )
})
