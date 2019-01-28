context("test-plot")

test_that("plotting works", {
  data(AirPassengers, package = "datasets")
  obj <- x13Single$new(AirPassengers, "RSA1")
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

  obj <- tramoseatsSingle$new(AirPassengers)
  expect_error(plot(obj, drawPoints = TRUE), NA)
  obj$updateParams(tradingdays.pftd = NA_integer_)
  obj$run()
  expect_error(plot(obj, drawPoints = TRUE), NA)
  obj$run(verbose = TRUE)
})

test_that("plotSIC", {
  obj <- x13Single$new(AirPassengers, "RSA1")
  expect_error(obj$plotSIC(), "No results from run available.\n")
  obj$run()
  expect_error(obj$plotSIC(), NA)
  expect_error(plotSIC(obj, annualComparison = 1), NA)
  expect_error(plotSIC(obj, forecasts = FALSE), NA)

})

test_that("plotResiduals", {
  obj <- x13Single$new(AirPassengers, "RSA1")
  expect_error(obj$plotResiduals(), "No results from run available.\n")
  obj$run(verbose = TRUE)
  expect_error(obj$plotResiduals(), NA)

  for (i in 2:6)
    expect_error(plotResiduals(obj, which = i), NA)

})
