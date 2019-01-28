context("test-plot")

test_that("plotting works", {
  data(AirPassengers, package = "datasets")
  obj <- x13Single$new(AirPassengers, "RSA1")
  expect_error(plot(obj, drawPoints = TRUE), NA)

  obj$run()
  expect_error(plot(obj, drawPoints = TRUE), NA)

  obj$updateParams(
    usrdef.outliersEnabled = TRUE,
    usrdef.outliersType = c("AO", "LS", "LS"),
    usrdef.outliersDate = c("1950-01-01", "1955-04-01", "1959-10-01"))
  obj$run()
  expect_error(plot(obj), NA)

  expect_error(plot(obj, annualComparison = 1), NA)

  ## tramoseats ----

  obj <- tramoseatsSingle$new(AirPassengers)
  expect_error(plot(obj, drawPoints = TRUE), NA)

  obj$run()
  expect_error(plot(obj, drawPoints = TRUE), NA)
})

test_that("plotSIC", {
  obj <- x13Single$new(AirPassengers, "RSA1")

  obj$run()
  expect_error(plotSIC(obj), NA)
})

test_that("plotResiduals", {
  obj <- x13Single$new(AirPassengers, "RSA1")
  obj$run()
  expect_error(plotResiduals(obj), NA)
  expect_error(plotResiduals(obj, which = "acf"), NA)
})
