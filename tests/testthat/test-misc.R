context("misc")

test_that("is.persephone", {
  obj <- x13Single$new(AirPassengers)
  expect_true(is.persephone(obj))
  expect_false(is.persephone(1L))
})

test_that("implement this function", {
  obj <- x13Single$new(AirPassengers)
  superClass <- obj$.__enclos_env__$super
  expect_error(superClass$initialize(), "implement this function")
  expect_error(superClass$run(), "implement this function")
  expect_error(superClass$updateFun(2L), "implement this method")
  expect_error(superClass$print(), NA)
})

test_that("generateQrList", {
  obj <- x13Single$new(AirPassengers)
  expect_error(persephone:::generateQrList(obj),
               "No results from run available.\n")
  obj$run()
  persephone:::generateQrList(obj)

  obj <- per_tramo(UKgas)
  obj$run()
  persephone:::generateQrList(obj)
})
