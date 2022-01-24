message("misc")

# is.persephone", {
  obj <- perX13(AirPassengers)
  expect_true(is.persephone(obj))
  expect_false(is.persephone(1L))
#

# implement this function", {
  obj <- perX13(AirPassengers)
  superClass <- obj$.__enclos_env__$super$.__enclos_env__$super
  expect_error(superClass$initialize(), "implement this function")
  expect_error(superClass$run(), "implement this function")
  expect_error(superClass$updateFun(2L), "implement this method")
  expect_error(superClass$print(), NA)
#

# generateQrList", {
  obj <- perX13(AirPassengers)
  expect_error(persephone:::generateQrList(obj),
               "No results from run available.\n")
  obj$run()
  persephone:::generateQrList(obj)

  obj <- perTramo(UKgas)
  obj$run()
  persephone:::generateQrList(obj)
#
