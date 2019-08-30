context("adjusted")

test_that("adjusted", {
  obj_x13 <- per_x13(AirPassengers, "RSA3")

  ht <- per_hts(a = obj_x13, b = obj_x13)
  expect_true(is.null(ht$adjusted))
  expect_true(is.null(ht$adjusted_indirect))
  ht$run()
  expect_s3_class(ht$adjusted, "ts")
  expect_s3_class(ht$adjusted_indirect, "ts")
  expect_identical(tsp(ht$adjusted), tsp(ht$adjusted_indirect))
})

context("nested hierarchies")

test_that("nested hierarchies", {
  obj_x13 <- per_x13(AirPassengers, "RSA3")
  ht <- per_hts(a = obj_x13, b = obj_x13)
  ht2 <- per_hts(a = ht, b = obj_x13)
  ht2$run()
  expect_s3_class(ht2$adjusted, "ts")
  expect_s3_class(ht2$adjusted_indirect, "ts")
  expect_identical(tsp(ht2$adjusted), tsp(ht2$adjusted_indirect))
})

context("incompatible time instances")

test_that("incompatible time instances", {
  obj1 <- per_x13(AirPassengers, "RSA3")
  obj2 <- per_x13(JohnsonJohnson, "RSA3")
  expect_error(
    per_hts(a = obj1, b = obj2)
  )
})

context("object run by reference")

test_that("object run by reference", {
  obj_x13 <- per_x13(AirPassengers, "RSA3")
  ht <- per_hts(a = obj_x13)
  expect_true(is.null(obj_x13$adjusted))
  ht$run()
  expect_s3_class(obj_x13$adjusted, "ts")
})

context("hierarchical misc")

test_that("hierarchical generics", {
  obj_x13 <- per_x13(AirPassengers, "RSA3")
  ht <- per_hts(a = obj_x13)
  print(ht)
  plot(ht, annualComparison = 1)
  ht$run()
  print(ht)
  plot(ht, annualComparison = 1)
  obj_x132 <- per_x13(JohnsonJohnson)
  ht2 <- per_hts(a = obj_x132)
  plot(ht2, annualComparison = 1)
})

context("hierarchical misc")

test_that("components must be named", {
  obj_x13 <- per_x13(AirPassengers, "RSA3")
  expect_error(per_hts(obj_x13), "must be named")
})
