context("adjusted")

test_that("adjusted", {
  obj_x13 <- x13Single$new(AirPassengers, "RSA3")

  ht <- hierarchicalTimeSeries$new(a = obj_x13, b = obj_x13)
  expect_true(is.null(ht$adjusted))
  expect_true(is.null(ht$adjusted_indirect))
  ht$run()
  expect_s3_class(ht$adjusted, "ts")
  expect_s3_class(ht$adjusted_indirect, "ts")
  expect_identical(tsp(ht$adjusted), tsp(ht$adjusted_indirect))
})

context("nested hierarchies")

test_that("nested hierarchies", {
  obj_x13 <- x13Single$new(AirPassengers, "RSA3")
  ht <- hierarchicalTimeSeries$new(a = obj_x13, b = obj_x13)
  ht2 <- hierarchicalTimeSeries$new(a = ht, b = obj_x13)
  ht2$run()
  expect_s3_class(ht2$adjusted, "ts")
  expect_s3_class(ht2$adjusted_indirect, "ts")
  expect_identical(tsp(ht2$adjusted), tsp(ht2$adjusted_indirect))
})

context("incompatible time instances")

test_that("incompatible time instances", {
  obj1 <- x13Single$new(AirPassengers, "RSA3")
  obj2 <- x13Single$new(JohnsonJohnson, "RSA3")
  expect_error(
    hierarchicalTimeSeries$new(a = obj1, b = obj2)
  )
})

context("object run by reference")

test_that("object run by reference", {
  obj_x13 <- x13Single$new(AirPassengers, "RSA3")
  ht <- hierarchicalTimeSeries$new(a = obj_x13)
  expect_true(is.null(obj_x13$adjusted))
  ht$run()
  expect_s3_class(obj_x13$adjusted, "ts")
})

context("hierarchical misc")

test_that("hierarchical generics", {
  obj_x13 <- x13Single$new(AirPassengers, "RSA3")
  ht <- hierarchicalTimeSeries$new(a = obj_x13)
  print(ht)
  plot(ht, annualComparison = 1)
  ht$run()
  print(ht)
  plot(ht, annualComparison = 1)
  obj_x132 <- x13Single$new(JohnsonJohnson)
  ht2 <- hierarchicalTimeSeries$new(a = obj_x132)
  plot(ht2, annualComparison = 1)
})

context("hierarchical misc")

test_that("components must be named", {
  obj_x13 <- x13Single$new(AirPassengers, "RSA3")
  expect_error(hierarchicalTimeSeries$new(obj_x13), "must be named")
})
