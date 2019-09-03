context("adjusted")

test_that("adjusted", {
  obj_x13 <- per_x13(AirPassengers, "RSA3")

  ht <- per_hts(a = obj_x13, b = obj_x13)
  expect_true(is.null(ht$adjusted_direct))
  expect_true(is.null(ht$adjusted_indirect))
  ht$run()
  expect_s3_class(ht$adjusted_direct, "ts")
  expect_s3_class(ht$adjusted_indirect, "ts")
  expect_identical(tsp(ht$adjusted_direct), tsp(ht$adjusted_indirect))
})

context("nested hierarchies")

test_that("nested hierarchies", {
  obj_x13 <- per_x13(AirPassengers, "RSA3")
  ht <- per_hts(a = obj_x13, b = obj_x13)
  ht2 <- per_hts(a = ht, b = obj_x13)
  ht2$run()
  expect_s3_class(ht2$adjusted_direct, "ts")
  expect_s3_class(ht2$adjusted_indirect, "ts")
  expect_identical(tsp(ht2$adjusted_direct), tsp(ht2$adjusted_indirect))
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
  expect_true(is.null(obj_x13$adjusted_direct))
  ht$run()
  expect_s3_class(obj_x13$adjusted_direct, "ts")
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


context("hierachical weights")

test_that("weights as scalars", {
  obj_x13 <- per_x13(AirPassengers, "RSA3")
  ht <- per_hts(a = obj_x13,
                b = obj_x13,
                c = obj_x13,
                weights = c(2, 2, 2)
                )

  expect_true(all.equal(ht$ts, obj_x13$ts))
  expect_true(all(ht$weights == 2))
  ht$run()
  expect_true(all.equal(ht$components$a$adjusted_direct, ht$adjusted_indirect))

  ht2 <- per_hts(a = obj_x13,
                 b = obj_x13,
                 c = obj_x13,
                 weights = c(3, 3, 3)
  )
  expect_true(all.equal(ht2$ts, obj_x13$ts))
  expect_true(all(ht2$weights == 3))

  ht0 <- per_hts(a0 = ht,
                 b0 = ht2)

  expect_true(all.equal(ht0$ts, obj_x13$ts))

  ht0$run()
  expect_true(all.equal(
    ht0$adjusted_indirect,
    ht0$components$a0$components$a$adjusted_direct))

  expect_true(is.character(all.equal(
    ht0$adjusted_direct,
    ht0$components$a0$components$a$adjusted_direct)))
})


test_that("weights as mts", {
  obj_x13 <- per_x13(AirPassengers, "RSA3")
  w2 <- ts(2, start = start(AirPassengers),
           end = end(AirPassengers),
           frequency = frequency(AirPassengers))
  w3 <- ts(3, start = start(AirPassengers),
           end = end(AirPassengers),
           frequency = frequency(AirPassengers))
  ht <- per_hts(a = obj_x13,
                b = obj_x13,
                c = obj_x13,
                weights = cbind(w2, w2, w2)
  )

  expect_true(all.equal(ht$ts, obj_x13$ts))
  expect_true(all(ht$weights == 2))
  ht$run()
  expect_true(all.equal(ht$components$a$adjusted_direct, ht$adjusted_indirect))

  ht2 <- per_hts(a = obj_x13,
                 b = obj_x13,
                 c = obj_x13,
                 weights = cbind(w3, w3, w3)
  )
  expect_true(all.equal(ht2$ts, obj_x13$ts))
  expect_true(all(ht2$weights == 3))

  ht0 <- per_hts(a0 = ht,
                 b0 = ht2)

  expect_true(all.equal(ht0$ts, obj_x13$ts))

  ht0$run()
  expect_true(all.equal(
    ht0$adjusted_indirect,
    ht0$components$a0$components$a$adjusted_direct))

  expect_true(is.character(all.equal(
    ht0$adjusted_direct,
    ht0$components$a0$components$a$adjusted_direct)))
})


test_that("weights as list", {
  obj_x13 <- per_x13(AirPassengers, "RSA3")
  w2 <- ts(2, start = start(AirPassengers),
           end = end(AirPassengers),
           frequency = frequency(AirPassengers))
  w3 <- ts(3, start = start(AirPassengers),
           end = end(AirPassengers),
           frequency = frequency(AirPassengers))
  ht <- per_hts(a = obj_x13,
                b = obj_x13,
                c = obj_x13,
                weights = list(w2, w2, w2)
  )

  expect_true(all.equal(ht$ts, obj_x13$ts))
  expect_true(all(ht$weights == 2))
  ht$run()
  expect_true(all.equal(ht$components$a$adjusted_direct, ht$adjusted_indirect))

  ht2 <- per_hts(a = obj_x13,
                 b = obj_x13,
                 c = obj_x13,
                 weights = list(w3, w3, w3)
  )
  expect_true(all.equal(ht2$ts, obj_x13$ts))
  expect_true(all(ht2$weights == 3))

  ht0 <- per_hts(a0 = ht,
                 b0 = ht2)

  expect_true(all.equal(ht0$ts, obj_x13$ts))

  ht0$run()
  expect_true(all.equal(
    ht0$adjusted_indirect,
    ht0$components$a0$components$a$adjusted_direct))

  expect_true(is.character(all.equal(
    ht0$adjusted_direct,
    ht0$components$a0$components$a$adjusted_direct)))
})



test_that("weights as scalars - unequal", {
  ht1 <- per_hts(a = per_x13(AirPassengers * 0 + 1, "RSA3"),
                b = per_x13(AirPassengers * 0 + 1, "RSA3"),
                c = per_x13(AirPassengers * 0 + 1, "RSA3"),
                weights = c(1, 2, 3)
  )

  expect_true(all.equal(ht1$ts, AirPassengers * 0 + 1))

  ht2 <- per_hts(a = per_x13(AirPassengers * 0 + 100, "RSA3"),
                b = per_x13(AirPassengers * 0 + 200, "RSA3"),
                c = per_x13(AirPassengers * 0 + 400, "RSA3"),
                weights = c(1, 2, 4)
  )

  expect_true(all.equal(ht2$ts, AirPassengers * 0 + 300))


  ht0 <- per_hts(a0 = ht1,
                 b0 = ht2)

  expect_true(all.equal(ht0$ts, AirPassengers * 0 + 162))

})
