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

context("hierarchical misc")

test_that("hierarchical generics", {
  obj_x13 <- per_x13(AirPassengers, "RSA3")
  ht <- per_hts(a = obj_x13)
  capture_output(print(ht))
  expect_true("dygraphs" %in% class(plot(ht, annualComparison = 1)))
  ht$run()
  capture_output(print(ht))
  expect_true("dygraphs" %in% class(plot(ht, annualComparison = 1)))
  obj_x132 <- per_x13(JohnsonJohnson)
  ht2 <- per_hts(a = obj_x132)

  expect_true("dygraphs" %in% class(plot(ht2, annualComparison = 1)))
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
  expect_true(all.equal(ht0$adjusted_indirect, ht0$components$a0$components$a$adjusted_direct))
  expect_true(is.character(all.equal(ht0$adjusted_direct, ht0$components$a0$components$a$adjusted_direct)))
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
  expect_true(all.equal(ht0$adjusted_indirect, ht0$components$a0$components$a$adjusted_direct))
  expect_true(is.character(all.equal(ht0$adjusted_direct, ht0$components$a0$components$a$adjusted_direct)))
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
  expect_warning(ht0$adjusted)
  expect_warning(ht0$forecasts)
  expect_true(is.character(all.equal(
    ht0$adjusted_direct,
    ht0$components$a0$components$a$adjusted_direct)))
  expect_true(all(ht0$forecasts_indirect[,"y_f"] -
                    ht0$forecasts_direct[,"y_f"] < 5))
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



test_that("hierachical input checks", {
  ht1 <- per_hts(list = list(per_x13(AirPassengers * 0 + 1, "RSA3"),
                 per_x13(AirPassengers * 0 + 1, "RSA3"),
                 per_x13(AirPassengers * 0 + 1, "RSA3"))
  )

  expect_true(all.equal(names(ht1$components), paste0("ts",1:3)))
  expect_warning(per_hts(list = list(per_x13(AirPassengers * 0 + 1, "RSA3"),
                             per_x13(AirPassengers * 0 + 1, "RSA3"),
                             per_x13(AirPassengers * 0 + 1, "RSA3")),
                  a = per_x13(AirPassengers * 0 + 100, "RSA3")))

  expect_error(per_hts(a = per_x13(AirPassengers * 0 + 100, "RSA3"),
                 b = per_x13(AirPassengers * 0 + 200, "RSA3"),
                 c = per_x13(AirPassengers * 0 + 400, "RSA3"),
                 weights = c(1, 2, 4, 5)
  ))
  ht2 <- per_hts(list = list(per_x13(AirPassengers * 0 + 1, "RSA3"),
                             per_x13(AirPassengers * 0 + 1, "RSA3"),
                             per_x13(AirPassengers * 0 + 1, "RSA3")),
                 weights = c(1, 2, 3)
  )

  expect_error(per_hts(a0 = ht1,
                 b0 = ht2))
})

test_that("output of indirect and direct and option in indirect", {
  n1 <- length(AirPassengers)
  ht1 <- per_hts(list = list(per_x13(AirPassengers + 10 * rlnorm(n1),
                                     "RSA3"),
                             per_x13(AirPassengers * 5 + 10 * rlnorm(n1),
                                     "RSA3"),
                             per_x13(AirPassengers * 10 + 10 * rlnorm(n1),
                                     "RSA3"))
  )
  ht2 <- per_hts(list = list(a = per_x13(AirPassengers * 2 + 10 * rlnorm(n1),
                                         "RSA3"),
                             b = per_x13(AirPassengers * 4 + 10 * rlnorm(n1),
                                         "RSA3"),
                             c = per_x13(AirPassengers * 6 + 10 * rlnorm(n1),
                                         "RSA3"))
  )
  ht3 <- per_hts(list = list(d = per_x13(AirPassengers * 3 + 10 * rlnorm(n1),
                                         "RSA3"),
                             e = per_x13(AirPassengers * 6 + 10 * rlnorm(n1),
                                         "RSA3"),
                             f = per_x13(AirPassengers * 9 + 10 * rlnorm(n1),
                                         "RSA3"))
  )
  ht4 <- per_hts(t0 = per_hts( a0 = ht1, b0 = ht2),
                 t1 = ht3)
  ht4$run()

  # output should be with a warning, but the output should be the adjusted_direct
  expect_warning(adj <- ht4$adjusted)
  expect_true(all.equal(adj,ht4$adjusted_direct))
  expect_true(is.character(all.equal(adj,ht4$adjusted_indirect)))

  expect_warning(fore <- ht4$forecasts)
  expect_true(all.equal(fore,ht4$forecasts_direct))
  expect_true(is.character(all.equal(fore,ht4$forecasts_indirect)))

  # output should be without a warning and equal to indirect
  ht4$indirect <- TRUE

  ht4$components$t0$indirect <- TRUE
  ht4$components$t1$indirect <- TRUE

  ht4$components$t0$components$a0$indirect <- TRUE
  ht4$components$t0$components$b0$indirect <- TRUE

  expect_true(all.equal(ht4$adjusted,ht4$adjusted_indirect))
  expect_true(is.character(all.equal(ht4$adjusted,ht4$adjusted_direct)))

  expect_true(all.equal(ht4$forecasts,ht4$forecasts_indirect))
  expect_true(is.character(all.equal(ht4$forecasts,ht4$forecasts_direct)))

  # output should be without a warning and equal to direct
  ht4$indirect <- FALSE

  ht4$components$t0$indirect <- FALSE
  ht4$components$t1$indirect <- FALSE

  ht4$components$t0$components$a0$indirect <- FALSE
  ht4$components$t0$components$b0$indirect <- FALSE

  expect_true(all.equal(ht4$adjusted, ht4$adjusted_direct))
  expect_true(is.character(all.equal(ht4$adjusted, ht4$adjusted_indirect)))

  expect_true(all.equal(ht4$forecasts, ht4$forecasts_direct))
  expect_true(is.character(all.equal(ht4$forecasts, ht4$forecasts_indirect)))

  # Only indirect on top level

  ht4$indirect <- TRUE

  ht4$components$t0$indirect <- FALSE
  ht4$components$t1$indirect <- FALSE

  ht4$components$t0$components$a0$indirect <- FALSE
  ht4$components$t0$components$b0$indirect <- FALSE

  expect_true(is.character(all.equal(ht4$adjusted, ht4$adjusted_direct)))
  expect_true(is.character(all.equal(ht4$adjusted, ht4$adjusted_indirect)))

  expect_true(is.character(all.equal(ht4$forecasts, ht4$forecasts_direct)))
  expect_true(is.character(all.equal(ht4$forecasts, ht4$forecasts_indirect)))

  # we expect it to be the sum of the direct adjusted/forecasts below
  expect_true(all.equal(ht4$adjusted,
  ht4$components$t0$adjusted_direct + ht4$components$t1$adjusted_direct))

  expect_true(all.equal(ht4$forecasts,
                        ht4$components$t0$forecasts_direct + ht4$components$t1$forecasts_direct,
                        check.attributes = FALSE))

})

test_that("PI time-invariant weights", {
## ------------------------------------------------------------------------
  data(pi_caladj, pi_sa, pi_unadj, weights_pi_ea19, weights_pi_eu28)
  pi_caladj <- pi_caladj[ , -c(1:2)]
  ts_28 <- lapply(pi_caladj, per_x13)
  non_EA19 <- weights_pi_eu28$country[which(!weights_pi_eu28$country %in% weights_pi_ea19$country)]
  w_EA19 <- weights_pi_eu28$weight[which(weights_pi_eu28$country %in% weights_pi_ea19$country)]
  w_non_EA19 <- weights_pi_eu28$weight[which(!weights_pi_eu28$country %in% weights_pi_ea19$country)]

  hts_EA19 <- per_hts(list = ts_28[weights_pi_ea19$country], method = "x13", weights=w_EA19)
# Mit zwischen Aggregat
  hts_non_EA19 <- per_hts(list = ts_28[non_EA19], weights = w_non_EA19)
  hts_EU28 <- per_hts(EA19 = hts_EA19, non_EA19 = hts_non_EA19)

#Ohne zwischen Aggregat
  hts_EA19 <- per_hts(list = ts_28[weights_pi_ea19$country], method = "x13", weights=w_EA19)
  hts_EU28b <- per_hts(list = c(list(EA19 = hts_EA19),ts_28[non_EA19]), weights = w_non_EA19)
  expect_true(all.equal(hts_EU28$ts, hts_EU28b$ts))

})



test_that("PI time-variant weights", {
  data(pi_caladj, pi_sa, pi_unadj, weights_pi_ea19, weights_pi_eu28)
  pi_caladj <- pi_caladj[ , -c(1:2)]
  ts_28 <- lapply(pi_caladj, per_x13)
  weights_pi_eu28_time <- do.call("c", apply(weights_pi_eu28,1, function(x){
    l <-  list(ts(rlnorm(nrow(pi_caladj))/5 + as.numeric(x[2]),
                  start = start(pi_caladj),
                  end = end(pi_caladj),
                  frequency = frequency(pi_caladj)))
    names(l) <- x[1]
    return(l)
  }))
  non_EA19 <- weights_pi_eu28$country[which(!weights_pi_eu28$country %in% weights_pi_ea19$country)]
  w_EA19 <- weights_pi_eu28_time[which(weights_pi_eu28$country %in% weights_pi_ea19$country)]
  w_non_EA19 <- weights_pi_eu28_time[which(!weights_pi_eu28$country %in% weights_pi_ea19$country)]

  hts_EA19 <- per_hts(list = ts_28[weights_pi_ea19$country], method = "x13", weights=w_EA19)
  # Mit zwischen Aggregat
  hts_non_EA19 <- per_hts(list = ts_28[non_EA19], weights = w_non_EA19)
  hts_EU28 <- per_hts(EA19 = hts_EA19, non_EA19 = hts_non_EA19)

  #Ohne zwischen Aggregat
  hts_EU28b <- per_hts(list = c(list(EA19 = hts_EA19),ts_28[non_EA19]), weights = w_non_EA19)
  expect_true(all.equal(hts_EU28$ts, hts_EU28b$ts))
})

test_that("PI time-variant weights mts", {
  data(pi_caladj, pi_sa, pi_unadj, weights_pi_ea19, weights_pi_eu28)
  pi_caladj <- pi_caladj[ , -c(1:2)]
  ts_28 <- lapply(pi_caladj, per_x13)
  weights_pi_eu28_time <- do.call("c", apply(weights_pi_eu28,1, function(x){
    l <-  list(ts(rlnorm(nrow(pi_caladj))/5 + as.numeric(x[2]),
                  start = start(pi_caladj),
                  end = end(pi_caladj),
                  frequency = frequency(pi_caladj)))
    names(l) <- x[1]
    return(l)
  }))
  weights_pi_eu28_time <- do.call("cbind", weights_pi_eu28_time)
  non_EA19 <- weights_pi_eu28$country[which(!weights_pi_eu28$country %in% weights_pi_ea19$country)]
  w_EA19 <- weights_pi_eu28_time[, which(weights_pi_eu28$country %in% weights_pi_ea19$country)]
  w_non_EA19 <- weights_pi_eu28_time[, which(!weights_pi_eu28$country %in% weights_pi_ea19$country)]

  hts_EA19 <- per_hts(list = ts_28[weights_pi_ea19$country], method = "x13", weights=w_EA19)
  # Mit zwischen Aggregat
  hts_non_EA19 <- per_hts(list = ts_28[non_EA19], weights = w_non_EA19)
  hts_EU28 <- per_hts(EA19 = hts_EA19, non_EA19 = hts_non_EA19)

  #Ohne zwischen Aggregat
  hts_EU28b <- per_hts(list = c(list(EA19 = hts_EA19),ts_28[non_EA19]), weights = w_non_EA19)
  expect_true(all.equal(hts_EU28$ts, hts_EU28b$ts))
})
