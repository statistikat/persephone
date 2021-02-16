context("batch")

test_that("adjusted", {
  objX13 <- perX13(AirPassengers, "RSA3")

  bt <- perBatch(a = objX13, b = objX13)
  expect_true(is.null(bt$components$a$adjusted))
  expect_true(is.null(bt$components$b$adjusted))
  bt$run()
  expect_identical(bt$components$a$adjusted,bt$components$b$adjusted)
  expect_true(!is.null(bt$components$b$adjusted))
  expect_s3_class(bt$components$a$adjusted, "ts")
  expect_s3_class(bt$components$b$adjusted, "ts")

  expect_true(is.list(bt$adjusted))
  expect_true(length(bt$adjusted)==2)
})


test_that("change parameters", {
  objX13 <- perX13(AirPassengers, "RSA3")

  bt <- perBatch(a = objX13, b = objX13)
  bt$updateParams(easter.enabled = FALSE)
  bt$updateParams(component = "a",
                  usrdef.outliersEnabled = TRUE,
                    usrdef.outliersType = c("AO","LS","LS"),
                      usrdef.outliersDate=c("1950-01-01","1955-04-01","1959-10-01"))
  bt$run()
  expect_false(identical(bt$components$a$adjusted,bt$components$b$adjusted))
})
