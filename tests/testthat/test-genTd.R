context("genTd")
test_that("genTd 1", {
  hdAT <- genTd(hd = list("01-01","01-06","05-01","easter+1", "easter+39",
                          "easter+50","easter+60",
                          "08-15","10-26","11-01","12-08","12-24","12-25",
                          "12-26","12-31"))
  expect_true(length(hdAT) == 3)
})

test_that("genTd 2", {
  hdAT1 <- genTd(hd = list("01-01","01-06","05-01","easter+1", "easter+39",
                           "easter+50","easter+60",
                           "08-15","10-26","11-01","12-08","12-24","12-25",
                           "12-26","12-31"),
                 weight = c(rep(1,11),0.5,rep(1,2),0.5))
  expect_true(length(hdAT1) == 3)
  expect_warning(objX13 <- perX13(AirPassengers, template = "RSA3", tradingdays.option = "UserDefined",
                                  usrdef.varType = "Calendar",
                                  usrdef.varEnabled = TRUE, usrdef.var = hdAT1[[3]]))
  objX13$run()
  expect_true(all.equal(objX13$output$regarima$specification$regression$userdef$variables$series,
                        hdAT1[[3]]))
})


test_that("genTd no easter", {
  hdAT <- genTd(hd = list("01-01","01-06","05-01"))
  expect_true(length(hdAT) == 3)
})

test_that("genTd only easter", {
  hdAT <- genTd(hd = list("easter+1", "easter+39",
                          "easter+50","easter+60"))
  expect_true(length(hdAT) == 3)
})


test_that("genTd no easter 1", {
  hdAT <- genTd(hd = list("01-01"))
  expect_true(length(hdAT) == 3)
})

test_that("genTd only easter 1", {
  hdAT <- genTd(hd = list("easter+12"))
  expect_true(length(hdAT) == 3)
})

test_that("genTd approx", {
  hdAT1 <- genTd(hd = list("01-01","easter+1"),
                 weight = c(5,4), fYear=1999, lYear=2100, adjustEaster = "approximate")
  expect_true(length(hdAT1) == 3)
})
test_that("genTd Markus quarter",{
  td7 <- genTd(ff = 4, hd = list("01-01", "01-06", "05-01", "easter+1", "easter+39",
                                 "easter+50", "easter+60", "08-15", "10-26", "11-01",
                                 "12-08", "12-24", "12-25", "12-26", "12-31"),
               weight = c(rep(1,11), 0.5, rep(1,2), 0.5))
  expect_true(length(td7) == 3)
})
