context("genTd")
test_that("genTd 1", {
  hdAT <- genTd(hd = list("NewYearsDay","Epiphany","EasterMonday","LaborDay","PentecostMonday","Ascension",
                          "CorpusChristi","AssumptionOfMary","10-26","AllSaints","ITImmaculateConception",
                          "ChristmasEve","ChristmasDay","BoxingDay","12-31"))
  expect_true(length(hdAT) == 4)
})

test_that("genTd 2", {
  hdAT1 <- genTd(hd = list("NewYearsDay","Epiphany","EasterMonday","LaborDay","PentecostMonday","Ascension",
                           "CorpusChristi","AssumptionOfMary","10-26","AllSaints","ITImmaculateConception",
                           "ChristmasEve","ChristmasDay","BoxingDay","12-31"),
                 weight = c(rep(1,11),0.6,rep(1,2),0.6))
  expect_true(length(hdAT1) == 4)
  obj_x13 <- per_x13(AirPassengers, template = "RSA3", tradingdays.option = "UserDefined",
                     usrdef.varType = "Calendar",
                     usrdef.varEnabled = TRUE, usrdef.var = hdAT1[[4]][,1:6])
  obj_x13$run()
  expect_true(all.equal(obj_x13$output$regarima$specification$regression$userdef$variables$series,
                        hdAT1[[4]][,1:6]))
})
