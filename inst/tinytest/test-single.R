message("single")
# change parameters X13", {
  objX13 <- perX13(AirPassengers, "RSA3")
  expect_false(objX13$params$regarima$regression$easter$enabled[3])
  objX13$updateParams(easter.enabled = TRUE)
  expect_true(objX13$params$regarima$regression$easter$enabled[3])
  objX13$updateParams(usrdef.outliersEnabled = TRUE,
                  usrdef.outliersType = c("AO","LS","LS"),
                  usrdef.outliersDate=c("1950-01-01","1955-04-01","1959-10-01"))
  expect_true(objX13$params$regarima$regression$easter$enabled[3])
#
