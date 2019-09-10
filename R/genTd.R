#' Generation of trading day regressors
#'
#' User defined trading day regressors are generated depending on a list of country-specific
#' holidays and associated weights. Each holiday can be given a specific weight depending on the
#' extent of the work load of that specific holiday.
#'
#' @param freq frequency of series
#' @param fYear first year of calculation of trading day regressor
#' @param lYear last year of calculation of trading day regressor
#' @param hd holidays, list of 1. exact date ("mm-dd") and/or 2. name of day
#'           (listHolidays() from timeDate function) and/or
#'           3. easter relation (e.g. "easter+39", "easter-3")
#' @param weight vector of individual weights for each holiday with length of hd
#' @return list with tree list elements. 1. matrix of trading day counts for each individual day,
#'         month and year. Holidays which are delivered through the parameter hd are assigned to the
#'         number of sundays. 2. multiple time series object with 6 non-centered trading day regressors
#'         plus 1 non-centered working day regressor (columns 1 to 6 refer to Mondays to Saturdays,
#'         colunm 7 refers to the one-regressor case of 5 working days). In case of trading day
#'         adjustment the first 6 columns have to be selected, in case of wording day adjustment
#'         only column 7 has to be selected. 3. multiple time series object with 6 centered trading
#'         day regressors plus 1 centered working day regressor
#'
#'
#' @examples
#' hdAT <- genTd(hd = list("NewYearsDay","Epiphany","EasterMonday","LaborDay","PentecostMonday","Ascension",
#'                         "CorpusChristi","AssumptionOfMary","10-26","AllSaints","ITImmaculateConception",
#'                         "ChristmasEve","ChristmasDay","BoxingDay","12-31"))
#' hdAT1 <- genTd(hd = list("NewYearsDay","Epiphany","EasterMonday","LaborDay","PentecostMonday","Ascension",
#'                         "CorpusChristi","AssumptionOfMary","10-26","AllSaints","ITImmaculateConception",
#'                         "ChristmasEve","ChristmasDay","BoxingDay","12-31"),
#'                         weight = c(rep(1,11),0.6,rep(1,2),0.6))
# myspec1 <- per_x13(AirPassengers, template = "RSA3", tradingdays.option = "None",
#                         usrdef.varEnabled = TRUE, usrdef.var = dhAT1[[3]][,1:6])
# myspec1$regarima
#'
#' @importFrom stats ts
# @importFrom timeDate listHolidays Easter
#' @import timeDate
#' @importFrom zoo as.yearmon
#' @export

genTd <- function(freq = 12, fYear = 1960, lYear = 2099, hd, weight = rep(1,length(hd))){
  y <- ts(frequency = freq, start = c(fYear, 1), end = c(lYear, freq))
  dNam <- c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")
  preDef <- listHolidays()

  # dat: List of dates of holidays from fYear - lYear
  dat <- lapply(hd,function(x){
    if(substr(x,1,7)=="easter+"){
      a <- as.Date(Easter(year =start(y)[1]:end(y)[1])) +
                           as.numeric(substr(x,8,nchar(x)))
    } else if(substr(x,1,7)=="easter-"){
      a <- as.Date(Easter(year =start(y)[1]:end(y)[1])) -
                           as.numeric(substr(x,8,nchar(x)))
    } else if(x %in% preDef){
      a <- as.Date(get(x)(year = start(y)[1]:end(y)[1]))
    } else{
      a <- as.Date(paste0(seq(start(y)[1],end(y)[1]),paste0("-",x)))
    }
  })
  dat <- do.call("c", dat)
  dat <- data.frame(dat,rep(weight, each = 1+lYear-fYear))
  # Elimination of redundant dates (e.g. 1st of May 2008 is both, Labour Day and Ascension )
  dat <- unique(dat)
  colnames(dat) <- c("dat", "weight")

  dd <- matrix(nrow=length(y),ncol=7,dimnames=c(list(NULL,dNam)))
  td <- matrix(nrow=length(y),ncol=7,dimnames=c(list(NULL,c(dNam[1:6],"wd5"))))
  dd0 <- matrix(rep(0,7), nrow=length(y),ncol=7,dimnames=c(list(NULL,dNam)))

  for (ii in seq_along(y)){
    ti <- time(y)[ii]
    fti <- floor(ti)
    si <- round(freq * (ti - fti)) + 1

    d0 <- as.Date(as.yearmon(paste(c(fti, si), collapse="-")), frac=0)
    dN <- as.Date(as.yearmon(paste(c(fti, si), collapse="-")), frac=1)
    rT <- seq(from=d0,to=dN,by="day")
    days <- weekdays(rT, abbreviate = TRUE)
    dd[ii,] <- as.vector(table(factor(days,levels=dNam)))

    moDays <- data.frame(dat=rT, days)
    moDays0 <- merge(moDays, dat, by = "dat", all.x = TRUE)
    if(any(!is.na(moDays0$weight))){
      moDays1 <- aggregate(. ~ days, data = moDays0, FUN = "sum")
      moDays1$days <- as.numeric(factor(moDays1$days, levels = dNam))
      dd0[ii, moDays1$days] <- moDays1$weight
      dd0[ii, 7] <- -sum(dd0[ii, 1:6])
      dd0[ii, ] <- dd[ii, ] - dd0[ii, ]
    } else{
      dd0[ii, ] <- dd[ii, ]
    }

    td[ii,1:6] <- c(dd0[ii, 1] - dd0[ii, 7], dd0[ii, 2] - dd0[ii, 7], dd0[ii, 3] - dd0[ii, 7],
                    dd0[ii, 4] - dd0[ii, 7], dd0[ii, 5] - dd0[ii, 7], dd0[ii, 6] - dd0[ii, 7])
    td[ii,7] <- sum(dd0[ii, 1:5]) - (5/2) * sum(dd0[ii, 6:7])
  }

  td1 <- td
  for(ii in 1:12){
    t1 <- colMeans(td[seq(ii, nrow(td), 12), 1:6, drop = FALSE])
    td1[seq(ii, nrow(td), 12),] <- td1[seq(ii, nrow(td), 12),] - t1
  }
  td1 <- ts(matrix(td1, nrow = nrow(td1), ncol = ncol(td1)), start = c(fYear, 1), frequency = freq)

  row.names(dd) <- row.names(dd1) <- row.names(td0) <- row.names(td1) <- substr(as.character(as.Date(time(y))),1,7)
  days <- list(dd0, dd1, td0, td1)
  return(days[])
}
