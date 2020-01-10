#' Generation of trading day regressors
#'
#' User defined trading day regressors are generated depending on a list of
#' country-specific holidays and associated weights. Each holiday can be given
#' a specific weight depending on the extent of the work load of that specific holiday.
#'
#' @param freq frequency of series
#' @param fYear first year of calculation of trading day regressor
#' @param lYear last year of calculation of trading day regressor
#' @param hd holidays, list of 1. exact date ("mm-dd") and/or 2. name of day
#'           (listHolidays() from timeDate function) and/or
#'           3. easter relation (e.g. "easter+39", "easter-3")
#' @param weight vector of individual weights for each holiday with length of hd
#' @return list with tree list elements. 1. matrix of trading day counts for each
#'        individual day, month and year. Holidays which are delivered through the
#'        parameter hd are assigned to the number of sundays. 2. multiple time
#'        series object with 6 non-centered trading day regressors plus 1
#'        non-centered working day regressor (columns 1 to 6 refer to Mondays to
#'        Saturdays, colunm 7 refers to the one-regressor case of 5 working days).
#'        In case of trading day adjustment the first 6 columns have to be selected,
#'        in case of wording day adjustment only column 7 has to be selected.
#'        3. multiple time series object with 6 centered trading day regressors
#'        plus 1 centered working day regressor
#'
#'
#' @examples
#' hdAT <- gen_td(hd = list("01-01","01-06","05-01","easter+1", "easter+39",
#'                         "easter+50","easter+60",
#'                          "08-15","10-26","11-01","12-08","12-24","12-25",
#'                          "12-26","12-31"),
#'                         weight = c(rep(1,11),0.5,rep(1,2),0.5))
#' obj_x13 <- per_x13(AirPassengers, template = "RSA3", tradingdays.option = "UserDefined",
#'                         usrdef.varType = "Calendar",
#'                         usrdef.varEnabled = TRUE, usrdef.var = hdAT[[4]][,1:6])
#' obj_x13$run()
#' obj_x13$output$regarima
#'
#' @importFrom stats ts
#' @importFrom zoo as.yearmon as.Date
#' @export
#'
#'
library(lubridate)
library(timeDate)
rm(list=ls())
gen_td <- function(freq = 12, fYear = 1960, lYear = 2099, hd, weight = rep(1,length(hd))){
  y <- ts(frequency = freq, start = c(fYear, 1), end = c(lYear, freq))
  dNam <- c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")

  easterRel <- which(substr(hd,1,6)=="easter")
  easterOff <- substr(hd[easterRel],7,10)
  #easterExpF <- paste("as.Date(\"2285-03-22\")", easterOff)  # Easter on March 23rd
  #easterExpL <- paste("as.Date(\"2038-04-25\")", easterOff)  # Easter on April 25th
  eMeans <- list()
  for(ii in 1:length(easterRel)){
    start <- paste("as.Date(\"2285-03-22\")", easterOff[ii])
    start <- eval(parse(text = start))
    end <-  paste("as.Date(\"2038-04-25\")", easterOff[ii])
    end <- eval(parse(text=end))
    start00 <- as.Date(paste0("2019-", substr(start,6,10)))
    end00 <- as.Date(paste0("2019-", substr(end,6,10)))
    ll <- seq(start00, end00, by = "day")
    nD <- table(substr(ll,6,7))
    j0 <- 1
    ww <- NULL
    for(jj in 1:length(nD)){
      ww0 <- sum(z[j0:cumsum(nD)[jj]]) # z ist die Verteilung der Osterdaten (35 Anteile)
      j0 <- cumsum(nD)[jj]+1
      ww <- cbind(ww, ww0)
    }
    eMeans[[ii]] <- data.frame(rel = as.numeric(ww), mon = names(nD),
                               dd = weekdays(start))
  }
  dat <- lapply(hd, function(x){
    if(substr(x, 1, 6) == "easter"){
      z <- paste("as.Date(Easter(year = start(y)[1] : end(y)[1]))",
                 substr(x, 7, nchar(x)))
      a <- eval(parse(text=z))
    } else{
      a <- as.Date(paste0(seq(start(y)[1],end(y)[1]),paste0("-",x)))
    }
  })
  datNE <- dat[-easterRel]
  dat <- do.call("c", dat)
  datNE <- do.call("c", datNE)
  dat <- data.frame(dat, rep(weight, each = 1+lYear-fYear))
  datNE <- data.frame(datNE, rep(weight[-easterRel], each = 1+lYear-fYear))
  # Elimination of redundant dates (e.g. 1st of May 2008 is both, Labour Day and Ascension )
  dat <- unique(dat)
  datNE <- unique(datNE)
  colnames(dat) <- c("dat", "weight")
  colnames(datNE) <- c("dat", "weight")
  datNE$mon <- as.numeric(substr(datNE$dat,6,7))
  aMeans <- aggregate(datNE[, 2], list(datNE$mon), sum)

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
    td[ii,1:6] <- c(dd0[ii, 1] - dd0[ii, 7], dd0[ii, 2] - dd0[ii, 7],
                    dd0[ii, 3] - dd0[ii, 7], dd0[ii, 4] - dd0[ii, 7],
                    dd0[ii, 5] - dd0[ii, 7], dd0[ii, 6] - dd0[ii, 7])
    td[ii,7] <- sum(dd0[ii, 1:5]) - (5/2) * sum(dd0[ii, 6:7])
  }

  ltermM <- matrix(0,nrow=12,ncol=6)
  colnames(ltermM) <- dNam[1:6]
  ltermM[aMeans[,1],1:6] <- aMeans[,2]/(lYear-fYear+1)
  for(ii in 1:length(eMeans)){
    row <- as.numeric(levels(eMeans[[ii]][,2]))
    col <- rep(which(dNam %in% substr(eMeans[[ii]][,3],1,3)),nrow(eMeans[[ii]]))
    val <- eMeans[[ii]][,1]
    ltermM[row, col] <- ltermM[row, col] + 2*val
    ltermM[row, -col] <- ltermM[row, -col] + val
  }
  ltermMAll <- do.call(rbind, replicate((lYear-fYear+1), ltermM, simplify=FALSE))
  td1 <- td[,1:6] + ltermMAll

  td1 <- ts(matrix(td1, nrow = nrow(td1), ncol = ncol(td1)), start = c(fYear, 1),
            frequency = freq)
  colnames(td1) <- c("Monday","Tuesday","Wednesday","Thursday","Friday",
                     "Saturday","WorkingDay")
  row.names(dd) <- row.names(dd0) <- row.names(td) <-
                                   substr(as.character(as.Date(time(y))),1,7)
  days <- list(dd, dd0, td, td1)
  return(days[])
}


pb <- txtProgressBar(min = 0, max = 57001959, style=3)
a <- Easter1(year=1960)
start <- Sys.time()
for(ii in 1961:5701959){
  a <- append(a,Easter1(year = ii))
  cat(ii,"\n")
}
end <- Sys.time()
b <- a
b0 <- as.Date(paste0("2019-",b))
save(b0,file="EasterDate.RData")
summary(b0)
test <- table(b0)
b00 <- as.numeric(test/5700000)
save(b00,file="EasterDateExact.RData")
barplot(test)
#c <- as.numeric(paste0(substr(b,2,2),substr(b,4,5)))
#d <- ifelse(c<401,c+69,c)

Easter1(2000)

Easter1 <- function(year){
a = year%%19
b = year%/%100
c = year%%100
d = b%/%4
e = b%%4
f = (b + 8)%/%25
g = (b - f + 1)%/%3
h = (19 * a + b - d - g + 15)%%30
i = c%/%4
k = c%%4
l = (32 + 2 * e + 2 * i - h - k)%%7
m = (a + 11 * h + 22 * l)%/%451
easter.month = (h + l - 7 * m + 114)%/%31
p = (h + l - 7 * m + 114)%%31
easter.day = p + 1
mm <- ifelse(easter.month<10,paste0("0",easter.month),as.character(easter.month))
dd <- ifelse(easter.day<10,paste0("0",easter.day),as.character(easter.day))
out <- paste0(mm,"-",dd)
out
}

b1 <- c((1/7)*1/29.53059, (1/7)*2/29.53059, (1/7)*3/29.53059, (1/7)*4/29.53059,
        (1/7)*5/29.53059, (1/7)*6/29.53059, 1/29.53059, 1/29.53059, 1/29.53059,
        1/29.53059, 1/29.53059, 1/29.53059, 1/29.53059, 1/29.53059, 1/29.53059,
        1/29.53059, 1/29.53059, 1/29.53059, 1/29.53059, 1/29.53059, 1/29.53059,
        1/29.53059, 1/29.53059, 1/29.53059, 1/29.53059, 1/29.53059, 1/29.53059,
        1/29.53059, (1/7)*(6+1.53059)/29.53059, (1/7)*(5+1.53059)/29.53059,
        (1/7)*(4+1.53059)/29.53059, (1/7)*(3+1.53059)/29.53059, (1/7)*(2+1.53059)/29.53059,
        (1/7)*(1+1.53059)/29.53059, (1/7)*(0+1.53059)/29.53059)
b1
save(b1, file="EasterDateApprox.RData")
b0
