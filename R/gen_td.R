#' Generation of trading day regressors
#'
#' User defined trading day regressors are generated depending on a list of
#' country-specific holidays and associated weights. Each holiday can be given
#' a specific weight depending on the extent of the work load of that specific holiday.
#'
#' @param ff frequency of series
#' @param fYear first year of calculation of trading day regressor
#' @param lYear last year of calculation of trading day regressor
#' @param hd holidays, list of 1. exact date ("mm-dd") and/or 2. name of day
#'           (listHolidays() from timeDate function) and/or
#'           3. easter relation (e.g. "easter+39", "easter-3")
#' @param weight vector of individual weights for each holiday with length of hd
#' @param adjustEaster employ theoretical distribution of easter dates (default) or
#'               approximative distribution of easter dates for centering of
#'               trading days
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
#'                         usrdef.varEnabled = TRUE, usrdef.var = hdAT[[3]])
#' obj_x13$run()
#' obj_x13$output$regarima
#'
#' @importFrom stats ts
#' @importFrom zoo as.yearmon as.Date
#' @importFrom timeDate Easter
#' @importFrom stats aggregate
#' @importFrom utils data
#' @export
#'
#'

gen_td <- function(ff = 12, fYear = 1960, lYear = 2099, hd, weight = rep(1,length(hd)),
                   adjustEaster = 1){
  y <- ts(frequency = 12, start = c(fYear, 1), end = c(lYear, 12))
  dNam <- c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")
  if(adjustEaster){
    #data(eaDist_exact, envir = environment())
    eaDist <- eaDist_exact
  } else{
    #data(eaDist_approx, envir = environment())
    eaDist <- eaDist_approx
  }

  easterRel <- which(substr(hd,1,6)=="easter")
  easterOff <- substr(hd[easterRel],7,10)

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
      ww0 <- sum(eaDist[j0:cumsum(nD)[jj]])
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
  td <- matrix(nrow=length(y),ncol=6,dimnames=c(list(NULL,c(dNam[1:6]))))
  dd0 <- matrix(rep(0,7), nrow=length(y),ncol=7,dimnames=c(list(NULL,dNam)))
  for (ii in seq_along(y)){
    ti <- time(y)[ii]
    fti <- floor(ti)
    si <- round(12 * (ti - fti)) + 1
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
  td1 <- td + ltermMAll

  td1 <- ts(matrix(td1, nrow = nrow(td1), ncol = ncol(td1)), start = c(fYear, 1),
            frequency = 12)
  colnames(td1) <- c("Monday","Tuesday","Wednesday","Thursday","Friday",
                     "Saturday")
  td <- ts(matrix(td, nrow = nrow(td), ncol = ncol(td)), start = c(fYear, 1),
            frequency = 12)
  colnames(td) <- c("Monday","Tuesday","Wednesday","Thursday","Friday",
                     "Saturday")
  dd <- ts(matrix(dd, nrow = nrow(dd), ncol = ncol(dd)), start = c(fYear, 1),
           frequency = 12)
  colnames(dd) <- c("Monday","Tuesday","Wednesday","Thursday","Friday",
                    "Saturday", "Sunday")
  if(ff == 4){
    td1 <- aggregate(td1, nfrequency = 4)
    td <- aggregate(td, nfrequency = 4)
    dd <- aggregate(dd, nfrequency = 4)
  }
  days <- list(dd, td, td1)
  return(days[])
}

