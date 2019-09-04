#' Generation of centered trading day regressor
#'
#' @param ff frequency of series
#' @param fYear first year of calculation of trading day regressor
#' @param lYear last year of calculation of trading day regressor
#' @param ft holidays, list of 1. exact date ("mm-dd") and/or 2. name of day
#'   (listHolidays() from timeDate function) and/or 3. easter relation
#'   (e.g. "easter+39")
#' @param ww vector of individual weights for each holiday with length of ft
#' @param reg seven day of a week regressor (reg = "td") or weekday/weekend
#'   regressor (reg = "wd"). Not yet implemented.
#' @return matrix of centered trading day variables
#'
#' @importFrom stats ts
#' @importFrom timeDate Easter
#' @importFrom data.table as.data.table .SD
#' @export

genTd <- function(ff = 12, fYear = 1960, lYear = 2099, ft, ww, reg = NULL) {
  y <- ts(frequency = ff, start = c(fYear, 1), end = c(lYear, ff))
  dNam <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
  preDef <- timeDate::listHolidays()
  #ft <- list("NewYearsDay","Epiphany","EasterMonday","LaborDay",
  # "PentecostMonday","Ascension",
  #  "CorpusChristi","AssumptionOfMary","10-26","AllSaints",
  # "ITImmaculateConception","ChristmasEve",
  #  "ChristmasDay","BoxingDay","12-31")
  #ww <- rep(1,length(ft))
  #ww[c(12,15)] <- 0.5

  # dat: List of dates of holidays from 1960-2099
  dat <- lapply(ft, function(x){
    if (substr(x, 1, 7) == "easter+") {
      a <- as.data.table(as.Date(Easter(year = start(y)[1]:end(y)[1])) +
                           as.numeric(substr(x, 8, nchar(x))))
    } else if (substr(x, 1, 7) == "easter-") {
      a <- as.data.table(as.Date(Easter(year = start(y)[1]:end(y)[1])) -
                           as.numeric(substr(x, 8, nchar(x))))
    } else if (x %in% preDef) {
      a <- as.data.table(as.Date(get(x)(year = start(y)[1]:end(y)[1])))
    } else {
      a <- as.data.table(as.Date(paste0(seq(start(y)[1], end(y)[1]),
                                        paste0("-", x))))
    }
  })
  names(dat) <- unlist(ft)

  dd <- matrix(nrow = length(y), ncol = 7, dimnames = c(list(NULL, dNam)))
  td <- matrix(nrow = length(y), ncol = 7,
               dimnames = c(list(NULL, c(dNam[1:6], "wd5"))))

  for (ii in seq_along(y)) {
    ti <- time(y)[ii]
    fti <- floor(ti)
    si <- round(ff * (ti - fti)) + 1

    d0 <- as.Date(zoo::as.yearmon(paste(c(fti, si), collapse = "-")), frac = 0)
    dN <- as.Date(zoo::as.yearmon(paste(c(fti, si), collapse = "-")), frac = 1)

    rT <- seq(from = d0, to = dN, by = "day")
    days <- weekdays(rT, abbreviate = TRUE)
    dd[ii, ] <- as.vector(table(factor(days, levels = dNam)))
    rB1 <- lapply(dat, function(x) {
      weekdays(rT[which(rT %in% x$V1)], abbreviate = TRUE)
    })
    ftWeight <- lapply(seq_along(rB1), function(ii) {
      x <- rB1[[ii]]
      table(factor(x, levels = dNam)) * ww[ii]
    })
    a <- rowSums(matrix(unlist(ftWeight), nrow = 7))
    dd[ii, ] <- dd[ii, ] - a
    dd[ii, 7] <- dd[ii, 7] + sum(a[1:7])

    td[ii, 1:6] <- c(
      dd[ii, 1] - dd[ii, 7], dd[ii, 2] - dd[ii, 7], dd[ii, 3] - dd[ii, 7],
      dd[ii, 4] - dd[ii, 7], dd[ii, 5] - dd[ii, 7], dd[ii, 6] - dd[ii, 7])
    td[ii, 7] <- sum(dd[ii, 1:5]) - (5 / 2) * sum(dd[ii, 6:7])
  }
  td <- as.data.table(td)
  td1 <- td[, lapply(.SD, function(x) x - mean(x))]
  tt <- as.Date(time(y))
  td$date <- substr(as.character(tt), 1, 7)
  td1$date <- substr(as.character(tt), 1, 7)
  row.names(dd) <- substr(as.character(tt), 1, 7)
  days <- list(dd, td, td1)
  return(days[])
}
