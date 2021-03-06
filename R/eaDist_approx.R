#' Distribution of the dates for Easter (Approximation)
#'
#' The date of Easter (Sunday) varies between March 22nd and April 25th.
#' The sequence of Easter is periodic with a period of 5.7 mio years.
#' The distribution of this sequence can be appoximated by the following
#' table (see Eurostat)
#'
#' 22/3: 1/7 * 1/29.53059
#' 23/3: 1/7 * 2/29.53059
#' 24/3: 1/7 * 3/29.53059
#' 25/3: 1/7 * 4/29.53059
#' 26/3: 1/7 * 5/29.53059
#' 27/3: 1/7 * 6/29.53059
#' 28/3: 1/29.53059
#' 29/3: 1/29.53059
#' ... ...
#' 18/4: 1/29.53059
#' 19/4: 1/7 * (6 + 1.53059)/29.53059
#' 20/4: 1/7 * (5 + 1.53059)/29.53059
#' 21/4: 1/7 * (4 + 1.53059)/29.53059
#' 22/4: 1/7 * (3 + 1.53059)/29.53059
#' 23/4: 1/7 * (2 + 1.53059)/29.53059
#' 24/4: 1/7 * (1 + 1.53059)/29.53059
#'
#' @docType data
#' @keywords datasets
#' @name eaDist_approx
#' @usage data(eaDist_approx)
#' @references Extracted on	15.02.20 from \url{https://ec.europa.eu/eurostat/cros/system/files/jd__calendars_0.docx}
#' @format Distribution of Easter Dates
NULL
