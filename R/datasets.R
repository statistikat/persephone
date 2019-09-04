#' Production in industry - Calendar adjusted, not seasonally adjusted
#'
#' A multivariate time series object containing the volume index of production (Index, 2015=100)
#' for mining and quarrying; manufacturing; electricity, gas, steam and air conditioning supply
#' for the EU-28 countries and EU-28 and EA-19 (Euro Area) aggregates.
#'
#' @docType data
#' @keywords datasets
#' @name pi_caladj
#' @usage data(pi_caladj)
#' @references Extracted on	03.09.19 from \url{https://ec.europa.eu/eurostat/web/short-term-business-statistics/data/database}
#' @format A multivariate time series objects with 30 time series of length 234.
NULL

#' Production in industry - Seasonally and calendar adjusted
#'
#' A multivariate time series object containing the volume index of production (Index, 2015=100)
#' for mining and quarrying; manufacturing; electricity, gas, steam and air conditioning supply
#' for the EU-28 countries and EU-28 and EA-19 (Euro Area) aggregates.
#'
#' @docType data
#' @keywords datasets
#' @name pi_sa
#' @usage data(pi_sa)
#' @references Extracted on	03.09.19 from \url{https://ec.europa.eu/eurostat/web/short-term-business-statistics/data/database}
#' @format A multivariate time series objects with 30 time series of length 234.
NULL

#' Production in industry - Unadjusted data (i.e. neither seasonally adjusted nor calendar adjusted data)
#'
#' A multivariate time series object containing the volume index of production (Index, 2015=100)
#' for mining and quarrying; manufacturing; electricity, gas, steam and air conditioning supply
#' for the EU-28 countries and EU-28 and EA-19 (Euro Area) aggregates.
#'
#' @docType data
#' @keywords datasets
#' @name pi_unadj
#' @usage data(pi_unadj)
#' @references Extracted on	03.09.19 from \url{https://ec.europa.eu/eurostat/web/short-term-business-statistics/data/database}
#' @format A multivariate time series objects with 30 time series of length 234.
NULL


#' Production Weightings: Countries' share as a \% of EU-28 for Total Industry (except construction)
#'
#' A dataset containing the EU-28 countries and the respective weights
#'
#' \itemize{
#'   \item country. EU-28 country
#'   \item weight. weight of the respective country
#' }
#'
#' @docType data
#' @keywords datasets
#' @name weights_pi_eu28
#' @usage data(weights_pi_eu28)
#' @references See \url{https://ec.europa.eu/eurostat/documents/2995521/9490436/4-14012019-AP-EN.pdf/9d606acd-97bc-4a5b-8c4c-a270888aab0a}
#' @format A data.frame with 28 rows and 2 columns.
NULL

#' Production Weightings: Countries' share as a \% of EA-19 for Total Industry (except construction)
#'
#' A dataset containing the EA-19 (Euro Area) countries and the respective weights
#'
#' \itemize{
#'   \item country. EU-28 country
#'   \item weight. weight of the respective country
#' }
#'
#' @docType data
#' @keywords datasets
#' @name weights_pi_ea19
#' @usage data(weights_pi_ea19)
#' @references See \url{https://ec.europa.eu/eurostat/documents/2995521/9490436/4-14012019-AP-EN.pdf/9d606acd-97bc-4a5b-8c4c-a270888aab0a}
#' @format A data.frame with 28 rows and 2 columns.
NULL
