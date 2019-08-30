tramoseatsSingle <- R6::R6Class(
  "tramoseatsSingle",
  inherit = persephoneSingle,
  private = list(
    updateFun = function(...) {
      tramoseats_spec(...)
    },
    runFun = function(...) {
      tramoseats(...)
    }
  )
)

#' tramoseats class
#'
#' an R6 class that represents a tramoseats model
#'
#' @section Inherits: [persephone]
#' @param ts a time series
#' @param template passed as the `spec` argument to [tramoseats_spec()]
#' @param userdefined passed as the `userdefined` argument to [tramoseats()]
#' @param ... passed to [tramoseats_spec()]
#' @examples
#' data(AirPassengers, package = "datasets")
#'
#' obj <- per_tramo(AirPassengers, "RSA3")
#' obj$run()
#' obj$ts
#' @export
per_tramo <- function(ts, template = c("RSAfull", "RSA0", "RSA1", "RSA2",
                                       "RSA3", "RSA4", "RSA5"),
                      userdefined = NULL, ...) {
  tramoseatsSingle$new(ts, match.arg(template), userdefined, ...)
}
