#' tramoseats class
#'
#' an R6 class that represents a tramoseats model
#'
#' @examples
#' data(AirPassengers, package = "datasets")
#'
#' obj <- perTramo(AirPassengers, "RSA3")
#' obj$run()
#' obj$ts
#' @export
tramoseatsSingle <- R6::R6Class(
  "tramoseatsSingle",
  inherit = persephoneSingle,
  private = list(
    updateFun = function(...) {
      rjd3tramoseats::tramoseats_spec(...)
    },
    runFun = function(...) {
      rjd3tramoseats::tramoseats_fast(...)
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
#' obj <- perTramo(AirPassengers, "RSA3")
#' obj$run()
#' obj$ts
#' @export
perTramo <- function(ts, template = c("rsafull", "rsa0", "rsa1", "rsa2", "rsa3", "rsa4", "rsa5"),
                      userdefined = NULL, ...) {
  tramoseatsSingle$new(ts, match.arg(template), userdefined, ...)
}
