x13Single <- R6::R6Class(
  "x13Single",
  inherit = persephoneSingle,
  private = list(
    updateFun = function(...) {
      x13_spec(...)
    },
    runFun = function(...) {
      x13(...)
    }
  )
)

#' x13 class
#'
#' An R6 class that represents a x13 model.
#'
#' @section Inherits: `persephone`
#' @param ts a time series
#' @param template passed as the `spec` argument to [x13_spec()]
#' @param userdefined passed as the `userdefined` argument to [x13()]
#' @param ... passed to [x13_spec()]
#' @examples
#' data(AirPassengers, package = "datasets")
#'
#' obj <- per_x13(AirPassengers, "RSA3")
#' obj$run()
#' obj$ts
#' @export
per_x13 <- function(ts, template = c("RSA5c", "RSA0", "RSA1", "RSA2c", "RSA3",
                                     "RSA4c", "X11"),
                    userdefined = NULL, ...) {
  x13Single$new(ts, match.arg(template), userdefined, ...)
}
