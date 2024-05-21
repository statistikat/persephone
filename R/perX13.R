#' x13 class
#'
#' An R6 class that represents a x13 model.
#'
#' @examples
#' data(AirPassengers, package = "datasets")
#'
#' obj <- perX13(AirPassengers, "rsa3")
#' obj$run()
#' obj$ts
#' @export
x13Single <- R6::R6Class(
  "x13Single",
  inherit = persephoneSingle,
  private = list(
    updateFun = function(...) {
      rjd3x13::x13_spec(...)
    },
    runFun = function(...) {
      rjd3x13::x13_fast(...)
    }
  )
)

#' x13 class
#'
#' An R6 class that represents a x13 model.
#'
#' @section Inherits: [persephone]
#' @param ts a time series
#' @param template passed as the `name` argument to [x13_spec()]
#' @param userdefined passed as the `userdefined` argument to [x13_fast()]
#' @param ... passed to [x13_spec()]
#' @examples
#' data(AirPassengers, package = "datasets")
#'
#' obj <- perX13(AirPassengers, "rsa3")
#' obj$run()
#' obj$ts
#' @export
perX13 <- function(ts, template = c("rsa4", "rsa0", "rsa1", "rsa2c", "rsa3", "rsa5c"),
                    userdefined = NULL, ...) {
  x13Single$new(ts, match.arg(template), userdefined, ...)
}
