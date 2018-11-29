#' x13 class
#'
#' An R6 class that represents a x13 model.
#'
#' @section Inherits: [persephone]
#' @section Contructor:
#' \preformatted{
#' x13Single$new(
#'   ts, template = c("RSA5c", "RSA0", "RSA1", "RSA2c", "RSA3", "RSA4c", "X11"), ...
#' )
#' }
#' * `ts` denotes a time series
#' * `template` is passed as the `spec` argument to [x13_spec_def]
#' * `...` is passed to [x13_spec_def]
#'
#' @examples
#' library(RJDemetra)
#' data(myseries)
#'
#' obj <- x13Single$new(myseries, "RSA3")
#' obj$run()
#' obj$ts
#' @name x13Single
NULL
#' @export
x13Single <- R6::R6Class(
  "x13Single",
  inherit = persephone,
  public = list(
    initialize = function(ts, template = c(
      "RSA5c", "RSA0", "RSA1", "RSA2c", "RSA3", "RSA4c", "X11"), ...) {
      params <- x13_spec_def(spec = template)
      x13_spec(params, ...)
      private$params_internal <- params
      private$ts_internal <- ts
    },
    run = function() {
      output <- x13(private$ts_internal, private$params_internal)
      private$output_internal <- output
      output
    }
  ),
  private = list(
    updateFun = function(...) {
      x13_spec
    }
  )
)
