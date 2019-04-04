#' x13 class
#'
#' An R6 class that represents a x13 model.
#'
#' @section Inherits: [persephone]
#' @section Contructor:
#' \preformatted{
#' x13Single$new(
#'   ts, template = c("RSA5c", "RSA0", "RSA1", "RSA2c", "RSA3", "RSA4c", "X11"),
#'   userdefined=NULL, ...
#' )
#' }
#' * `ts` denotes a time series
#' * `template` is passed as the `spec` argument to [x13_spec]
#' * `userdefined` is passed as the `userdefined` argument to [x13]
#' * `...` is passed to [x13_spec]
#'
#' @examples
#' data(AirPassengers, package = "datasets")
#'
#' obj <- x13Single$new(AirPassengers, "RSA3")
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
      "RSA5c", "RSA0", "RSA1", "RSA2c", "RSA3", "RSA4c", "X11"),
      userdefined=NULL, ...) {
      userdefined <- union(userdefined, userdefined_default)
      params <- x13_spec(spec = template)
      params <- x13_spec(params, ...)
      private$params_internal <- params
      private$ts_internal <- ts
      private$userdefined <- userdefined
    },
    run = function(verbose = FALSE) {
      output <- x13(private$ts_internal, private$params_internal,
                    private$userdefined)
      private$output_internal <- output
      if (!verbose)
        invisible(output)
      else
        output
    }
  ),
  private = list(
    userdefined = NULL,
    updateFun = function(...) {
      x13_spec(...)
    }
  )
)
