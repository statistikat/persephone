#' tramoseats class
#'
#' an R6 class that represents a tramoseats model
#'
#' @section Inherits: [persephone]
#' @section Contructor:
#' \preformatted{
#' tramoseatsSingle$new(
#'   ts, template = c("RSAfull", "RSA0", "RSA1", "RSA2", "RSA3", "RSA4", "RSA5"),
#'   userdefined=NULL, ...
#' )
#' }
#' * `ts` denotes a time series
#' * `template` is passed as the `spec` argument to [tramoseats_spec_def]
#' * `userdefined` is passed as the `userdefined` argument to [tramoseats]
#' * `...` passed to [tramoseats_spec]
#' @examples
#' data(AirPassengers, package = "datasets")
#'
#' obj <- tramoseatsSingle$new(AirPassengers, "RSA3")
#' obj$run()
#' obj$ts
#' @name tramoseatsSingle
NULL
#' @export
tramoseatsSingle <- R6::R6Class(
  "tramoseatsSingle",
  inherit = persephone,
  public = list(
    initialize = function(ts, template = c("RSAfull", "RSA0", "RSA1", "RSA2",
                                           "RSA3", "RSA4", "RSA5"),
                          userdefined = NULL, ...) {
      userdefined <- union(userdefined, userdefined_default)
      params <- tramoseats_spec_def(spec = template)
      params <- tramoseats_spec(params, ...)
      private$params_internal <- params
      private$ts_internal <- ts
      private$userdefined <- userdefined
    },
    run = function(verbose = FALSE) {
      output <- tramoseats(private$ts_internal, private$params_internal,
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
