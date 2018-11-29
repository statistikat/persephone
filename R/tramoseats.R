#' tramoseats class
#'
#' an R6 class that represents a tramoseats model
#'
#' @section Inherits: [persephone]
#' @section Contructor:
#' \preformatted{
#' tramoseatsSingle$new(
#'   ts, template = c("RSAfull", "RSA0", "RSA1", "RSA2", "RSA3", "RSA4", "RSA5"), ...
#' )
#' }
#' * `ts` denotes a time series
#' * `template` is passed as the `spec` argument to [tramoseats_spec_def]
#' * `...` passed to [tramoseats_spec_def]
#' @examples
#' library(RJDemetra)
#' data(myseries)
#'
#' obj <- tramoseatsSingle$new(myseries, "RSA3")
#' obj$run()
#' obj$ts
#' @name tramoseatsSingle
NULL
#' @export
tramoseatsSingle <- R6::R6Class(
  "tramoseatsSingle",
  inherit = persephone,
  public = list(
    initialize = function(ts, template = c("RSAfull", "RSA0", "RSA1", "RSA2", "RSA3",
                                           "RSA4", "RSA5"), ...) {
      params <- tramoseats_spec_def(spec = template)
      tramoseats_spec(params, ...)
      private$params_internal <- params
      private$ts_internal <- ts
    },
    run = function() {
      output <- tramoseats(private$ts_internal, private$params_internal)
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
