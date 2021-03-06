#' Class for single time series
#'
#' A common class for the wrapper classes [x13Single] and [tramoseatsSingle]
#'
#' @export
persephoneSingle <- R6::R6Class(
  "persephoneSingle",
  inherit = persephone,
  public = list(
    #' @description create a new single object
    #' @param ts a time series object
    #' @param template passed as the `spec` argument of [x13()] or
    #'   [tramoseats()]
    #' @param userdefined passed as the `userdefined` argument of [x13()] or
    #'   [tramoseats()]
    #' @param ... passed to [x13_spec()] or [tramoseats_spec()]
    initialize = function(ts, template = "RSA0", userdefined = NULL, ...) {
      private$ts_internal <- ts
      private$tsp_internal <- tsp(ts)
      private$userdefined <- union(userdefined, userdefined_default)
      private$params_internal <- private$updateFun(spec = template, ...)
    },
    #' @description run the model
    #' @param verbose wether to display outputs after the run
    run = function(verbose = FALSE) {
      output <- private$runFun(private$ts_internal, private$params_internal,
                               private$userdefined)
      private$output_internal <- output
      if (!verbose)
        invisible(output)
      else
        output
    }
  )
)
