persephoneSingle <- R6::R6Class(
  "persephoneSingle",
  inherit = persephone,
  public = list(
    initialize = function(ts, template = "RSA0", userdefined = NULL, ...) {
      private$ts_internal <- ts
      private$tsp_internal <- tsp(ts)
      private$userdefined <- union(userdefined, userdefined_default)
      private$params_internal <- private$updateFun(spec = template, ...)
    },
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
