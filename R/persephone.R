#' persephone class
#'
#' Common interface for x13 and tramoseats
#'
#' @section Methods:
#'
#' - `run()`: Runs the model
#' - `updateParams(...)`: Updates the current parameters
#'
#' @section Read-Only fields:
#' * params
#' * ts
#' * output
#' @name persephone
#' @import RJDemetra
NULL
#' @export
persephone <- R6::R6Class(
  "persephone",
  public = list(
    initialize = function() {
      stop("implement this function")
    },
    run = function() {
      stop("implement this function")
    },
    updateParams = function(...) {
      private$params_internal <- private$updateFun(self$params, ...)
    },
    # plot = function(a = 1) {
    #   paste("plot", a)
    # },
    print = function() {

    }
  ),
  ## read-only access to params, ts, and output
  active = list(
    params = function() {
      private$params_internal
    },
    ts = function() {
      private$ts_internal
    },
    output = function() {
      private$output_internal
    }
  ),
  private = list(
    ts_internal = NULL,
    params_internal = NULL,
    output_internal = NULL,
    updateFun = function(params, ...) {
      stop("implement this method")
    }
  )
)
