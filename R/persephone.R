#' persephone class
#'
#' Common interface for x13 and tramoseats
#'
#' @section Methods:
#'
#' - `run()`: Runs the model
#' - `updateParams(...)`: Updates the current parameters
#'
#' @md
#' @name persephone
#' @import RJDemetra
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
      private$params_internal <- private$updateFun(params, ...)
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
