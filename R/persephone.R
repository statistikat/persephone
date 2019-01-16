#' persephone class
#'
#' Common interface for x13 and tramoseats
#'
#' @section Constructor:
#'
#' - `new()`: Initializes an object. See the documentation for the derived classes for details.
#'
#' @section Methods:
#'
#' - `run(verbose = FALSE)`: Runs the model
#' - `updateParams(...)`: Updates the current parameters
#'
#' @section Read-Only fields:
#'
#' - `params`: A parameters object of class `SA_spec`. See [x13_spec] and [tramoseats_spec].
#' - `ts`: The time series.
#' - `output`: The return value from the underlying functions `x13` or `tramoseats`. This slot will
#'   be empty (`NULL`) before `run()` is called for the first time.
#'
#' @section Parameters:
#' - `verbose`: should the output from the underlying RJDemetra function be displayed after the
#'   run? If `FALSE` (the default), the output will be returned invisibly.
#' @seealso [x13Single], [tramoseatsSingle]
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
    run = function(verbose = FALSE) {
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

userdefined_default <- c(
  "y", "t", "sa", "s", "i", "cal", "y_f", "t_f", "sa_f", "s_f", "i_f",
  "cal_f", "preprocessing.model.y_f", "preprocessing.model.y_ef"
)
