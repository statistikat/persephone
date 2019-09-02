#' persephone class
#'
#' Common interface for x13 and tramoseats
#'
#' @section Constructor:
#'
#' - `new()`: Initializes an object. See the documentation for the derived
#'   classes for details.
#'
#' @section Methods:
#'
#' - `run(verbose = FALSE)`: Runs the model
#' - `updateParams(...)`: Updates the current parameters
#' - `plot(...)`: Interactive time series plot for a persephone object, see
#'   [plot.persephoneSingle] or [plot.hierarchicalTimeSeries]
#' - `plotSeasIrrCal(...)`: Interactive plot of the seasonal component,
#'   irregular component and calendar
#' effects for a persephone object, see [plotSeasIrrCal]
#' - `plotResiduals(...)`: Several interactive plots in connection with
#'   residuals for a persephone
#' object, see [plotResiduals]
#'
#' @section Read-Only fields:
#'
#' - `params`: A parameters object of class `SA_spec`. See [x13_spec] and
#'   [tramoseats_spec].
#' - `ts`: The time series.
#' - `output`: The return value from the underlying functions `x13` or
#'   `tramoseats`. This slot will
#'   be empty (`NULL`) before `run()` is called for the first time.
#'
#' @section Parameters:
#' - `verbose`: should the output from the underlying RJDemetra function be
#'   displayed after the
#'   run? If `FALSE` (the default), the output will be returned invisibly.
#' @seealso [per_x13()], [per_tramo()]
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
    plot = function(...) {
     plot(self, ...)
    },
    plotSeasIrrCal = function(...) {
      plotSeasIrrCal(self, ...)
    },
    plotResiduals = function(...) {
      plotResiduals(self, ...)
    },
    print = function() {
      message("A persephone object")
      if (!is.null(self$output)) {
        message("Output:")
        tbl <- private$print_table("")
        tbl <- tbl[, -1]
        print(tbl, right = FALSE, row.names = FALSE)
      } else {
        message("Not yet run.")
      }
    },
    iterate = function(fun) {
      list(value = fun(self))
    },
    set_options = function(userdefined = NA,
                           spec = NA, recursive = TRUE) {
      if (is.null(userdefined) || !is.na(userdefined))
        private$userdefined <- union(userdefined, userdefined_default)
      if (is.null(spec) || !is.na(spec))
        private$spec_internal <- spec
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
    tsp = function() {
      private$tsp_internal
    },
    output = function() {
      private$output_internal
    },
    adjusted = function() {
      self$output$user_defined$sa
    },
    spec = function() {
      private$spec_internal
    }
  ),
  private = list(
    ts_internal = NULL,
    tsp_internal = NULL,
    params_internal = NULL,
    output_internal = NULL,
    userdefined = NULL,
    spec_internal = NULL,
    print_table = function(prefix) {
      cbind(
        data.frame(
          component = sub("/", "", prefix),
          class = class(self)[1],
          run = !is.null(self$output)
        ),
        printDiagnostics(self)
      )
    },
    updateFun = function(params, ...) {
      stop("implement this method")
    }
  )
)

userdefined_default <- c(
  "y", "t", "sa", "s", "i", "cal", "y_f", "t_f", "sa_f", "s_f", "i_f",
  "cal_f", "preprocessing.model.y_f", "preprocessing.model.y_ef",
  "decomposition.d6", "decomposition.d7", "decomposition.d9",
  "mode"
)
