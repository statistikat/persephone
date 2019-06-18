#' Define a hierarchical time series
#'
#' Combine mutliple objects of persephone objects into a new persephone object.
#' The resulting time series can perform direct and indirect adjustments.
#'
#' @section Inherits: [persephone]
#' @section Constructor:
#' \preformatted{
#' hierarchicalTimeSeries$new(...)
#' }
#' - `...` should contain one or more `persephone` objects that use the same
#'   time instances. All lements supplied here must be named.
#' - `model` specifies a model to be used. tramoseats or x13
#' - `userdefined` is passed as the userdefined argument to `tramoseats` or
#'   `x13`
#' - `spec` a model specification returned by [x13_spec()] or
#'   [tramoseats_spec()]
#'
#' @examples
#' obj_x13 <- x13Single$new(AirPassengers, "RSA3")
#'
#' ht <- hierarchicalTimeSeries$new(a = obj_x13, b = obj_x13)
#' ht$run()
#' ht$adjusted
#' ht$adjusted_indirect
#'
#' ht2 <- hierarchicalTimeSeries$new(a = ht, b = obj_x13)
#' ht2$run()
#' ht2$adjusted
#' ht2$adjusted_indirect
#'
#' @export
hierarchicalTimeSeries <- R6::R6Class(
  "hierarchicalTimeSeries",
  inherit = persephone,
  public = list(
    initialize = function(..., model = c("tramoseats", "x13"),
                          userdefined = NULL, spec = NULL) {
      private$model <- match.arg(model)
      components <- list(...)
      private$check_classes(components)
      names(components) <- private$coerce_component_names(components)
      private$tsp_internal <- private$check_time_instances(components)
      self$components <- components
      private$ts_internal <- private$aggregate(components)
      private$userdefined <- userdefined
      private$spec <- spec
    },
    run = function(...) {
      ## indirect
      lapply(self$components, function(component) {
        component$run(...)
      })
      ## direct
      private$run_direct(self$ts)
    },
    components = NULL,
    print = function() {
      print(private$print_table())
    }
  ),
  active = list(
    adjusted_indirect = function() {
      if (is.null(self$output))
        return(NULL)
      tss <- lapply(self$components, function(component) {
        component$adjusted
      })
      private$aggregate_ts(tss)
    }
  ),
  private = list(
    check_classes = function(components) {
      lapply(components, function(component) {
        stopifnot(is.persephone(component))
      })
    },
    print_table = function(prefix = "") {
      do.call(rbind, lapply(seq_along(self$components), function(i) {
        name <- names(self$components)[[i]]
        component <- self$components[[i]]
        component$.__enclos_env__$private$print_table(
          prefix = paste0(prefix, "/", name)
        )
      }))
    },
    check_time_instances = function(components) {
      tsps <- lapply(components, function(component) {
        component$tsp
      })
      if (length(unique(tsps)) != 1)
        stop("All components need to have the same time instances")
      tsps[[1]]
    },
    aggregate = function(components) {
      tss <- lapply(components, function(component) {
        component$ts
      })
      private$aggregate_ts(tss)
    },
    aggregate_ts = function(ts_vec) {
      sum <- 0
      for (ts in ts_vec) {
        sum <- sum + ts
      }
      sum
    },
    coerce_component_names = function(components) {
      lapply(seq_along(components), function(i) {
        parname <- names(components)[i]
        if (!is.null(parname) & parname != "")
          return(parname)
        else
          stop("all components in 'hierarchicalTimeSeries' must be named")
      })
    },
    tsp_internal = NULL,
    model = NULL,
    spec = NULL,
    run_direct = function(ts) {
      modelFunction <- switch(private$model, tramoseats = tramoseats, x13 = x13)
      if (is.null(private$spec))
        private$spec <- switch(private$model, tramoseats = tramoseats_spec(),
                               x13 = x13_spec())
      private$output_internal <- modelFunction(
        ts,
        spec = private$spec,
        userdefined = union(private$userdefined, userdefined_default)
      )
    }
  )
)
