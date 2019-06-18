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
#' - `...` One or more `persephone` objects that use the same time instances.
#'         Must be named.
#'
#' @examples
#' obj_x13 <- x13Single$new(AirPassengers, "RSA3")
#'
#' ht <- hierarchicalTimeSeries$new(obj_x13, obj_x13)
#' ht$run()
#' ht$adjusted
#' ht$adjusted_indirect
#'
#' ht2 <- hierarchicalTimeSeries$new(ht, obj_x13)
#' ht2$run()
#' ht2$adjusted
#' ht2$adjusted_indirect
#'
#' @export
hierarchicalTimeSeries <- R6::R6Class(
  "hierarchicalTimeSeries",
  inherit = persephone,
  public = list(
    initialize = function(...) {
      components <- list(...)
      private$check_classes(components)
      names(components) <- private$coerce_component_names(components)
      private$tsp_internal <- private$check_time_instances(components)
      self$components <- components
      private$ts_internal <- private$aggregate(components)
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
      tss <- lapply(self$components, function(component) {
        component$adjusted
      })
      sum <- 0
      for (ts in tss) {
        if (is.null(ts))
          return(NULL)
        sum <- sum + ts
      }
      sum
    }
  ),
  private = list(
    check_classes = function(components) {
      lapply(components, function(component) {
        stopifnot(inherits(component, "persephone"))
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
      sum <- 0
      for (ts in tss) {
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
    run_direct = function(ts) {
      ## TODO: make this more flexible
      private$output_internal <- tramoseats(
        ts,
        userdefined = userdefined_default
      )
    }
  )
)
