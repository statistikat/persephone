#' Define a hierarchical time series
#'
#' Combine mutliple objects of persephone objects into a new persephone object.
#' The resulting time series can perform direct and indirect adjustments.
#'
#' @section Inherits: [persephone]
#' @usage NULL
#' @format NULL
#' @section Constructor:
#' \preformatted{
#' per_hts(..., method = c("tramoseats", "x13"),
#'         userdefined = NULL, spec = NULL)
#' }
#' - `...` should contain one or more `persephone` objects that use the same
#'   time instances. All elements supplied here must be named.
#' - `list` a list of `persephone` objects as alternative input to `...`.
#' - `method` specifies the method to be used. tramoseats or x13
#' - `userdefined` is passed as the userdefined argument to [tramoseats()] or
#'   [x13()]
#' - `spec` a model specification returned by [x13_spec()] or
#'   [tramoseats_spec()]
#'
#' @section Fields:
#'
#' * `$components`. A list of `persephone` objects.
#' * __`$adjusted_indirect`__. Results from indirect adjustments which means the
#'   components are first adjusted and the adjusted series are then aggregated
#'
#' @examples
#' obj_x13 <- per_x13(AirPassengers, "RSA3")
#'
#' ht <- per_hts(a = obj_x13, b = obj_x13, method = "x13")
#' ht$run()
#' ht$adjusted
#' ht$adjusted_indirect
#'
#' ht2 <- per_hts(a = ht, b = obj_x13)
#' ht2$run()
#' ht2$adjusted
#' ht2$adjusted_indirect
#'
#' #-------- example with industrial price indices -----------
#'
#' data(ipi_c_eu, package = "RJDemetra")
#' # Reducing the data set to the EU28 countries
#' ipi_eu <- ipi_c_eu[, -c(1:3, 32:37)]
#' # We now build persephone objects for the countries
#' # (the lowest level in the hierachy)
#' ts_28 <- lapply(ipi_eu, per_tramo)
#'
#' # We want to add an extra layer and split the EU28 countries in two groups
#' ht_half_europe_1  <- per_hts(list = ts_28[1:14], method = "tramoseats")
#'
#' # Alternative way to use do.call
#' ht_half_europe_2 <- do.call(per_hts, ts_28[15:28])
#'
#' # Now we generate the object for EU28
#' ht_europe <- per_hts(
#'   halfEU_1 = ht_half_europe_1,
#'   halfEU_2 = ht_half_europe_2,
#'   spec = "RSA5c",
#'   method = "x13"
#' )
#'
#' # start the seasonal adjustment
#' ht_europe$run()
#'
#' # accessing the directly and indirectly adjusted series for EU28
#' ht_europe$adjusted
#' ht_europe$adjusted_indirect
#'
#' # accessing the directly and indirectly adjusted series for half of Europe
#' ht_europe$components$halfEU_2$adjusted
#' ht_europe$components$halfEU_2$adjusted_indirect
#'
#' # accessing the adjusted series for a country
#' ht_europe$components$halfEU_2$components$AT$adjusted
#' @export
hierarchicalTimeSeries <- R6::R6Class(
  "hierarchicalTimeSeries",
  inherit = persephone,
  public = list(
    initialize = function(..., method = c("tramoseats", "x13"),
                          userdefined = NULL, spec = NULL, list = NULL) {
      private$method <- match.arg(method)
      if(!is.null(list)){
        components <- list
        if(is.null(names(components))){
          names(components) <- paste0("ts",seq_along(components))
        }
        if(length(list(...))>0){
          warning("If the list argument is specified, additional arguments as ...
                  will be ignored.")
        }
      }else{
        components <- list(...)
      }
      
      private$check_classes(components)
      names(components) <- private$coerce_component_names(components)
      private$tsp_internal <- private$check_time_instances(components)
      self$components <- components
      private$ts_internal <- private$aggregate(components)
      super$set_options(userdefined = userdefined, spec = spec)
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
      tbl <- private$print_table()
      if (all(!tbl$run))
        tbl <- tbl[, 1:3]
      print(tbl, right = FALSE, row.names = FALSE)
    },
    set_options = function(userdefined = NA, spec = NA, recursive = TRUE) {
      super$set_options(userdefined, spec, recursive)
      if (recursive)
        lapply(self$components, function(x) {
          x$set_options(userdefined, spec, recursive)
        })
      invisible(NULL)
    },
    iterate = function(fun, as_table = FALSE) {
      comp <- lapply(
        self$components,
        function(component) {
          component$iterate(fun)
        }
      )

      res <- c(super$iterate(fun), comp)
      if (as_table)
        res <- as_table_nested_list(res)
      res
    },
    get_component = function(component_id) {
      if (length(component_id) == 0 || component_id == "")
        return(self)
      component_path <- strsplit(component_id, "/")[[1]]
      direct_child <- component_path[1]
      if (length(component_path) == 1)
        return(self$components[[direct_child]])
      rest <- paste(component_path[-1], collapse = "/")
      self$components[[direct_child]]$get_component(rest)
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
    print_table = function() {
      self$iterate(printDiagnostics, as_table = TRUE)
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
        if (!is.null(parname) && parname != "")
          return(parname)
        else
          stop("all components in 'hierarchicalTimeSeries' must be named")
      })
    },
    method = NULL,
    spec = NULL,
    run_direct = function(ts) {
      methodFunction <- switch(private$method, tramoseats = tramoseats, x13 = x13)
      if (is.null(self$spec))
        spec <- switch(private$method, tramoseats = tramoseats_spec(),
                       x13 = x13_spec())
      else
        spec <- self$spec
      private$output_internal <- methodFunction(
        ts,
        spec = spec,
        userdefined = private$userdefined
      )
    }
  )
)

#' @rdname hierarchicalTimeSeries
#' @usage NULL
#' @export
per_hts <- hierarchicalTimeSeries$new
