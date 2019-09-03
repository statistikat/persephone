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
#' - `weights` either a vector
#' if the same weight is used for all time points or a list of ts objects or a mts object
#' if the weight varies for different time points. They must have the same length as
#' the number of components.
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
#' ht$adjusted_direct
#' ht$adjusted_indirect
#'
#' ht2 <- per_hts(a = ht, b = obj_x13)
#' ht2$run()
#' ht2$adjusted_direct
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
#' ht_europe$adjusted_direct
#' ht_europe$adjusted_indirect
#'
#' # accessing the directly and indirectly adjusted series for half of Europe
#' ht_europe$components$halfEU_2$adjusted_direct
#' ht_europe$components$halfEU_2$adjusted_indirect
#'
#' # accessing the adjusted series for a country
#' ht_europe$components$halfEU_2$components$AT$adjusted_direct
#' @export
hierarchicalTimeSeries <- R6::R6Class(
  "hierarchicalTimeSeries",
  inherit = persephone,
  public = list(
    initialize = function(..., method = c("tramoseats", "x13"),
                          userdefined = NULL, spec = NULL, list = NULL,
                          weights = NULL) {
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
      componentsHts <- sapply(components, function(x) "hierarchicalTimeSeries"%in%class(x))
      if(!is.null(weights)){
        if(ifelse(is.list(weights)||is.vector(weights), length(weights), ncol(weights)) != sum(!componentsHts)){
        stop("If the weights argument is provided,
             its length must be equal to the number of components.")
        }
      }
      weights_ts <- list()
      if(any(componentsHts)){
        weightsNull <- sapply(components, function(x)is.null(x$weights))
        if(any(!weightsNull)){
          if(any(weightsNull)){
            stop("At the moment it is only supported to use either weights for all
                 components or none.")
          }
          for(i in which(componentsHts)){
            weights_ts[[i]] <- ts(rowSums(components[[i]]$weights),
                                  start = start(components[[i]]$weights[,1]),
                                  end = end(components[[i]]$weights[,1]),
                                  frequency = frequency(components[[i]]$weights[,1]))
          }
        }
      }
      if(!is.list(weights)&!is.null(weights)){
        for(i in which(!componentsHts)){
          weights_ts[[i]] <- ts(weights[i], start = start(components[[i]]$ts),
                                end = end(components[[i]]$ts),
                                frequency = frequency(components[[i]]$ts))
        }
      }

      private$check_classes(components)
      names(components) <- private$coerce_component_names(components)
      private$tsp_internal <- private$check_time_instances(components)
      self$components <- components
      if("mts"%in%class(weights)){
        weights_ts <- weights
      }else{
        if(length(weights_ts)==0&&!is.list(weights)){
          weights_ts <- NULL
        }else if (length(weights_ts)==0&&is.list(weights)){
          weights_ts <- do.call("cbind",weights)
          colnames(weights_ts) <- names(components)
        }else{
          weights_ts <- do.call("cbind",weights_ts)
          colnames(weights_ts) <- names(components)
        }
      }
      self$weights <- weights_ts
      private$ts_internal <- private$aggregate(components, self$weights)
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
    weights = NULL,
    indirect = NA,
    print = function() {
      tbl <- private$print_table()
      if (all(!tbl$run))
        tbl <- tbl[, 1:3]
      print(tbl, right = FALSE, row.names = FALSE)
    },
    set_options = function(userdefined = NA, spec = NA, recursive = TRUE,
                           component = "") {
      if (component != "") {
        root <- self$get_component(component)
        return(root$set_options(userdefined, spec, recursive))
      }
      super$set_options(userdefined, spec, recursive)
      if (recursive)
        lapply(self$components, function(x) {
          x$set_options(userdefined, spec, recursive)
        })
      invisible(NULL)
    },
    iterate = function(fun, as_table = FALSE, component = "", unnest = FALSE) {
      if (component != "") {
        root <- self$get_component(component)
        return(root$iterate(fun, as_table))
      }
      comp <- lapply(
        self$components,
        function(component) {
          component$iterate(fun)
        }
      )

      res <- c(super$iterate(fun), comp)
      if (as_table)
        return(as_table_nested_list(res))
      if (unnest)
        return(unnest_nested_list(res))
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
    },
    generate_qr_table = function() {
      self$iterate(generateQrList, as_table = TRUE)
    }
  ),
  active = list(
    adjusted_indirect = function() {
      if (is.null(self$output))
        return(NULL)
      private$aggregate(self$components, self$weights, which = "adjusted_indirect")
    },
    adjusted = function() {
      if (is.na(self$indirect)){
        warning("The decision between direct and indirect adjustment was not recoreded yet.
                Direct adjustment is returned.")
      }else if(self$indirect){
        return(self$adjusted_indirect)
      }
      return(self$adjusted_direct)
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
    aggregate = function(components, weights, which = "ts") {
      tss <- lapply(components, function(component) {
        if(which == "adjusted_indirect" & "persephoneSingle" %in% class(component)){
          return(component[["adjusted_direct"]])
        }
        component[[which]]
      })
      weights_ts <- lapply(tss, function(x){
        x <- x*0+1
        x[is.na(x)] <- 1
      })
      if(!is.null(weights)){
        for(i in seq_along(tss)){
          weights_ts[[i]] <- window(weights[,i], start = start(tss[[i]]),
                 end = end(tss[[i]]))
        }
      }
      private$aggregate_ts(tss, weights_ts)
    },
    aggregate_ts = function(ts_vec,weights_ts) {
      sum <- 0
      sumW <- 0
      for (i in seq_along(ts_vec)) {
        sum <- sum + ts_vec[[i]] * weights_ts[[i]]
        sumW <- sumW +  weights_ts[[i]]
      }
      sum/sumW
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
