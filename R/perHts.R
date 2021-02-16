#' R6 Class for hierarchical time series
#'
#' @description Combine mutliple objects of persephone objects into a new
#'   hierachical persephone object. On the resulting time series the user can perform direct and
#'   indirect seasonal adjustments.
#' @examples
#' \dontrun{
#' objX13 <- perX13(AirPassengers, "RSA3")
#'
#' ht <- perHts(a = objX13, b = objX13, method = "x13")
#'
#' ht$updateParams(easter.enabled = TRUE)
#'
#' ht$updateParams(component = "a", usrdef.outliersEnabled = TRUE,
#'                      usrdef.outliersType = c("AO","LS","LS"),
#'                      usrdef.outliersDate=c("1950-01-01","1955-04-01","1959-10-01"))
#' ht$run()
#' ht$adjustedDirect
#' ht$adjustedIndirect
#'
#' ht2 <- perHts(a = ht, b = objX13)
#' ht2$run()
#' ht2$adjustedDirect
#' ht2$adjustedIndirect
#' }
#' @export
hierarchicalTimeSeries <- R6::R6Class(
  "hierarchicalTimeSeries",
  inherit = multipleTimeSeries,
  public = list(
    #' @description create a new hierarchical time series
    #' @param ... one or more objects which are either of class persephone or
    #'   can be coerced to persephone objects with asPersephone. If more than
    #'   one element is supplied, the underlying time series must have the same
    #'   time instances. All elements supplied in ... must be named.
    #' @param method specifies the method to be used for the direct adjustment
    #'   of the aggregate series. tramoseats or x13
    #' @param userdefined passed as the userdefined argument to tramoseats() or
    #'   x13()
    #' @param spec a model specification returned by x13_spec() or
    #'   tramoseats_spec()
    #' @param list a list of persephone objects as alternative input to `...`.
    #'   This argument can also handle mts objects
    #' @param weights either a vector if the same weight is used for all time
    #'   points or a list of ts objects or a mts object if the weight varies
    #'   for different time points. They must have the same length as the
    #'   number of components.
    initialize = function(..., method = c("tramoseats", "x13"),
                          userdefined = NULL, spec = NULL, list = NULL,
                          weights = NULL) {
      private$method <- match.arg(method)
      if (!is.null(list)) {
        components <- list
        if (is.null(names(components))) {
          names(components) <- paste0("ts", seq_along(components))
        }
        if (length(list(...)) > 0) {
          warning("If the list argument is specified, additional arguments ",
                  " as ... will be ignored.")
        }
      } else {
        components <- list(...)
      }
      components <- lapply(components, as.persephone)
      componentsHts <- sapply(
        components,
        function(x) "hierarchicalTimeSeries" %in% class(x))
      if (!is.null(weights)) {
        if (ifelse(is.list(weights) || is.vector(weights),
                   length(weights), ncol(weights)) != sum(!componentsHts)) {
        stop("If the weights argument is provided,
             its length must be equal to the number of components.")
        }
      }
      weights_ts <- list()
      if (any(componentsHts)) {
        weightsNull <- sapply(components[componentsHts],
                              function(x) is.null(x$weights))
        if (any(!weightsNull)) {
          if (any(weightsNull)) {
            stop("At the moment it is only supported to use either weights ",
                 "for all components or none.")
          }
          for (i in which(componentsHts)) {
            weights_ts[[i]] <- ts(
              rowSums(components[[i]]$weights),
              start = start(components[[i]]$weights[, 1]),
              end = end(components[[i]]$weights[, 1]),
              frequency = frequency(components[[i]]$weights[, 1]))
          }
        }

      }
      if (!is.list(weights) & !is.null(weights)) {
        which_components_not_Hts <- which(!componentsHts)
        for (i in seq_along(which_components_not_Hts)) {
          j <- which_components_not_Hts[i]
          weights_ts[[j]] <- ts(weights[i], start = start(components[[j]]$ts),
                                end = end(components[[j]]$ts),
                                frequency = frequency(components[[j]]$ts))
        }
      }

      private$check_classes(components)
      names(components) <- private$coerce_component_names(components)
      private$tsp_internal <- private$check_time_instances(components)
      self$components <- components
      if ("mts" %in% class(weights)) {
        weights_ts[!componentsHts] <- as.list(weights)
      }
      if (length(weights_ts) == 0 && !is.list(weights)) {
        weights_ts <- NULL
      } else if (length(weights_ts) == 0 && is.list(weights)) {
        weights_ts <- do.call("cbind", weights)
        colnames(weights_ts) <- names(components)
      }else if (length(weights_ts) > 0 && is.list(weights)){
        weights_ts[!componentsHts] <- weights
        weights_ts <- do.call("cbind", weights_ts)
        colnames(weights_ts) <- names(components)
      }else{
        weights_ts <- do.call("cbind", weights_ts)
        colnames(weights_ts) <- names(components)
      }

      self$weights <- weights_ts
      private$ts_internal <- private$aggregate(components, self$weights)
      super$super2()$set_options(userdefined = userdefined, spec = spec)
    },
    #' @description run the model
    #' @param verbose if `FALSE` (the default), the results of the run will
    #'   be returned invisibly
    #' @examples per_x13(AirPassengers)$run()
    run = function(verbose = FALSE) {
      ## indirect
      lapply(self$components, function(component) {
        component$run(verbose = verbose)
      })
      ## direct
      private$runDirect(self$ts)
    },
    #' @field components the sub series of the hierarchical time series
    components = NULL,
    #' @field weights the weights used for aggregating components
    weights = NULL,
    #' @field indirect wether to use direct or indirect adjustement
    indirect = NA,
    #' @description print a hierarchical timeseries to screen
    print = function() {
      tbl <- private$print_table()
      if (all(!tbl$run))
        tbl <- tbl[, 1:3]
      print(tbl, right = FALSE, row.names = FALSE)
    },
    #' @description sets options for all entries of the dependency tree
    #'   recursively (if recursive = TRUE). See
    #'   vignette("persephone-hierarchical") for more details.
    #' @param userdefined additional outputs to generate while running. See
    #'   [x13()] and [tramoseats()].
    #' @param spec specifications generated by `x13_spec()` or
    #'   `tramoseats_spec()`
    #' @param recursive apply this setting to all subseries as well?
    #' @param component which component to modify.
    setOptions = function(userdefined = NA, spec = NA, recursive = TRUE,
                           component = "") {
      if (component != "") {
        root <- self$getComponent(component)
        return(root$setOptions(userdefined, spec, recursive))
      }
      super$super2()$setOptions(userdefined, spec, recursive)
      if (recursive)
        lapply(self$components, function(x) {
          x$setOptions(userdefined, spec, recursive)
        })
      invisible(NULL)
    },
    #' @description iterate over all components
    #' @details this functin is similar to `lapply()` in the sense that it
    #'   can be used to apply a function to several persephone objects
    #'   simultaniusely
    #' @param fun a function that takes a persephone object as a parameter
    #' @param asTable if true, the return value of this method will be coerced
    #'   to a data.frame
    #' @param component the id of the component
    #' @param unnest if `asTable = FALSE`, converts the return value from a
    #'   nested list into a flat list
    iterate = function(fun, asTable = FALSE, component = "", unnest = FALSE) {
      if (component != "") {
        root <- self$getComponent(component)
        return(root$iterate(fun, asTable))
      }
      comp <- lapply(
        self$components,
        function(component) {
          component$iterate(fun)
        }
      )

      res <- c(super$super2()$iterate(fun), comp)
      private$convert_list(res, asTable, unnest)
    },
    #' @description extract a component series
    #' @param componentId the id of a component
    getComponent = function(componentId) {
      if (length(componentId) == 0 || componentId == "")
        return(self)
      component_path <- strsplit(componentId, "/")[[1]]
      direct_child <- component_path[1]
      if (length(component_path) == 1)
        return(self$components[[direct_child]])
      rest <- paste(component_path[-1], collapse = "/")
      self$components[[direct_child]]$getComponent(rest)
    },
    #' @description Generate a table for the eurostat quality report
    #' @param component (optional) a sub-component to create the report for
    generateQrTable = function(component = "") {
      self$iterate(generateQrList, asTable = TRUE, component = component)
    }
  ),
  active = list(
    #' @field adjustedIndirect results from the indirect adjustment where
    #'   all components are adjusted and then aggregated
    adjustedIndirect = function() {
      if (is.null(self$output))
        return(NULL)
      private$aggregate(self$components, self$weights,
                        which = "adjustedIndirect")
    },
    #' @field adjusted results from the seasonal adjustment
    adjusted = function() {
      if (is.na(self$indirect)) {
        warning("The decision between direct and indirect adjustment was not ",
                 "recorded yet. \nDirect adjustment is returned.")
      } else if (self$indirect) {
        return(private$adjusted_indirect_one_step())
      }
      return(self$adjustedDirect)
    },
    #' @field forecasts get forecasts
    forecasts = function() {
      if (is.na(self$indirect)) {
        warning("The decision between direct and indirect adjustment was not ",
                "recorded yet. \nDirect forecasts are returned.")
      } else if (self$indirect) {
        return(private$forecasts_indirect_one_step())
      }
      return(self$forecastsDirect)
    },
    #' @field forecastsIndirect get forecasts according to indirect adjustments
    forecastsIndirect = function() {
      if (is.null(self$output))
        return(NULL)
      private$aggregate(self$components, self$weights,
                        which = "forecastsIndirect")
    }
  ),
  private = list(
    forecasts_indirect_one_step = function() {
      if (is.null(self$output))
        return(NULL)
      private$aggregate(self$components, self$weights, which = "forecasts")
    },
    adjusted_indirect_one_step = function() {
      if (is.null(self$output))
        return(NULL)
      private$aggregate(self$components, self$weights, which = "adjusted")
    },
    check_classes = function(components) {
      lapply(components, function(component) {
        stopifnot(is.persephone(component))
      })
    },
    print_table = function() {
      self$iterate(printDiagnostics, asTable = TRUE)
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
         if (which == "adjustedIndirect" &
             "persephoneSingle" %in% class(component)) {
           return(component[["adjusted"]])
         }
         if (which == "forecastsIndirect" &
             "persephoneSingle" %in% class(component)) {
           return(component[["forecasts"]])
         }
        component[[which]]
      })
      weights_ts <- NULL
      if (!is.null(weights)) {
        weights_ts <- lapply(tss, function(x) {
          x <- x * 0 + 1
          x[is.na(x)] <- 1
        })
        for (i in seq_along(tss)) {
          if (startEndAsDecimal(end(weights[, i])) <
              startEndAsDecimal(end(tss[[i]]))) {
            weights_ts[[i]] <- ts(tail(weights[, i], 1),
                                  start = start(tss[[i]]),
            end = end(tss[[i]]), frequency = frequency(tss[[i]]))
          } else {
            weights_ts[[i]] <- window(weights[, i], start = start(tss[[i]]),
                                      end = end(tss[[i]]))
          }
        }
      }
      out <- private$aggregate_ts(tss, weights_ts)
      return(out)
    },
    aggregate_ts = function(ts_vec, weights_ts) {
      if (!"mts" %in% class(ts_vec[[1]])) {
        return(private$aggregate_ts0(ts_vec, weights_ts))
      }
      # TODO: Maybe a do.call on 4 lapplys is not the easiest option here
      out <- do.call("cbind", lapply(
        lapply(1:ncol(ts_vec[[1]]),
               function(ind) lapply(ts_vec, function(x) x[, ind])),
        private$aggregate_ts0, weights_ts = weights_ts))
      colnames(out) <- colnames(ts_vec[[1]])
      return(out)
    },
    aggregate_ts0 = function(ts_vec, weights_ts) {
      if (length(ts_vec) == 1) {
        return(ts_vec[[1]])
      }
      if (!is.null(weights_ts)) {
        ts_vec <- do.call("cbind", ts_vec)
        weights_ts <- do.call("cbind", weights_ts)
        out <- ts_vec[, 1] * 0
        for (i in 1:nrow(ts_vec)) {
          out[i] <- weighted.mean(ts_vec[i, ], weights_ts[i, ])
        }
      } else {
        ts_vecX <- do.call("cbind", ts_vec)
        out <- ts(rowSums(ts_vecX),
                  start = start(ts_vecX),
                  frequency = frequency(ts_vecX))

      }
      return(out)
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
    runDirect = function(ts) {
      methodFunction <- switch(private$method, tramoseats = tramoseats,
                               x13 = x13)
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

#' Define a hierarchical time series
#'
#' Combine mutliple objects of persephone objects into a new persephone object.
#' The resulting time series can perform direct and indirect adjustments.
#'
#' @param ... ne or more objects which are either of class persephone or can be
#'   coerced to persephone objects with as_persephone. If more than one element
#'   is supplied, the underlying time series must have the same time instances.
#'   All elements supplied in ... must be named.
#' @param method specifies the method to be used for the direct adjustment of
#'   the aggregate series. tramoseats or x13
#' @param userdefined passed as the userdefined argument to tramoseats() or x13()
#' @param spec  a model specification returned by x13_spec() or
#'   tramoseats_spec()
#' @param list a list of persephone objects as alternative input to `...`. This
#'   argument can also handle mts objects
#' @param weights  either a vector if the same weight is used for all time
#'   points or a list of ts objects or a mts object if the weight varies for
#'   different time points. They must have the same length as the number of
#'   components.
#' @examples
#' \dontrun{
#' objX13 <- perX13(AirPassengers, "RSA3")
#'
#' ht <- perHts(a = objX13, b = objX13, method = "x13")
#'
#' ht$updateParams(easter.enabled = TRUE)
#'
#' ht$updateParams(component = "a", usrdef.outliersEnabled = TRUE,
#'                      usrdef.outliersType = c("AO","LS","LS"),
#'                      usrdef.outliersDate=c("1950-01-01","1955-04-01","1959-10-01"))
#' ht$run()
#' ht$adjustedDirect
#' ht$adjustedIndirect
#'
#' ht2 <- perHts(a = ht, b = objX13)
#' ht2$run()
#' ht2$adjustedDirect
#' ht2$adjustedIndirect
#' }
#' @export
perHts <- function(..., method = c("tramoseats", "x13"),
                    userdefined = NULL, spec = NULL, list = NULL,
                    weights = NULL) {
  hierarchicalTimeSeries$new(..., method = method, userdefined = userdefined,
                             spec = spec, list = list, weights = weights)
}


startEndAsDecimal <- function(x){
  x[1] + (x[2] - 1) / 12
}
