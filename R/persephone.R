#' persephone class
#'
#' @description Common interface for x13 and tramoseats
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
#' @section derived classes:
#' [perX13()], [perTramo()], [perHts()]
#' @import RJDemetra
#' @export
persephone <- R6::R6Class(
  "persephone",
  public = list(
    #' @description create a new persephone object
    #' @details this function should not be used directly because this class is
    #'   abstract. Only initialize derived classes.
    initialize = function() {
      stop("implement this function")
    },
    #' @description perform a run of the model
    #' @param verbose whether to show outputs
    run = function(verbose = FALSE) {
      stop("implement this function")
    },
    #' @description update parameters for the adjustment
    #' @param ... passed to `x13_spec()` of `tramoseats_spec()`
    updateParams = function(...) {
      private$params_internal <- private$updateFun(self$params, ...)
    },
    #' @description visualize the results of an adjustment
    #' @param ...  Interactive time series plot for a persephone object,
    #'   see [plot.persephoneSingle()] or [plot.hierarchicalTimeSeries()]
    plot = function(...) {
     plot(self, ...)
    },
    #' @description  Interactive plot of the seasonal component, irregular
    #'   component and calendar effects for a persephone object
    #' @param ... passed to [plotSeasIrrCal()]
    plotSeasIrrCal = function(...) {
      plotSeasIrrCal(self, ...)
    },
    #' @description visualize residuals
    #' @param ... passed to [plotResiduals()]
    plotResiduals = function(...) {
      plotResiduals(self, ...)
    },
    #' @description display a persephone object in the console
    print = function() {
      message("A persephone object")
      if (!is.null(self$output)) {
        message("Output:")
        tbl <- private$printTable("")
        tbl <- tbl[, -1]
        print(tbl, right = FALSE, row.names = FALSE)
      } else {
        message("Not yet run.")
      }
    },
    #' @description see [perHts]
    #' @param fun see [perHts]
    #' @param asTable see [perHts]
    #' @param unnest see [perHts]
    iterate = function(fun, asTable = FALSE, unnest = FALSE) {
      res <- list(value = fun(self))
      private$convert_list(res, asTable, unnest)
    },
    #' @description create a table for the eurostat quality report
    generateQrTable = function() {
      self$iterate(generateQrList, asTable = TRUE)
    },
    #' @description update options for the model
    #' @param userdefined see [x13()] and [tramoseats()]
    #' @param spec see [x13()] and [tramoseats()]
    #' @param recursive only applicable to hierarchical series. propagates
    #'   the updates to sub-series. see [perHts]
    setOptions = function(userdefined = NA,
                           spec = NA, recursive = TRUE) {
      if (is.null(userdefined) || !is.na(userdefined))
        private$userdefined <- union(userdefined, userdefined_default)
      if (is.null(spec) || !is.na(spec))
        private$spec_internal <- spec
    },
    #' @description fix the arima model
    #' @param verbose if TRUE the changed parameters will be reported
    fixModel = function(verbose = FALSE) {
      if(is.null(self$output)){
        warning("not run yet.")
        return(invisible(NULL))
      }
      p <- self$output$regarima$specification$arima$specification$arima.p
      d <- self$output$regarima$specification$arima$specification$arima.d
      q <- self$output$regarima$specification$arima$specification$arima.q
      bp <- self$output$regarima$specification$arima$specification$arima.bp
      bd <- self$output$regarima$specification$arima$specification$arima.bd
      bq <- self$output$regarima$specification$arima$specification$arima.bq
      if(self$output$regarima$specification$arima$specification$enabled){
        if(verbose){
          message("The model", paste("(",p,d,q,") (",bp,bd,bq,") is now fixed."))
        }
        self$updateParams(arima.p=p,arima.d=d,arima.q=q,
                          arima.bp=bp,arima.bd=bd,arima.bq=bq,
                          automdl.enabled = FALSE
        )

      }else if(verbose){
        message("The model", paste("(",p,d,q,") (",bp,bd,bq,") was already fixed."))
      }
    },
    #' @description create a new single object
    #' @param timespan number of months from the end of the time series
    #' where outliers are not fixed
    #' @param verbose if TRUE the changed parameters will be reported
    fixOutlier = function(timespan = 12, verbose = FALSE) {
      xxx <<- self
      if(is.null(self$output)){
        warning("not run yet.")
        return(invisible(NULL))
      }
      from <- time(self$ts)[length(self$ts)-timespan+1]
      y <- floor(from)
      m <- as.character(round((from-floor(from))*self$tsp[3]+1))
      if(frequency(self$ts)==4){
        m <- c("01","04","07","10")[as.numeric(m)]
      }
      if(nchar(m)==1){
        m <- paste0("0",m)
      }
      from <- paste0(y,"-",m,"-01")
      if(self$output$regarima$specification$outliers$enabled){
        possibleOutliers <- row.names(self$output$regarima$regression.coefficients)
        possibleOutliers <- possibleOutliers[substring(possibleOutliers,1,2)%in%
                                               c("AO","LS","TC")]
        if(self$tsp[3]==12){
          outliers <- lapply(possibleOutliers, function(x){
            x2 <- strsplit(x = substring(x,5,nchar(x)-1), split ="-")[[1]]
            x2[1] <- ifelse(nchar(x2[1])==1,paste0("0",x2[1]),x2[1])
            data.frame(type=substr(x,1,2),date=paste0(x2[2],"-",x2[1],"-01"))
          })
        }else if(self$tsp[3]==4){
          outliers <- lapply(possibleOutliers, function(x){
            x2 <- strsplit(x = substring(x,5,nchar(x)-1), split ="-")[[1]]
            x2[1] <- c(I="01",II="04",III="07",IV="10")[x2[1]]
            data.frame(type=substr(x,1,2),date=paste0(x2[2],"-",x2[1],"-01"))
          })
        }
        if(!is.na(self$output$regarima$specification$regression$userdef$outliers[1])){
          if(self$tsp[3]==12){
            outliers <- outliers[sapply(outliers,
                userdefOut = self$output$regarima$specification$regression$userdef$outliers[,1:2],
                function(x,userdefOut){
                  m <- merge(x, userdefOut, by = c("type","date"))
                  return(nrow(m)==0)
            })]

          }else if(self$tsp[3]==4){
            outliers <- outliers[sapply(outliers,
                                        userdefOut = self$output$regarima$specification$regression$userdef$outliers[,1:2],
                                        function(x,userdefOut){
                                          m <- merge(x, userdefOut, by = c("type","date"))
                                          return(nrow(m)==0)
                                        })]
          }
          oldType <- self$output$regarima$specification$regression$userdef$outliers$type
          oldDate <- self$output$regarima$specification$regression$userdef$outliers$date
        }else{
          oldType <- NULL
          oldDate <- NULL
        }
        if(length(outliers)>0){
          if(verbose){
            for(i in seq_along(outliers)){
              message(outliers[[i]]$type," outlier saved at ",outliers[[i]]$date,".")
            }
          }
          df <- data.frame(type=c(oldType,
                                  sapply(outliers, function(x)x$type)),
                           date=c(oldDate,
                                  sapply(outliers, function(x)x$date)))
          df <- unique(df)
          if(class(self)[1]=="hierarchicalTimeSeries"){
            private$updateParamsDirect(usrdef.outliersEnabled = TRUE,
                              usrdef.outliersType = df$type,
                              usrdef.outliersDate = df$date)
          }else{
            self$updateParams(usrdef.outliersEnabled = TRUE,
                              usrdef.outliersType = df$type,
                              usrdef.outliersDate = df$date)
          }

        }else{
          if(verbose){
            message("No automatic outliers found.")
          }
        }
        if(verbose){
          message("Updating parameter outlier.from to '",from,"'")
        }
        if(class(self)[1]=="hierarchicalTimeSeries"){
          private$updateParamsDirect(outlier.from = from)
        }else{
          self$updateParams(outlier.from = from)
        }

      }else if(verbose){
        message("Automatic outliers not enabled.")
      }
    }
  ),
  ## read-only access to params, ts, and output
  active = list(
    #' @field params A parameters object of class `SA_spec`. See [x13_spec()]
    #'   and [tramoseats_spec()].
    params = function() {
      private$params_internal
    },
    #' @field ts the (unajusted) time series
    ts = function() {
      private$ts_internal
    },
    #' @field tsp the [tsp()] of the underlying time series
    tsp = function() {
      private$tsp_internal
    },
    #' @field output The return value from the underlying functions x13 or
    #'   tramoseats. This slot will be empty (NULL) before run() is called for
    #'   the first time.
    output = function() {
      private$output_internal
    },
    #' @field adjusted get the adjusted series
    adjusted = function() {
      self$adjustedDirect
    },
    #' @field adjustedDirect see [perHts]
    adjustedDirect = function() {
      self$output$user_defined$sa
    },
    #' @field spec specifications passed to [x13()] and [tramoseats()] when the
    #'   `$run()` method is invoked
    spec = function() {
      private$spec_internal
    },
    #' @field forecasts get forecasts from the model
    forecasts = function(){
      self$output$final$forecasts
    },
    #' @field forecastsDirect get direct forecasts from the model
    forecastsDirect = function(){
      self$output$final$forecasts
    }
  ),
  private = list(
    convert_list = function(res, asTable = FALSE, unnest = FALSE) {
      if (asTable)
        return(asTable_nested_list(res))
      else if (unnest)
        return(unnest_nested_list(res))
      else
        return(res)
    },
    ts_internal = NULL,
    tsp_internal = NULL,
    params_internal = NULL,
    output_internal = NULL,
    userdefined = NULL,
    spec_internal = NULL,
    printTable = function(prefix) {
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
      #print(class(params))
      #print(str(params))
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
