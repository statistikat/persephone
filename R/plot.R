
persephone$set("public", "plot",overwrite = TRUE, function(main=NULL, forecasts=TRUE, showOutliers=TRUE, rangeSelector=TRUE, drawPoints=FALSE){
  
  if(drawPoints){
    dP <- TRUE
  }else{
    dP <- FALSE
  }
  
  preRunPlot <- function(ts, rangeSelector=rangeSelector, drawPoints=dP){  
    
    if(is.null(main)){
      main <- "Original Time Series"
    }
    
    graphObj <- dygraph(ts, main=main) %>% 
      dySeries("V1", label = "Original", drawPoints=dP)
    if(rangeSelector){
      graphObj <- graphObj %>% 
        dyRangeSelector(height = 20)
    }
    graphObj
    
  }
  
  postRunPlot <- function(main=main, forecasts=forecasts, showOutliers=showOutliers, rangeSelector=rangeSelector,  drawPoints=dP){
    
    if(is.null(main)){
      main <- "Original, SA and Trend Series"
    }
    
    y <- self$output$user_defined$y
    t <- self$output$user_defined$t
    sa <- self$output$user_defined$sa
    ppm_y_f <- self$output$user_defined$preprocessing.model.y_f
    ppm_y_ef <- self$output$user_defined$preprocessing.model.y_ef
    
    if(forecasts & !is.null(ppm_y_f) & !is.null(ppm_y_ef)){
      lowerci <- ppm_y_f-1.96*ppm_y_ef
      upperci <- ppm_y_f+1.96*ppm_y_ef  
      ts <- cbind(y,t,sa,ppm_y_f,lowerci,upperci)
    }else{
      ts <- cbind(y,t,sa) 
    }
    
    ## Initialize Graph Object
    graphObj <- dygraph(ts, main=main) %>% 
      dySeries("y", label = "Original", drawPoints=dP)
    
    ## Outliers 
    if(showOutliers & !is.null(self$output$regarima$regression.coefficients)){
      outliers <- rownames(self$output$regarima$regression.coefficients)
      outliers <- outliers[substr(outliers,1,2)%in%c("AO","LS","TC")]
      outliersName <- outliers
      outliers <- gsub("(","",outliers,fixed=TRUE)
      outliers <- gsub(")","",outliers,fixed=TRUE)
      outliers <- strsplit(outliers, " ")
      if(frequency(self$ts)==12){
        outliers <-sapply(sapply(outliers,function(x)strsplit(x[[2]],"-")), function(y) paste0(y[[2]],"-",str_pad(y[[1]],2,"left","0"),"-01"))
      }else{
        outliers <-sapply(sapply(outliers,function(x)strsplit(x[[2]],"-")), function(y) paste0(y[[2]],"-",str_pad(c(1,4,7,10)[as.numeric(as.roman(y[[1]]))],2,"left","0"),"-01"))
      }
      
      for(i in 1:length(outliers)){  
        graphObj <-  graphObj %>% dyAnnotation(outliers[i], text=substr(outliersName[i],1,2),tooltip =outliersName[i],width=21,height=15)
      }
      # for(i in 1:length(outliers)){
      #   graphObj <-  graphObj %>% dyEvent(outliers[i], outliersName[i], labelLoc = "bottom")
      # }
    }
    
    ## Back-/Forecasts
    if(forecasts & !is.null(ppm_y_f) & !is.null(ppm_y_ef)){
      graphObj <- graphObj  %>%
        dySeries("sa", label = "Seasonally Adjusted", drawPoints=dP) %>% 
        dySeries("t", label = "Trend", drawPoints=dP) %>% 
        dySeries(c("lowerci", "ppm_y_f", "upperci"), label = "Forecasts", strokePattern ="dashed", drawPoints=dP) %>% 
        dyLegend(width=400)
    }else{
      graphObj <- graphObj  %>%
        dySeries("sa", label = "Seasonally Adjusted", drawPoints=dP) %>% 
        dySeries("t", label = "Trend", drawPoints=dP)%>%
        dyLegend(width=290)
    }  
    
    if(rangeSelector){
      graphObj <- graphObj %>% 
        dyRangeSelector(height = 20)
    }
    
    graphObj <- graphObj %>%
      dyHighlight(highlightSeriesOpts = list(strokeWidth = 2), highlightCircleSize = 4, highlightSeriesBackgroundAlpha = 0.5)
    
    graphObj
  } 
  
  if(!is.null(self$output$user_defined)){
    
    graphObj <- postRunPlot(main=main, forecasts=forecasts, showOutliers=showOutliers, rangeSelector=rangeSelector)
    graphObj
    
  }else{
    ts <- self$ts
    preRunPlot(ts=ts, rangeSelector=rangeSelector)
    
  }
  
})

#####################     EXAMPLE 1     #########################
data(myseries, package = "RJDemetra")
obj <- x13Single$new(myseries, "RSA1", userdefined=c("y","t","sa",
                                                     "s","i",
                                                     "y_f","t_f","sa_f",
                                                     "preprocessing.model.y_f",
                                                     "preprocessing.model.y_ef"))## noch zu x13Single dazugeben
obj$plot(drawPoints = TRUE)
obj$run()
obj$plot(drawPoints=TRUE)
obj$updateParams(usrdef.outliersEnabled = TRUE,
                 usrdef.outliersType = c("AO","LS","LS"),
                 usrdef.outliersDate=c("2002-01-01","2003-01-01","2008-10-01"))
obj$run()
obj$plot()

obj$plot(forecasts=FALSE, rangeSelector=FALSE, main="Different Title")

main=NULL; forecasts=FALSE;rangeSelector=TRUE; showOutliers=TRUE;drawPoints=NULL


#####################     EXAMPLE 2      #########################

## Beispiel mit Quartalsdaten

#####################     EXAMPLE 3     #########################
data(AirPassengers, package = "datasets")
obj <- x13Single$new(AirPassengers, "RSA1", userdefined=c("y","t","sa",
                                                          "s","i",
                                                          "y_f","t_f","sa_f",
                                                          "preprocessing.model.y_f",
                                                          "preprocessing.model.y_ef"))## noch zu x12Single dazugeben
obj$plot()
obj$run()

obj$plot()



