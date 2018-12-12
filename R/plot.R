
persephone$set("public", "plot",overwrite = TRUE, function(main=NULL, forecasts=TRUE, showOutliers=TRUE, rangeSelector=TRUE, drawPoints=FALSE, annualComparison=NULL){

  # Helper for annual comparison in plot
  annComp <- function(ts, annualComparison){
   
    annCompVec <- format(time(ts)[cycle(ts)==annualComparison])
    if(frequency(ts)==12){
      annCompLab <- month.abb[annualComparison]
      #annCompLab <- month.name[annualComparison]
      annCompVec <- paste0(substr(annCompVec,1,4), "-", str_pad(annualComparison,2,"left","0"), "-01")
       }else if(frequency(ts)==4){
      annCompLab <- paste0("Q",annualComparison)
      #annCompLab <- paste0(annualComparison,". Quarter")
      annCompVec <- paste0(substr(annCompVec,1,4), "-", str_pad(c(1,4,7,10)[annualComparison],2,"left","0"), "-01")
       }
    
    return(list(annCompVec=annCompVec, annCompLab=annCompLab))
    
}
  
  preRunPlot <- function(ts, rangeSelector=rangeSelector, drawPoints=drawPoints, annualComparison=annualComparison){  
    
    if(is.null(main)){
      main <- "Original Time Series"
    }
    
    graphObj <- dygraph(ts, main=main) %>% 
      dySeries("V1", label = "Original", drawPoints=drawPoints)
    
    if(rangeSelector){
      graphObj <- graphObj %>% 
        dyRangeSelector(height = 20)
    }
    if(!is.null(annualComparison)){
      annCompRes <- annComp(ts,annualComparison)
      for(i in 1:length(annCompRes[["annCompVec"]])){
      graphObj <- graphObj %>%
      #dyEvent(annCompRes[["annCompVec"]][i], paste(substr(annCompRes[["annCompVec"]][i],1,4),annCompRes[["annCompLab"]]),labelLoc = "bottom",color="lightgrey")
      dyEvent(annCompRes[["annCompVec"]][i], annCompRes[["annCompLab"]],labelLoc = "bottom",color="lightgrey")
      }
    }
    
    graphObj
    
  }
  
  postRunPlot <- function(main=main, forecasts=forecasts, showOutliers=showOutliers, rangeSelector=rangeSelector, drawPoints=drawPoints, annualComparison=annualComparison){
    
    if(is.null(main)){
      main <- "Original, SA and Trend Series"
    }
    
    y <- self$output$user_defined$y
    t <- self$output$user_defined$t
    sa <- self$output$user_defined$sa
    ppm_y_f <- self$output$user_defined$preprocessing.model.y_f
    ppm_y_ef <- self$output$user_defined$preprocessing.model.y_ef
    
    # Initialize Graph Object
    # Back-/Forecasts
    if(forecasts & !is.null(ppm_y_f) & !is.null(ppm_y_ef)){
      lowerci <- ppm_y_f-1.96*ppm_y_ef
      upperci <- ppm_y_f+1.96*ppm_y_ef  
      ts <- cbind(y,t,sa,ppm_y_f,lowerci,upperci)
      
      graphObj <- dygraph(ts, main=main) %>% 
        dySeries("y", label = "Original", drawPoints=drawPoints) %>% 
        dySeries("sa", label = "Seasonally Adjusted") %>% 
        dySeries("t", label = "Trend") %>% 
        dySeries(c("lowerci", "ppm_y_f", "upperci"), label = "Forecasts", strokePattern ="dashed",drawPoints=drawPoints) %>% 
        dyLegend(width=400)
      
    }else{
      ts <- cbind(y,t,sa) 
      
      graphObj <- dygraph(ts, main=main) %>% 
        dySeries("y", label = "Original", drawPoints=drawPoints) %>% 
        dySeries("sa", label = "Seasonally Adjusted") %>% 
        dySeries("t", label = "Trend")%>%
        dyLegend(width=290)
    }
    
    # Outliers 
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
        graphObj <-  graphObj %>% dyAnnotation(series="Original",outliers[i], text=substr(outliersName[i],1,2),tooltip=outliersName[i],width=21,height=15,tickHeight=10)
      }
      # for(i in 1:length(outliers)){
      #   graphObj <-  graphObj %>% dyEvent(outliers[i], outliersName[i], labelLoc = "bottom")
      # }
    }
    
    
    if(rangeSelector){
      graphObj <- graphObj %>% 
        dyRangeSelector(height = 20)
    }
    
    if(!is.null(annualComparison)){
      annCompRes <- annComp(ts,annualComparison)
      for(i in 1:length(annCompRes[["annCompVec"]])){
        graphObj <- graphObj %>%
          dyEvent(annCompRes[["annCompVec"]][i], annCompRes[["annCompLab"]],labelLoc = "bottom",color="lightgrey")
      }
    }
    
    graphObj <- graphObj %>%
      dyHighlight(highlightSeriesOpts = list(strokeWidth = 2), highlightCircleSize = 4, highlightSeriesBackgroundAlpha = 0.5)
    
    graphObj
  } 
  
  if(!is.null(self$output$user_defined)){
    
    graphObj <- postRunPlot(main=main, forecasts=forecasts, showOutliers=showOutliers, rangeSelector=rangeSelector, drawPoints=drawPoints,annualComparison=annualComparison)
    graphObj
    
  }else{
    ts <- self$ts
    preRunPlot(ts=ts, rangeSelector=rangeSelector, drawPoints=drawPoints,annualComparison=annualComparison)
    
  }
  
})

# #####################     EXAMPLE 1     #########################
# data(myseries, package = "RJDemetra")
# obj <- x13Single$new(myseries, "RSA1", userdefined=c("y","t","sa",
#                                                      "s","i",
#                                                      "y_f","t_f","sa_f",
#                                                      "preprocessing.model.y_f",
#                                                      "preprocessing.model.y_ef"))## noch zu x13Single dazugeben
# obj$plot(drawPoints = TRUE)
# obj$run()
# obj$plot(drawPoints=TRUE)
# obj$updateParams(usrdef.outliersEnabled = TRUE,
#                  usrdef.outliersType = c("AO","LS","LS"),
#                  usrdef.outliersDate=c("2002-01-01","2003-01-01","2008-10-01"))
# obj$run()
# obj$plot()
# 
# obj$plot(forecasts=FALSE, rangeSelector=FALSE, main="Different Title")
# 
# 
# #####################     EXAMPLE 2      #########################
# 
# ## Beispiel mit Quartalsdaten
# 
# #####################     EXAMPLE 3     #########################
# data(AirPassengers, package = "datasets")
# obj <- x13Single$new(AirPassengers, "RSA1", userdefined=c("y","t","sa",
#                                                           "s","i",
#                                                           "y_f","t_f","sa_f",
#                                                           "preprocessing.model.y_f",
#                                                           "preprocessing.model.y_ef"))## noch zu x12Single dazugeben
# obj$plot()
# obj$run()
# 
# obj$plot()

