plotSeasFac <- function(x, main=NULL, plotly=TRUE, ...){

  if (is.null(x$output$user_defined)) {
    stop("No results from run available.\n")
  }
  
  if (inherits(x$output$decomposition, "decomposition_X11")) {
    # evt auch implementieren nachdem das anscheinend in JDemetra gewuenscht ist:
    # first_date, last_date parameters
    # if(!missing(first_date)){
    #   x$si_ratio <- window(x$si_ratio, start = first_date)
    # }
    # if(!missing(last_date)){
    #   x$si_ratio <- window(x$si_ratio, end = last_date)
    # }

   
    d8  <- x$output$decomposition$si_ratio[,1] # Seasonal Factors
    d10 <- x$output$decomposition$si_ratio[,2] # Final unmodified SI Ratios
    d9 <- x$output$user_defined$decomposition.d9 # Final replacement for SI Ratios
    
    # case  x11.excludeFcasts=TRUE abdecken??
    # v <- as.vector(x@d10)[1:length(x@d8)] # Seasonal Factors without forecast
    
    ts <- 
    
        require(grid)
    p1 <- ggplot(daten,aes(ja,dat,color=ii)) + ggtitle(paste("Saisonfaktoren und SI-Ratios",min(daten[,1]),"bis",max(daten[,1]))) +
      geom_point(data=daten[daten[,"ii"]=="SI-Ratios",],size=I(1),alpha=I(0.6),colour="black") +
      geom_point(data=daten[daten[,"ii"]=="SI_Replacement",],size=I(1),alpha=I(1),colour="red") +
      geom_line(data=daten[daten[,"ii"]=="Seasonal Factor",],size=I(0.3),alpha=I(0.9),colour="#386CB0") +
      geom_line(data=daten[daten[,"ii"]=="Mean",],size=I(0.3),alpha=I(0.7),color="black") + 
      scale_x_continuous(labels=NULL,breaks=NULL) + ylab("")
    
    p1 <- p1 + facet_grid(. ~ mon) + xlab("") + #xlab(paste("Years:",min(daten[,1]),"to", max(daten[,1]))) + 
      theme_bw() +  
      theme(strip.background = element_blank(), panel.margin=unit(0, "null"),
            panel.border = element_rect(linetype = "solid", colour = "grey", size=0.1)) 
    
    
  }
    
  
  # if (inherits(x$output$decomposition, "decomposition_SEATS")) {
  #   
  # }
    
  
  
}