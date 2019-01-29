#' Interactive plot of SI-ratios (and, in case of x11, of seasonal factors) by period
#'
#' Produces a `ggplot`/`plotly` object for objects of class [persephone].
#' If the `persephone` object is of class `tramoseatsSingle`, the function generates an interactive
#' plot of the SI-ratios and their mean. Otherwise, i.e. if the object is of class `x13Single`,
#' the SI-ratios, the replaced SI-ratios, the seasonal factors and the mean of the seasonal factors are
#' plotted.
#'
#' @param x an object of class [persephone].
#' @param main plot title
#' @param plotly If the return value would be a `ggplot` object, wrap it in [plotly::ggplotly]
#'   before returning.
#' @param ... other plotting parameters to affect the plot. Not currently used.
#'
#' @return Returns an object of class `ggplot` or `plotly`
#'
#' @examples
#'
#' data(AirPassengers, package = "datasets")
#' # Generate a persephone object, in this case an x13Single object
#' obj <- x13Single$new(AirPassengers, "RSA1")
#' obj$run()
#' # Plot the SI-ratios after run
#' plotSIRatios(obj)
#'
#' data(UKgas, package = "datasets")
#' # Generate a persephone object, in this case a tramoseatsSingle object
#' obj2 <- tramoseatsSingle$new(UKgas, "RSA3")
#' obj2$run()
#' plotSIRatios(obj2)
#'
#' @importFrom dplyr group_by summarize
#' @importFrom reshape melt
#'
#' @export


plotSIRatios <- function(x, main=NULL, plotly=TRUE, ...){

  variable <- year <- value <- NULL # nolint

  if (is.null(x$output$user_defined)) {
    stop("No results from run available.\n")
  }

  cycleName <- function(ts){
    if ( frequency(ts)==12 ) {
      res <-  month.abb[cycle(ts)]
      res <- ordered(res, levels = c(month.abb))
    }else if ( frequency(ts)==4 ){
      res <-  c("Q1","Q2","Q3","Q4")[cycle(ts)]
      res <- ordered(res, levels = c("Q1","Q2","Q3","Q4"))
    }else if ( frequency(ts)==2 ){
      res <-  c("H1","H2")[cycle(ts)]
      res <- ordered(res, levels = c("H1","H2"))
    }
    return(res)
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
    #
    # case  x11.excludeFcasts=TRUE abdecken??
    # v <- as.vector(x@d10)[1:length(x@d8)] # Seasonal Factors without forecast


    d8 <- x$output$decomposition$si_ratio[,"d8"] # Final unmodified SI Ratios
    d9 <- x$output$user_defined$decomposition.d9 # Final replacement for SI Ratios
    d10 <- x$output$decomposition$si_ratio[,"d10"] # Seasonal Factors

    # #data.table vs
    #
    # d10Mean <- data.frame(d10,year=floor(time(d10)),cycleName=cycleName(d10),cycle=cycle(d10),stringsAsFactors = FALSE)
    # data.table::setDT(d10Mean)
    # d10Mean[,d10Mean:=mean(d10), by=cycle]
    # dat <- merge(data.frame(d8, d9, d10), d10Mean,by="d10", sort=FALSE)
    # dat <- reshape::melt(data=dat, id.vars=c("year","cycle","cycleName") , measure.vars=(c("d8","d9","d10","d10Mean")))

    # dplyr vs
    d10Mean <- data.frame(d10,cycleName=cycleName(d10))
    d10Mean <- d10Mean %>%
      dplyr::group_by(cycleName) %>%
      dplyr::summarize(d10Mean = mean(d10))
    dat <- merge(data.frame(year=floor(time(d10)),cycleName=cycleName(d10),
                            d8, d9, d10, stringsAsFactors = FALSE), d10Mean, by="cycleName", sort=FALSE, all.x=TRUE)
    dat <- reshape::melt(data=dat, id.vars=c("year","cycleName") , measure.vars=(c("d8","d9","d10","d10Mean")))

    if (is.null(main)) {
      main <- "SI Ratios and Seasonal Factors by period"
    }

    p <- ggplot() +
      geom_point(data=subset(dat,variable=="d8"), aes(x=year, y=value, colour=variable)) +
      geom_point(data=subset(dat,variable=="d9"),na.rm=TRUE, aes(x=year, y=value, colour=variable)) +
      geom_line(data=subset(dat,variable=="d10"), aes(x=year, y=value, colour=variable)) +
      geom_line(data=subset(dat,variable=="d10Mean"), aes(x=year, y=value, colour=variable)) +
      scale_x_continuous(labels=NULL,breaks=NULL) +
      facet_grid(cols=vars(cycleName), switch="x") +
      ggtitle(main) +
      ylab("") +
      xlab("") +
      scale_colour_manual(breaks=c("d8","d9","d10","d10Mean"),
                          labels=c("SI-Ratio", "Replaced SI-Ratio","Seasonal Factor", "SF Mean"),
                          values=c("d8"="darkgreen", "d9"="red","d10"="black", "d10Mean"="blue"),
                          guide = guide_legend(override.aes = list(
                            linetype = c("blank", "blank","solid","solid"),
                            shape = c(16,16,NA,NA)))) +
      theme_bw() +
      theme(panel.spacing=unit(0, "lines"), #increase/decrease spacing between faceted plots
            strip.background = element_blank(),
            panel.border = element_rect(linetype = "solid", colour = "grey"),# size=0.1),
            legend.title=element_blank(),
            legend.position="bottom")

    ## plotly schirch -> d8,d9,.. umbenennen oder ueberhaupt mit plotly package arbeiten statt mit ggplot2

  }

  if (inherits(x$output$decomposition, "decomposition_SEATS")) {

    # evt auch implementieren nachdem das anscheinend in JDemetra gewuenscht ist:
    # if(!missing(first_date)){
    #   x$components <- window(x$components, start = first_date)
    # }
    # if(!missing(last_date)){
    #   x$components <- window(x$components, end = last_date)
    # }


    sln  <- x$output$decomposition$components[,"s_cmp"]
    iln <- x$output$decomposition$components[,"i_cmp"]
    mode <- x$output$decomposition$mode

    siRatio <- if (mode == "Additive") {sln+iln} else {sln*iln} # SI-Ratio

    # # data.table vs
    # siRatioMean <- data.frame(siRatio,year=floor(time(siRatio)),cycleName=cycleName(siRatio),cycle=cycle(siRatio),
    #                           stringsAsFactors = FALSE)
    # data.table::setDT(siRatioMean)
    # siRatioMean[,siRatioMean:=mean(siRatio), by=cycle]
    # dat <- merge(data.frame(siRatio), siRatioMean,by="siRatio", sort=FALSE)
    # dat <- reshape::melt(data=dat, id.vars=c("year","cycle","cycleName") , measure.vars=(c("siRatio","siRatioMean")))

    # dplyr vs
    siRatioMean <- data.frame(siRatio,cycleName=cycleName(siRatio))
    siRatioMean <- siRatioMean %>%
      dplyr::group_by(cycleName) %>%
      dplyr::summarize(siRatioMean = mean(siRatio))
    dat <- merge(data.frame(year=floor(time(siRatio)),cycleName=cycleName(siRatio),siRatio, stringsAsFactors = FALSE),
                 siRatioMean, by="cycleName", sort=FALSE, all.x=TRUE)
    dat <- reshape::melt(data=dat, id.vars=c("year","cycleName") , measure.vars=(c("siRatio","siRatioMean")))


    if (is.null(main)) {
      main <- "SI-Ratios by period"
    }

    p <- ggplot() +
      geom_point(data=subset(dat,variable=="siRatio"), aes(x=year, y=value, colour=variable)) +
      geom_line(data=subset(dat,variable=="siRatio"), aes(x=year, y=value, colour=variable)) +
      geom_line(data=subset(dat,variable=="siRatioMean"), aes(x=year, y=value, colour=variable)) +
      scale_x_continuous(labels=NULL,breaks=NULL) +
      facet_grid(cols=vars(cycleName), switch="x") +
      ggtitle(main) +
      ylab("") +
      xlab("") +
      scale_colour_manual(breaks=c("siRatio","siRatioMean"),
                          labels=c("SI-Ratio", "Mean"),
                          values=c("siRatio"="red", "siRatioMean"="blue"),
                          guide = guide_legend(override.aes = list(
                            linetype = c("solid", "solid"), #"blank
                            shape = c(16,NA))) )+ #default shape
      theme_bw() +
      theme(panel.spacing=unit(0, "lines"),#increase/decrease spacing between faceted plots
            strip.background = element_blank(),
            panel.border = element_rect(linetype = "solid", colour = "grey"),# size=0.1),
            legend.title=element_blank(),
            legend.position="bottom")

  }

  if (plotly) {
    p <- plotly::ggplotly(p)
  }

  p

}
