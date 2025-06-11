# x <- obj$clone()

getOutliers <- function(x) {

  if(is.null(x$output)) {
    stop("No results from run available.\n")
  }
  stopifnot(inherits(x$output, c("JD3_X13_RSLTS","JD3_TRAMOSEATS_RSLTS")))

  variables <- x$output$preprocessing$description$variables
  vartype <- sapply(variables, function(v) v$type )
  otlind <- which(vartype %in% c("AO","TC","LS"))
  outliers <- variables[otlind]

  if (length(outliers) > 0) {
    outliers <- as.data.frame(do.call(rbind, lapply(outliers, function(x) unlist(x))))
    # outliers$origname <- outliers$name
    outliers$coef.value <- as.numeric(outliers$coef.value)
    outliers$date <- substr(outliers$name,5,14)
    newname <- lapply(strsplit(substr(outliers$date,1,7),"-"), as.numeric)
    outliers$date <- as.Date(outliers$date , origin = "1970-01-01")

    if (frequency(x$ts) == 12) {
      outliers$name <-lapply(newname, function(nn) paste0(nn, collapse="."))
    } else {
      newname <- lapply(newname, function(nn) c(nn[1], lubridate::quarter(nn[2])))
      outliers$name <-lapply(newname, function(nn2) paste0(nn2, collapse="."))
    }
    outliers$name <- paste0(outliers$type, outliers$name)

    stde <- sqrt(diag(x$output$preprocessing$estimation$bvar))[otlind]
    t <- outliers$coef.value / stde
    ndf <- x$output$preprocessing$estimation$likelihood$neffectiveobs - x$output$preprocessing$estimation$likelihood$nparams
    pval <- 2 * pt(abs(t), ndf, lower.tail = FALSE)
    outliers$stde <- stde
    outliers$t <- t
    outliers$pvalue <- pval

    outliers <- outliers[,c("name","type","coef.value","stde","t","pvalue","date","coef.type")]
  } else {
    outliers <- NA
  }
  outliers
}

# we now have a userdefined output available for outliers

# sp <- x13_spec("rsa5c")
# sp <- rjd3toolkit::add_outlier(sp,
#                                type = c("AO"), c("2015-01-01", "2010-01-01")
# )
# sp <- rjd3toolkit::set_transform(
#   rjd3toolkit::set_tradingdays(
#     rjd3toolkit::set_easter(sp, enabled = FALSE),
#     option = "workingdays"
#   ),
#   fun = "None"
# )
# x13(y, spec = sp)
# sp <- set_x11(sp,
#               henderson.filter = 13
# )
# q <- x13_fast(y, spec = sp)
# q <- q$preprocessing

regarimaCoefTable <- function(x) {
  if(is.null(x$output)) {
    stop("No results from run available.\n")
  }
  stopifnot(inherits(x$output, c("JD3_X13_RSLTS","JD3_TRAMOSEATS_RSLTS")))
  q <- x$output$preprocessing

  if (length(q$description$variables) > 0) {
    regs <- do.call("rbind", lapply(q$description$variables, function(z) {
      z$coef
    }))
    xregs <- cbind(name = unlist(rownames(regs)), regs, stde = NA, t = NA, pvalue = NA)
    rownames(xregs) <- NULL
    stde <- sqrt(diag(q$estimation$bvar))
    sel <- xregs$type == "ESTIMATED"
    t <- xregs$value[sel] / stde
    ndf <- q$estimation$likelihood$neffectiveobs - q$estimation$likelihood$nparams
    pval <- 2 * pt(abs(t), ndf, lower.tail = FALSE)
    xregs$stde[sel] <- stde
    xregs$t[sel] <- t
    xregs$pvalue[sel] <- pval

    otlind <- which(grepl(c("AO|LS|TC"),xregs$name))
    otltype <- substr(xregs[otlind,]$name,1,2)
    date <- substr(xregs[otlind,]$name,5,14)
    newname <- lapply(strsplit(substr(date,1,7),"-"), as.numeric)
    if (frequency(x$ts) == 12) {
      xregs[otlind,]$name <-lapply(newname, function(nn) paste0(nn, collapse="."))
    } else {
      newname <- lapply(newname, function(nn) c(nn[1], ceiling(as.numeric(nn[2]) / 3)))
      xregs[otlind,]$name <-lapply(newname, function(nn2) paste0(nn2, collapse="."))
    }
    xregs[otlind,]$name <- paste0(otltype, xregs[otlind,]$name)

    # colnames(xregs) <- c(
    #   "Estimate", "Type", "Std. Error",
    #   "T-stat", "Pr(>|t|)"
    # )
    xregs <- xregs[,c("name","value","stde","t","pvalue")]
  } else {
    NULL
  }
}

arimaCoefTable.print <- function(xregs) {
  xregs.print <- xregs[,c("value","stde","t","pvalue")]
  rownames(xregs.print) <- xregs$name
  colnames(xregs.print) <- c("Estimate", "Std.Err", "Z value", "Pr(>z)")

  printCoefmat(xregs.print)
}


########################################
###         from rdj3toolkit         ###
########################################
# x <- obj$output$preprocessing
# x <- rs$preprocessing
#
# .regarima_coef_table.default <- function(x, ...) {
#   q <- x
#   if (length(q$description$variables) > 0) {
#     regs <- do.call("rbind", lapply(q$description$variables, function(z) {
#       z$coef
#     }))
#     xregs <- cbind(regs, stde = NA, t = NA, pvalue = NA)
#     stde <- sqrt(diag(q$estimation$bvar))
#     sel <- xregs$type == "ESTIMATED"
#     t <- xregs$value[sel] / stde
#     ndf <- q$estimation$likelihood$neffectiveobs - q$estimation$likelihood$nparams
#     pval <- 2 * pt(abs(t), ndf, lower.tail = FALSE)
#     xregs$stde[sel] <- stde
#     xregs$t[sel] <- t
#     xregs$pvalue[sel] <- pval
#     colnames(xregs) <- c(
#       "Estimate", "Type", "Std. Error",
#       "T-stat", "Pr(>|t|)"
#     )
#     xregs
#   } else {
#     NULL
#   }
# }


gettsout<- function(x) {
  outliers <- getOutliers(x)
  if(frequency(x$ts) == 12) {
    outliers$mq <- lubridate::month(outliers$date)
    outliers$y <- lubridate::year(outliers$date)

  }else {
    outliers$mq <- lubridate::quarter(outliers$date)
    outliers$y <- lubridate::year(outliers$date)
  }
  #Date format für dyevent

  dateout <- list()
  for(i in 1:nrow(outliers)) {
    dateout[[length(dateout)+1]] <- outliers[i,]$date
    names(dateout)[length(dateout)] <- outliers[i,]$type
  }

  #outliersAO <-outliers[outliers$name %in% "AO1953.7",c("y","mq")]

  outliersAO <- outliers[outliers$type %in% "AO",c("y","mq")]
  outliersLS <- outliers[outliers$type %in% "LS",c("y","mq")]
  outliersTC <- outliers[outliers$type %in% "TC",c("y","mq")]

  tsout <- list()

  if(nrow(outliersAO)>0){
    otlAO <- ts(start = start(x$ts), end = end(x$ts), frequency = frequency(x$ts))
    for(i in 1:nrow(outliersAO)) {
      wi <- as.numeric(outliersAO[i,])
      window(otlAO, start=wi, end=wi) <- window(x$ts, start=wi, end=wi)
    }
    tsout[[length(tsout)+1]] <- otlAO
    names(tsout)[length(tsout)] <- "otlAO"
  }
  if(nrow(outliersLS)>0){
    otlLS <- ts(start = start(x$ts), end = end(x$ts), frequency = frequency(x$ts))
    for(i in 1:nrow(outliersLS)) {
      wi <- as.numeric(outliersLS[i,])
      window(otlLS, start=wi, end=wi) <- window(x$ts, start=wi, end=wi)
    }
    tsout[[length(tsout)+1]] <- otlLS
    names(tsout)[length(tsout)] <- "otlLS"
  }
  if(nrow(outliersTC)>0){
    otlTC <- ts(start = start(x$ts), end = end(x$ts), frequency = frequency(x$ts))
    for(i in 1:nrow(outliersTC)) {
      wi <- as.numeric(outliersTC[i,])
      window(otlTC, start=wi, end=wi) <- window(x$ts, start=wi, end=wi)
    }
    tsout[[length(tsout)+1]] <- otlTC
    names(tsout)[length(tsout)] <- "otlTC"
  }


  tsout <- do.call(cbind, tsout)

  return(list(tsout, dateout))
}
