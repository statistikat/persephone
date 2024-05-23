getOutliers <- function(x) {
  if(is.null(x$output)) {
    stop("No results from run available.\n")
  }
  stopifnot(inherits(x$output, c("JD3_X13_RSLTS","JD3_TRAMOSEATS_RSLTS")))

  outliers <- variables[which(vartype %in% c("AO","TC","LS"))]
  if (length(outliers) > 0) {
    outliers <- as.data.frame(do.call(rbind, lapply(outliers, function(x) unlist(x))))
    outliers$origname <- outliers$name
    outliers$coef.value <- as.numeric(outliers$coef.value)
    outliers$date <- substr(outliers$name,5,14)
    newname <- lapply(strsplit(substr(outliers$date,1,7),"-"), as.numeric)
    if (frequency(x$ts) == 12) {
      outliers$name <-lapply(newname, function(nn) paste0(nn, collapse="."))
    } else {
      newname <- lapply(newname, function(nn) c(nn[1], ceiling(as.numeric(nn[2]) / 3)))
      outliers$name <-lapply(newname, function(nn2) paste0(nn2, collapse="."))
    }
    outliers$name <- paste0(outliers$type, outliers$name)
    outliers$date <- as.Date( outliers$date , origin = "1970-01-01")
  } else {
    outliers <- NA
  }
  outliers
}

