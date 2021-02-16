#' is.persephone
#'
#' Check if an R object is of or derives from the class `persephone` or convert
#' an object to persephone
#' @param x R object to be tested or converted.
#' @examples
#' is.persephone(2)
#' obj <- as.persephone(AirPassengers)
#' obj$run()
#' @export
is.persephone <- function(x) {
  inherits(x, "persephone")
}

#' @rdname is.persephone
#' @export
as.persephone <- function(x) {
  if (is.persephone(x))
    return(x$clone(deep = TRUE))
  if (stats::is.ts(x))
    return(perTramo(x))
  else
    stop("cannot coerce object of type ", class(x)[1], " to persephone")
}
