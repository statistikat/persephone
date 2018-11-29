#' is.persephone
#'
#' Check if an R object is of or derives from the class `persephone`
#' @examples
#' is.persephone(2)
#' @export
is.persephone <- function(x) {
  inherits(x, "persephone")
}
