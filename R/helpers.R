#' @importFrom highcharter hchart hc_title
#' @export
highcharter::hchart

#' @export
highcharter::hc_title

#' @importFrom highcharter %>%
#' @export
highcharter::`%>%`

#' @keywords internal
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL

ignore_unused_imports <- function() {
  rjd3tramoseats::tramoseats
  rjd3x13::x13
}

.onLoad <- function(...) {
  # avoid issues with missing rprotobuf descriptor which looks like this during
  # devtools::check()
  #> perX13(AirPassengers)$run()
  #> Error in h(simpleError(msg, call)) :
  #>   error in evaluating the argument 'descriptor' in selecting a method for
  #>   function 'read': object 'x13.X13Output' not found
  #> Calls: <Anonymous> ... .x13_output -> <Anonymous> -> .handleSimpleError -> h
  if (!requireNamespace("rjd3toolkit", quietly = T))
    stop("Loading rjd3toolkit failed")

}

`%||%` <- function(x, y) {
  if (is.null(x))
    y
  else x
}

rescale <- function(x, min, max) {
  stopifnot(is.numeric(x), is.numeric(min), is.numeric(max), length(min) == 1,
            length(max) == 1)
  rg <- range(x)
  min + (x - rg[1])/(rg[2] - rg[1])*(max - min)
}
