asTable_nested_list <- function(x, prefix = "") {
  . <- NULL
  seq_along(x) %>%
    lapply(
      function(i) {
        name <- names(x)[i]
        if (name != "value") {
          asTable_nested_list(
            x[[i]],
            prefix = paste0(prefix, "/", name)
          )
        } else {
          cbind(
            component = gsub("^/", "", prefix),
            as.data.frame(x[[i]])
          )
        }
      }
    ) %>%
    do.call(rbind, .)
}

unnest_nested_list <- function(x, prefix = "") {
  . <- NULL
  seq_along(x) %>%
    lapply(
      function(i) {
        name <- names(x)[i]
        if (name != "value") {
          unnest_nested_list(
            x[[i]],
            prefix = paste0(prefix, "/", name)
          )
        } else {
          out <- list(x[[i]])
          names(out) <- gsub("^/", "", prefix)
          out
        }
      }
    ) %>%
    do.call(c, .)
}

#' Iterate over a hierarchical persephone object
#'
#' This funtion is an alias to the method `iterate` and can be used to iterate
#' over all children of a hierarchical time series produced with `perHts()`.
#'
#' @param x a hierarchical persephone object
#' @param fun a function with one argument
#' @param asTable should the return value be coerced to a `data.frame`?
#' @param component a component id from which the iteration should start
#' @param unnest should the return value be unnested, so that every component
#'   is an entry of the resulting list?
#'
#' @examples
#' obj_x13 <- perX13(AirPassengers, "RSA3")
#' ht <- perHts(a = obj_x13, b = obj_x13, method = "x13")
#' ht2 <- perHts(a = ht, b = obj_x13)
#' ht2$iterate(function(x) {class(x)[1]})
#' ht2$iterate(function(x) {list(class = class(x)[1])}, asTable = TRUE)
#' ht2$iterate(function(x) {list(class = class(x)[1])}, unnest = TRUE)
#' @export
iterate <- function(x, fun, asTable = FALSE, component = "", unnest = FALSE) {
 x$iterate(fun, asTable = asTable, component = component, unnest = unnest)
}
