as_table_nested_list <- function(x, prefix = "") {
  . <- NULL
  seq_along(x) %>%
    lapply(
      function(i) {
        name <- names(x)[i]
        if (name != "value") {
          as_table_nested_list(
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
