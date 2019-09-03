renderReport <- function(obj) {
  stopifnot(is.persephone(obj))
  template_folder <- fs::path_package(.packageName, "template_markdown") # contains dashboardTemplate.Rmd, dashboardTemplateChild.Rmd
  res <- fs::dir_copy(template_folder, fs::file_temp(pattern = "folder"))
  res2 <- rmarkdown::render(fs::path(res, "dashboardTemplate.Rmd"), params = list(
    obj=obj))
  res3 <- fs::file_copy(res2, tempfile(pattern = "report", fileext = ".html"))
  fs::dir_delete(res)
  rstudioapi::viewer(res3)
  res3
}