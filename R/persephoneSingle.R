persephoneSingle <- R6::R6Class(
  "persephoneSingle",
  inherit = persephone,
  public = list(
    initialize = function() {
      super$initialize()
    },
    run = function(verbose = FALSE) {
      super$run()
    },
    updateParams = function(...) {
      super$updateParams(...)
    },
    plot = function(...) {
      super$plot(...)
    },
    plotSeasIrrCal = function(...) {
      super$plotSeasIrrCal(...)
    },
    plotResiduals = function(...) {
      super$plotResiduals(...)
    },
    print = function() {
      super$print()
    }
  ),
  private = list(

  )
)
