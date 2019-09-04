# persephone 0.1.1

## Implement utilities for hierarchical time series

* Iterating
* recursive setting of options
* weighted aggregates (for example: price indices)
    * also support time-variant weights
* implement direct and indirect adjustments

## documentation

* vignette "persephone-hierarchical"
* vignette "persephone-plotting"
* add Rd-documentation for R6 classes (constructor, methods and fields)

## plotting

add several interactive plotting mehtods with the packages `dygraphs` and
`plotly`

## EUROSTAT quality report (first steps)

add function `generateQrList()` and method `$generate_qr_table()` for
hierarchical models.
