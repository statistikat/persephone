
<!-- README.md is generated from README.Rmd. Please edit that file -->

# persephone

[![Travis-CI Build
Status](https://travis-ci.org/statistikat/persephone.svg?branch=master)](https://travis-ci.org/statistikat/persephone)
[![](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![](https://img.shields.io/github/languages/code-size/statistikat/persephone.svg)](https://github.com/statistikat/persephone)
[![](https://img.shields.io/github/last-commit/statistikat/persephone.svg)](https://github.com/statistikat/persephone/commits/master)

This package is an object oriented wrapper around the R Package
[RJDemetra](https://github.com/nbbrd/rjdemetra), which performs time
series adjustments with the java library
[JDemetra+](https://github.com/jdemetra)

## Installation

You can install the package directly from GitHub with

``` r
devtools::install_github("statistikat/persephone")
```

## Usage

Objects can be constructed with `x13Single$new` or
`tramoseatsSingle$new`. Subseqentually, the `run` method runs the model
and `output` gives access to the output object from `RJDemetra`.

``` r
library(RJDemetra)
library(persephone)

data("AirPassengers")

obj <- x13Single$new(AirPassengers)
plot(obj)
```

<img src="man/figures/README-unnamed-chunk-1-1.png" width="90%" />

``` r
obj$run()
obj$output$regarima
#> y = regression model + arima (0, 1, 0, 0, 1, 1)
#> Log-transformation: yes
#> Coefficients:
#>           Estimate Std. Error
#> BTheta(1)  -0.5007      0.081
#> 
#>              Estimate Std. Error
#> Monday      -0.001527      0.004
#> Tuesday     -0.007677      0.004
#> Wednesday   -0.001125      0.004
#> Thursday    -0.005350      0.004
#> Friday       0.004676      0.004
#> Saturday     0.003025      0.004
#> Easter [1]   0.017999      0.008
#> AO (5-1951)  0.109258      0.020
#> 
#> 
#> Residual standard error: 0.03006 on 131 degrees of freedom
#> Log likelihood = 271.5, aic = 947.6 aicc = 949.5, bic(corrected for length) = -6.674
AirPassengers
#>      Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec
#> 1949 112 118 132 129 121 135 148 148 136 119 104 118
#> 1950 115 126 141 135 125 149 170 170 158 133 114 140
#> 1951 145 150 178 163 172 178 199 199 184 162 146 166
#> 1952 171 180 193 181 183 218 230 242 209 191 172 194
#> 1953 196 196 236 235 229 243 264 272 237 211 180 201
#> 1954 204 188 235 227 234 264 302 293 259 229 203 229
#> 1955 242 233 267 269 270 315 364 347 312 274 237 278
#> 1956 284 277 317 313 318 374 413 405 355 306 271 306
#> 1957 315 301 356 348 355 422 465 467 404 347 305 336
#> 1958 340 318 362 348 363 435 491 505 404 359 310 337
#> 1959 360 342 406 396 420 472 548 559 463 407 362 405
#> 1960 417 391 419 461 472 535 622 606 508 461 390 432
```
