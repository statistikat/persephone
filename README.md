
<!-- README.md is generated from README.Rmd. Please edit that file -->
persephone
==========

This package is an object oriented wrapper around the R Package [RJDemetra](https://github.com/nbbrd/rjdemetra), which performs time series adjustments with the java library [JDemetra+](https://github.com/jdemetra)

Installation
------------

You can install the package directly from GitHub with

``` r
devtools::install_github("statistikat/persephone")
```

Usage
-----

Objects can be constructed with `x13Single$new` or `tramoseatsSingle$new`. Subseqentually, the `run` method runs the model and `output` gives access to the output object from `RJDemetra`.

``` r
library(RJDemetra)
library(persephone)

data(myseries)

obj <- x13Single$new(myseries)
invisible(
  obj$run()
)
obj$output$regarima
#> y = regression model + arima (1, 1, 1, 0, 1, 1)
#> Log-transformation: no
#> Coefficients:
#>           Estimate Std. Error
#> Phi(1)     -0.8632      0.071
#> Theta(1)   -0.6166      0.111
#> BTheta(1)  -0.9518      0.051
#> 
#>              Estimate Std. Error
#> Mean           -251.4      97.15
#> Monday         -370.7     242.54
#> Tuesday        -506.7     254.48
#> Wednesday      -101.5     248.47
#> Thursday         88.9     246.96
#> Friday          285.4     246.01
#> Saturday        299.4     249.11
#> Leap year      1493.1     841.98
#> LS (10-2008)  36640.0    2304.75
#> AO (1-2002)   14565.9    1514.34
#> LS (1-2003)  -20746.5    2314.34
#> 
#> 
#> Residual standard error:  2246 on 179 degrees of freedom
#> Log likelihood = -1648, aic =  3326 aicc =  3329, bic(corrected for length) = 15.84
myseries
#>          Jan     Feb     Mar     Apr     May     Jun     Jul     Aug
#> 2001                                                                
#> 2002  246680  240476  254298  261655  273847  285696  296574  301114
#> 2003  312120  319317  327218  336320  343756  351042  361475  362668
#> 2004  389117  393469  399591  409373  416623  423014  436246  433400
#> 2005  459893  463566  471751  481074  485829  496407  506284  500775
#> 2006  520777  524765  532187  540257  543523  553658  562621  558909
#> 2007  575640  578720  588522  594796  597610  604978  612926  610644
#> 2008  623127  628987  632868  641391  645746  652087  658817  655993
#> 2009  712199  715829  719841  729136  731911  734897  745341  741044
#> 2010  757084  759517  768585  772608  778963  785493  793930  787969
#> 2011  796249  796244  798344  805450  810435  819663  828194  823445
#> 2012  842958  842541  844906  847598  856303  867744  871509  870166
#> 2013  856975  855786  867536  874708  879698  885908  892804  894196
#> 2014  908272  910194  916522  921814  928897  935294  944707  946755
#> 2015  979087  983220  990949  999766 1006424 1017083 1031285 1029364
#> 2016 1037667 1038867 1042518 1047084 1049349 1057708 1067801 1064305
#> 2017 1075137 1078141 1082422 1089204 1089662 1099145 1104797 1102805
#>          Sep     Oct     Nov     Dec
#> 2001                          239720
#> 2002  306676  313872  321353  341158
#> 2003  364830  371239  379148  397902
#> 2004  438041  444394  448753  468426
#> 2005  507014  510363  514338  532738
#> 2006  563110  566998  571419  592122
#> 2007  610479  613502  618655  638551
#> 2008  657080  698784  703624  722746
#> 2009  740515  745290  749965  769871
#> 2010  786756  789040  790206  808562
#> 2011  831170  837498  841367  857482
#> 2012  866652  864266  864074  876787
#> 2013  894026  897951  903364  921221
#> 2014  947025  950566  956813  980634
#> 2015 1026546 1028789 1034492 1048926
#> 2016 1066501 1069742 1070979 1087082
#> 2017 1103295 1106170 1107118
```
