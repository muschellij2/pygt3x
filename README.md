
# PLEASE SEE

Actigraph has released its pygt3x package at
<https://github.com/actigraph/pygt3x>. This is likely going to be better
maintained than this repo, but it is only in python.

<!-- README.md is generated from README.Rmd. Please edit that file -->

# pygt3x

<!-- badges: start -->

[![Travis build
status](https://travis-ci.com/muschellij2/pygt3x.svg?branch=master)](https://travis-ci.com/muschellij2/pygt3x)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/muschellij2/pygt3x?branch=master&svg=true)](https://ci.appveyor.com/project/muschellij2/pygt3x)
[![R build
status](https://github.com/muschellij2/pygt3x/workflows/R-CMD-check/badge.svg)](https://github.com/muschellij2/pygt3x/actions)
<!-- badges: end -->

## Installation

You can install the released version of pygt3x from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("pygt3x")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("muschellij2/pygt3x")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(pygt3x)
## basic example code
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
summary(cars)
#>      speed           dist       
#>  Min.   : 4.0   Min.   :  2.00  
#>  1st Qu.:12.0   1st Qu.: 26.00  
#>  Median :15.0   Median : 36.00  
#>  Mean   :15.4   Mean   : 42.98  
#>  3rd Qu.:19.0   3rd Qu.: 56.00  
#>  Max.   :25.0   Max.   :120.00
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date.

You can also embed plots, for example:

<img src="man/figures/README-pressure-1.png" width="100%" />

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub!
