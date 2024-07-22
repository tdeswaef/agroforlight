
<!-- README.md is generated from README.Rmd. Please edit that file -->

# agroforlight

<!-- badges: start -->
<!-- badges: end -->

The goal of agroforlight is to provide functions that enable processing
results from the 3-D tool for agroforestry light simulations available
at <https://agroforestry.ugent.be>.

[![Screenshot from
https://agroforestry.ugent.be](man/figures/printscreen.png)](https://agroforestry.ugent.be)

## Installation

You can install the development version of agroforlight from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("tdeswaef/agroforlight")
```

## Field design

The application available at <https://agroforestry.ugent.be> allows to
run a 3-D simulation of an agroforestry field, using LiDAR scans of real
trees and designing the field outline.

``` r
library(agroforlight)
```

## Light distribution calculation

Once the field is designed, one can calculate the light distribution for
a single moment, or for an entire year. The `Time of day` and the `Day`
settings define the position of the sun (given the `Latitude` in the
`Geography` panel), whereas the `Leaf growth` setting defines the
presence of leaves on the trees, ranging from 0 (no leaves) to 1 (all
leaves fully developed).

The `Time of day` setting refers to the *solar time*, where a value of
`12` corresponds to the solar noon (highest position of the sun during
that day, south), and `0` corresponds to the lowest position of the sun.

The `Day` setting refers to the day number in the *solar year*, starting
at December 21 (`Day` = 0).

To enable easy conversion from UTC time to the corresponding input, we
developed the `timetosolar()` function.

## Light conversion
