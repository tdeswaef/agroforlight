
<!-- README.md is generated from README.Rmd. Please edit that file -->

# agroforlight

<!-- badges: start -->
<!-- badges: end -->

The goal of `agroforlight` is to provide functions that enable
processing results from the 3-D tool for agroforestry light simulations
available at <https://agroforestry.ugent.be>.

[![Screenshot from
https://agroforestry.ugent.be](man/figures/printscreen.png)](https://agroforestry.ugent.be)

## Installation

You can install the development version of `agroforlight` from
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

To enable easy conversion between UTC time and the solar time, required
for input, we developed the `convert_utc_solar()` and
`convert_solar_utc()` functions.

## Light conversion

The output of the application available at
<https://agroforestry.ugent.be> contains a value per sensor per time
point. The unit of this value is $m^2$, as this corresponds to the area
of that sensor $A_{s}$ that can be *seen* by the sun, and the diffuse
sources. The calculations for conversion are covered by the functions
`convert_afl_ts()` for time series and `convert_afl_1()` for a single
moment and are based on the principles described below.

The theoretical maximum value per sensor is the sensor’s surface area
($A$), which results from the `Size` and `Count` settings in the
`Sensors` panel:

$$A = \frac{Size_{x} \times Size_{y}}{Count_{x} \times Count_{y}}$$.

This value can only be reached for the sun source, when the sun is in
the zenith, and no scene objects are present between the sun and the
sensor.

The reference, un-shaded value $A_{0}$ can be calculated from from the
following formula:

$$ A_{0} = \cos(\theta)\times\cos(\beta) + sin(\theta) \times \sin(\beta) \times \cos(\phi - \gamma) $$
where $\theta$ is the solar angle from the zenith, $\beta$ is the
inclination angle of the field, $\phi$ is the solar azimuthal angle from
the north and $\gamma$ is the rotation angle from the field. As the
diffuse sources remain the same over latitudes and times, its reference
value $A_{0}$, resulting from the weighted average over the available
sources, has the constant value around 0.58.

To scale the application output to the real light intensity, one needs a
data set of un-shaded solar radiation at the required temporal
resolution ($I_{0}$). This data set is split up in diffuse and direct
components using the `calc_light_attr()` function. The direct and
diffuse fractions are then used to re-scale the application output for
each sensor and light fraction (subscript $f$ for *diffuse* and
*direct*):

$$ I_{s,f} = I_{0,f} \times \frac{A_{s,f}}{A_{0,f}} $$
