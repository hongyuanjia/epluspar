
<!-- README.md is generated from README.Rmd. Please edit that file -->

# epScan

Conduct sensitivity analysis and Bayesian calibration of EnergyPlus
models.

[![Travis-CI Build
Status](https://travis-ci.org/ideas-lab-nus/epScan.svg?branch=master)](https://travis-ci.org/ideas-lab-nus/epScan)
[![AppVeyor Build
Status](https://ci.appveyor.com/api/projects/status/github/ideas-lab-nus/epScan?branch=master&svg=true)](https://ci.appveyor.com/project/ideas-lab-nus/epScan)
[![codecov](https://codecov.io/gh/ideas-lab-nus/epScan/branch/master/graph/badge.svg)](https://codecov.io/gh/ideas-lab-nus/epScan)
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/epScan)](https://cran.r-project.org/package=epScan)
[![CRAN Download
Badge](https://cranlogs.r-pkg.org/badges/epScan)](https://cran.r-project.org/package=epScan)

## Installation

Currently, epScan is not on CRAN. You can install the development
version from GitHub.

``` r
# install.packages("remotes")
remotes::install_github("ideas-lab-nus/epScan")
```

# Get started

## Sensitivity Analysis

Create a `SensitivityJob` object:

``` r
# use an example file from EnergyPlus v8.8 for demonstration here
path_idf <- file.path(eplusr::eplus_config(8.8)$dir, "ExampleFiles", "5Zone_Transformer.idf")
path_epw <- file.path(eplusr::eplus_config(8.8)$dir, "WeatherData", "USA_CA_San.Francisco.Intl.AP.724940_TMY3.epw")

# create a `SensitivityJob` class which inheris from eplusr::ParametricJob class
sen <- sensi_job(path_idf, path_epw)
#> IDD v8.8.0 has not been parsed before.
#> Try to locate `Energy+.idd` in EnergyPlus v8.8.0 installation folder `/usr/local/EnergyPlus-8-8-0`.
#> IDD file found: `/usr/local/EnergyPlus-8-8-0/Energy+.idd`.
#> Start parsing...
#> Parsing completed.
#> Adding an object in class `Output:SQLite` and setting its `Option Type` to `SimpleAndTabular` in order to create SQLite output file.
```

Set sensitivity parameters using `$param()` or `$apply_measure()`.

  - Using `$param()`

<!-- end list -->

``` r
# set parameter using similar syntax to `Idf$set()` in eplusr
sen$param(
    # For adding a single object field as parameter
    # Syntax: Object = list(Field = c(Min, Max, Levels))
    `Supply Fan 1` = list(Fan_Total_Efficiency = c(0.1, 1.0, 5)),

    # For adding a class field as parameter
    Material := list(
        Thickness = c(min = 0.01, max = 0.08, levels = 5),
        Conductivity = c(min = 0.01, max = 0.6, levels = 6)
    ),

    # use `.names` to give names to each parameter
    .names = c("thickness", "conducitivy", "efficiency"),

    # See `r` and `grid_jump` in `sensitivity::morris`
    .r = 8, .grid_jump = 1
)
```

  - Using `$apply_measure()`

<!-- end list -->

``` r
# first define a "measure"
my_actions <- function (idf, efficiency, thickness, conducitivy) {
    idf$set(
        `Supply Fan 1` = list(Fan_Total_Efficiency = efficiency),
        Material := list(Thickness = thickness, Conductivity = conducitivy)
    )

    idf
}

# then apply that measure with parameter space definitions as function arguments
sen$apply_measure(my_actions,
    efficiency = c(0.1, 1.0, 5),
    thickness = c(0.01, 0.08, 5),
    conducitivy = c(0.1, 0.6, 6),
    .r = 8, .grid_jump = 1
)
#> ── EnergPlus Parametric Job ───────────────────────────────────────────────
#> Seed Model: `/usr/local/EnergyPlus-8-8-0/ExampleFiles/5Zone_Transforme...
#> Weather: `/usr/local/EnergyPlus-8-8-0/WeatherData/USA_CA_San.Francisco...
#> EnergyPlus Version: `8.8.0`
#> EnergyPlus Path: `/usr/local/EnergyPlus-8-8-0`
#> Applied Measure: `my_actions`
#> Parametric Models [32]: 
#> [01]: `1_effic(0.1)_thick(0.045)_condu(0.5)`
#> [02]: `2_effic(0.325)_thick(0.045)_condu(0.5)`
#> [03]: `3_effic(0.325)_thick(0.045)_condu(0.4)`
#> [04]: `4_effic(0.325)_thick(0.0625)_condu(0.4)`
#> [05]: `5_effic(1)_thick(0.0625)_condu(0.5)`
#> [06]: `6_effic(1)_thick(0.045)_condu(0.5)`
#> [07]: `7_effic(1)_thick(0.045)_condu(0.6)`
#> [08]: `8_effic(0.775)_thick(0.045)_condu(0.6)`
#> [09]: `9_effic(0.325)_thick(0.045)_condu(0.6)`
#> [10]: `10_effic(0.55)_thick(0.045)_condu(0.6)`
#> [11]: `11_effic(0.55)_thick(0.045)_condu(0.5)`
#> [12]: `12_effic(0.55)_thick(0.0625)_condu(0.5)`
#> [13]: `13_effic(0.325)_thick(0.01)_condu(0.2)`
#> [14]: `14_effic(0.325)_thick(0.0275)_condu(0.2)`
#> [15]: `15_effic(0.55)_thick(0.0275)_condu(0.2)`
#> [16]: `16_effic(0.55)_thick(0.0275)_condu(0.1)`
#> [17]: `17_effic(0.55)_thick(0.01)_condu(0.6)`
#> [18]: `18_effic(0.55)_thick(0.0275)_condu(0.6)`
#> [19]: `19_effic(0.325)_thick(0.0275)_condu(0.6)`
#> [20]: `20_effic(0.325)_thick(0.0275)_condu(0.5)`
#> [21]: `21_effic(0.1)_thick(0.08)_condu(0.2)`
#> [22]: `22_effic(0.1)_thick(0.08)_condu(0.1)`
#> [23]: `23_effic(0.1)_thick(0.0625)_condu(0.1)`
#> [24]: `24_effic(0.325)_thick(0.0625)_condu(0.1)`
#> [25]: `25_effic(0.1)_thick(0.01)_condu(0.4)`
#> [26]: `26_effic(0.1)_thick(0.01)_condu(0.3)`
#> [27]: `27_effic(0.1)_thick(0.0275)_condu(0.3)`
#> [28]: `28_effic(0.325)_thick(0.0275)_condu(0.3)`
#> [29]: `29_effic(0.1)_thick(0.0275)_condu(0.2)`
#> [30]: `30_effic(0.1)_thick(0.045)_condu(0.2)`
#> [31]: `31_effic(0.1)_thick(0.045)_condu(0.3)`
#> [32]: `32_effic(0.325)_thick(0.045)_condu(0.3)`
#> << Job has not been run before >>
```

Get samples

``` r
sen$samples()
#>     case efficiency thickness conducitivy
#>  1:    1      0.100    0.0450         0.5
#>  2:    2      0.325    0.0450         0.5
#>  3:    3      0.325    0.0450         0.4
#>  4:    4      0.325    0.0625         0.4
#>  5:    5      1.000    0.0625         0.5
#>  6:    6      1.000    0.0450         0.5
#>  7:    7      1.000    0.0450         0.6
#>  8:    8      0.775    0.0450         0.6
#>  9:    9      0.325    0.0450         0.6
#> 10:   10      0.550    0.0450         0.6
#> 11:   11      0.550    0.0450         0.5
#> 12:   12      0.550    0.0625         0.5
#> 13:   13      0.325    0.0100         0.2
#> 14:   14      0.325    0.0275         0.2
#> 15:   15      0.550    0.0275         0.2
#> 16:   16      0.550    0.0275         0.1
#> 17:   17      0.550    0.0100         0.6
#> 18:   18      0.550    0.0275         0.6
#> 19:   19      0.325    0.0275         0.6
#> 20:   20      0.325    0.0275         0.5
#> 21:   21      0.100    0.0800         0.2
#> 22:   22      0.100    0.0800         0.1
#> 23:   23      0.100    0.0625         0.1
#> 24:   24      0.325    0.0625         0.1
#> 25:   25      0.100    0.0100         0.4
#> 26:   26      0.100    0.0100         0.3
#> 27:   27      0.100    0.0275         0.3
#> 28:   28      0.325    0.0275         0.3
#> 29:   29      0.100    0.0275         0.2
#> 30:   30      0.100    0.0450         0.2
#> 31:   31      0.100    0.0450         0.3
#> 32:   32      0.325    0.0450         0.3
#>     case efficiency thickness conducitivy
```

Run simulations and calculate statistic indicators

``` r
# run simulations in temporary directory
sen$run(dir = tempdir())
#> 01|RUNNING    --> [IDF]`1_effic(0.1)_thick(0.045)_condu(0.5).idf` + [E...
#> 02|RUNNING    --> [IDF]`2_effic(0.325)_thick(0.045)_condu(0.5).idf` + ...
#> 03|RUNNING    --> [IDF]`3_effic(0.325)_thick(0.045)_condu(0.4).idf` + ...
#> 04|RUNNING    --> [IDF]`4_effic(0.325)_thick(0.0625)_condu(0.4).idf` +...
#> 05|RUNNING    --> [IDF]`5_effic(1)_thick(0.0625)_condu(0.5).idf` + [EP...
#> 06|RUNNING    --> [IDF]`6_effic(1)_thick(0.045)_condu(0.5).idf` + [EPW...
#> 07|RUNNING    --> [IDF]`7_effic(1)_thick(0.045)_condu(0.6).idf` + [EPW...
#> 08|RUNNING    --> [IDF]`8_effic(0.775)_thick(0.045)_condu(0.6).idf` + ...
#> 01|COMPLETED  --> [IDF]`1_effic(0.1)_thick(0.045)_condu(0.5).idf` + [E...
#> 09|RUNNING    --> [IDF]`9_effic(0.325)_thick(0.045)_condu(0.6).idf` + ...
#> 02|COMPLETED  --> [IDF]`2_effic(0.325)_thick(0.045)_condu(0.5).idf` + ...
#> 10|RUNNING    --> [IDF]`10_effic(0.55)_thick(0.045)_condu(0.6).idf` + ...
#> 03|COMPLETED  --> [IDF]`3_effic(0.325)_thick(0.045)_condu(0.4).idf` + ...
#> 11|RUNNING    --> [IDF]`11_effic(0.55)_thick(0.045)_condu(0.5).idf` + ...
#> 12|RUNNING    --> [IDF]`12_effic(0.55)_thick(0.0625)_condu(0.5).idf` +...
#> 06|COMPLETED  --> [IDF]`6_effic(1)_thick(0.045)_condu(0.5).idf` + [EPW...
#> 13|RUNNING    --> [IDF]`13_effic(0.325)_thick(0.01)_condu(0.2).idf` + ...
#> 07|COMPLETED  --> [IDF]`7_effic(1)_thick(0.045)_condu(0.6).idf` + [EPW...
#> 14|RUNNING    --> [IDF]`14_effic(0.325)_thick(0.0275)_condu(0.2).idf` ...
#> 08|COMPLETED  --> [IDF]`8_effic(0.775)_thick(0.045)_condu(0.6).idf` + ...
#> 15|RUNNING    --> [IDF]`15_effic(0.55)_thick(0.0275)_condu(0.2).idf` +...
#> 16|RUNNING    --> [IDF]`16_effic(0.55)_thick(0.0275)_condu(0.1).idf` +...
#> 09|COMPLETED  --> [IDF]`9_effic(0.325)_thick(0.045)_condu(0.6).idf` + ...
#> 17|RUNNING    --> [IDF]`17_effic(0.55)_thick(0.01)_condu(0.6).idf` + [...
#> 10|COMPLETED  --> [IDF]`10_effic(0.55)_thick(0.045)_condu(0.6).idf` + ...
#> 18|RUNNING    --> [IDF]`18_effic(0.55)_thick(0.0275)_condu(0.6).idf` +...
#> 11|COMPLETED  --> [IDF]`11_effic(0.55)_thick(0.045)_condu(0.5).idf` + ...
#> 19|RUNNING    --> [IDF]`19_effic(0.325)_thick(0.0275)_condu(0.6).idf` ...
#> 20|RUNNING    --> [IDF]`20_effic(0.325)_thick(0.0275)_condu(0.5).idf` ...
#> 14|COMPLETED  --> [IDF]`14_effic(0.325)_thick(0.0275)_condu(0.2).idf` ...
#> 21|RUNNING    --> [IDF]`21_effic(0.1)_thick(0.08)_condu(0.2).idf` + [E...
#> 13|COMPLETED  --> [IDF]`13_effic(0.325)_thick(0.01)_condu(0.2).idf` + ...
#> 22|RUNNING    --> [IDF]`22_effic(0.1)_thick(0.08)_condu(0.1).idf` + [E...
#> 16|COMPLETED  --> [IDF]`16_effic(0.55)_thick(0.0275)_condu(0.1).idf` +...
#> 23|RUNNING    --> [IDF]`23_effic(0.1)_thick(0.0625)_condu(0.1).idf` + ...
#> 24|RUNNING    --> [IDF]`24_effic(0.325)_thick(0.0625)_condu(0.1).idf` ...
#> 17|COMPLETED  --> [IDF]`17_effic(0.55)_thick(0.01)_condu(0.6).idf` + [...
#> 25|RUNNING    --> [IDF]`25_effic(0.1)_thick(0.01)_condu(0.4).idf` + [E...
#> 18|COMPLETED  --> [IDF]`18_effic(0.55)_thick(0.0275)_condu(0.6).idf` +...
#> 26|RUNNING    --> [IDF]`26_effic(0.1)_thick(0.01)_condu(0.3).idf` + [E...
#> 20|COMPLETED  --> [IDF]`20_effic(0.325)_thick(0.0275)_condu(0.5).idf` ...
#> 27|RUNNING    --> [IDF]`27_effic(0.1)_thick(0.0275)_condu(0.3).idf` + ...
#> 28|RUNNING    --> [IDF]`28_effic(0.325)_thick(0.0275)_condu(0.3).idf` ...
#> 22|COMPLETED  --> [IDF]`22_effic(0.1)_thick(0.08)_condu(0.1).idf` + [E...
#> 29|RUNNING    --> [IDF]`29_effic(0.1)_thick(0.0275)_condu(0.2).idf` + ...
#> 23|COMPLETED  --> [IDF]`23_effic(0.1)_thick(0.0625)_condu(0.1).idf` + ...
#> 30|RUNNING    --> [IDF]`30_effic(0.1)_thick(0.045)_condu(0.2).idf` + [...
#> 24|COMPLETED  --> [IDF]`24_effic(0.325)_thick(0.0625)_condu(0.1).idf` ...
#> 31|RUNNING    --> [IDF]`31_effic(0.1)_thick(0.045)_condu(0.3).idf` + [...
#> 32|RUNNING    --> [IDF]`32_effic(0.325)_thick(0.045)_condu(0.3).idf` +...
#> 25|COMPLETED  --> [IDF]`25_effic(0.1)_thick(0.01)_condu(0.4).idf` + [E...
#> 26|COMPLETED  --> [IDF]`26_effic(0.1)_thick(0.01)_condu(0.3).idf` + [E...
#> 28|COMPLETED  --> [IDF]`28_effic(0.325)_thick(0.0275)_condu(0.3).idf` ...
#> 29|COMPLETED  --> [IDF]`29_effic(0.1)_thick(0.0275)_condu(0.2).idf` + ...
#> 30|COMPLETED  --> [IDF]`30_effic(0.1)_thick(0.045)_condu(0.2).idf` + [...
#> 31|COMPLETED  --> [IDF]`31_effic(0.1)_thick(0.045)_condu(0.3).idf` + [...
#> 32|COMPLETED  --> [IDF]`32_effic(0.325)_thick(0.045)_condu(0.3).idf` +...
#> ── EnergPlus Parametric Job ───────────────────────────────────────────────
#> Seed Model: `/usr/local/EnergyPlus-8-8-0/ExampleFiles/5Zone_Transforme...
#> Weather: `/usr/local/EnergyPlus-8-8-0/WeatherData/USA_CA_San.Francisco...
#> EnergyPlus Version: `8.8.0`
#> EnergyPlus Path: `/usr/local/EnergyPlus-8-8-0`
#> Applied Measure: `my_actions`
#> Parametric Models [32]: 
#> [01]: `1_effic(0.1)_thick(0.045)_condu(0.5)`     <-- SUCCEEDED
#> [02]: `2_effic(0.325)_thick(0.045)_condu(0.5)`   <-- SUCCEEDED
#> [03]: `3_effic(0.325)_thick(0.045)_condu(0.4)`   <-- SUCCEEDED
#> [04]: `4_effic(0.325)_thick(0.0625)_condu(0.4)`  <-- SUCCEEDED
#> [05]: `5_effic(1)_thick(0.0625)_condu(0.5)`      <-- SUCCEEDED
#> [06]: `6_effic(1)_thick(0.045)_condu(0.5)`       <-- SUCCEEDED
#> [07]: `7_effic(1)_thick(0.045)_condu(0.6)`       <-- SUCCEEDED
#> [08]: `8_effic(0.775)_thick(0.045)_condu(0.6)`   <-- SUCCEEDED
#> [09]: `9_effic(0.325)_thick(0.045)_condu(0.6)`   <-- SUCCEEDED
#> [10]: `10_effic(0.55)_thick(0.045)_condu(0.6)`   <-- SUCCEEDED
#> [11]: `11_effic(0.55)_thick(0.045)_condu(0.5)`   <-- SUCCEEDED
#> [12]: `12_effic(0.55)_thick(0.0625)_condu(0.5)`  <-- SUCCEEDED
#> [13]: `13_effic(0.325)_thick(0.01)_condu(0.2)`   <-- SUCCEEDED
#> [14]: `14_effic(0.325)_thick(0.0275)_condu(0.2)` <-- SUCCEEDED
#> [15]: `15_effic(0.55)_thick(0.0275)_condu(0.2)`  <-- SUCCEEDED
#> [16]: `16_effic(0.55)_thick(0.0275)_condu(0.1)`  <-- SUCCEEDED
#> [17]: `17_effic(0.55)_thick(0.01)_condu(0.6)`    <-- SUCCEEDED
#> [18]: `18_effic(0.55)_thick(0.0275)_condu(0.6)`  <-- SUCCEEDED
#> [19]: `19_effic(0.325)_thick(0.0275)_condu(0.6)` <-- SUCCEEDED
#> [20]: `20_effic(0.325)_thick(0.0275)_condu(0.5)` <-- SUCCEEDED
#> [21]: `21_effic(0.1)_thick(0.08)_condu(0.2)`     <-- SUCCEEDED
#> [22]: `22_effic(0.1)_thick(0.08)_condu(0.1)`     <-- SUCCEEDED
#> [23]: `23_effic(0.1)_thick(0.0625)_condu(0.1)`   <-- SUCCEEDED
#> [24]: `24_effic(0.325)_thick(0.0625)_condu(0.1)` <-- SUCCEEDED
#> [25]: `25_effic(0.1)_thick(0.01)_condu(0.4)`     <-- SUCCEEDED
#> [26]: `26_effic(0.1)_thick(0.01)_condu(0.3)`     <-- SUCCEEDED
#> [27]: `27_effic(0.1)_thick(0.0275)_condu(0.3)`   <-- SUCCEEDED
#> [28]: `28_effic(0.325)_thick(0.0275)_condu(0.3)` <-- SUCCEEDED
#> [29]: `29_effic(0.1)_thick(0.0275)_condu(0.2)`   <-- SUCCEEDED
#> [30]: `30_effic(0.1)_thick(0.045)_condu(0.2)`    <-- SUCCEEDED
#> [31]: `31_effic(0.1)_thick(0.045)_condu(0.3)`    <-- SUCCEEDED
#> [32]: `32_effic(0.325)_thick(0.045)_condu(0.3)`  <-- SUCCEEDED
#>  Simulation started at `2019-08-28 02:18:46` and completed successfully after 13.36 secs.

# extract output
# here is just am example
eng <- sen$tabular_data(table_name = "site and source energy",
    column_name = "energy per total building area",
    row_name = "total site energy")[, as.numeric(value)]

# calculate sensitivity
(result <- sen$evaluate(eng))
#> 
#> Call:
#> sensitivity::morris(model = NULL, factors = fctr, r = .r, design = list(type = "oat",     levels = par$num$meta$levels, grid.jump = .grid_jump), binf = par$num$meta$min,     bsup = par$num$meta$max, scale = FALSE)
#> 
#> Model runs: 32 
#>                    mu  mu.star     sigma
#> efficiency  -1.072222 1.072222 0.9171499
#> thickness   -7.928571 8.071429 4.6567297
#> conducitivy  0.212500 0.487500 0.5718079

# extract data
attr(result, "data")
#>    index        name        mu  mu.star     sigma
#> 1:     1  efficiency -1.072222 1.072222 0.9171499
#> 2:     2   thickness -7.928571 8.071429 4.6567297
#> 3:     3 conducitivy  0.212500 0.487500 0.5718079
```

Plot

``` r
# plot
plot(result)
```

![](man/figures/get-started-1.png)<!-- -->
