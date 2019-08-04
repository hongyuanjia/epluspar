
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

``` r
# use an example file from EnergyPlus v8.8 for demonstration here
path_idf <- file.path(eplusr::eplus_config(8.8)$dir, "ExampleFiles", "5Zone_Transformer.idf")
path_epw <- file.path(eplusr::eplus_config(8.8)$dir, "WeatherData", "USA_CA_San.Francisco.Intl.AP.724940_TMY3.epw")

# create a `SensitivityJob` class which inheris from eplusr::ParametricJob class
sen <- Sensitivity$new(path_idf, path_epw)
#> IDD v8.8.0 has not been parsed before.
#> Try to locate `Energy+.idd` in EnergyPlus v8.8.0 installation folder `C:/EnergyPlusV8-8-0`.
#> IDD file found: `C:\EnergyPlusV8-8-0\Energy+.idd`.
#> Start parsing...
#> Parsing completed.
#> Adding an object in class `Output:SQLite` and setting its `Option Type` to `SimpleAndTabular` in order to create SQLite output file.

# set parameter using similar syntax to `Idf$set()` in eplusr
sen$param(
    # Syntax: Object Name = list(Field = c(Min, Max, Levels))
    GP01 = list(Thickness = c(min = 0.01, max = 1, levels = 5)),
    `Supply Fan 1` = list(Fan_Total_Efficiency = c(0.1, 1.0, 5)),
    # See `r` and `grid_jump` in `sensitivity::morris`
    .rep = 12, .grid_jump = 4
)
#> Warning in sensitivity::morris(model = NULL, factors = num_info$path, r
#> = .rep, : keeping 6 repetitions out of 12
#> -- EnergPlus Parametric Job -----------------------------------------------
#> Seed Model: `C:\EnergyPlusV8-8-0\ExampleFiles\5Zone_Transformer.idf`
#> Weather: `C:\EnergyPlusV8-8-0\WeatherData\USA_CA_San.Francisco.Intl.AP...
#> EnergyPlus Version: `8.8.0`
#> EnergyPlus Path: `C:\EnergyPlusV8-8-0`
#> Applied Measure: ``
#> Parametric Models [18]: 
#>   - 1
#>   - 2
#>   - 3
#>   - 4
#>   - 5
#>   - 6
#>   - 7
#>   - 8
#>   - 9
#>   - 10
#>   - 11
#>   - 12
#>   - 13
#>   - 14
#>   - 15
#>   - 16
#>   - 17
#>   - 18
#> << Job has not been run before >>

# get samples
# returns a data.table with similar structure to `Idf$to_table()`.
sen$samples()
#>     case  id         name              class index                field
#>  1:    1  50         GP01           Material     3            Thickness
#>  2:    1 192 Supply Fan 1 Fan:VariableVolume     3 Fan_Total_Efficiency
#>  3:    2  50         GP01           Material     3            Thickness
#>  4:    2 192 Supply Fan 1 Fan:VariableVolume     3 Fan_Total_Efficiency
#>  5:    3  50         GP01           Material     3            Thickness
#>  6:    3 192 Supply Fan 1 Fan:VariableVolume     3 Fan_Total_Efficiency
#>  7:    4  50         GP01           Material     3            Thickness
#>  8:    4 192 Supply Fan 1 Fan:VariableVolume     3 Fan_Total_Efficiency
#>  9:    5  50         GP01           Material     3            Thickness
#> 10:    5 192 Supply Fan 1 Fan:VariableVolume     3 Fan_Total_Efficiency
#> 11:    6  50         GP01           Material     3            Thickness
#> 12:    6 192 Supply Fan 1 Fan:VariableVolume     3 Fan_Total_Efficiency
#> 13:    7  50         GP01           Material     3            Thickness
#> 14:    7 192 Supply Fan 1 Fan:VariableVolume     3 Fan_Total_Efficiency
#> 15:    8  50         GP01           Material     3            Thickness
#> 16:    8 192 Supply Fan 1 Fan:VariableVolume     3 Fan_Total_Efficiency
#> 17:    9  50         GP01           Material     3            Thickness
#> 18:    9 192 Supply Fan 1 Fan:VariableVolume     3 Fan_Total_Efficiency
#> 19:   10  50         GP01           Material     3            Thickness
#> 20:   10 192 Supply Fan 1 Fan:VariableVolume     3 Fan_Total_Efficiency
#> 21:   11  50         GP01           Material     3            Thickness
#> 22:   11 192 Supply Fan 1 Fan:VariableVolume     3 Fan_Total_Efficiency
#> 23:   12  50         GP01           Material     3            Thickness
#> 24:   12 192 Supply Fan 1 Fan:VariableVolume     3 Fan_Total_Efficiency
#> 25:   13  50         GP01           Material     3            Thickness
#> 26:   13 192 Supply Fan 1 Fan:VariableVolume     3 Fan_Total_Efficiency
#> 27:   14  50         GP01           Material     3            Thickness
#> 28:   14 192 Supply Fan 1 Fan:VariableVolume     3 Fan_Total_Efficiency
#> 29:   15  50         GP01           Material     3            Thickness
#> 30:   15 192 Supply Fan 1 Fan:VariableVolume     3 Fan_Total_Efficiency
#> 31:   16  50         GP01           Material     3            Thickness
#> 32:   16 192 Supply Fan 1 Fan:VariableVolume     3 Fan_Total_Efficiency
#> 33:   17  50         GP01           Material     3            Thickness
#> 34:   17 192 Supply Fan 1 Fan:VariableVolume     3 Fan_Total_Efficiency
#> 35:   18  50         GP01           Material     3            Thickness
#> 36:   18 192 Supply Fan 1 Fan:VariableVolume     3 Fan_Total_Efficiency
#>     case  id         name              class index                field
#>     value
#>  1:     1
#>  2:   0.1
#>  3:  0.01
#>  4:   0.1
#>  5:  0.01
#>  6:     1
#>  7:     1
#>  8:   0.1
#>  9:     1
#> 10:     1
#> 11:  0.01
#> 12:     1
#> 13:  0.01
#> 14:     1
#> 15:  0.01
#> 16:   0.1
#> 17:     1
#> 18:   0.1
#> 19:     1
#> 20:     1
#> 21:  0.01
#> 22:     1
#> 23:  0.01
#> 24:   0.1
#> 25:  0.01
#> 26:     1
#> 27:     1
#> 28:     1
#> 29:     1
#> 30:   0.1
#> 31:  0.01
#> 32:   0.1
#> 33:     1
#> 34:   0.1
#> 35:     1
#> 36:     1
#>     value

# run simulations in temporary directory
sen$run(dir = tempdir())
#>  1|RUNNING    --> [IDF]`1.idf` + [EPW]`USA_CA_San.Francisco.Intl.AP.72...
#>  2|RUNNING    --> [IDF]`2.idf` + [EPW]`USA_CA_San.Francisco.Intl.AP.72...
#>  3|RUNNING    --> [IDF]`3.idf` + [EPW]`USA_CA_San.Francisco.Intl.AP.72...
#>  4|RUNNING    --> [IDF]`4.idf` + [EPW]`USA_CA_San.Francisco.Intl.AP.72...
#>  1|COMPLETED  --> [IDF]`1.idf` + [EPW]`USA_CA_San.Francisco.Intl.AP.72...
#>  5|RUNNING    --> [IDF]`5.idf` + [EPW]`USA_CA_San.Francisco.Intl.AP.72...
#>  4|COMPLETED  --> [IDF]`4.idf` + [EPW]`USA_CA_San.Francisco.Intl.AP.72...
#>  6|RUNNING    --> [IDF]`6.idf` + [EPW]`USA_CA_San.Francisco.Intl.AP.72...
#>  7|RUNNING    --> [IDF]`7.idf` + [EPW]`USA_CA_San.Francisco.Intl.AP.72...
#>  8|RUNNING    --> [IDF]`8.idf` + [EPW]`USA_CA_San.Francisco.Intl.AP.72...
#>  5|COMPLETED  --> [IDF]`5.idf` + [EPW]`USA_CA_San.Francisco.Intl.AP.72...
#>  9|RUNNING    --> [IDF]`9.idf` + [EPW]`USA_CA_San.Francisco.Intl.AP.72...
#>  8|COMPLETED  --> [IDF]`8.idf` + [EPW]`USA_CA_San.Francisco.Intl.AP.72...
#> 10|RUNNING    --> [IDF]`10.idf` + [EPW]`USA_CA_San.Francisco.Intl.AP.7...
#> 11|RUNNING    --> [IDF]`11.idf` + [EPW]`USA_CA_San.Francisco.Intl.AP.7...
#> 12|RUNNING    --> [IDF]`12.idf` + [EPW]`USA_CA_San.Francisco.Intl.AP.7...
#>  9|COMPLETED  --> [IDF]`9.idf` + [EPW]`USA_CA_San.Francisco.Intl.AP.72...
#> 13|RUNNING    --> [IDF]`13.idf` + [EPW]`USA_CA_San.Francisco.Intl.AP.7...
#> 10|COMPLETED  --> [IDF]`10.idf` + [EPW]`USA_CA_San.Francisco.Intl.AP.7...
#> 14|RUNNING    --> [IDF]`14.idf` + [EPW]`USA_CA_San.Francisco.Intl.AP.7...
#> 11|COMPLETED  --> [IDF]`11.idf` + [EPW]`USA_CA_San.Francisco.Intl.AP.7...
#> 15|RUNNING    --> [IDF]`15.idf` + [EPW]`USA_CA_San.Francisco.Intl.AP.7...
#> 12|COMPLETED  --> [IDF]`12.idf` + [EPW]`USA_CA_San.Francisco.Intl.AP.7...
#> 16|RUNNING    --> [IDF]`16.idf` + [EPW]`USA_CA_San.Francisco.Intl.AP.7...
#> 13|COMPLETED  --> [IDF]`13.idf` + [EPW]`USA_CA_San.Francisco.Intl.AP.7...
#> 17|RUNNING    --> [IDF]`17.idf` + [EPW]`USA_CA_San.Francisco.Intl.AP.7...
#> 14|COMPLETED  --> [IDF]`14.idf` + [EPW]`USA_CA_San.Francisco.Intl.AP.7...
#> 18|RUNNING    --> [IDF]`18.idf` + [EPW]`USA_CA_San.Francisco.Intl.AP.7...
#> 15|COMPLETED  --> [IDF]`15.idf` + [EPW]`USA_CA_San.Francisco.Intl.AP.7...
#> 16|COMPLETED  --> [IDF]`16.idf` + [EPW]`USA_CA_San.Francisco.Intl.AP.7...
#> 17|COMPLETED  --> [IDF]`17.idf` + [EPW]`USA_CA_San.Francisco.Intl.AP.7...
#> 18|COMPLETED  --> [IDF]`18.idf` + [EPW]`USA_CA_San.Francisco.Intl.AP.7...
#> -- EnergPlus Parametric Job -----------------------------------------------
#> Seed Model: `C:\EnergyPlusV8-8-0\ExampleFiles\5Zone_Transformer.idf`
#> Weather: `C:\EnergyPlusV8-8-0\WeatherData\USA_CA_San.Francisco.Intl.AP...
#> EnergyPlus Version: `8.8.0`
#> EnergyPlus Path: `C:\EnergyPlusV8-8-0`
#> Applied Measure: ``
#> Parametric Models [18]: 
#>   - 1
#>   - 2
#>   - 3
#>   - 4
#>   - 5
#>   - 6
#>   - 7
#>   - 8
#>   - 9
#>   - 10
#>   - 11
#>   - 12
#>   - 13
#>   - 14
#>   - 15
#>   - 16
#>   - 17
#>   - 18
#>  Simulation started at `2019-08-05 06:42:31` and completed successfully after 43.62 secs.

# extract output
# here is just am example
eng <- sen$tabular_data(table_name = "site and source energy",
    column_name = "energy per total building area",
    row_name = "total site energy")[, as.numeric(value)]

# calculate sensitivity
(result <- sen$evaluate(eng))
#> 
#> Call:
#> sensitivity::morris(model = NULL, factors = num_info$path, r = .rep,     design = list(type = "oat", levels = num_info$levels, grid.jump = .grid_jump),     binf = num_info$min, bsup = num_info$max, scale = FALSE)
#> 
#> Model runs: 18 
#>                                                                          mu
#> Material-->GP01[50]-->3:Thickness                               -0.07070707
#> Fan:VariableVolume-->Supply Fan 1[192]-->3:Fan_Total_Efficiency -0.35555556
#>                                                                    mu.star
#> Material-->GP01[50]-->3:Thickness                               0.07070707
#> Fan:VariableVolume-->Supply Fan 1[192]-->3:Fan_Total_Efficiency 0.35555556
#>                                                                      sigma
#> Material-->GP01[50]-->3:Thickness                               0.03319531
#> Fan:VariableVolume-->Supply Fan 1[192]-->3:Fan_Total_Efficiency 0.03651484

# extract data
attr(result, "data")
#>    index                                                            name
#> 1:     1                               Material-->GP01[50]-->3:Thickness
#> 2:     2 Fan:VariableVolume-->Supply Fan 1[192]-->3:Fan_Total_Efficiency
#>             mu    mu.star      sigma
#> 1: -0.07070707 0.07070707 0.03319531
#> 2: -0.35555556 0.35555556 0.03651484

# plot
plot(result)
```

![](man/figures/get-started-1.png)<!-- -->
