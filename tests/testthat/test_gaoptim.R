context("Optimization Implementation")

test_that("GAOptimJob Class", {
    skip_if_not(eplusr::is_avail_eplus(8.8))

    path_idf <- file.path(eplusr::eplus_config(8.8)$dir, "ExampleFiles", "RefBldgLargeOfficeNew2004_Chicago.idf")
    path_epw <- file.path(eplusr::eplus_config(8.8)$dir, "WeatherData", "USA_CA_San.Francisco.Intl.AP.724940_TMY3.epw")
    ga <- gaoptim_job(path_idf, path_epw)
})
