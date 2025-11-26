# Helper functions used across tests
total_energy <- function(job) {
    as.numeric(
        job$tabular_data(
            table_name = "Site and Source Energy",
            column_name = "Total Energy",
            row_name = "Total Site Energy"
        )$value
    )
}

not_comfort_hours <- function(job) {
    as.numeric(
        job$tabular_data(
            table_name = "Comfort and Setpoint Not Met Summary",
            column_name = "Facility",
            row_name = "Time Not Comfortable Based on Simple ASHRAE 55-2004"
        )$value
    )
}

rotate_building <- function(idf, degree = 0L) {
    if (!idf$is_valid_class("Building")) {
        stop("Input model does not have a Building object")
    }

    if (degree > 360 || degree < -360) {
        stop("Input degree should in range [-360, 360]")
    }

    cur <- idf$Building$North_Axis
    new <- cur + degree

    if (new > 360) {
        new <- new %% 360
        warning("Calculated new north axis is greater than 360. Final north axis will be ", new)
    } else if (new < -360) {
        new <- new %% -360
        warning("Calculated new north axis is smaller than -360. Final north axis will be ", new)
    }

    idf$Building$North_Axis <- new
    idf
}

modify_building <- function(idf, degree = 0L, thickness = 0.1) {
    idf$Building$North_Axis <- degree
    idf
}

# Functional tests
test_that("GAOptimJob basic workflow", {
    skip_if_not(eplusr::is_avail_eplus(23.1))

    path_idf <- eplusr::path_eplus_example(23.1, "RefBldgLargeOfficeNew2004_Chicago.idf")
    path_epw <- eplusr::path_eplus_weather(23.1, "USA_CA_San.Francisco.Intl.AP.724940_TMY3.epw")

    # Create job
    expect_s3_class(gaopt <- gaoptim_job(path_idf, path_epw), "GAOptimJob")
    expect_s3_class(gaopt, "ParametricJob")
    expect_s3_class(gaopt, "EplusGroupJob")

    # Configure GA operators (all methods return self for chaining)
    expect_identical(gaopt$mutator(), gaopt)
    expect_identical(gaopt$recombinator(), gaopt)
    expect_identical(gaopt$selector(), gaopt)
    expect_identical(gaopt$terminator(max_gen = 3), gaopt)

    # Apply measure
    expect_identical(gaopt$apply_measure(
        rotate_building,
        degree = paradox::p_dbl(0, 360)
    ), gaopt)

    # Set objectives
    expect_identical(gaopt$objective(
        total_energy = total_energy,
        -not_comfort_hours := not_comfort_hours
    ), gaopt)

    # Run optimization
    expect_identical(gaopt$run(mu = 2L, dir = tempdir()), gaopt)

    # Get population
    pop <- gaopt$population()
    expect_s3_class(pop, "data.table")
    expect_true(nrow(pop) > 0)
    expect_true("index_gen" %in% names(pop))
    expect_true("index_ind" %in% names(pop))
    expect_true("degree" %in% names(pop))
    expect_true("total_energy" %in% names(pop))
    expect_true("not_comfort_hours" %in% names(pop))
    expect_equal(max(pop$index_gen), 3)  # 3 generations

    # Get Pareto set
    pareto <- gaopt$pareto_set()
    expect_s3_class(pareto, "data.table")
    expect_true(nrow(pareto) > 0)
    expect_true(nrow(pareto) <= nrow(pop))  # Pareto set is subset of population
    expect_true("degree" %in% names(pareto))
    expect_true("total_energy" %in% names(pareto))
    expect_true("not_comfort_hours" %in% names(pareto))

    # Verify parameter values are within bounds
    expect_true(all(pareto$degree >= 0 & pareto$degree <= 360))
})

# Print method tests
test_that("$print() works with minimal configuration", {
    skip_if_not(eplusr::is_avail_eplus(23.1))

    path_idf <- eplusr::path_eplus_example(23.1, "1ZoneUncontrolled.idf")
    path_epw <- eplusr::path_eplus_weather(23.1, "USA_CA_San.Francisco.Intl.AP.724940_TMY3.epw")
    gaopt <- gaoptim_job(path_idf, path_epw)

    expect_snapshot(gaopt$print())
})

test_that("$print() works with measure and parameters", {
    skip_if_not(eplusr::is_avail_eplus(23.1))

    path_idf <- eplusr::path_eplus_example(23.1, "1ZoneUncontrolled.idf")
    path_epw <- eplusr::path_eplus_weather(23.1, "USA_CA_San.Francisco.Intl.AP.724940_TMY3.epw")

    gaopt <- gaoptim_job(path_idf, path_epw)

    gaopt$apply_measure(
        rotate_building,
        degree = paradox::p_dbl(0, 360)
    )

    expect_snapshot(gaopt$print())
})

test_that("$print() works with objectives", {
    skip_if_not(eplusr::is_avail_eplus(23.1))

    path_idf <- eplusr::path_eplus_example(23.1, "1ZoneUncontrolled.idf")
    path_epw <- eplusr::path_eplus_weather(23.1, "USA_CA_San.Francisco.Intl.AP.724940_TMY3.epw")

    gaopt <- gaoptim_job(path_idf, path_epw)

    gaopt$apply_measure(
        rotate_building,
        degree = paradox::p_dbl(0, 360)
    )

    gaopt$objective(
        energy = total_energy,
        comfort = not_comfort_hours
    )

    expect_snapshot(gaopt$print())
})

test_that("$print() works with GA operators", {
    skip_if_not(eplusr::is_avail_eplus(23.1))

    path_idf <- eplusr::path_eplus_example(23.1, "1ZoneUncontrolled.idf")
    path_epw <- eplusr::path_eplus_weather(23.1, "USA_CA_San.Francisco.Intl.AP.724940_TMY3.epw")

    gaopt <- gaoptim_job(path_idf, path_epw)

    gaopt$mutator()
    gaopt$recombinator()
    gaopt$selector()
    gaopt$terminator(max_gen = 100, max_eval = 1000)

    expect_snapshot(gaopt$print())
})

test_that("$print() shows parameter-specific operators", {
    skip_if_not(eplusr::is_avail_eplus(23.1))

    path_idf <- eplusr::path_eplus_example(23.1, "1ZoneUncontrolled.idf")
    path_epw <- eplusr::path_eplus_weather(23.1, "USA_CA_San.Francisco.Intl.AP.724940_TMY3.epw")

    gaopt <- gaoptim_job(path_idf, path_epw)

    gaopt$apply_measure(
        modify_building,
        degree = paradox::p_dbl(0, 360),
        thickness = paradox::p_dbl(0.1, 0.5)
    )

    gaopt$mutator(
        degree = miesmuschel::mut("gauss", sdev = 0.1)
    )

    expect_snapshot(gaopt$print())
})
