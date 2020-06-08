context("BayesCalib Implementation")

test_that("BayesCalib Class", {
    skip_if_not(eplusr::is_avail_eplus(8.8))

    path_idf <- file.path(eplusr::eplus_config(8.8)$dir, "ExampleFiles", "RefBldgLargeOfficeNew2004_Chicago.idf")
    path_epw <- file.path(eplusr::eplus_config(8.8)$dir, "WeatherData", "USA_CA_San.Francisco.Intl.AP.724940_TMY3.epw")
    bc <- bayes_job(path_idf, path_epw)

    # Output:Variable and Output:Meter should be removed
    expect_false(bc$seed()$is_valid_class("Output:Variable"))
    expect_false(bc$seed()$is_valid_class("Output:Meter"))

    expect_message(bc$read_rdd())
    expect_is(bc$read_rdd(), "RddFile")
    expect_equal(nrow(bc$read_rdd()), 620)

    expect_is(bc$read_mdd(), "MddFile")
    expect_equal(nrow(bc$read_mdd()), 177)

    # $input() {{{
    expect_null(bc$input(append = NULL))
    expect_error(bc$input(name = "a"), class = "error_bc_invalid_input")
    expect_error(bc$input(bc$read_mdd()), class = "error_bc_invalid_input")

    # using rdd or mdd
    expect_equal(bc$input(bc$read_rdd()[1L]),
        data.table(index = 1L, class = "Output:Variable", key_value = "*",
            variable_name = "Site Outdoor Air Drybulb Temperature",
            reporting_frequency = "Timestep"
        )
    )

    # can stop if input one has already been set
    expect_error(bc$input(bc$read_rdd()[1L], append = TRUE))

    # can stop if input one has already been set as output
    expect_null(bc$input(append = NULL))
    expect_silent(bc$output(bc$read_rdd()[1L]))
    expect_error(bc$input(bc$read_rdd()[1L]), class = "error_bc_invalid_input")
    expect_null(bc$output(append = NULL))

    # can stop if input reporting frequency is not the same as existing one
    expect_silent(bc$input(bc$read_rdd()[1L]))
    expect_error(bc$input(bc$read_rdd()[1L], append = TRUE, reporting_frequency = "hourly"),
        class = "error_bc_invalid_input"
    )

    # can customize reporting frequency
    expect_equal(bc$input(bc$read_rdd()[1L], reporting_frequency = "hourly"),
        data.table(index = 1L, class = "Output:Variable", key_value = "*",
            variable_name = "Site Outdoor Air Drybulb Temperature",
            reporting_frequency = "Hourly"
        )
    )

    # can retrieve existing ones
    expect_equal(bc$input(),
        data.table(index = 1L, class = "Output:Variable", key_value = "*",
            variable_name = "Site Outdoor Air Drybulb Temperature",
            reporting_frequency = "Hourly"
        )
    )

    # can remove all existing ones
    expect_null(bc$input(append = NULL))

    # input can not be inserted if there is one with key value being "*"
    expect_silent(bc$input(bc$read_rdd()[1]))
    expect_error(bc$input(bc$read_rdd()[1][, key_value := "Environment"], append = TRUE),
        class = "error_bc_invalid_input"
    )

    # input with key value being "*" can not be inserted if there is one with
    # specific key value
    expect_silent(bc$input(bc$read_rdd()[1][, key_value := "Environment"]))
    expect_error(bc$input(bc$read_rdd()[1], append = TRUE), class = "error_bc_invalid_input")

    # can stop if incorrect column supplied
    expect_error(bc$input(data.frame()), class = "error_bc_invalid_input")

    # can stop if invalid class name supplied
    expect_error(bc$input(data.table(class = "a", index = 1, value = "a")), class = "error_bc_invalid_input")

    # can stop if invalid index supplied
    expect_error(bc$input(data.table(class = "Output:Variable", index = 1.1, value = "a")), class = "error_bc_invalid_input")
    expect_error(bc$input(data.table(class = "Output:Variable", index = NA, value = "a")), class = "error_bc_invalid_input")

    # can stop if duplications
    expect_error(bc$input(data.table(id = c(1L, 1L), class = "Output:Variable", index = c(1L, 1L), value = "a")), class = "error_bc_invalid_input")

    # can stop if invalid field number provided
    expect_error(bc$input(data.table(id = 1L, class = "Output:Variable", index = 1:5, value = "a")), class = "error_bc_invalid_input")
    expect_error(bc$input(data.table(id = 1L, class = "Output:Meter", index = 1:3, value = "a")), class = "error_bc_invalid_input")

    # can stop if Output:Meter is provided
    expect_error(bc$input(rbindlist(list(rdd_to_load(bc$read_rdd()[1:2]), mdd_to_load(bc$read_mdd()[1:2])))), class = "error_bc_invalid_input")

    expect_equal(bc$input(eplusr::rdd_to_load(bc$read_rdd()[1L], reporting_frequency = "hourly")),
        data.table(index = 1L, class = "Output:Variable", key_value = "*",
            variable_name = "Site Outdoor Air Drybulb Temperature",
            reporting_frequency = "Hourly"
        )
    )

    # can stop if input variables do not have the same reporting frequency
    expect_error(bc$input(bc$read_rdd()[2L], append = TRUE))
    expect_equal(bc$input(bc$read_rdd()[2L], reporting_frequency = "hourly", append = TRUE),
        data.table(index = 1L:2L, class = "Output:Variable", key_value = "*",
            variable_name = paste0("Site Outdoor Air ", c("Drybulb", "Dewpoint"), " Temperature"),
            reporting_frequency = "Hourly"
        )
    )

    # can take directly variable names
    expect_silent(bc$input(
        c("Environment", "environment"),
        c("site outdoor air wetbulb temperature", "site outdoor air humidity ratio")
    ))
    expect_silent(bc$input(
        c("Environment"),
        c("site outdoor air wetbulb temperature", "site outdoor air humidity ratio")
    ))
    expect_silent(bc$input(
        name = c("site outdoor air wetbulb temperature", "site outdoor air humidity ratio")
    ))
    # }}}

    # $output() {{{
    # can stop if input one has already been set as output
    expect_silent(bc$input(bc$read_rdd()[1L]))
    expect_error(bc$output(bc$read_rdd()[1L]), class = "error_bc_invalid_output")
    # can stop if input reporting frequency is not the same
    expect_error(bc$output(mdd_to_load(bc$read_mdd()[1L]), reporting_frequency = "hourly"))
    # }}}

    # $param(), $samples() {{{
    # give message if input and output are not set
    expect_null(bc$input(append = NULL))
    expect_null(bc$output(append = NULL))
    expect_message(bc$models())
    expect_error(bc$param(), class = "error_bc_empty_param_input")
    expect_message(
        bc$param(
            ZoneInfiltration_DesignFlowRate := list(flow_per_exterior_surface_area = c(0.0003, 0.001)),
            Lights := list(watts_per_zone_floor_area = c(5, 20)),
            ElectricEquipment := list(watts_per_zone_floor_area = c(5, 20)),
            `CoolSys1 Chiller 1` = list(reference_cop = c(1, 5)),
            `CoolSys1 Chiller 2` = list(reference_cop = c(1, 5)),
            VAV_1_Fan = list(fan_total_efficiency = c(0.1, 0.9)),
            VAV_2_Fan = list(fan_total_efficiency = c(0.1, 0.9)),
            list(
              VAV_3_Fan = list(fan_total_efficiency = c(0.1, 0.9)),
              VAV_5_Fan = list(fan_total_efficiency = c(0.1, 0.9))
            ),
            # num of simulations
            .num_sim = 2L
        ),
        "No parametric models have been created"
    )

    expect_is(smpl <- (bc$samples()), "data.table")
    expect_equal(nrow(smpl), 2L)
    expect_equal(ncol(smpl), 10L)
    expect_equal(names(smpl), c("case", paste0("t", 1:9)))
    # }}}

    # $models() {{{
    # should return NULL if no input and output are set
    expect_message(m <- bc$models())
    expect_null(m)

    # can create parametric models
    expect_silent(bc$input(bc$read_rdd()[1L:2L]))
    expect_silent(bc$output(bc$read_mdd()[1L:2L]))
    expect_silent(m <- bc$models())
    expect_is(m, "list")
    expect_true(all(sapply(m, eplusr::is_idf)))
    # }}}

    # $data_sim() {{{
    # can stop if invalid key value
    expect_silent(bc$input(bc$read_rdd()[1L:2L][, key_value := "a"]))
    bc$eplus_run(tempdir(), echo = FALSE)
    expect_error(bc$data_sim(), class = "error_bc_input_invalid_key_value")

    expect_silent(bc$input(bc$read_rdd()[c(10, 4, 1)]))
    bc$eplus_run(tempdir(), run_period = list("bc", 1, 1, 1, 3), echo = FALSE)

    # can extract sim data
    expect_silent(dt <- bc$data_sim())
    expect_equal(names(dt), c("input", "output"))
    expect_equal(names(dt$input), c("case", "Date/Time",
        "Environment:Site Horizontal Infrared Radiation Rate per Area [W/m2](TimeStep)",
        "Environment:Site Outdoor Air Humidity Ratio [kgWater/kgDryAir](TimeStep)",
        "Environment:Site Outdoor Air Drybulb Temperature [C](TimeStep)"
    ))
    expect_equal(names(dt$output), c("case", "Date/Time",
        "Electricity:Building [J](TimeStep)",
        "Electricity:Facility [J](TimeStep)"
    ))
    expect_equal(nrow(dt$input), 864)
    expect_equal(nrow(dt$output), 864)

    # can stop if input resolution is smaller than reporting frequency
    expect_error(bc$data_sim(resolution = "1 min"), class = "error_bc_invalid_resolution")
    # can stop if input resolution is not divisible by reporting frequency
    expect_error(bc$data_sim(resolution = "13 min"), class = "error_bc_invalid_resolution")
    # can change data resolution
    expect_silent(dt <- bc$data_sim(resolution = "1 day"))
    expect_equal(nrow(dt$input), 6)
    expect_equal(nrow(dt$output), 6)
    expect_equal(names(dt$input), c("case", "Date/Time",
        "Environment:Site Horizontal Infrared Radiation Rate per Area [W/m2](1 Day)",
        "Environment:Site Outdoor Air Humidity Ratio [kgWater/kgDryAir](1 Day)",
        "Environment:Site Outdoor Air Drybulb Temperature [C](1 Day)"
    ))
    expect_equal(names(dt$output), c("case", "Date/Time",
        "Electricity:Building [J](1 Day)",
        "Electricity:Facility [J](1 Day)"
    ))

    expect_silent(dt <- bc$data_sim(resolution = "1 month"))
    expect_equal(nrow(dt$input), 2)
    expect_equal(nrow(dt$output), 2)
    expect_equal(names(dt$input), c("case", "Date/Time",
        "Environment:Site Horizontal Infrared Radiation Rate per Area [W/m2](1 Month)",
        "Environment:Site Outdoor Air Humidity Ratio [kgWater/kgDryAir](1 Month)",
        "Environment:Site Outdoor Air Drybulb Temperature [C](1 Month)"
    ))
    expect_equal(names(dt$output), c("case", "Date/Time",
        "Electricity:Building [J](1 Month)",
        "Electricity:Facility [J](1 Month)"
    ))

    expect_silent(dt <- bc$data_sim(exclude_ddy = FALSE))
    expect_equal(nrow(dt$input), 1440)
    expect_equal(nrow(dt$output), 1440)

    expect_silent(dt <- bc$data_sim(all = TRUE))
    expect_equal(names(dt$input), c("case", "environment_period_index", "environment_name",
        "simulation_days", "datetime", "month", "day", "hour", "minute",
        "day_type", "Date/Time",
        "Environment:Site Horizontal Infrared Radiation Rate per Area [W/m2](TimeStep)",
        "Environment:Site Outdoor Air Humidity Ratio [kgWater/kgDryAir](TimeStep)",
        "Environment:Site Outdoor Air Drybulb Temperature [C](TimeStep)"
    ))
    expect_equal(names(dt$output), c("case", "environment_period_index", "environment_name",
        "simulation_days", "datetime", "month", "day", "hour", "minute",
        "day_type", "Date/Time",
        "Electricity:Building [J](TimeStep)",
        "Electricity:Facility [J](TimeStep)"
    ))
    # }}}

    # $data_field() {{{
    expect_error(bc$data_field(""), class = "error_bc_invalid_data_field_output")
    expect_error(bc$data_field(data.frame()), class = "error_bc_invalid_data_field_output")
    expect_error(bc$data_field(data.frame(a = 1:10, b = 11:20)), class = "error_bc_invalid_data_field_output")

    expect_silent(dt <- bc$data_field(data.frame(a = 1:432, b = 1:432)))
    expect_equal(names(dt), c("input", "output", "new_input"))
    expect_equal(names(dt$input), c("case", "Date/Time",
        "Environment:Site Horizontal Infrared Radiation Rate per Area [W/m2](TimeStep)",
        "Environment:Site Outdoor Air Humidity Ratio [kgWater/kgDryAir](TimeStep)",
        "Environment:Site Outdoor Air Drybulb Temperature [C](TimeStep)"
    ))
    expect_equal(names(dt$output), c("case", "Date/Time",
        "Electricity:Building [J](TimeStep)",
        "Electricity:Facility [J](TimeStep)"
    ))
    expect_equal(nrow(dt$input), 864/2)
    expect_equal(nrow(dt$output), 864/2)
    expect_equal(nrow(dt$new_input), 864/2)
    # }}}

    # $data_bc() {{{
    expect_silent(dt_sim <- bc$data_sim())
    expect_silent(dt_field <- bc$data_field(data.frame(a = 1:432, b = 1:432)))
    expect_silent(bc$data_bc())
    expect_silent(bc$data_bc(data_sim = dt_sim))
    expect_silent(bc$data_bc(data_field = dt_field))
    expect_silent(bc$data_bc(data_field = dt_field, data_sim = dt_sim))
    # }}}

    # a whole game {{{
    path_idf <- file.path(eplusr::eplus_config(8.8)$dir, "ExampleFiles", "RefBldgLargeOfficeNew2004_Chicago.idf")
    path_epw <- file.path(eplusr::eplus_config(8.8)$dir, "WeatherData", "USA_CA_San.Francisco.Intl.AP.724940_TMY3.epw")
    bc <- bayes_job(path_idf, path_epw)

    # set parameters
    expect_equivalent(
        bc$input("CoolSys1 Chiller 1", paste("chiller evaporator", c("inlet temperature", "outlet temperature", "mass flow rate")), "hourly"),
        data.table(index = 1L:3L, class = "Output:Variable",
            key_value = "CoolSys1 Chiller 1",
            variable_name = c(
                "Chiller Evaporator Inlet Temperature",
                "Chiller Evaporator Outlet Temperature",
                "Chiller Evaporator Mass Flow Rate"
            ),
            reporting_frequency = "Hourly"
        )
    )
    expect_equivalent(
        bc$output("CoolSys1 Chiller 1", "chiller electric power", "hourly"),
        data.table(index = 1L, class = "Output:Variable",
            key_value = "CoolSys1 Chiller 1",
            variable_name = "Chiller Electric Power",
            reporting_frequency = "Hourly"
        )
    )

    # set parameter
    bc$param(
        `CoolSys1 Chiller 1` = list(reference_cop = c(4, 6), reference_capacity = c(2.5e6, 3.0e6)),
        .names = c("cop1", "cap1"), .num_sim = 5
    )
    # run simulations
    bc$eplus_run(dir = tempdir(), run_period = list("example", 7, 1, 7, 3), echo = FALSE)
    s <- bc$eplus_status()

    expect_equal(s[c("run_before", "alive", "terminated", "successful")],
        list(run_before = TRUE, alive = FALSE, terminated = FALSE, successful = TRUE)
    )

    # set simulation data
    expect_silent(dt_sim <- bc$data_sim("6 hour"))
    expect_equal(names(dt_sim), c("input", "output"))
    expect_equal(names(dt_sim$input), c("case", "Date/Time",
        "COOLSYS1 CHILLER 1:Chiller Evaporator Inlet Temperature [C](6 Hour)",
        "COOLSYS1 CHILLER 1:Chiller Evaporator Outlet Temperature [C](6 Hour)",
        "COOLSYS1 CHILLER 1:Chiller Evaporator Mass Flow Rate [kg/s](6 Hour)"
    ))
    expect_equal(names(dt_sim$output), c("case", "Date/Time",
        "COOLSYS1 CHILLER 1:Chiller Electric Power [W](6 Hour)"
    ))
    expect_equal(nrow(dt_sim$input), 60)
    expect_equal(nrow(dt_sim$output), 60)

    # use the seed model to get field data
    ## clone the seed model
    seed <- bc$seed()$clone()
    ## remove existing RunPeriod objects
    seed$RunPeriod <- NULL
    ## set run period as the same as in `$eplus_run()`
    seed$add(RunPeriod = list("test", 7, 1, 7, 3))
    seed$SimulationControl$set(
        `Run Simulation for Sizing Periods` = "No",
        `Run Simulation for Weather File Run Periods` = "Yes"
    )
    ## save the model to tempdir
    seed$save(tempfile(fileext = ".idf"))
    ## run
    job <- seed$run(bc$weather(), echo = FALSE)
    ## get output data
    fan_power <- epluspar:::report_dt_aggregate(job$report_data(name = bc$output()$variable_name, all = TRUE), "6 hour")
    fan_power <- eplusr:::report_dt_to_wide(fan_power)
    # add Gaussian noice
    fan_power <- fan_power[, -"Date/Time"][
        , lapply(.SD, function (x) x + rnorm(length(x), sd = 0.05 * sd(x)))][
        , lapply(.SD, function (x) {x[x < 0] <- 0; x})
    ]

    # set field data
    expect_silent(dt_fld <- bc$data_field(fan_power))
    expect_equal(names(dt_fld), c("input", "output", "new_input"))
    expect_equal(names(dt_fld$input), c("case", "Date/Time",
        "COOLSYS1 CHILLER 1:Chiller Evaporator Inlet Temperature [C](6 Hour)",
        "COOLSYS1 CHILLER 1:Chiller Evaporator Outlet Temperature [C](6 Hour)",
        "COOLSYS1 CHILLER 1:Chiller Evaporator Mass Flow Rate [kg/s](6 Hour)"
    ))
    expect_equal(names(dt_fld$output), c("case", "Date/Time",
        "COOLSYS1 CHILLER 1:Chiller Electric Power [W](6 Hour)"
    ))
    expect_equal(nrow(dt_fld$input), 12)
    expect_equal(nrow(dt_fld$output), 12)
    expect_equal(nrow(dt_fld$new_input), 12)

    # check stan input
    expect_silent(stan_data <- bc$data_bc())
    expect_equal(sapply(stan_data, function (x) class(x)[1]),
        c("n" = "integer", "n_pred" = "integer", "m" = "integer", "p" = "integer", "q" = "integer",
          "yf" = "numeric", "yc" = "numeric",
          "xf" = "data.table", "xc" = "data.table", "x_pred" = "data.table", "tc" = "data.table"
        )
    )

    expect_error(bc$prediction(), class = "error_bc_stan_not_ready")

    # run stan
    suppressWarnings(res <- bc$stan_run(iter = 300, chains = 3))

    expect_equal(names(res), c("fit", "y_pred"))
    expect_is(res$fit, "stanfit")
    expect_equal(names(res$y_pred), c("index", "Date/Time",
        "COOLSYS1 CHILLER 1:Chiller Evaporator Inlet Temperature [C](6 Hour)",
        "COOLSYS1 CHILLER 1:Chiller Evaporator Outlet Temperature [C](6 Hour)",
        "COOLSYS1 CHILLER 1:Chiller Evaporator Mass Flow Rate [kg/s](6 Hour)",
        "COOLSYS1 CHILLER 1:Chiller Electric Power [W](6 Hour)",
        "COOLSYS1 CHILLER 1:Chiller Electric Power [W](6 Hour) [Prediction]"
    ))

    expect_is(bc$stan_file(), "character")
    expect_silent(f <- bc$stan_file(tempfile()))
    expect_equal(bc$stan_file(), readLines(f))

    expect_equivalent(bc$prediction(), res$y_pred)

    expect_is(bc$post_dist(), "data.table")
    expect_equal(names(bc$post_dist()), c("cop1", "cap1"))
    expect_equal(nrow(bc$post_dist()), 450)
    # }}}
})
