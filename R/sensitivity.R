#' @include utils.R
#' @importFrom data.table data.table set setorder setattr ":="
#' @importFrom sensitivity morris tell
#' @importFrom purrr pmap
NULL

#' Conduct Sensitivity Analysis for A EnergyPlus Model
#'
#' `sensi_job()` takes an IDF and EPW as input and returns a `SensitivityJob`,
#' which provides a prototype of conducting sensitivity analysis of EnergyPlus
#' simulations using [Morris][sensitivity::morris] method.
#'
#' `SensitivityJob` inherits from [`ParametricJob`][eplusr::param_job()] class,
#' which means that all methods provided by
#' [`ParametricJob`][eplusr::param_job()] class are also available for
#' `SensitivityJob` class.
#'
#' @section Usage:
#' ```
#' sensi <- sensi_job(idf, epw)
#' sensi$version()
#' sensi$seed()
#' sensi$weather()
#' sensi$param(..., .names = NULL, .r = 12L, .grid_jump = 4L)
#' sensi$apply_measure(measure, ..., .r = 12L, .grid_jump = 4L)
#' sensi$samples()
#' sensi$evaluate(results)
#' sensi$models()
#' sensi$save(dir = NULL, separate = TRUE, copy_external = FALSE)
#' sensi$run(dir = NULL, wait = TRUE, force = FALSE, copy_external = FALSE, echo = wait)
#' sensi$kill()
#' sensi$status()
#' sensi$errors(info = FALSE)
#' sensi$output_dir(which = NULL)
#' sensi$locate_output(which = NULL, suffix = ".err", strict = TRUE)
#' sensi$report_data_dict(which = NULL)
#' sensi$report_data(which = NULL, key_value = NULL, name = NULL, year = NULL, tz = "UTC",
#'                   case = "auto", all = FALSE, wide = FALSE, period = NULL, month = NULL,
#'                   day = NULL, hour = NULL, minute = NULL, interval = NULL,
#'                   simulation_days = NULL, day_type = NULL, environment_name = NULL)
#' sensi$tabular_data(which, report_name = NULL, report_for = NULL, table_name = NULL,
#'                    column_name = NULL, row_name = NULL)
#' sensi$print()
#' ```
#' @section Create:
#' ```
#' sensi <- sensi_job(idf, epw)
#' ```
#'
#' **Arguments**
#'
#' * `idf`: Path to EnergyPlus IDF file or an `Idf` object.
#' * `epw`: Path to EnergyPlus EPW file or an `Epw` object. `epw` can also be
#'   `NULL` which will force design-day-only simulation when
#'   [`$run()`][ParametricJob] method is called. Note this needs at least one
#'   `Sizing:DesignDay` object exists in the [Idf].
#'
#' @section Set Parameters:
#' ```
#' sensi$param(..., .names = NULL, .r = 12L, .grid_jump = 4L)
#' sensi$apply_measure(measure, ..., .r = 12L, .grid_jump = 4L)
#' sensi$samples()
#' sensi$models()
#' sensi$evaluate(results)
#' ```
#'
#' There are 2 ways to set sensitivity parameters in `SensitivityJob` class,
#' i.e. `$param()` and `$apply_measure()`.
#'
#' `$param()` takes parameter definitions in list format, which is similar to
#' `$set()` in [eplusr::Idf] class except that each field is not assigned with a
#' single value, but a numeric vector of length 3, indicating the minimum,
#' maximum and number of levels of the parameter. Every list in `$param()`
#' should be named with a valid object name. Object ID can also be used but have
#' to be combined with prevailing two periods `..`, e.g. `..10` indicates the
#' object with ID `10`. There is a special syntax `class := list(field = value)`
#' in `$param()`. Note the use of `:=` instead of `=`. The main difference is
#' that, unlike `=`, the left hand side of `:=` should be a valid class name in
#' current `Idf` object.  It will set the field of all objects in specified
#' class to specified value.
#'
#' For example, the code block below defines 3 parameters:
#'
#' * Field `Fan Total Efficiency` in object named `Supply Fan 1` in class
#'   `Fan:VariableVolume` class, with minimum, maximum and number of levels
#'   being 0.1, 1.0 and 5, respectively.
#' * Field `Thickness` in all objects in class `Material`, with minimum, maximum
#'   and number of levels being 0.01, 1.0 and 5, respectively.
#' * Field `Conductivity` in all objects in class `Material`, with minimum,
#'   maximum and number of levels being 0.1, 0.6 and 10, respectively.
#'
#' ```
#' sensi$param(
#'     `Supply Fan 1` = list(Fan_Total_Efficiency = c(min = 0.1, max = 1.0, levels = 5)),
#'     Material := list(Thickness = c(0.01, 1, 5), Conductivity = c(0.1, 0.6, 10))
#' )
#' ```
#'
#' **Arguments**
#'
#' * `...`: Lists of paramter definitions. Each list should be named with a valid
#'   object name or a valid object ID denoted in style `..1`, `..2` and etc. For
#'   specifying the fields for all objects in a class, the class name instead of
#'   the object name should be used, and a special notation `:=` should be used
#'   instead of `=`, e.g. `class := list(field = value)`.
#' * `.r`: An positive integer specifying the number of elementary effect
#'   computed per factor. For details, see [sensitivity::morris]. Default: `12`.
#' * `.grid_jump` : An integer or a vector of integers specifying the number of
#'   levels that are increased/decreased for computing the elementary effects.
#'   Default: `1L`. For details, see [sensitivity::morris].
#' * `.names`: A character vector of the parameter names. If `NULL`,
#'   the parameter will be named in format `theta + number`. Default: `NULL`.
#'
#' `$apply_measure()` works in a similar way as the `$apply_measure` in
#' [eplusr::ParametricJob] class, with only exception that each argument
#' supplied in `...` should be a numeric vector of length 3, indicating the
#' minimum, maximum and number of levels of the parameter.
#' Basically `$apply_measure()` allows to apply a measure to an [Idf].
#' A measure here is just a function that takes an [Idf] object and other
#' arguments as input, and returns a modified [Idf] object as output. The names
#' of function parameter will be used as the names of sensitivity parameter. For
#' example, the equivalent version of specifying parameters described above
#' using `$apply_measure()` can be:
#'
#' ```
#' measure <- function (idf, efficiency, thickness, conducitivy) {
#'     idf$set(
#'         `Supply Fan 1` = list(Fan_Total_Efficiency = efficiency),
#'         Material := list(Thickness = thickness, Conductivity = conducivity)
#'     )
#'
#'     idf
#' }
#'
#' sensi$apply_measure(measure,
#'     efficiency = c(min = 0.1, max = 1.0, levels = 5),
#'     thickness = c(0.01, 1, 5), conductivity = c(0.1, 0.6, 10)
#' )
#' ```
#'
#' **Arguments**
#'
#' * `measure`: A function that takes an `Idf` and other arguments as input and
#'     returns an `Idf` object as output.
#' * `...`: Arguments **except first `Idf` argument** that are passed to that
#'   `measure`.
#' * `.r`: An positive integer specifying the number of elementary effect
#'   computed per factor. For details, see [sensitivity::morris].
#' * `.grid_jump` : An integer or a vector of integers specifying the number of
#'   levels that are increased/decreased for computing the elementary effects.
#'   For details, see [sensitivity::morris].
#'
#' All models created using `$param()` and `$apply_measure()` will be named in
#' the same pattern, i.e. `Case_ParameterName(ParamterValue)...`. Note that
#' paramter names will be abbreviated using [abbreviate()] with `minlength`
#' being `5L` and `use.classes` being `TRUE`. If samples contain duplications,
#' [make.unique()] will be called to make sure every model has a unique name.
#'
#' `$samples()` returns a [data.table][data.table::data.table()] which contains
#' the sampled value for each parameter using [Morris][sensitivity::morris]
#' method. The returned data.table has `1 + n` columns, where `n` is the
#' parameter number, while `1` indicates an extra column named `case` giving the
#' index of each sample.
#'
#' `$models()` returns a list of parametric [Idf][eplusr::Idf] objects created
#' using sensitivity parameter values genereated using Morris method. This means
#' that parametric models can only be created after sensitivity parameters have
#' been set using `$param()` or `$apply_measure()`.
#'
#' `$evaluate()` takes a numeric vector with the same length as total sample
#' number and returns the a [sensitivity::morris()] object. The statistics of
#' interest (mu, mu* and sigma) are stored as an attribute named `data` and can
#' be retrieved using `atrr(sensi$evaluate(), "data")`.
#'
#' @section Inherited Methods from `ParametricJob`:
#' ```
#' sensi$version()
#' sensi$seed()
#' sensi$weather()
#' sensi$save(dir = NULL, separate = TRUE, copy_external = FALSE)
#' sensi$run(dir = NULL, wait = TRUE, force = FALSE, copy_external = FALSE, echo = wait)
#' sensi$kill()
#' sensi$status()
#' sensi$errors(info = FALSE)
#' sensi$output_dir(which = NULL)
#' sensi$locate_output(which = NULL, suffix = ".err", strict = TRUE)
#' sensi$report_data_dict(which = NULL)
#' sensi$report_data(which = NULL, key_value = NULL, name = NULL, year = NULL, tz = "UTC",
#'                   case = "auto", all = FALSE, wide = FALSE, period = NULL, month = NULL,
#'                   day = NULL, hour = NULL, minute = NULL, interval = NULL,
#'                   simulation_days = NULL, day_type = NULL, environment_name = NULL)
#' sensi$tabular_data(which, report_name = NULL, report_for = NULL, table_name = NULL,
#'                    column_name = NULL, row_name = NULL)
#' sensi$print()
#' ```
#'
#' All methods listed above are inherited from eplusr's
#' [`ParametricJob`][eplusr::ParametricJob]. For detailed documentation on each
#' method, please see [eplusr's documentation][eplusr::ParametricJob].
#'
#' @examples
#' \dontrun{
#' if (eplusr::is_avail_eplus(8.8)) {
#'     idf_name <- "5Zone_Transformer.idf"
#'     epw_name <-  "USA_CA_San.Francisco.Intl.AP.724940_TMY3.epw"
#'
#'     idf_path <- file.path(eplusr::eplus_config(8.8)$dir, "ExampleFiles", idf_name)
#'     epw_path <- file.path(eplusr::eplus_config(8.8)$dir, "WeatherData", epw_name)
#'
#'     # create from local files
#'     sensi_job(idf_path, epw_path)
#'
#'     # create from an Idf and an Epw object
#'     sensi <- sensi_job(read_idf(idf_path), read_epw(epw_path))
#'
#'     # get the seed model
#'     sensi$seed()
#'
#'     # get the weather
#'     sensi$weather()
#'
#'     # get status of current job
#'     sensi$status()
#'
#'     # set sensitivity parameters using $param()
#'     sensi$param(
#'         `Supply Fan 1` = list(Fan_Total_Efficiency = c(min = 0.1, max = 1.0, levels = 5)),
#'         Material := list(Thickness = c(0.01, 1, 5), Conductivity = c(0.1, 0.6, 10))
#'     )
#'
#'     # set sensitivity parameters using $apply_measure()
#'     # (a) first define a "measure"
#'     measure <- function (idf, efficiency, thickness, conducitivy) {
#'         idf$set(
#'             `Supply Fan 1` = list(Fan_Total_Efficiency = efficiency),
#'             Material := list(Thickness = thickness, Conductivity = conducivity)
#'         )
#'         idf
#'     }
#'     # (b) then apply that measure with parameter space definitions as
#'     # function arguments
#'     sensi$apply_measure(measure,
#'         efficiency = c(min = 0.1, max = 1.0, levels = 5),
#'         thickness = c(0.01, 1, 5), conductivity = c(0.1, 0.6, 10)
#'     )
#'
#'     # extract samples
#'     sensi$samples()
#'
#'     # extract all models
#'     sensi$models()
#'
#'     # save all models with each model in a separate folder
#'     sensi$save(tempdir())
#'
#'     # save all parametric models with all models in the same folder
#'     sensi$save(tempdir(), separate = FALSE)
#'
#'     # run parametric simulations
#'     sensi$run(wait = TRUE)
#'
#'     # run in background
#'     sensi$run(wait = FALSE)
#'     # get detailed job status by printing
#'     print(sensi)
#'
#'     # status now includes a data.table with detailed information on each simulation
#'     sensi$status()
#'
#'     # print simulation errors
#'     sensi$errors()
#'
#'     # extract output of all simulations
#'     sensi$report_data()
#'
#'     # extract only some simulations
#'     sensi$report_data(c(1, 3))
#'     sensi$tabular_data(c(1, 3))
#'     sensi$report_data(c("rotate_30", "rotate_120"))
#'     sensi$tabular_data(c("rotate_30", "rotate_120"))
#'
#'     # get output directory
#'     sensi$output_dir()
#'     sensi$output_dir(c(1, 3))
#'
#'     # get path of specific output file
#'     sensi$locate_output(c(1, 3), ".csv")
#'
#'     # extract a target simulation output value for each case to evaluate the
#'     # sensitivity results
#'     eng <- sen$tabular_data(table_name = "site and source energy",
#'         column_name = "energy per total building area",
#'         row_name = "total site energy")[, as.numeric(value)]
#'     (result <- sensi$evaluate(eng))
#'
#'    # extract sensivitity data
#'    attr(result, "data")
#'
#'    # plot
#'    plot(result)
#' }
#' }
#' @docType class
#' @name SensitivityJob
#' @author Hongyuan Jia
NULL

#' Create a Sensitivity Analysis Job
#'
#' `sensi_job()` takes an IDF and EPW as input, and returns an `SensitivityJob`
#' object for conducting sensitivity analysis on an EnergyPlus model. For more
#' details, please see [SensitivityJob].
#'
#' @param idf A path to an local EnergyPlus IDF file or an `Idf` object.
#' @param epw A path to an local EnergyPlus EPW file or an `Epw` object.
#' @return An `SensitivityJob` object.
#' @examples
#' \dontrun{
#' if (eplusr::is_avail_eplus(8.8)) {
#'     idf_name <- "1ZoneUncontrolled.idf"
#'     epw_name <-  "USA_CA_San.Francisco.Intl.AP.724940_TMY3.epw"
#'
#'     idf_path <- file.path(eplusr::eplus_config(8.8)$dir, "ExampleFiles", idf_name)
#'     epw_path <- file.path(eplusr::eplus_config(8.8)$dir, "WeatherData", epw_name)
#'
#'     # create from local files
#'     sensi_job(idf_path, epw_path)
#'
#'     # create from an Idf and an Epw object
#'     sensi_job(read_idf(idf_path), read_epw(epw_path))
#' }
#' }
#' @seealso [bayes_job()] for creating a Bayesican calibration job.
#' @author Hongyuan Jia
#' @export
# sensi_job{{{
sensi_job <- function (idf, epw) {
    Sensitivity$new(idf, epw)
}
# }}}

# Sensitivity {{{
Sensitivity <- R6::R6Class(classname = "SensitivityJob",
    inherit = eplusr:::Parametric, cloneable = FALSE, lock_class = FALSE,
    public = list(
        # PUBLIC FUNCTIONS {{{
        param = function (..., .names = NULL, .r = 12L, .grid_jump = 4L)
            sen_param(self, private, ..., .r = .r, .grid_jump = .grid_jump, .names = .names),

        apply_measure = function (measure, ..., .r = 12L, .grid_jump = 4L)
            sen_apply_measure(self, private, measure, ..., .r = .r, .grid_jump = .grid_jump),

        samples = function ()
            sen_samples(self, private),

        evaluate = function (results)
            sen_evaluate(self, private, results)
        # }}}
    ),

    private = list(
        # PRIVATE FIELDS {{{
        m_idf = NULL,
        m_epw = NULL,
        m_job = NULL,
        m_log = NULL,
        m_param = NULL,
        m_morris = NULL
        # }}}
    )
)
# }}}

# sen_param {{{
sen_param <- function (self, private, ..., .names = NULL, .r = 12L, .grid_jump = 4L, .env = parent.frame()) {
    assert(is_count(.r))
    assert(is_count(.grid_jump))

    l <- tryCatch(eplusr:::sep_value_dots(..., .empty = FALSE, .scalar = FALSE, .null = FALSE, .env = .env),
        error_empty_input = function (e) {
            abort("error_sa_empty_param_input", "Please give parameters to set.")
        }
    )

    # match Idf data
    # TODO: `match_set_idf_data` should be generalized to be a helper for all
    # `Idf$set()`'s friends, including `$add()`, `$load()`, `$update()`, and
    # etc.
    obj_val <- eplusr:::match_set_idf_data(
        ._get_private(private$m_idf)$idd_env(),
        ._get_private(private$m_idf)$idf_env(),
        l
    )

    # handle whole-class case
    l$value <- data.table::rbindlist(list(
        obj_val$value[J(l$dot[class == TRUE, rleid]), on = "input_rleid", .SD[1L], by = c("class_id", "field_index")],
        obj_val$value[!J(l$dot[class == TRUE, rleid]), on = "input_rleid"]
    ), use.names = TRUE)

    # validate input
    par <- validate_par_space(l, private$m_idf, "sa")

    # sample
    sam <- morris_samples(par, obj_val$value, .names, .r, .grid_jump)

    private$m_morris <- sam$morris
    private$m_log$sample <- sam[names(sam) != "morris"]

    create_par_models(self, private, type = "sa")

    self
}
# }}}
# sen_apply_measure {{{
sen_apply_measure <- function (self, private, measure, ..., .r = 12L, .grid_jump = 4L) {
    # measure name
    mea_nm <- deparse(substitute(measure, parent.frame()))

    assert(is.function(measure), msg = "`measure` should be a function.")
    if (length(formals(measure)) < 2L) {
        abort("error_measure_no_arg", "`measure` function must have at least two argument.")
    }

    # match fun arg
    mc <- match.call(measure, quote(measure(private$m_idf, ...)))[-1L]
    l <- vector("list", length(mc[-1L]))
    names(l) <- names(mc[-1L])
    # get value
    for (nm in names(l)) l[[nm]] <- eval(mc[-1L][[nm]])

    # check input format
    par <- validate_par_space(l, type = "sa")

    sam <- morris_samples(par, NULL, par$dot$dot_nm, .r, .grid_jump)

    # store morris object
    private$m_morris <- sam$morris

    # store
    private$m_log$sample <- sam[names(sam) != "morris"]

    measure_wrapper <- function (idf, ...) {
        assert(eplusr::is_idf(idf), msg = paste0("Measure should take an `Idf` object as input, not `", class(idf)[[1]], "`."))
        idf <- idf$clone(deep = TRUE)
        idf <- measure(idf, ...)
        assert(eplusr::is_idf(idf), msg = paste0("Measure should return an `Idf` object, not `", class(idf)[[1]], "`."))
        idf
    }

    private$m_log$measure_wrapper <- measure_wrapper
    private$m_log$measure_name <- mea_nm

    create_par_models(self, private, type = "sa")

    self
}
# }}}
# sen_samples {{{
sen_samples <- function (self, private) {
    sen_assert_has_sampled(self, private, stop = FALSE)
    private$m_log$sample$sample
}
# }}}
# sen_evaluate {{{
sen_evaluate <- function (self, private, results) {
    sen_assert_can_evaluate(self, private)

    if (!is.data.frame(results)) {
        assert(is.numeric(results), no_na(results))
    } else {
        type <- vapply(results, typeof, character(1L))

        if (any(!type %in% c("integer", "double"))) {
            abort("error_sa_invalid_results", paste0("Non-numeric columns found in results: ",
                paste0(sQuote(names(results)[!type %in% c("integer", "double")]), collapse = ", "))
            )
        }
    }

    private$m_morris <- sensitivity::tell(private$m_morris, results)

    attr(private$m_morris, "data") <- morris_data(private$m_morris)

    private$m_morris
}
# }}}

# sen_assert_has_sampled {{{
sen_assert_has_sampled <- function (self, private, stop = FALSE) {
    if (is.null(private$m_morris)) {
        if (stop) {
            abort("error_sa_not_ready", paste0("No sensitivity samples are generated. ",
                "Please use `$param()` or `$apply_measure()` to set parameters and ",
                "perform Morris sampling."
            ))
        } else {
            message("No sensitivity samples are generated. ",
                "Please use `$param()` or `$apply_measure()` to set parameters and ",
                "perform Morris sampling."
            )
            return(FALSE)
        }
    }
    TRUE
}
# }}}
# sen_assert_can_evaluate {{{
sen_assert_can_evaluate <- function (self, private, stop = FALSE) {
    if (stop) {
        fun <- function (...) abort("error_sa_not_ready", paste0(...))
    } else {
        fun <- message
    }

    if (is.null(private$m_param)) {
        fun("No models have been created. Please use $param() or $apply_measure() ",
            "to create parametric models after parameters are set."
        )
        return(FALSE)
    }

    # use $output_dir() to perform other checking
    self$output_dir()

    TRUE
}
# }}}
# case_names {{{
case_names <- function (sample, minlength = 5L) {
    case_names <- do.call(paste,
        c(
            mapply(function (name, value) paste0(name, "(", prettyNum(value), ")"),
                # only use first 5 character to keep it short
                name = abbreviate(names(sample), minlength, use.classes = FALSE),
                value = sample, SIMPLIFY = FALSE
            ),
            sep = "_"
        )
    )
    paste0(seq_along(case_names), "_", substring(case_names, 1L, 94L))
}
# }}}
# par_names {{{
par_names <- function (par, names = NULL, type = c("sa", "bc")) {
    if (is.null(names)) {
        type <- match.arg(type)
        nm <- if (type == "sa") "theta" else "t"
        paste0(nm, seq_len(nrow(par$num$meta)))
    } else {
        assert(length(names) == nrow(par$num$meta), msg = paste0(
            "`.name` should have the same length as number of input parameters, which is ",
            nrow(par$num$meta), "."
        ))
        as.character(names)
    }
}
# }}}
# morris_samples {{{
morris_samples <- function (par, value = NULL, names = NULL, r, grid_jump) {
    fctr <- par_names(par, names)

    # use sensitivity::morris to generate input
    mo <- sensitivity::morris(model = NULL, factors = fctr, r = r,
        design = list(type = "oat", levels = par$num$meta$levels, grid.jump = grid_jump),
        binf = par$num$meta$min, bsup = par$num$meta$max, scale = FALSE
    )

    # get parameter value
    val <- data.table::as.data.table(mo$X)

    # get case name
    nms <- case_names(val)

    # number the case
    val[, case := .I]
    data.table::setcolorder(val, "case")

    # melt
    val_m <- data.table::melt.data.table(val, id.vars = "case",
        variable.name = "name_par", value.name = "value",
        variable.factor = FALSE
    )[, `:=`(value = as.list(value))]

    # add parameter index
    val_m[data.table(index_par = seq_along(fctr), name_par = fctr),
        on = "name_par", index_par := i.index_par
    ]

    # combine
    if (!is.null(value)) {
        # format val for `Idf$update()`
        val_m <- match_sample_data(par, val_m, value)
    }

    list(names = nms, morris = mo, sample = val, value = val_m)
}
# }}}
# match_sample_data {{{
match_sample_data <- function (par, sample, value) {
    sample <- par$num$data[, list(value_rleid, name = object_name, class = class_name,
        index = field_index, field = field_name, is_sch_value, class_id
    )][sample, on = c("value_rleid" = "index_par")]
    # change value column to list
    set(sample, NULL, "value", as.list(sample$value))
    setnames(sample, "value_rleid", "index_par")

    # if schedule value detected, change it to character
    sample[J(TRUE), on = "is_sch_value", value := lapply(value, as.character)][
        , is_sch_value := NULL]

    # this is necessary to get the right order of val
    data.table::setorder(sample, "case")
    data.table::setcolorder(sample, c("case", "index_par", "name_par", "class",
        "name", "index", "field", "value"
    ))

    # handle grouped parameters
    grp <- par$dot[, list(grouped = class || vapply(dot_nm, length, 1L) > 1L), by = c("rleid")][
        grouped == TRUE
    ]

    # get parameter index
    target <- value[, list(id = object_id, input_rleid, input_object_rleid, field_index)]
    target[J(grp$rleid), on = "input_rleid", input_object_rleid := 1L]
    set(target, NULL, "index_par", rleidv(target, c("input_rleid", "input_object_rleid", "field_index")))
    set(target, NULL, c("input_rleid", "input_object_rleid", "field_index"), NULL)

    # merge
    sample <- target[sample, on = c("index_par"), allow.cartesian = TRUE, nomatch = 0L]
    data.table::setcolorder(sample, c("case", "index_par", "name_par", "class",
        "id", "name", "index", "field", "value"
    ))
}
# }}}
# morris_data {{{
morris_data <- function (morris) {
    stopifnot(inherits(morris, "morris"))

    mu <- apply(morris$ee, 2, mean)
    mu.star <- apply(morris$ee, 2, function(x) mean(abs(x)))
    sigma <- apply(morris$ee, 2, sd)

    data.table::data.table(
        index = seq_along(mu), name = morris$factors,
        mu = mu, mu.star = mu.star, sigma = sigma
    )
}
# }}}
# validate_par_space {{{
validate_par_space <- function (l, idf = NULL, type = c("sa", "bc")) {
    type <- match.arg(type)

    if (all(c("value", "dot") %in% names(l))) {
        input <- l
    } else {
        input <- vector("list", 2L)
        if (is.null(dot_nm <- names(l))) dot_nm <- NA_character_
        input$dot <- data.table(rleid = seq_along(l), dot = l, dot_nm = dot_nm)
        input$value <- data.table(rleid = seq_along(l), value_rleid = seq_along(l), new_value_num = l)
    }

    # check param format
    data.table::set(input$value, NULL, "is_chr", vapply(input$value$new_value_num, anyNA, logical(1)))

    # add input rield
    input$value[, value_rleid := .I]

    num <- input$value[J(FALSE), on = "is_chr", nomatch = 0L]
    chr <- input$value[J(TRUE), on = "is_chr", nomatch = 0L]

    # Number {{{
    # for numeric type, using position to determine min, max and levels
    # TODO: use element names instead of positions or use internal helper
    # `num_space()` and `chr_space()`. `patterns()` in data.table pkg can be a
    # good example of internal fun implementation
    if (!nrow(num)) {
        num_info <- data.table::data.table()
    } else {
        # Add support for field values in `Schedule:Compact` class. See #3
        if (all(c("object_id", "class_name", "field_index") %in% names(num))) {
            num <- check_compactsch(num, idf)
        }

        num_info <- num[, {
            id <- rleid[[1L]]
            range <- new_value_num[[1L]]
            if (type == "sa" && length(range) != 3L) {
                abort("error_param_num_format",
                    paste0(
                        "For numeric field, a numeric vector of length 3 should be provided ",
                        "which defines parameter's minimum value, maximum value and number of total levels. ",
                        "Invalid input:\n", dot_string(input$dot[J(id), on = "rleid"])
                    )
                )
            } else if (type == "bc" && length(range) != 2L) {
                abort("error_param_num_format",
                    paste0(
                        "For numeric field, a numeric vector of length 2 should be provided ",
                        "which defines parameter's minimum and maximum value. ",
                        "Invalid input:\n", dot_string(input$dot[J(id), on = "rleid"])
                    )
                )
            }

            if (range[[1L]] >= range[[2L]]) {
                abort("error_param_num_format", paste0(
                    "For numeric field, minimum value (1st element) should be less than maximum value (2nd element). ",
                    "Invalid input:\n", dot_string(input$dot[J(id), on = "rleid"])
                ))
            }

            if (type == "sa" && !is_count(range[[3L]])) {
                abort("error_param_num_format", paste0(
                    "For numeric field, number of total levels (3rd element) should be a positive integer. ",
                    "Invalid input:\n", dot_string(input$dot[J(id), on = "rleid"])
                ))
            }

            if (type == "sa") {
                list(min = range[[1L]], max = range[[2L]], levels = range[[3L]])
            } else {
                list(min = range[[1L]], max = range[[2L]], levels = NA_integer_)
            }

        }, by = "value_rleid"]
    }
    # }}}
    # Character {{{
    # TODO: convert to dummy variables
    if (!nrow(chr)) {
        chr_info <- data.table::data.table()
    } else {
        abort("error_param_chr_format", "Character fields are currently not supported.")
    }
    # }}}

    list(dot = input$dot, num = list(data = num, meta = num_info), chr = list(data = chr, meta = chr_info))
}
# }}}
# create_par_models {{{
create_par_models <- function (self, private, verbose = FALSE, stop = FALSE, type = c("sa", "bc")) {
    type <- match.arg(type)

    if (type == "sa") {
        if (!sen_assert_has_sampled(self, private)) return(invisible())
    } else if (type == "bc"){
        if (!bc_assert_can_model(self, private)) return(invisible())
    }

    # check if parameter is created using $apply_measure() or not
    if (is.null(private$m_log$measure_name)) {
        dt <- split(private$m_log$sample$value, by = "case", keep.by = FALSE)
        private$m_param <- lapply(dt, function (upd) {
            idf <- private$m_idf$clone()
            idf$update(upd)
            idf
        })
    } else {
        private$m_param <- purrr::pmap(private$m_log$sample$sample[, -"case"],
            private$m_log$measure_wrapper, idf = private$m_idf
        )
    }

    # assign name
    setattr(private$m_param, "names", private$m_log$sample$names)

    # log unique ids
    private$m_log$uuid <- vapply(private$m_param, function (idf) ._get_private(idf)$m_log$uuid, character(1L))

    private$m_param
}
# }}}
