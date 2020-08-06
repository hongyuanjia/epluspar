#' @include utils.R
#' @importFrom data.table data.table set setorder setattr ":="
#' @importFrom sensitivity morris tell
NULL

#' Conduct Sensitivity Analysis for An EnergyPlus Model
#'
#' `sensi_job()` takes an IDF and EPW as input and returns a `SensitivityJob`,
#' which provides a prototype of conducting sensitivity analysis of EnergyPlus
#' simulations using [Morris][sensitivity::morris] method.
#'
#' `SensitivityJob` inherits from [eplusr::ParametricJob] class, which means
#' that all methods provided by [eplusr::ParametricJob] class are also available
#' for `SensitivityJob` class.
#'
#' The basic workflow is basically:
#'
#' 1. Adding parameters for sensitivity analysis  using
#'    \href{../../epluspar/html/SensitivityJob#method-param}{\code{$param()}}
#'    or
#'    \href{../../epluspar/html/SensitivityJob#method-apply_measure}{\code{$apply_measure()}}.
#' 1. Check parameter sampled values and generated parametric models using
#'    \href{../../epluspar/html/SensitivityJob#method-samples}{\code{$samples()}}
#'    and
#'    \href{../../epluspar/html/SensitivityJob#method-models}{\code{$models()}},
#'    respectively.
#' 1. Run EnergyPlus simulations in parallel using
#'    \href{../../epluspar/html/SensitivityJob#method-run}{\code{$run()}},
#' 1. Gather EnergyPlus simulated data using
#'    [$report_data()][eplusr::EplusGroupJob] or
#'    [$tabular_data()][eplusr::EplusGroupJob].
#' 1. Evaluate parameter sensitivity using
#'    \href{../../epluspar/html/SensitivityJob#method-evaluate}{\code{$evaluate()}}.
#'
#' @docType class
#' @name SensitivityJob
#' @author Hongyuan Jia
NULL

#' @export
# SensitivityJob {{{
SensitivityJob <- R6::R6Class(classname = "SensitivityJob",
    inherit = eplusr::ParametricJob, cloneable = FALSE, lock_class = FALSE,
    public = list(
        # PUBLIC FUNCTIONS {{{
        # param {{{
        #' @description
        #' Set parameters for sensitivity analysis
        #'
        #' @details
        #' `$param()` takes parameter definitions in list format, which is
        #' similar to `$set()` in [eplusr::Idf] class except that each field is
        #' not assigned with a single value, but a numeric vector of length 3,
        #' indicating the minimum value, maximum value and number of levels of
        #' each parameter.
        #'
        #' Similar like the way of modifying object field values in
        #' [eplusr::Idf$set()][eplusr::Idf], there are 3 different ways of
        #' defining a parameter in epluspar:
        #'
        #' * `object = list(field = c(min, max, levels))`: Where `object` is a
        #'   valid object ID or name. Note object ID should be denoted with two
        #'   periods `..`, e.g. `..10` indicates the object with ID `10`, It
        #'   will set that specific field in that object as one parameter.
        #' * `.(object, object) := list(field = c(min, max, levels))`: Simimar
        #'   like above, but note the use of `.()` in the left hand side. You
        #'   can put multiple object ID or names in `.()`. It will set the field
        #'   of all specified objects as one parameter.
        #' * `class := list(field = c(min, max, levels))`: Note the use of `:=`
        #'   instead of `=`. The main difference is that, unlike `=`, the left
        #'   hand side of `:=` should be a valid class name in current
        #'   [eplusr::Idf]. It will set that field of all objects in specified
        #'   class as one parameter.
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
        #' @param ... Lists of paramter definitions. Please see above on the
        #'        syntax.
        #' @param .names A character vector of the parameter names. If `NULL`,
        #'        the parameter will be named in format `theta + number`, where
        #'        `number` is the index of parameter. Default: `NULL`.
        #' @param .r An positive integer specifying the number of elementary
        #'        effect computed per factor. For details, see
        #'        [sensitivity::morris]. Default: `12`.
        #' @param .grid_jump  An integer or a vector of integers specifying the
        #'        number of levels that are increased/decreased for computing
        #'        the elementary effects. Default: `1L`. For details, see
        #'        [sensitivity::morris].
        #' @param .scale If `TRUE`, the input design of experiments is scaled
        #'        after building the design and before computing the elementary
        #'        effects so that all factors vary within the range \[0,1\].
        #'        Default: `TRUE`. For details, see [sensitivity::morris].
        #'
        #' @return The modified `SensitivityJob` object itself.
        #'
        #' @examples
        #' \dontrun{
        #' sensi$param(
        #'     `Supply Fan 1` = list(Fan_Total_Efficiency = c(min = 0.1, max = 1.0, levels = 5)),
        #'     Material := list(Thickness = c(0.01, 1, 5), Conductivity = c(0.1, 0.6, 10))
        #' )
        #' }
        #'
        param = function (..., .names = NULL, .r = 12L, .grid_jump = 4L, .scale = TRUE)
            sen_param(self, private, ..., .r = .r, .grid_jump = .grid_jump, .names = .names),
        # }}}

        # apply_measure {{{
        #' @description
        #' Set parameters for sensitivity analysis using function
        #'
        #' @details
        #' `$apply_measure()` works in a similar way as the `$apply_measure` in
        #' [eplusr::ParametricJob] class, with only exception that each argument
        #' supplied in `...` should be a numeric vector of length 3, indicating
        #' the minimum, maximum and number of levels of each parameter.
        #'
        #' Basically `$apply_measure()` allows to apply a measure to an
        #' [eplusr::Idf]. A measure here is just a function that takes an
        #' [eplusr::Idf] object and other arguments as input, and returns a
        #' modified [eplusr::Idf] object as output.
        #'
        #' The names of function parameter will be used as the names of
        #' sensitivity parameter. For example, the equivalent version of
        #' specifying parameters described in
        #' \href{../../epluspar/html/SensitivityJob.html#method-param}{\code{$param()}}
        #' using `$apply_measure()` can be:
        #'
        #' ```
        #' # set sensitivity parameters using $apply_measure()
        #' # (a) first define a "measure"
        #' measure <- function (idf, efficiency, thickness, conducitivy) {
        #'     idf$set(
        #'         `Supply Fan 1` = list(Fan_Total_Efficiency = efficiency),
        #'         Material := list(Thickness = thickness, Conductivity = conducivity)
        #'     )
        #'     idf
        #' }
        #' # (b) then apply that measure with parameter space definitions as
        #' # function arguments
        #' sensi$apply_measure(measure,
        #'     efficiency = c(min = 0.1, max = 1.0, levels = 5),
        #'     thickness = c(0.01, 1, 5), conductivity = c(0.1, 0.6, 10)
        #' )
        #' ```
        #'
        #' @param measure A function that takes an [eplusr::Idf] and other
        #'        arguments as input and returns an [eplusr::Idf] object as
        #'        output.
        #' @param ... Arguments **except first `Idf` argument** that are passed
        #'        to that `measure`.
        #' @param .r An positive integer specifying the number of elementary
        #'        effect computed per factor. For details, see
        #'        [sensitivity::morris].
        #' @param .grid_jump An integer or a vector of integers specifying the
        #'        number of levels that are increased/decreased for computing
        #'        the elementary effects.  For details, see
        #'        [sensitivity::morris].
        #' @param .scale If `TRUE`, the input design of experiments is scaled
        #'        after building the design and before computing the elementary
        #'        effects so that all factors vary within the range \[0,1\].
        #'        Default: `TRUE`. For details, see [sensitivity::morris].
        #'
        #'
        #' @return The modified `SensitivityJob` object itself.
        #'
        #' @examples
        #' \dontrun{
        #' # set sensitivity parameters using $apply_measure()
        #' # (a) first define a "measure"
        #' measure <- function (idf, efficiency, thickness, conducitivy) {
        #'     idf$set(
        #'         `Supply Fan 1` = list(Fan_Total_Efficiency = efficiency),
        #'         Material := list(Thickness = thickness, Conductivity = conducivity)
        #'     )
        #'     idf
        #' }
        #' # (b) then apply that measure with parameter space definitions as
        #' # function arguments
        #' sensi$apply_measure(measure,
        #'     efficiency = c(min = 0.1, max = 1.0, levels = 5),
        #'     thickness = c(0.01, 1, 5), conductivity = c(0.1, 0.6, 10)
        #' )
        #' }
        #'
        apply_measure = function (measure, ..., .r = 12L, .grid_jump = 4L, .scale = TRUE)
            sen_apply_measure(self, private, measure, ..., .r = .r, .grid_jump = .grid_jump),
        # }}}

        # samples {{{
        #' @description
        #' Get sampled parameter values
        #'
        #' @details
        #' `$samples()` returns a [data.table::data.table()] which contains the
        #' sampled value for each parameter using [Morris][sensitivity::morris]
        #' method. The returned data.table has `1 + n` columns, where `n` is the
        #' parameter number, while `1` indicates an extra column named `case`
        #' giving the index of each sample.
        #'
        #' @return A [data.table::data.table()].
        #'
        #' @examples
        #' \dontrun{
        #' sensi$samples()
        #' }
        #'
        samples = function ()
            sen_samples(self, private),
        # }}}

        # evaluate {{{
        #' @description
        #' Evaluate sensitivity
        #'
        #' @details
        #' `$evaluate()` takes a numeric vector with the same length as total
        #' sample number and returns the a [sensitivity::morris()] object. The
        #' statistics of interest (mu, mu* and sigma) are stored as an attribute
        #' named `data` and can be retrieved using `atrr(sensi$evaluate(),
        #' "data")`.
        #'
        #' @param results A numeric vector. Usually the output of parametric
        #'        simulations extracted using
        #'        [$report_data()][eplusr::EplusGroupJob] or
        #'        [$tabular_data()][eplusr::EplusGroupJob].
        #'
        #' @return a [sensitivity::morris()] object with an extra `data`
        #' attribute.
        #'
        #' @examples
        #' \dontrun{
        #' # run parametric simulations
        #' sensi$run(wait = TRUE)
        #'
        #' # status now includes a data.table with detailed information on each simulation
        #' sensi$status()
        #'
        #' # print simulation errors
        #' sensi$errors()
        #'
        #' # extract a target simulation output value for each case to evaluate the
        #' # sensitivity results
        #' eng <- sen$tabular_data(table_name = "site and source energy",
        #'     column_name = "energy per total building area",
        #'     row_name = "total site energy")[, as.numeric(value)]
        #' (result <- sensi$evaluate(eng))
        #'
        #' # extract sensivitity data
        #' attr(result, "data")
        #'
        #' # plot
        #' plot(result)
        #' }
        #'
        evaluate = function (results)
            sen_evaluate(self, private, results),
        # }}}

        # print {{{
        #' @description
        #' Print `SensitivityJob` object
        #'
        #' @details
        #' `$print()` shows the core information of this `SensitivityJob`,
        #' including the path of IDFs and EPWs and also the simulation job
        #' status.
        #'
        #' `$print()` is quite useful to get the simulation status, especially
        #' when `wait` is `FALSE` in `$run()`. The job status will be updated
        #' and printed whenever `$print()` is called.
        #'
        #' @return The `SensitivityJob` object itself, invisibly.
        #'
        #' @examples
        #' \dontrun{
        #' sen$print()
        #' }
        #'
        print = function ()
            sen_print(self, private)
        # }}}
        # }}}
    ),

    private = list(
        # PRIVATE FIELDS {{{
        m_param = NULL,
        m_sample = NULL,
        m_morris = NULL
        # }}}
    )
)
# }}}

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
#' @seealso [bayes_job()] for creating a Bayesian calibration job.
#' @author Hongyuan Jia
#' @export
# sensi_job{{{
sensi_job <- function (idf, epw) {
    SensitivityJob$new(idf, epw)
}
# }}}

# sen_param {{{
sen_param <- function (self, private, ..., .names = NULL, .r = 12L, .grid_jump = 4L,
                       .scale = TRUE, .env = parent.frame()) {
    checkmate::assert_count(.r, positive = TRUE)
    checkmate::assert_count(.grid_jump, positive = TRUE)

    # clean measure created using $apply_measure() if any
    private$m_log$measure_wrapper <- NULL
    private$m_log$measure_name <- NULL

    l <- expand_param_specs(private$m_seed, ..., .env = .env, .names = .names, .specs_len = 3L)

    # use sensitivity::morris to generate input
    mo <- sensitivity::morris(model = NULL, factors = l$param$param_name, r = .r,
        design = list(type = "oat", levels = l$param$levels, grid.jump = .grid_jump),
        binf = l$param$min, bsup = l$param$max, scale = .scale
    )

    # get parameter sample value
    samples <- data.table::as.data.table(mo$X)
    samples[, case := .I]
    data.table::setcolorder(samples, "case")

    idfs <- create_par_models(private$m_seed, l$param, samples, l$value, NULL)

    private$m_param <- l$param
    private$m_sample <- samples
    private$m_morris <- mo
    private$m_idfs <- idfs

    # log
    private$log_new_uuid()
    private$log_idf_uuid()
    private$m_log$unsaved <- rep(TRUE, length(idfs))

    self
}
# }}}
# sen_apply_measure {{{
sen_apply_measure <- function (self, private, measure, ..., .r = 12L, .grid_jump = 4L, .scale = TRUE) {
    l <- match_param_measure(measure, ..., .specs_len = 3L, .env = parent.frame())

    # use sensitivity::morris to generate input
    mo <- sensitivity::morris(model = NULL, factors = l$param$param_name, r = .r,
        design = list(type = "oat", levels = l$param$levels, grid.jump = .grid_jump),
        binf = l$param$min, bsup = l$param$max, scale = .scale
    )

    # get parameter sample value
    samples <- data.table::as.data.table(mo$X)
    samples[, case := .I]
    data.table::setcolorder(samples, "case")

    idfs <- create_par_models(private$m_seed, l$param, samples, measure = l$measure)

    private$m_param <- l$param
    private$m_sample <- samples
    private$m_morris <- mo
    private$m_idfs <- idfs

    private$m_log$measure <- measure
    private$m_log$measure_name <- l$name

    # log
    private$log_new_uuid()
    private$log_idf_uuid()
    private$m_log$unsaved <- rep(TRUE, length(idfs))

    self
}
# }}}
# sen_samples {{{
sen_samples <- function (self, private) {
    sen_assert_has_sampled(self, private)
    private$m_sample
}
# }}}
# sen_evaluate {{{
sen_evaluate <- function (self, private, results) {
    sen_assert_can_evaluate(self, private)

    if (!is.data.frame(results)) {
        checkmate::assert_number(results)
    } else {
        type <- vapply(results, typeof, character(1L))

        if (any(!type %in% c("integer", "double"))) {
            abort(paste0("Non-numeric columns found in results: ",
                paste0(sQuote(names(results)[!type %in% c("integer", "double")]), collapse = ", ")),
                "error_sa_invalid_results"
            )
        }
    }

    private$m_morris <- sensitivity::tell(private$m_morris, results)

    attr(private$m_morris, "data") <- morris_data(private$m_morris)

    private$m_morris
}
# }}}
# sen_print {{{
#' @importFrom cli cat_line
sen_print <- function (self, private) {
    eplusr:::print_job_header(title = "EnergPlus Sensitivity Analysis Job",
        path_idf = private$m_seed$path(),
        path_epw = private$m_epws_path,
        eplus_ver = private$m_seed$version(),
        name_idf = "Seed", name_epw = "Weather"
    )

    if (is.null(private$m_idfs)) {
        cli::cat_line("<< No parameter has been set yet >>",
            col = "white", background_col = "blue")
        return(invisible())
    }

    cli::cat_line(c(
        sprintf("Applied Measure: '%s'", private$m_log$measure_name),
        sprintf("Parameters [%i]", nrow(private$m_param)),
        private$m_param[, sprintf("[%s]: '%s' [%s, %s] (lvl: %i)",
            param_index, param_name, min, max, levels)],
        paste0("Parametric Models [", length(private$m_idfs), "]: ")
    ))

    eplusr:::epgroup_print_status(self, private, epw = FALSE)
}
# }}}

# sen_assert_has_sampled {{{
sen_assert_has_sampled <- function (self, private, stop = FALSE) {
    if (is.null(private$m_morris)) {
        if (stop) {
            abort(paste0("No sensitivity samples are generated. ",
                "Please use '$param()' or '$apply_measure()' to set parameters and ",
                "perform Morris sampling."
            ), "sa_not_ready")
        } else {
            message("No sensitivity samples are generated. ",
                "Please use '$param()' or '$apply_measure()' to set parameters and ",
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
        fun <- function (...) abort(paste0(...), "sa_not_ready")
    } else {
        fun <- message
    }

    if (is.null(private$m_idfs)) {
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
# expand_param_specs {{{
expand_param_specs <- function (idf, ..., .env = parent.frame(), .names = NULL, .specs_len = 2L) {
    l <- eplusr::expand_idf_dots_value(
        get_priv_env(idf)$idd_env(), get_priv_env(idf)$idf_env(), ...,
        .type = "object", .complete = FALSE, .unique = TRUE, .empty = FALSE,
        .default = FALSE, .scalar = FALSE, .pair = FALSE, .env = .env)

    # check type
    eplusr:::add_field_property(get_priv_env(idf)$idd_env(), l$value, "type")
    # handle schedule:compact fields
    if (nrow(invld <- l$value[type != "real" & class_name != "Schedule:Compact"])) {
        abort(paste0("Currently only numeric fields are supported. Non-numeric fields found:\n",
            paste0(invld[, sprintf("#%s: '%s' in class '%s'", lpad(seq_len(.N), "0"), field_name, class_name)], collapse = "\n")
        ))
    }
    set(l$value, NULL, "type", NULL)

    # check data range
    param <- unique(l$value, by = c("rleid", "class_id", "field_id"))
    # if no duplicates, param and l$value is the same
    if (nrow(param) == nrow(l$value)) param <- copy(param)
    set(param, NULL, "param_index", seq_len(nrow(param)))
    checkmate::qassertr(param$value_num, sprintf("N%i", .specs_len), .var.name = "Parameter Range Specs")

    if (.specs_len == 2L) {
        param[, by = "param_index", c("min", "max") := bc_param_specs(value_num[[1L]], .BY$param_index)]
        # clean
        set(param, NULL, setdiff(names(param), c("param_index", "min", "max")), NULL)
    } else if (.specs_len == 3L) {
        param[, by = "param_index", c("min", "max", "levels") :=
            sen_param_specs(value_num[[1L]], .BY$param_index)]
        # clean
        set(param, NULL, setdiff(names(param), c("param_index", "min", "max", "levels")), NULL)
    }

    # add value id mapping
    param[
        l$value[, by = c("rleid", "class_id", "field_id"),
            list(param_index = .GRP, value_id = list(value_id))],
        on = "param_index", value_id := i.value_id
    ]

    # clean
    set(l$value, NULL, c("value_chr", "value_num"), NULL)

    # create parameter names
    if (is.null(.names)) {
        nm <- if (.specs_len == 2L) "t" else "theta"
        set(param, NULL, "param_name", paste0(nm, seq_len(nrow(param))))
    } else {
        checkmate::assert_character(.names, any.missing = FALSE, unique = TRUE,
            len = nrow(param), .var.name = ".names")
        set(param, NULL, "param_name", .names)
    }

    list(object = l$object, value = l$value, param = param)
}
# }}}
# sen_param_specs {{{
sen_param_specs <- function (value_num, index) {
    if (!is.null(nm <- names(value_num))) {
        nm_valid <- nm[nm != ""]

        if (any(invld <- !nm_valid %in% c("min", "max", "levels"))) {
            abort(paste0("Parameter Range Specs should only contain ",
                "'min', 'max' and 'levels'. Invalid element found: ",
                paste0("{", index, ":'", nm_valid[invld], "'}", collapse = ", ")
            ))
        }
        if (anyDuplicated(nm_valid)) {
            abort(paste0("Parameter Range Specs should contain only one ",
                "'min', 'max' and 'levels'. Duplicated element found: ",
                paste0("{", index, ":'", nm_valid[duplicated(nm_valid)], "'}", collapse = ", ")
            ))
        }
        m <- match(c("min", "max", "levels"), nm)
        m[is.na(m)] <- setdiff(seq_along(value_num), m)
        value_num[m]
    }
    if (value_num[[1L]] >= value_num[[2L]]) {
        abort(paste0("For numeric field, minimum value should be less than ",
            "maximum value. Invalid input found for ",
            sprintf("{%i: %s(min), %s(max)}", index, value_num[[1L]], value_num[[2L]])
        ), "param_num_format")
    }
    if (!checkmate::test_count(value_num[[3]], positive = TRUE)) {
        abort(paste0("For numeric field, number of total levels should be positive integer. ",
            "Invalid input found for ",
            sprintf("{%i: %s(levels)}", index, value_num[[3L]])
        ), "param_num_format")
    }
    value_num[[3L]] <- as.integer(value_num[[3L]])
    list(min = value_num[[1L]], max = value_num[[2L]], levels = value_num[[3L]])
}
# }}}
# match_param_measure {{{
match_param_measure <- function (measure, ..., .specs_len = 2L, .env = parent.frame()) {
    checkmate::assert_function(measure)
    # measure name
    mea_nm <- deparse(substitute(measure, .env))

    if (length(formals(measure)) < 2L) {
        abort("'measure' function must have at least two argument.", "bc_measure_no_arg")
    }

    # match fun arg
    mc <- match.call(measure, quote(measure(private$m_seed, ...)))[-1L]
    l <- vector("list", length(mc[-1L]))
    names(l) <- names(mc[-1L])
    # get value
    for (nm in names(l)) l[[nm]] <- eval(mc[-1L][[nm]])

    checkmate::qassertr(l, sprintf("N%i", .specs_len), .var.name = "Parameter Range Specs")
    param <- data.table(param_index = seq_along(l), param_name = names(l), value_num = l)

    if (.specs_len == 2L) {
        param[, by = "param_index", c("min", "max") :=
            bc_param_specs(value_num[[1L]], .BY$param_index)]
    } else if (.specs_len == 3L) {
        param[, by = "param_index", c("min", "max", "levels") :=
            sen_param_specs(value_num[[1L]], .BY$param_index)]
    }

    if (is.name(substitute(measure, .env))) {
        mea_nm <- deparse(substitute(measure, .env))
    } else {
        mea_nm <- "Function"
    }

    list(name = mea_nm, measure = measure, param = param)
}
# }}}
# create_par_models {{{
create_par_models <- function (idf, param, samples, matched = NULL, measure = NULL, name = TRUE) {
    if (is.null(measure)) {
        # create param models
        m <- melt.data.table(samples, id.vars = "case", variable.name = "param_name", variable.factor = FALSE)
        val <- param[m, on = "param_name"][, by = c("case", "param_index"), {
            len <- vapply(value_id, length, integer(1L))
            list(value_id = unlist(value_id, FALSE, FALSE), value = rep(value, len))
        }]
        val <- matched[val, on = "value_id"][, list(
            case, id = object_id, class = class_name, index = field_index, value = as.character(value)
        )]

        idfs <- lapply(split(val, by = "case", keep.by = FALSE), function (d) {
            idf <- idf$clone()
            eplusr::with_silent(idf$update(d))
            idf
        })
    } else {
        measure_wrapper <- function (idf, ...) {
            if (!eplusr::is_idf(idf)) {
                abort(paste0("Measure should take an 'Idf' object as input, not '", class(idf)[[1]], "'."))
            }
            idf <- idf$clone(deep = TRUE)
            idf <- measure(idf, ...)
            if (!eplusr::is_idf(idf)) {
                abort(paste0("Measure should return an 'Idf' object, not '", class(idf)[[1]], "'."))
            }
            idf
        }

        idfs <- do.call("mapply", c(
            FUN = list(quote(measure_wrapper)),
            samples[, -"case"],
            MoreArgs = quote(list(idf = idf)),
            SIMPLIFY = FALSE, USE.NAMES = FALSE
        ))
    }

    # get new idf names
    if (name) setattr(idfs, "names", case_names(samples[, -"case"]))

    idfs
}
# }}}
# case_names {{{
case_names <- function (sample) {
    paste0("Case", lpad(seq_len(nrow(sample)), "0"))
}
# }}}
