#' @include utils.R
#' @importFrom data.table data.table set setorder setattr ":="
#' @importFrom sensitivity morris tell
#' @importFrom purrr pmap
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
            sen_evaluate(self, private, results)
        # }}}
        # }}}
    ),

    private = list(
        # PRIVATE FIELDS {{{
        m_seed = NULL,
        m_idfs = NULL,
        m_epws = NULL,
        m_morris = NULL,
        m_job = NULL,
        m_log = NULL
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
        ._get_private(private$m_seed)$idd_env(),
        ._get_private(private$m_seed)$idf_env(),
        l
    )

    # handle whole-class case
    cls <- l$dot[class == TRUE, rleid]
    # get rid of R CMD check NOTE
    dep <- V1 <- NULL
    # handle .() case
    flat <- l$dot[dep == 2L & class == FALSE, rleid]
    multi <- l$object[J(flat), on = "rleid", .N > 1L, by = "rleid"][V1 == TRUE, rleid]
    l$value <- data.table::rbindlist(list(
        obj_val$value[J(c(cls, multi)), on = "input_rleid", .SD[1L], by = c("class_id", "field_index")],
        obj_val$value[!J(c(cls, multi)), on = "input_rleid"]
    ), use.names = TRUE)

    # validate input
    par <- validate_par_space(l, private$m_seed, "sa")

    # sample
    sam <- morris_samples(par, obj_val$value, .names, .r, .grid_jump, .scale)

    private$m_morris <- sam$morris
    private$m_log$sample <- sam[names(sam) != "morris"]

    create_par_models(self, private, type = "sa")

    self
}
# }}}
# sen_apply_measure {{{
sen_apply_measure <- function (self, private, measure, ..., .r = 12L, .grid_jump = 4L, .scale = TRUE) {
    # measure name
    mea_nm <- deparse(substitute(measure, parent.frame()))

    assert(is.function(measure), msg = "`measure` should be a function.")
    if (length(formals(measure)) < 2L) {
        abort("error_measure_no_arg", "`measure` function must have at least two argument.")
    }

    # match fun arg
    mc <- match.call(measure, quote(measure(private$m_seed, ...)))[-1L]
    l <- vector("list", length(mc[-1L]))
    names(l) <- names(mc[-1L])
    # get value
    for (nm in names(l)) l[[nm]] <- eval(mc[-1L][[nm]])

    # check input format
    par <- validate_par_space(l, type = "sa")

    sam <- morris_samples(par, NULL, par$dot$dot_nm, .r, .grid_jump, .scale)

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
morris_samples <- function (par, value = NULL, names = NULL, r, grid_jump, scale) {
    fctr <- par_names(par, names)

    # use sensitivity::morris to generate input
    mo <- sensitivity::morris(model = NULL, factors = fctr, r = r,
        design = list(type = "oat", levels = par$num$meta$levels, grid.jump = grid_jump),
        binf = par$num$meta$min, bsup = par$num$meta$max, scale = scale
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

    # retain input param order
    if ("input_rleid" %in% names(input$value)) setorderv(input$value, "input_rleid")

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
        private$m_idfs <- lapply(dt, function (upd) {
            idf <- private$m_seed$clone()
            idf$update(upd)
            idf
        })
    } else {
        private$m_idfs <- purrr::pmap(private$m_log$sample$sample[, -"case"],
            private$m_log$measure_wrapper, idf = private$m_seed
        )
    }

    # assign name
    setattr(private$m_idfs, "names", private$m_log$sample$names)

    # log unique ids
    private$m_log$uuid <- vapply(private$m_idfs, function (idf) ._get_private(idf)$m_log$uuid, character(1L))

    private$m_idfs
}
# }}}
