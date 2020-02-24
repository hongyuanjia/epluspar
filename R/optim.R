#' Conduct Multi-Objective Optimization on An EnergyPlus Model
#'
#' `OptimJob` class provides a prototype of conducting Bayesian calibration
#' of EnergyPlus model.
#'
#' The basic workflow is basically:
#'
#' @docType class
#' @name OptimJob
#' @author Hongyuan Jia
NULL

#' @export
# OptimJob {{{
OptimJob <- R6::R6Class(classname = "OptimJob",
    inherit = eplusr::ParametricJob, cloneable = FALSE, lock_objects = FALSE,

    public = list(
        # INITIALIZE {{{
        #' @description
        #' Create a `OptimJob` object
        #'
        #' @param idf A path to an local EnergyPlus IDF file or an [eplusr::Idf] object.
        #' @param epw A path to an local EnergyPlus EPW file or an [eplusr::Epw] object.
        #' @param type A single string specifying the internal method used for
        #'        optimization. Possible values are:
        #'     * `NSGA2`:
        #'     * `GPareto`:
        #'     * `GA`: Only for mono-objective optimization problem
        #'
        #' @return A `OptimJob` object.
        #'
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
        #'     OptimJob$new(idf_path, epw_path)
        #'
        #'     # create from an Idf and an Epw object
        #'     opt <- OptimJob$new(eplusr::read_idf(idf_path), eplusr::read_epw(epw_path))
        #' }
        #' }
        #'
        initialize = function (idf, epw) {
            eplusr:::with_silent(super$initialize(idf, epw))
            private$m_controler <- ecr::initECRControl(identity, 1L)
            private$m_logger <- ecr::initLogger(private$m_controller, log.pop = TRUE)
            private$m_archive <- ecr::initParetoArchive(private$m_controller)
        },
        # }}}

        # PUBLIC FUNCTIONS {{{
        # apply_measure {{{
        apply_measure = function (measure, ..., .names = NULL)
            optim_apply_measure(super, self, private, measure, ..., .names = .names),
        # }}}

        # objective {{{
        objective = function (..., .n = NULL, .dir = "min")
            optim_objective(super, self, private, ..., .n = .n, .dir = .dir, .env = parent.frame()),
        # }}}

        # parameter {{{
        parameter = function ()
            optim_parameter(super, self, private),
        # }}}

        # population {{{
        population = function () {},
        # }}}

        # recombinator {{{
        recombinator = function (type = "cx", ...)
            optim_recombinator(super, self, private, type = type, ...),
        # }}}

        # mutator {{{
        mutator = function (type = "bitflip", ...)
            optim_mutator(super, self, private, type = type, ...),
        # }}}

        # selector {{{
        selector = function (parent = "simple", survival = "greedy", strategy = "plus")
            optim_selector(super, self, private, parent = parent, survival = survival, strategy = strategy),
        # }}}

        # terminator {{{
        terminator = function (fun, name, message, max_eval = Inf, max_iter = Inf)
            optim_terminator(super, self, private, fun = fun, name = name, message = message,
                max_eval = max_eval, max_iter = max_iter),
        # }}}

        # logger{{{
        logger = function (stats = list(fitness = list("min", "mean", "max")), pop = TRUE, init.size = 1000L)
            optim_logger(super, self, private, stats = stats, pop = pop, init.size = init.size),
        # }}}

        # pareto_archive {{{
        pareto_archive = function (max_size = Inf, trunc.fun = NULL)
            optim_pareto_archive(super, self, private, max_size = max_size, trunc.fun = trunc.fun),
        # }}}

        # pareto_front {{{
        pareto_front = function ()
            optim_pareto_front(super, self, private),
        # }}}

        # pareto_set {{{
        pareto_set = function ()
            optim_pareto_set(super, self, private),
        # }}}

        # run {{{
        run = function (dir = NULL, wait = TRUE, force = FALSE, copy_external = FALSE, echo = wait)
            optim_run(super, self, private, dir, wait, force, copy_external, echo),
        # }}}

        # plot {{{
        # plot results
        plot = function (pareto_front = TRUE, pareto_set = TRUE)
            optim_plot(super, self, private, pareto_front, pareto_set),
        # }}}

        # print {{{
        # Instead of merely printing EnergyPlus simulation statuses, provide a
        # similar interface as GA::plot.ga() to get a summary of optimization
        # results
        print = function ()
            optim_print(super, self, private)
        # }}}
        # }}}
    ),

    private = list(
        # PRIVATE FIELDS {{{
        m_seed = NULL,
        m_idfs = NULL,
        m_epws = NULL,
        m_job = NULL,
        m_log = NULL,
        m_controller = NULL,
        m_logger = NULL,
        m_archive = NULL
        # }}}
    )
)
# }}}

#' Create a Bayesian Calibration Job
#'
#' `optim_job()` takes an IDF and EPW as input, and returns an `OptimJob`
#' object for conducting optimization on an EnergyPlus model. For more
#' details, please see [OptimJob].
#'
#' @param idf A path to an local EnergyPlus IDF file or an `Idf` object.
#' @param epw A path to an local EnergyPlus EPW file or an `Epw` object.
#' @return A `OptimJob` object.
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
#'     optim_job(idf_path, epw_path)
#'
#'     # create from an Idf and an Epw object
#'     optim_job(read_idf(idf_path), read_epw(epw_path))
#' }
#' }
#' @seealso [sensi_job()] for creating a sensitivity analysis job.
#' @author Hongyuan Jia
#' @export
# optim_job {{{
optim_job <- function (idf, epw) {
    bc <- OptimJob$new(idf, epw)
    lockEnvironment(bc)
    bc
}
# }}}

# optim_apply_measure {{{
# TODO: How to solve mixed-encoding problems?
optim_apply_measure <- function (super, self, private, measure, ..., .names = NULL) {
    # measure name
    measure_name <- deparse(substitute(measure, parent.frame()))

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

    measure_wrapper <- function (idf, ...) {
        assert(eplusr::is_idf(idf), msg = paste0("Measure should take an `Idf` object as input, not `", class(idf)[[1]], "`."))
        idf <- idf$clone(deep = TRUE)
        idf <- measure(idf, ...)
        assert(eplusr::is_idf(idf), msg = paste0("Measure should return an `Idf` object, not `", class(idf)[[1]], "`."))
        idf
    }

    private$m_log$measure_wrapper <- measure_wrapper
    private$m_log$measure_name <- measure_name
    private$m_log$parameter <- l

    self
}
# }}}
# optim_parameter {{{
optim_parameter <- function (super, self, private) {
    private$m_log$parameter
}
# }}}
# optim_objective {{{
optim_objective <- function (super, self, private, ..., .n = NULL, .dir = "min", .env = parent.frame()) {
    l <- eval(substitute(alist(...)))

    # stop if empty input
    if (!length(l)) abort("error_empty_input", "Please give objective(s) to set.")

    .dir <- match.arg(.dir, c("min", "max"))
    if (.dir == "min") {
        .dir <- rep(-1L, length(l))
    } else {
        .dir <- rep(1L, length(l))
    }

    # objective names
    obj <- character(length(l))

    # constructed calls
    cl <- vector("list", length(l))

    # construct objective function
    for (i in seq_along(l)) {
        # use minus sign to indicate whether we want to minimize or maximize
        # Reference: data.table/data.table.R
        if (is.call(l[[i]]) && deparse(l[[i]][[1L]], 500L, backtick = FALSE) == "-") {
            .dir[[i]] <- .dir[[i]] * -1L
            l[[i]] <- l[[i]][[2L]]
        }

        # get function name
        obj[[i]] <- deparse(l[[i]])

        # get function content
        l[[i]] <- eval(l[[i]], .env, .env)

        # check if parameters have been set
        if (!is.null(private$m_log$measure_wrapper)) {
            # TODO: how to get parameter values?
            idf <- private$m_log$measure_wrapper(private$m_seed, 0)
            # how to let the user to determine whether to run the simulation or
            # not?
            args <- formals(l[[i]])
            if (!"idf" %in% names(args)) {
                abort("error_objective_idf_arg", paste0(
                    "Objective function '", obj[[i]], "' must have an parameter named 'idf'."
                ))
            }

            # store the call for future evaluation when run method is called
            # add param if necessary
            if ("param" %in% names(args)) {
                cl[[i]] <- match.call(l[[i]], quote(l[[i]](idf = idf, param = private$m_log$parameter)))
            } else {
                cl[[i]] <- match.call(l[[i]], quote(l[[i]](idf = idf)))
            }
        }
    }

    # store
    private$m_log$objective$name <- obj
    private$m_log$objective$fun <- l
    private$m_log$objective$call <- cl
    private$m_log$objective$direction <- .dir

    self
}
# }}}
# optim_run {{{
optim_run <- function (super, self, private, dir = NULL, wait = TRUE, force = FALSE, copy_external = FALSE, echo = wait) {
    # initial run on a single sample to:
    # 1. measure and parameter settings work fine
    # 2. make sure objective functions return numeric values
    # 3. determine number of objectives

    # procedure
    # 1. generate initial population
    # 2. calculate intital solutions
    # 3. generate offspring:
    #    1. ecr::generateOffspring
    #    2. ecr::evaluateFitness
    #    3. attr(offspring, "fitness") <- fitness
    #    4. select based on survial strategy
    #    5. do logging
    #    6. check terminate condition
    # 4. summary results using ecr::makeECRResults

    # TODO:
    # 1. is pareto archive really needed?
    # 2. use ecr internal parallelization instead of eplusr?
}
# }}}
# optim_print {{{
optim_print <- function (super, self, private) {
    path_epw <- if (is.null(private$m_epws)) NULL else vcapply(private$m_epws, function (epw) epw$path())
    eplusr:::print_job_header(title = "EnergPlus Optimization Simulation Job",
        path_idf = private$m_seed$path(),
        path_epw = path_epw,
        eplus_ver = private$m_seed$version(),
        name_idf = "Seed", name_epw = "Weather"
    )

    if (is.null(private$m_log$measure_name)) {
        cli::cat_line(("Measure to apply: << No measure has been set >>"), col = "white", background_col = "blue")
        cli::cat_line("Parameter [0]: << No measure has been set >>", col = "white", background_col = "blue")
    } else {
        cli::cat_line(c(
            sprintf("Measure to apply: '%s'", private$m_log$measure_name),
            sprintf("Parameter [%i]: ", length(private$m_log$parameter))
        ))
        for (i in seq_along(private$m_log$parameter)) {
            cli::cat_line(sprintf(
                "  [%i] %s: %s", i,
                names(private$m_log$parameter)[[i]],
                format(private$m_log$parameter[[i]])
            ))
        }
    }

    if (is.null(private$m_log$objective)) {
        cli::cat_line("Objective [0]: << No objective has been set >>",
            col = "white", background_col = "blue")
    } else {
        cli::cat_line(c(
            sprintf("Objective [%i]: ", length(private$m_log$objective$name))
        ))
        cli::cat_line(sprintf("  [%i] %s <%s>",
            seq_along(private$m_log$objective$name),
            private$m_log$objective$name,
            ifelse(private$m_log$objective$direction == -1L, cli::symbol$arrow_down, cli::symbol$arrow_up)
        ))
    }

    return(invisible(self))
}
# }}}

# HELPERS
# float_space {{{
float_space <- function (min, max, init = mean(c(min, max))) {
    assert(is_number(min), is_number(max), is_number(init))
    assert(min <= max, min <= init, init <= max)
    structure(list(min = min, max = max, init = init), class = "FloatSpace")
}
print.FloatSpace <- function (x, ...) {
    cat(sprintf("[%s, %s] | Init: %s\n", x[["min"]], x[["max"]], x[["init"]]))
    invisible(x)
}

format.FloatSpace <- function (x, ...) {
    sprintf("[%s, %s] | Init: %s", x[["min"]], x[["max"]], x[["init"]])
}
# }}}
# choice_space {{{
choice_space <- function (choices, init = choices[1]) {
    assert(is.character(choices), !anyNA(choices))
    assert(eplusr:::is_string(init), init %in% choices)
    structure(list(x = choices, init = init), class = "ChoiceSpace")
}
# }}}
# integer_space {{{
integer_space <- function (integers, init = integers[1]) {
    assert(eplusr:::are_integer(integers))
    assert(eplusr:::is_integer(init), init %in% integers)
    structure(list(x = integers, init = init), class = "IntegerSpace")
}
# }}}
