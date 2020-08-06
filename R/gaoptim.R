#' @importFrom ecr initECRControl initLogger
#' @include utils.R
NULL

#' Conduct Multi-Objective Optimization on An EnergyPlus Model
#'
#' `GAOptimJob` class provides a prototype of conducting single- or multi-
#' objective(s) optimizations on an EnergyPlus model using Genetic Algorithm
#'
#' The basic workflow is basically:
#'
#' @docType class
#' @name GAOptimJob
#' @author Hongyuan Jia
NULL

#' @export
# GAOptimJob {{{
GAOptimJob <- R6::R6Class(classname = "GAOptimJob",
    inherit = eplusr::ParametricJob, cloneable = FALSE, lock_objects = FALSE,

    public = list(
        # INITIALIZE {{{
        #' @description
        #' Create a `GAOptimJob` object
        #'
        #' @param idf A path to an local EnergyPlus IDF file or an [eplusr::Idf] object.
        #' @param epw A path to an local EnergyPlus EPW file or an [eplusr::Epw] object.
        #'
        #' @return A `GAOptimJob` object.
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
        #'     GAOptimJob$new(idf_path, epw_path)
        #'
        #'     # create from an Idf and an Epw object
        #'     opt <- GAOptimJob$new(eplusr::read_idf(idf_path), eplusr::read_epw(epw_path))
        #' }
        #' }
        #'
        initialize = function (idf, epw) {
            eplusr:::with_silent(super$initialize(idf, epw))

            # init controller
            private$m_ctrl <- ecr::initECRControl(identity, 1L)

            self$recombinator()
            self$mutator()
            self$selector()
            self$terminator()

            # init logger
            private$m_logger <- ecr::initLogger(private$m_ctrl, log.pop = TRUE)
            private$m_logger$env$time.begin <- Sys.time()
            private$m_logger$env$time.started <- proc.time()[3]

            private$m_archive <- ecr::initParetoArchive(private$m_ctrl)
        },
        # }}}

        # PUBLIC FUNCTIONS {{{
        # param {{{
        param = function (..., .names = NULL)
            gaopt_param(super, self, private, ..., .names = .names),
        # }}}

        # apply_measure {{{
        apply_measure = function (measure, ..., .names = NULL)
            gaopt_apply_measure(super, self, private, measure, ..., .names = .names),
        # }}}

        # objective {{{
        objective = function (..., .n = NULL, .dir = "min")
            gaopt_objective(super, self, private, ..., .n = .n, .dir = .dir, .env = parent.frame()),
        # }}}

        # recombinator {{{
        recombinator = function (...,
                                 .float = setwith(ecr::recSBX, eta = 15, p = 0.7),
                                 .integer = setwith(recPCrossover, p = 0.7),
                                 .choice = setwith(recPCrossover, p = 0.7))
            gaopt_recombinator(super, self, private, ..., .float = .float, .integer = .integer, .choice = .choice),
        # }}}

        # mutator {{{
        mutator = function (...,
                            .float = setwith(ecr::mutPolynomial, eta = 25, p = 0.1),
                            .integer = mutRandomChoice,
                            .choice = mutRandomChoice)
            gaopt_mutator(super, self, private, ..., .float = .float, .integer = .integer, .choice = .choice),
        # }}}

        # selector {{{
        selector = function (parent = ecr::selSimple, survival = ecr::selNondom, strategy = "plus")
            gaopt_selector(super, self, private, parent = parent, survival = survival, strategy = strategy),
        # }}}

        # terminator {{{
        terminator = function (fun = NULL, name, message, max_gen = NULL, max_eval = NULL, max_time = NULL)
            gaopt_terminator(super, self, private, fun = fun, name = name, message = message,
                max_gen = max_gen, max_eval = max_eval, max_time = max_time),
        # }}}

        # validate {{{
        validate = function (param = NULL, ddy_only = TRUE, verbose = TRUE)
            gaopt_validate(super, self, private, param = param, ddy_only = ddy_only, verbose = verbose),
        # }}}

        # run {{{
        run = function (mu = 20L, p_recomb = 0.7, p_mut = 0.1, dir = NULL, wait = TRUE, parallel = TRUE)
            gaopt_run(super, self, private, mu, p_recomb, p_mut, dir, wait, parallel),
        # }}}

        # best_set {{{
        best_set = function (unique = TRUE)
            gaopt_best_set(super, self, private, unique = unique),
        # }}}

        # pareto_set {{{
        pareto_set = function (unique = TRUE)
            gaopt_pareto_set(super, self, private, unique = unique),
        # }}}

        # population {{{
        population = function ()
            gaopt_population(super, self, private),
        # }}}

        # print {{{
        # Instead of merely printing EnergyPlus simulation statuses, provide a
        # similar interface as GA::plot.ga() to get a summary of optimization
        # results
        print = function ()
            gaopt_print(super, self, private)
        # }}}
        # }}}
    ),

    private = list(
        # PRIVATE FIELDS {{{
        m_seed = NULL,
        m_idfs = NULL,
        m_job = NULL,
        m_log = NULL,
        m_ctrl = NULL,
        m_logger = NULL,
        m_archive = NULL
        # }}}
    )
)
# }}}

#' Create an Optimization Job
#'
#' `gaoptim_job()` takes an IDF and EPW as input, and returns an `GAOptimJob`
#' object for conducting optimization on an EnergyPlus model. For more
#' details, please see [GAOptimJob].
#'
#' @param idf A path to an local EnergyPlus IDF file or an `Idf` object.
#' @param epw A path to an local EnergyPlus EPW file or an `Epw` object.
#' @return A `GAOptimJob` object.
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
#'     gaoptim_job(idf_path, epw_path)
#'
#'     # create from an Idf and an Epw object
#'     gaoptim_job(read_idf(idf_path), read_epw(epw_path))
#' }
#' }
#' @seealso [sensi_job()] for creating a sensitivity analysis job.
#' @author Hongyuan Jia
#' @export
# gaoptim_job {{{
gaoptim_job <- function (idf, epw) {
    bc <- GAOptimJob$new(idf, epw)
    lockEnvironment(bc)
    bc
}
# }}}

# gaopt_apply_measure {{{
gaopt_apply_measure <- function (super, self, private, measure, ..., .names = NULL) {
    # measure name
    measure_name <- deparse(substitute(measure, parent.frame()))

    checkmate::assert_function(measure)
    if (length(formals(measure)) < 2L) {
        abort("'measure' function must have at least two argument.", "ga_measure_no_arg")
    }

    # match fun arg
    cl <- match.call(measure, quote(measure(private$m_seed, ...)))
    mc <- cl[-1L]
    l <- vector("list", length(mc[-1L]))
    names(l) <- names(mc[-1L])

    # stop if parameter contains reserved names
    if (any(names(l) %in% c(".float", ".integer", ".choice"))) {
        abort("Parameters cannot contain any internal reserved names ('.float', '.integer', and '.choice')", "ga_invalid_param")
    }

    # get value
    for (nm in names(l)) {
        l[[nm]] <- eval(mc[-1L][[nm]])

        if (!inherits(l[[nm]], "ParamSpace")) {
            abort(sprintf(
                "Each parameter should be a 'ParamSpace' object. Invalid input found: '%s' ('%s').",
                nm, class(l[[nm]])[1L]
            ), "ga_invalid_param")
        }
    }

    private$m_log$measure$name <- measure_name
    private$m_log$measure$fun <- measure
    private$m_log$parameter <- l

    self
}
# }}}
# gaopt_parameter {{{
gaopt_parameter <- function (super, self, private) {
    private$m_log$parameter
}
# }}}
# gaopt_objective {{{
gaopt_objective <- function (super, self, private, ..., .n = NULL, .dir = "min", .env = parent.frame()) {
    l <- eval(substitute(alist(...)))

    # stop if empty input
    if (!length(l)) abort("Please give objective(s) to set.", "ga_empty_input")

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

        args <- formals(l[[i]])

        if (!"idf" %in% names(args)) {
            abort(paste0(
                "Objective function '", obj[[i]], "' must have an parameter named 'idf'."
            ), "ga_objective_idf_arg")
        }
    }

    # store
    private$m_log$objective$name <- obj
    private$m_log$objective$fun <- l
    private$m_log$objective$direction <- .dir

    self
}
# }}}
# gaopt_recombinator {{{
gaopt_recombinator <- function (super, self, private, ...,
                                .float = setwith(ecr::recSBX, eta = 15, p = 0.7),
                                .integer = setwith(recPCrossover, p = 0.7),
                                .choice = setwith(recPCrossover, p = 0.7)) {
    rec <- list(...)
    if (length(rec)) {
        for (i in seq_along(rec)) {
            do.call(gaopt_register_operator, list(super, self, private,
                slot = paste0("recombine_", names(rec)[i]),
                fun = rec[[i]]
            ))
        }
    }
    # TODO: check unused
    gaopt_register_operator(super, self, private, "recombine_.float", .float)
    gaopt_register_operator(super, self, private, "recombine_.integer", .integer)
    gaopt_register_operator(super, self, private, "recombine_.choice", .choice)
    self
}
# }}}
# gaopt_mutator {{{
gaopt_mutator <- function (super, self, private, ...,
                           .float = setwith(ecr::mutPolynomial, eta = 25, p = 0.2),
                           .integer = ecr::mutSwap,
                           .choice = ecr::mutSwap) {
    mut <- list(...)
    if (length(mut)) {
        for (i in seq_along(mut)) {
            do.call(gaopt_register_operator, list(super, self, private,
                slot = paste0("recombine_", names(mut)[i]),
                fun = mut[[i]]
            ))
        }
    }
    # TODO: check unused
    gaopt_register_operator(super, self, private, "mutate_.float", .float)
    gaopt_register_operator(super, self, private, "mutate_.integer", .integer)
    gaopt_register_operator(super, self, private, "mutate_.choice", .choice)
    self
}
# }}}
# gaopt_selector {{{
gaopt_selector <- function (super, self, private, parent = ecr::selSimple, survival = ecr::selNondom, strategy = "plus") {
    gaopt_register_operator(super, self, private, "selectForMating", parent)
    gaopt_register_operator(super, self, private, "selectForSurvival", survival)
    private$m_ctrl$survival.strategy <- match.arg(strategy, c("plus", "comma"))
    self
}
# }}}
# gaopt_terminator {{{
gaopt_terminator <- function (super, self, private, fun = NULL, name, message,
                              max_eval = NULL, max_gen = NULL, max_time = NULL) {
    term <- list()

    if (!is.null(max_eval)) term <- c(term, ecr::stopOnEvals(max_eval))
    if (!is.null(max_gen)) term <- c(term, list(ecr::stopOnIters(max_gen)))
    if (!is.null(max_time)) term <- c(term, list(stopOnMaxTime(max_time)))

    if (!is.null(fun)) {
        term <- c(term, list(ecr::makeTerminator(fun, name, message)))
    }

    private$m_log$term <- term

    self
}
# }}}
# gaopt_run {{{
gaopt_run <- function (super, self, private, mu = 20L, p_recomb = 0.7, p_mut = 0.1, dir = NULL, wait = TRUE, parallel = TRUE) {
    assert_ready_optim(super, self, private)

    cli::cat_rule("Initialization")
    # get initial population of parameters
    cli::cat_line("  * Create initial population")
    init <- gaopt_init_population(super, self, private, mu)

    # get objective dimension
    if (is.null(private$m_log$objective$dim)) gaopt_validate(super, self, private, as.list(init[1L]), FALSE)

    # update controller
    gaopt_update_controller(super, self, private)

    # evaluate fitness
    population <- transpose_param(init)
    cli::cat_line("  * Evaluate fitness values")
    fitness <- gaopt_evaluate_fitness(super, self, private, gen = -1, population,
        private$m_epws_path[[1L]], dir = dir, parallel = parallel)
    for (i in seq_along(population)) {
        data.table::setattr(population[[i]], "fitness", fitness[, i])
    }

    repeat {
        cli::cat_rule(sprintf("Generation [%i]", private$m_logger$env$n.gens + 1L))
        # generate offspring
        cli::cat_line("  * Generate offspring")
        offspring <- gaopt_gen_offspring(super, self, private, population, fitness,
            p.recomb = p_recomb, p.mut = p_mut)

        cli::cat_line("  * Evaluate fitness values")
        fitness.offspring <- gaopt_evaluate_fitness(super, self, private,
            gen = private$m_logger$env$n.gens, offspring, private$m_epws_path[[1L]],
            dir = dir, parallel)

        cli::cat_line(sprintf("  * Evaluate fitness values --> Average fitness: %s", mean(fitness.offspring[1, ])))

        for (i in seq_along(offspring)) {
            data.table::setattr(offspring[[i]], "fitness", fitness.offspring[, i])
        }

        cli::cat_line("  * Prepare next generation")
        sel = if (private$m_ctrl$survival.strategy == "plus") {
            ecr::replaceMuPlusLambda(private$m_ctrl, population, offspring, fitness, fitness.offspring)
        } else {
            ecr::replaceMuCommaLambda(private$m_ctrl, population, offspring, fitness, fitness.offspring)
        }

        population <- sel$population
        fitness <- sel$fitness

        # TODO: remove it before append
        save_gen_log(population, fitness, private$m_logger$env$n.gens + 1L, private$m_log$objective,
            file.path(if (is.null(dir)) dirname(private$m_seed$path()) else dir, sprintf("Results-%s.csv", Sys.Date()))
        )

        # do some logging
        cli::cat_line("  * Update log")
        ecr::updateLogger(private$m_logger, population, fitness, n.evals = mu)

        cli::cat_line("  * Check whether terminator conditions are met")
        stop.object <- ecr:::doTerminate(private$m_logger, private$m_log$term)

        if (length(stop.object) > 0L) {
            cli::cat_rule("Terminated")
            cli::cat_line(sprintf("  < %s: %s >", stop.object$name, stop.object$message))
            break
        }
    }

    private$m_log$results <- ecr:::makeECRResult(private$m_ctrl, private$m_logger, population, fitness, stop.object)

    private$m_log$results

    self
}
# }}}
# gaopt_print {{{
gaopt_print <- function (super, self, private) {
    path_epw <- if (is.null(private$m_epws_path)) NULL else private$m_epws_path
    eplusr:::print_job_header(title = "EnergPlus Optimization Simulation Job",
        path_idf = private$m_seed$path(),
        path_epw = path_epw,
        eplus_ver = private$m_seed$version(),
        name_idf = "Seed", name_epw = "Weather"
    )
    print(private$m_ctrl)

    if (is.null(private$m_log$measure)) {
        cli::cat_line(("Measure to apply: << No measure has been set >>"), col = "white", background_col = "blue")
        cli::cat_line("Parameter [0]: << No measure has been set >>", col = "white", background_col = "blue")
    } else {
        cli::cat_line(c(
            sprintf("Measure to apply: '%s'", private$m_log$measure$name),
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
# gaopt_init_population {{{
gaopt_init_population <- function (super, self, private, mu = 20L) {
    parameter <- private$m_log$parameter

    if (is.null(parameter)) {
        abort("No parameter has been set.", "ga_no_parameter")
    }

    checkmate::assert_count(mu, positive = TRUE)

    # get parameter types
    type_par <- vapply(parameter, function (x) class(x)[1L], character(1))

    # extract parameters of different types
    # 1. for float search space
    par_float <- parameter[type_par == "FloatSpace"]
    # 2. for integer search space
    par_integer <- parameter[type_par == "IntegerSpace"]
    # 3. for choice search space
    par_choice <- parameter[type_par == "ChoiceSpace"]

    # create initial population
    # 1. for float search space
    pop_float <- lapply(par_float, function (mu, par) {
        unlist(ecr::initPopulation(mu, ecr::genReal, n.dim = 1, lower = par$min, upper = par$max))
    }, mu = mu)
    # 2. for integer search space
    pop_integer <- lapply(par_integer, function (mu, par) replicate(mu, sample(par$x, 1)), mu = mu)
    # 3. for choice search space
    pop_choice <- lapply(par_choice, function (mu, par) replicate(mu, sample(par$x, 1)), mu = mu)

    # store results in a data.table
    pop <- as.data.table(c(pop_float, pop_integer, pop_choice))

    # restore the original order
    data.table::setcolorder(pop, names(parameter))

    pop
}
# }}}
# gaopt_fitness_fun {{{
gaopt_fitness_fun <- function (super, self, private, param, path, weather) {
    idf <- gaopt_gen_fitness_idf(super, self, private, param, path)
    idf$run(weather, NULL, force = TRUE, echo = FALSE)
    gaopt_gen_fitness_obj(super, self, private, idf, param)
}
# }}}
# gaopt_gen_fitness_idf {{{
gaopt_gen_fitness_idf <- function (super, self, private, param, outfile = NULL) {
    # initial run on a single individual
    measure <- private$m_log$measure
    idf <- do.call(measure$fun, c(private$m_seed$clone(), param))
    if (!is.null(outfile)) suppressMessages(idf$save(outfile, overwrite = TRUE))
    idf
}
# }}}
# gaopt_gen_fitness_obj {{{
gaopt_gen_fitness_obj <- function (super, self, private, idf, param) {
    objective <- private$m_log$objective

    n_fun <- length(objective$name)

    obj <- vector("list", n_fun)

    for (i in seq_len(n_fun)) {
        # check if 'param' is in the arguments
        if ("param" %in% names(formals(objective$fun[[i]]))) {
            obj[[i]] <- do.call(objective$fun[[i]], list(idf = idf, param = param))
        } else {
            obj[[i]] <- do.call(objective$fun[[i]], list(idf = idf))
        }

        if (is.na(obj[[i]])) {
            abort(sprintf(
                "Objective function '%s' returns value(s) of NA.",
                objective$name[i]
            ), "ga_wrong_obj_val")
        }

        if (length(obj[[i]]) != objective$dim[i]) {
            abort(sprintf(
                "Objective function '%s' returns value(s) of wrong dimention '%i' (should be '%i').",
                objective$name[i], length(obj[[i]]), objective$dim[i]
            ), "ga_wrong_obj_dim")
        }
    }

    unlist(obj)
}
# }}}
# gaopt_validate {{{
gaopt_validate <- function (super, self, private, param = NULL, ddy_only = TRUE, verbose = TRUE) {
    if (verbose) message("Checking if parameter(s) has been set ...")
    assert_ready_parameter(super, self, private)

    if (verbose) message("Checking if objective(s) has been set ...")
    assert_ready_objective(super, self, private)

    if (is.null(param)) {
        param <- as.list(gaopt_init_population(super, self, private, mu = 1))
    }

    if (verbose) message(sprintf("Validating parameter function '%s' ...", private$m_log$measure$name))
    idf <- gaopt_gen_fitness_idf(super, self, private, param, tempfile(fileext = ".idf"))

    if (verbose) message("Validating objective function(s)...")
    # check if model has DDY
    if (isTRUE(ddy_only)) {
        if ("SizingPeriod:DesignDay" %in% idf$class_name()) {
            idf$run(NULL, echo = FALSE)
        } else {
            if (verbose) message("    NOTE: No design day found in sample model. Full run will be conducted which may take longer time.")
            idf$SimulationControl$Run_Simulation_for_Weather_File_Run_Periods <- "Yes"
            idf$run(private$m_epws_path[[1]], echo = FALSE)
        }
    } else {
        idf$SimulationControl$Run_Simulation_for_Weather_File_Run_Periods <- "Yes"
        idf$run(private$m_epws_path[[1]], echo = FALSE)
    }

    if (!isTRUE(idf$last_job()$status()$successful)) {
        stop("Validation failed. Test simulation did not complete successfully. ",
            "The error messages are:\n",
            paste(paste0("  > ", utils::capture.output(print(idf$last_job()$errors()))), collapse = "\n"),
            call. = FALSE
        )
    }

    objective <- private$m_log$objective
    n_fun <- length(objective$name)
    obj <- vector("list", n_fun)
    for (i in seq_len(n_fun)) {
        # check if 'param' is in the arguments
        if ("param" %in% names(formals(objective$fun[[i]]))) {
            obj[[i]] <- do.call(objective$fun[[i]], list(idf = idf, param = param))
        } else {
            obj[[i]] <- do.call(objective$fun[[i]], list(idf = idf))
        }

        if (!length(obj[[i]])) {
            abort(sprintf(
                "Objective function '%s' should return at least a length-one vector instead of a 0-length one.",
                objective$name[[i]]
            ), "ga_invalid_objective")
        }

        if (!is.numeric(obj[[i]])) {
            abort(sprintf(
                "Objective function '%s' should return a numeric vector instead of a '%s' object.",
                objective$name[[i]], class(obj[[i]])[[1]]
            ), "ga_invalid_objective")
        }

        if (verbose) message(sprintf("  [%i] '%s' --> OK", i, objective$name[[i]]))
    }

    # store objective dimension
    private$m_log$objective$dim <- vapply(obj, length, integer(1))

    if (verbose) message("All checks have been passed.")

    invisible(TRUE)
}
# }}}
# gaopt_update_controller {{{
gaopt_update_controller <- function (super, self, private) {
    objective <- private$m_log$objective

    private$m_ctrl$task$fitness.fun <- gaopt_fitness_fun
    private$m_ctrl$task$n.objectives <- sum(objective$dim)
    private$m_ctrl$task$minimize <- ifelse(objective$direction == -1L, TRUE, FALSE)
    private$m_ctrl$task$objective.names <- make.unique(rep(objective$name, objective$dim), sep = "_")

    private$m_ctrl
}
# }}}
# gaopt_evaluate_fitness {{{
gaopt_evaluate_fitness <- function (super, self, private, gen, population, weather, dir = NULL, parallel = TRUE) {
    if (is.null(dir)) dir <- dirname(private$m_seed$path())

    # construct IDF path
    path <- file.path(dir, sprintf("Gen%i", gen),
        sprintf("Gen%i_Ind%i", gen, seq_along(population)),
        sprintf("Gen%i_Ind%i.idf", gen, seq_along(population))
    )

    if (eplusr:::is_flag(parallel)) {
        if (parallel) {
            future::plan(future::multiprocess)
        } else {
            future::plan(future::sequential)
        }
    } else {
        future::plan(parallel)
    }

    fitness <- future.apply::future_mapply(
        gaopt_fitness_fun, param = population, path = path,
        MoreArgs = list(super = super, self = self, private = private, weather = weather),
        SIMPLIFY = FALSE
    )
    ecr:::makeFitnessMatrix(do.call(cbind, fitness), private$m_ctrl)
}
# }}}
# gaopt_gen_offspring {{{
gaopt_gen_offspring <- function (super, self, private, inds, fitness, p.recomb = 0.7, p.mut = 0.1) {
    offspring <- gaopt_gen_offspring_action(super, self, private, "recombine", inds, fitness, p.recomb)
    gaopt_gen_offspring_action(super, self, private, "mutate", offspring, fitness, p.mut)
}
# }}}
# gaopt_gen_offspring_action {{{
gaopt_gen_offspring_action <- function (super, self, private, action, inds, fitness, p) {
    param <- private$m_log$parameter

    # get parameter type
    type <- vapply(param, function (x) class(x)[1L], character(1))

    # store all individuals into a data.table
    pop <- as.data.table(lapply(transpose_param(inds), unlist))

    # first apply parameter-wise
    self_act <- paste(action, names(type), sep = "_")
    data.table::setattr(self_act, "names", names(type))

    if (any(i_self_act <- self_act %in% names(private$m_ctrl))) {
        for (var in names(type)[i_self_act]) {
            if (type[var] == "FloatSpace" && "lower" %in% names(formals(private$self_act[var])) &&
                is.null(private$m_ctrl[[paste0(self_act[var], ".pars")]]$lower)) {
                r <- get_param_range(param[[var]])
                private$m_ctrl[[paste0(self_act[var], ".pars")]]$lower <- r[1L]
                private$m_ctrl[[paste0(self_act[var], ".pars")]]$upper <- r[2L]

            }

            if (type[var] != "FloatSpace" && "values" %in% names(formals(private$self_act[var])) &&
                is.null(private$m_ctrl[[paste0(self_act[var], ".pars")]]$values)) {
                r <- get_param_range(param[[var]])
                private$m_ctrl[[paste0(slot, ".pars")]]$values <- list(r)
            }

            if (action == "recombine") {
                data.table::set(pop, NULL, var,
                    unlist(ecr::recombinate(private$m_ctrl, pop[[var]], fitness, length(pop[[var]]), p.recomb = p, slot = self_act[var]))
                )
            } else if (action == "mutate") {
                data.table::set(pop, NULL, var,
                    unlist(ecr::mutate(private$m_ctrl, pop[[var]], p.mut = p, slot = self_act[var]))
                )
            }
            private$m_ctrl[[paste0(self_act[var], ".pars")]]$values <- NULL
            private$m_ctrl[[paste0(self_act[var], ".pars")]]$lower <- NULL
            private$m_ctrl[[paste0(self_act[var], ".pars")]]$upper <- NULL
        }
    }

    # exclude self rec/mut param and split others by type
    type_other <- split(type[!i_self_act], type)
    for (ty in type_other) {
        slot <- sprintf("%s_.%s", action,
            switch(ty[[1L]], "FloatSpace" = "float", "IntegerSpace" = "integer", "ChoiceSpace" = "choice"))

        for (nm in names(ty)) {

            if (ty[[nm]] == "FloatSpace" && "lower" %in% names(formals(private$m_ctrl[[slot]])) &&
                is.null(private$m_ctrl[[paste0(slot, ".pars")]]$lower)) {
                r <- get_param_range(param[[nm]])
                private$m_ctrl[[paste0(slot, ".pars")]]$lower <- r[1]
                private$m_ctrl[[paste0(slot, ".pars")]]$upper <- r[2]
            }

            if (ty[[nm]] != "FloatSpace" && "values" %in% names(formals(private$m_ctrl[[slot]])) &&
                is.null(private$m_ctrl[[paste0(slot, ".pars")]]$values)) {
                r <- get_param_range(param[[nm]])
                private$m_ctrl[[paste0(slot, ".pars")]]$values <- list(r)
            }

            if (action == "recombine") {
                data.table::set(pop, NULL, nm, unlist(
                    ecr::recombinate(private$m_ctrl, as.list(pop[[nm]]), fitness, nrow(pop), p.recomb = p, slot = slot)
                ))
            } else if (action == "mutate") {
                data.table::set(pop, NULL, nm, unlist(
                    ecr::mutate(private$m_ctrl, as.list((pop[[nm]])), p.mut = p, slot = slot),
                    recursive = FALSE, use.names = FALSE
                ))
            }
            private$m_ctrl[[paste0(slot, ".pars")]]$values <- NULL
            private$m_ctrl[[paste0(slot, ".pars")]]$upper <- NULL
            private$m_ctrl[[paste0(slot, ".pars")]]$lower <- NULL
        }
    }

    transpose_param(pop)
}
# }}}
# gaopt_register_operator {{{
gaopt_register_operator <- function (super, self, private, slot, fun, ...) {
    # remove existing
    private$m_ctrl[[slot]] <- NULL

    if (inherits(fun, "ecr_operator_setwith")) {
        private$m_ctrl <- ecr::registerECROperator(private$m_ctrl, slot, fun$fun)
        private$m_ctrl[[paste0(slot, ".pars")]] <- c(fun$args, ...)
    } else {
        private$m_ctrl <- ecr::registerECROperator(private$m_ctrl, slot, fun, ...)
    }

    private$m_ctrl
}
# }}}
# gaopt_update_logger {{{
gaopt_update_logger <- function (super, self, private, pop, fitness, n.evals) {
    ecr::updateLogger(private$m_logger, pop, fitness, n.evals = mu)

    # add time passed
    private$m_logger$env$time.end <- Sys.time()
    private$m_logger$env$time.passed <- proc.time()[3] - private$m_logger$env$time.started

    private$m_logger
}
# }}}
# gaopt_population {{{
gaopt_population <- function (super, self, private) {
    res <- private$m_log$results

    if (is.null(res)) {
        message("Optimization has not been run before.")
        return(invisible())
    }

    pop <- res$log$env$pop[1:res$log$env$n.gens]

    extract_population(pop, private$m_log$objective)
}
# }}}
# gaopt_pareto_set {{{
gaopt_pareto_set <- function (super, self, private, unique = TRUE) {
    res <- private$m_log$results

    if (is.null(res)) {
        message("Optimization has not been run before.")
        return(invisible())
    }

    if (private$m_ctrl$task$n.objectives < 2L) {
        message("'$pareto_set()' only works for multi-objective optimization problem.")
        return(invisible())
    }

    if (unique) {
        pset <- extract_population(list(list(population = res$pareto.set)),
            private$m_log$objective
        )

        set(pset, NULL, "index_gen", NULL)
        setnames(pset, "index_ind", "index")
    } else {
        pop <- gaopt_population(super, self, private)
        pset <- get_nondominated(pop, private$m_log$objective)
        pset[, index := .I]
        setcolorder(pset, "index")
    }

    pset
}
# }}}
# gaopt_best_set {{{
gaopt_best_set <- function (super, self, private, unique = TRUE) {
    res <- private$m_log$results

    if (is.null(res)) {
        message("Optimization has not been run before.")
        return(invisible())
    }

    if (private$m_ctrl$task$n.objectives != 1L) {
        message("'$best_set()' only works for single-objective optimization problem.")
        return(invisible())
    }

    pset <- extract_population(list(list(population = res$best.x)),
        private$m_log$objective
    )

    set(pset, NULL, c("index_gen", "index_ind"), NULL)
    if (unique) return(pset)

    population <- gaopt_population(super, self, private)
    pset <- population[pset, on = names(pset)]
    pset[, index := .I]
    setcolorder(pset, c("index", "index_gen", "index_ind"))

    pset[]
}
# }}}

# HELPERS
# float_space {{{
#' Specify optimizatino parameter of float type
#'
#' @param min Minimum value
#' @param max Maximum value
#' @param init Initial value. Currently not used.
#'
#' @return A `FloatSpace` object
#' @export
#'
#' @examples
#' float_space(1.0, 5.0)
float_space <- function (min, max, init = mean(c(min, max))) {
    checkmate::assert_number(min)
    checkmate::assert_number(max)
    if (min > max) abort("'min' should be no larger than 'max'.")
    checkmate::assert_number(init, lower = min, upper = max)
    structure(list(min = min, max = max, init = init), class = c("FloatSpace", "ParamSpace"))
}
#' @export
format.FloatSpace <- function (x, ...) {
    sprintf("[%s, %s] | Init: %s", x[["min"]], x[["max"]], x[["init"]])
}
#' @export
print.FloatSpace <- function (x, ...) {
    cat(format.FloatSpace(x), "\n", sep = "")
    invisible(x)
}
# for better print in data.table
#' @export
format.FloatRange <- function (x, ...) {
    sprintf("[%s, %s]", x[[1]], x[[2]])
}
#' @export
print.FloatRange <- function (x, ...) {
    cat(format.FloatRange(x), "\n", sep = "")
    invisible(x)
}
# }}}
# choice_space {{{
#' Specify optimizatino parameter of character type
#'
#' @param choices A character vector of choices
#' @param init Initial value. Currently not used.
#'
#' @return A `ChoiceSpace` object
#' @export
#'
#' @examples
#' choice_space(c("Roughness", "Smooth"))
choice_space <- function (choices, init = choices[1]) {
    checkmate::assert_character(choices, any.missing = FALSE)
    checkmate::assert_choice(init, choices)
    structure(list(x = choices, init = init), class = c("ChoiceSpace", "ParamSpace"))
}
#' @export
format.ChoiceSpace <- function (x, ...) {
    val <- if (length(x$x) > 3L) c(x$x[1:3], "...") else x$x
    sprintf("%s | Init: '%s'", paste0(val, collapse = ", "), x$init)
}
#' @export
print.ChoiceSpace <- function (x, ...) {
    cat(format.ChoiceSpace(x), "\n", sep = "")
    invisible(x)
}
# for better print in data.table
#' @export
format.ChoiceRange <- function (x, ...) {
    paste0(if (length(x) > 3L) c(x[1:3], "...") else x, collapse = ", ")
}
#' @export
print.ChoiceRange <- function (x, ...) {
    cat(format.ChoiceRange(x), "\n", sep = "")
    invisible(x)
}
# }}}
# integer_space {{{
#' Specify optimizatino parameter of integer type
#'
#' @param integers An integer vector.
#' @param init Initial value. Currently not used.
#'
#' @return A `IntegerSpace` object.
#' @export
#'
#' @examples
#' integer_space(1:5)
integer_space <- function (integers, init = integers[1]) {
    integers <- checkmate::assert_integerish(integers, any.missing = FALSE, coerce = TRUE)
    init <- checkmate::assert_integerish(init, any.missing = FALSE, coerce = TRUE, len = 1L)
    checkmate::assert_choice(init, integers)
    structure(list(x = integers, init = init), class = c("IntegerSpace", "ParamSpace"))
}
#' @export
format.IntegerSpace <- format.ChoiceSpace
#' @export
print.IntegerSpace <- print.ChoiceSpace
#' @export
format.IntegerRange <- format.ChoiceRange
#' @export
printf.IntegerRange <- print.ChoiceRange
# }}}
# get_param_range {{{
get_param_range <- function (x, ...) {
    UseMethod("get_param_range")
}
get_param_range.default <- function (x, ...) {
    stop("Unknown parameter space type")
}
get_param_range.FloatSpace <- function (x, ...) {
    structure(c(x[["min"]], x[["max"]]), class = "FloatRange")
}
get_param_range.ChoiceSpace <- function (x, ...) {
    structure(x[["x"]], class = "ChoiceRange")
}
get_param_range.IntegerSpace <- function (x, ...) {
    structure(x[["x"]], class = "IntegerRange")
}
# }}}
# assert_ready_optim {{{
assert_ready_optim <- function (super, self, private) {
    assert_ready_parameter(super, self, private)
    assert_ready_objective(super, self, private)

    TRUE
}
# }}}
# assert_ready_parameter {{{
assert_ready_parameter <- function (super, self, private) {
    if (is.null(private$m_log$parameter)) {
        abort("No parameter has been set. Please run '$apply_measure()' first.",
            "ga_no_parameter"
        )
    }

    TRUE
}
# }}}
# assert_ready_objective {{{
assert_ready_objective <- function (super, self, private) {
    if (is.null(private$m_log$objective)) {
        abort("No objecive has been set. Please run '$objective()' first.",
            "ga_no_objective"
        )
    }

    TRUE
}
# }}}
# transpose_param {{{
transpose_param <- function (param) {
    do.call(mapply, c(list(FUN = base::list, SIMPLIFY = FALSE), param))
}
# }}}
# flatten_list {{{
flatten_list <- function (lst, recursive = FALSE, use.names = FALSE) {
    unname(lapply(lst, unlist, recursive = recursive, use.names=  use.names))
}
# }}}
# setwith {{{
#' @export
setwith <- function (fun, ...) {
    checkmate::assert_class(fun, "ecr_operator")
    structure(list(fun = fun, args = list(...)),  class = "ecr_operator_setwith")
}
# }}}
# stopOnMaxTime {{{
#' Stopping on Maximum Time of evaluations
#'
#' @description
#' Stop the EA after a given cutoff time.
#'
#' @param max.time [\code{integer(1)}] Time limit in seconds. Default: `NULL`.
#' @return An `ecr_terminator` object
#' @export
stopOnMaxTime <- function(max.time = NULL) {
    if (!is.null(max.time)) {
        checkmate::assert_count(max.time, positive = TRUE)
    } else {
        max.time <- Inf
    }
    force(max.time)

    condition.fun <- function(log) {
        return(log$env$time.passed >= max.time)
    }

    ecr::makeTerminator(
        condition.fun,
        name = "TimeLimit",
        message = sprintf("Time limit reached: '%s' [seconds]", max.time)
    )
}
# }}}
# mutRandomChoice {{{
#' @title Random Choice Mutator
#'
#' @description
#' "Random Choice" mutation operator for discrete parameters: with probability
#'  `p` chooses one of the available categories at random (this *may* be
#'  the original value!)
#' @param ind `[character]` individual to mutate.
#' @param values `[list of character]` set of possible values for `ind` entries
#' to take. May be a list of length 1, in which case it is recycled.
#' @param p `[numeric(1)]` per-entry probability to perform mutation.
#' @return `[character]`
#' @importFrom checkmate assert_vector assert_list assert_number
#' @export
mutRandomChoice <- ecr::makeMutator(function(ind, values, p = 0.1) {
    assert_vector(ind, any.missing = FALSE)
    assert_list(values, any.missing = FALSE)

    if (!(length(values) %in% c(1, length(ind)))) {
        stop("length of values must be equal to length of ind")
    }

    assert_number(p, lower = 0, upper = 1)

    mapply(function(i, v) if (stats::runif(1) < p) sample(v, 1) else i, ind, values)

}, supported = "custom")
# }}}
# recPCrossover {{{
# borrowed from: https://github.com/compstat-lmu/mosmafs/blob/mosmafs-package/R/operators.R
#' General Uniform Crossover
#'
#' @description
#' Crossover recombination operator that crosses over each position iid with
#' prob. `p` and can also be used for non-binary operators.
#'
#' @param inds `[list of any]` list of two individuals to perform uniform crossover on
#' @param p `[numeric(1)]` per-entry probability to perform crossover.
#' @param ...  further arguments passed on to the method.
#' @return `[list of any]` The mutated individuals.
#' @export
recPCrossover <- ecr::makeRecombinator(function(inds, p = 0.1, ...) {
    assert_list(inds, len = 2, any.missing = FALSE)

    if (length(inds[[1]]) != length(inds[[2]])) {
        stop("Length of components of individuals must be the same.")
    }
    n <- length(inds[[1]])

    if (!(length(p) %in% c(n, 1))) {
        stop("Argument p must have same length as individual or 1.")
    }

    crossovers <- stats::runif(length(inds[[1]])) < p

    tmp <- inds[[1]][crossovers]

    inds[[1]][crossovers] <- inds[[2]][crossovers]
    inds[[2]][crossovers] <- tmp
    ecr::wrapChildren(inds[[1]], inds[[2]])

}, n.parents = 2, n.children = 2)
# }}}
# save_gen_log {{{
save_gen_log <- function (population, fitness, generation, objective, path, append = TRUE) {
    obj <- data.table(index = seq_along(objective$name), name = objective$name, dim = objective$dim)
    obj <- obj[, by = "index", {
        if (dim == 1L) {
            list(name = name)
        } else {
            list(name = paste(name, seq_len(dim), sep = "_"))
        }
    }]

    pop <- rbindlist(population)
    fit <- as.data.table(t(fitness))
    set(pop, NULL, names(fit), fit)
    data.table::setnames(pop, names(fit), obj$name)
    set(pop, NULL, "index_ind", seq_len(nrow(pop)))
    setcolorder(pop, "index_ind")
    set(pop, NULL, "gen", generation)
    data.table::setcolorder(pop, c("gen", "index_ind"))

    data.table::fwrite(pop, path, append = append)
}
# }}}
# extract_population {{{
extract_population <- function (population, objective, pareto = FALSE) {
    combine_results <- function (result) {
        pop <- rbindlist(result$population)
        if ("fitness" %in% names(result)) {
            fit <- as.data.table(t(result$fitness))
        } else {
            fit <- as.data.table(t(sapply(result$population, attr, "fitness", simplify = "array")))
        }
        set(pop, NULL, names(fit), fit)
        set(pop, NULL, "index_ind", seq_len(nrow(pop)))
        setcolorder(pop, names(fit))
    }

    results <- lapply(population, combine_results)

    for (i in seq_along(results)) set(results[[i]], NULL, "index_gen", i)
    results <- rbindlist(results)

    # get objective names
    nm <- unlist(mapply(name = objective$name, dim = objective$dim, SIMPLIFY = FALSE,
        FUN = function (name, dim) {
            if (dim == 1L) return(name)
            paste(name, seq_len(dim))
        }
    ))

    setnames(results, names(results)[seq_along(nm)], nm)
    setcolorder(results, c("index_gen", "index_ind"))
    setcolorder(results, setdiff(names(results), nm))
    results
}
# }}}
# get_nondominated {{{
get_nondominated <- function (population, objective) {
    d <- sum(objective$dim)
    objs <- t(as.matrix(population[, .SD, .SDcols = (ncol(population) - d + 1) : ncol(population)]))
    population[ecr::nondominated(objs)]
}
# }}}
