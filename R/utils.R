# tidy_names {{{
tidy_names <- function (dt) data.table::setnames(dt, tolower(gsub("[^[:alnum:]]", "_", names(dt))))
# }}}

# abort {{{
# reference: https://adv-r.hadley.nz/conditions.html#custom-conditions
abort <- function (message, class = NULL, call = NULL, ...) {
    ori <- getOption("warning.length")
    options(warning.length = 8170L)
    on.exit(options(warning.length = ori), add = TRUE)
    if (is.null(class)) {
        stop(errorCondition(message, ..., class = "epluspar_error", call = call))
    } else {
        stop(errorCondition(message, ..., class = unique(c(paste0("epluspar_error_", class), "epluspar_error")), call = call))
    }
}
# }}}

# pad {{{
rpad <- function(x, char = " ", width = NULL) {
    if (!length(x)) return(x)
    w <- nchar(x, type = "width")
    if (is.null(width)) width <- max(w)
    paste0(x, strrep(char, pmax(width - w, 0)))
}

lpad <- function(x, char = " ", width = NULL) {
    if (!length(x)) return(x)
    w <- nchar(x, type = "width")
    if (is.null(width)) width <- max(w)
    paste0(strrep(char, pmax(width - w, 0)), x)
}
# }}}

# str_trunc {{{
str_trunc <- function (x, width = cli::console_width()) {
    w <- nchar(x, "width")
    tr <- w > (0.95 * width)
    x[tr] <- paste0(substring(x[tr], 1L, w - 5L), "...")
    x
}
# }}}

# get_priv_env{{{
get_priv_env <- function (x) {
    .subset2(.subset2(x, ".__enclos_env__"), "private")
}
# }}}

# ecr {{{
doTerminate <- get("doTerminate", envir = asNamespace("ecr"))
makeECRResult <- get("makeECRResult", envir = asNamespace("ecr"))
makeFitnessMatrix <- function (fitness, control) {
    setattr(fitness, "class", c("ecr_fitness_matrix", class(fitness)))
    setattr(fitness, "minimize", control$task$minimize)
    fitness
}
# }}}

# init var{{{
`.` <- `..` <- `.GRP` <- `.I` <- `.N` <- `.SD` <- `.BY` <- `.EACHI` <- J <- N <- NULL

utils::globalVariables(c(
     ".BY", "i.value_id", "param_index", "param_name", "type", "value_id",
     "value_num", "Date/Time", "datetime", "day_type", "i.datetime", "i.day",
     "i.hour", "i.minute", "i.month", "reporting_frequency"
))
# }}}
