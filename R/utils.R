# tidy_names {{{
tidy_names <- function(dt) data.table::setnames(dt, tolower(gsub("[^[:alnum:]]", "_", names(dt))))
# }}}

# abort {{{
# reference: https://adv-r.hadley.nz/conditions.html#custom-conditions
abort <- function(message, class = NULL, call = NULL, ...) {
    ori <- getOption("warning.length")
    options(warning.length = 8170L)
    on.exit(options(warning.length = ori), add = TRUE)
    if (is.null(class)) {
        stop(errorCondition(message, ..., class = "epluspar_error", call = call))
    } else {
        stop(errorCondition(
            message,
            ...,
            class = unique(c(paste0("epluspar_error_", class), "epluspar_error")),
            call = call
        ))
    }
}
# }}}

# pad {{{
rpad <- function(x, char = " ", width = NULL) {
    if (!length(x)) {
        return(x)
    }
    w <- nchar(x, type = "width")
    if (is.null(width)) {
        width <- max(w)
    }
    paste0(x, strrep(char, pmax(width - w, 0)))
}

lpad <- function(x, char = " ", width = NULL) {
    if (!length(x)) {
        return(x)
    }
    w <- nchar(x, type = "width")
    if (is.null(width)) {
        width <- max(w)
    }
    paste0(strrep(char, pmax(width - w, 0)), x)
}
# }}}

# str_trunc {{{
str_trunc <- function(x, width = cli::console_width()) {
    w <- nchar(x, "width")
    tr <- w > (0.95 * width)
    x[tr] <- paste0(substring(x[tr], 1L, w - 5L), "...")
    x
}
# }}}

# get_priv_env{{{
get_priv_env <- function(x) {
    .subset2(.subset2(x, ".__enclos_env__"), "private")
}
# }}}

# as_function {{{
as_function <- function(x, env = parent.frame()) {
    if (is.function(x)) {
        return(x)
    }

    if (inherits(x, "formula")) {
        if (length(x) > 2) {
            stop("Can't convert a two-sided formula to a function")
        }

        env <- attr(x, ".Environment", exact = TRUE)
        rhs <- as.list(x)[[2L]]

        args <- list(... = substitute())
        args[["."]] <- quote(..1)
        args[[".x"]] <- quote(..1)
        args[[".y"]] <- quote(..2)
        args[[".z"]] <- quote(..3)
        args <- c(args, list(rhs))

        return(as.function(args, envir = env))
    }

    if (is.character(x) && length(x) == 1) {
        return(get(x, envir = env, mode = "function"))
    }

    stop("Can't convert an object of class ", class(x), " to a function.")
}
# }}}

# find_dependencies {{{
find_dependencies <- function(func, env = parent.frame(), ...) {
    deps <- globals::cleanup(globals::globalsOf(func, envir = env, ...))
    attr(deps, "where") <- NULL
    unclass(deps)
}
# }}}

# init var{{{
`.` <- `..` <- `.GRP` <- `.I` <- `.N` <- `.SD` <- `.BY` <- `.EACHI` <- J <- N <- NULL

utils::globalVariables(c(
    ".BY",
    "Date/Time",
    "datetime",
    "day_type",
    "field",
    "i.datetime",
    "i.day",
    "i.hour",
    "i.minute",
    "i.month",
    "i.value_id",
    "param_index",
    "param_name",
    "reporting_frequency",
    "type",
    "value_id",
    "value_num"
))
# }}}
