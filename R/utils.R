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
    x[tr] <- paste0(substring(x[tr], 1L, to = w - 5L), "...")
    x
}
# }}}

# get_priv_env{{{
get_priv_env <- function (x) {
    .subset2(.subset2(x, ".__enclos_env__"), "private")
}
# }}}
