# Funs below are temporarily borrowed from eplusr, should find a better way to
# avoid this

# ASSERTIONS
assert <- get("assert", envir = asNamespace("eplusr"), inherits = FALSE)
is_number <- get("is_number", envir = asNamespace("eplusr"), inherits = FALSE)
is_count <- get("is_count", envir = asNamespace("eplusr"), inherits = FALSE)
no_na <- get("no_na", envir = asNamespace("eplusr"), inherits = FALSE)

# CONDITIONS
abort <- get("abort", envir = asNamespace("eplusr"), inherits = FALSE)
warn <- get("warn", envir = asNamespace("eplusr"), inherits = FALSE)

# DOTS
dot_string <- get("dot_string", envir = asNamespace("eplusr"), inherits = FALSE)

# `._get_self`{{{
`._get_self` <- function (x) {
    .subset2(.subset2(x, ".__enclos_env__"), "self")
}
# }}}

# `._get_private`{{{
`._get_private` <- function (x) {
    .subset2(.subset2(x, ".__enclos_env__"), "private")
}
# }}}
