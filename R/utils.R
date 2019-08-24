#' @importFrom eplusr eplusr_option level_checks
# check_compactsch {{{
check_compactsch <- function (dt, idf) {
    # init a column indicator
    data.table::set(dt, NULL, "is_sch_value", FALSE)

    cls <- "Schedule:Compact"
    if (!idf$is_valid_class(cls)) return(dt)
    if (!cls %in% dt$class_name) return(dt)

    sch <- idf$to_table(class = cls)

    dt[sch, on = c("object_id" = "id", "field_index" = "index"), value := i.value]

    # check if original field is for `Through:`, `For:`, `Interpolate:`,
    # `Until:`
    dt[class_name == cls & !grepl("^through|for|interpolate|until", value, ignore.case = TRUE), is_sch_value := TRUE]

    data.table::set(dt, NULL, "value", NULL)
}
# }}}

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
