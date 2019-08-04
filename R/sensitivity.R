#' @include utils.R
#' @importFrom data.table data.table set setorder
#' @importFrom sensitivity morris tell
NULL

#' Conduct Sensitivity Analysis for A EnergyPlus Model
#'
#' @name sensitivity
#' @author Hongyuan Jia
#' @export
# Sensitivity {{{
Sensitivity <- R6::R6Class(classname = "SensitivityJob",
    inherit = eplusr:::Parametric, cloneable = FALSE,
    public = list(
        # PUBLIC FUNCTIONS {{{
        param = function (..., .rep = 12L, .grid_jump = 4L)
            sen_param(self, private, ..., .rep = .rep, .grid_jump = .grid_jump),

        evaluate = function (results)
            sen_evaluate(self, private, results),

        samples = function ()
            sen_samples(self, private)

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
sen_param <- function (self, private, ..., .rep = 12L, .grid_jump = 4L) {
    assert(is_count(.rep))
    assert(is_count(.grid_jump))

    l <- eplusr:::sep_value_dots(..., .empty = FALSE, .scalar = FALSE, .default = FALSE)

    # match Idf data
    # TODO: `match_set_idf_data` should be generalized to be a helper for all
    # `Idf$set()`'s friends, including `$add()`, `$load()`, `$update()`, and
    # etc.
    obj_val <- eplusr:::match_set_idf_data(
        ._get_private(private$m_idf)$idd_env(),
        ._get_private(private$m_idf)$idf_env(),
        l
    )

    vars <- obj_val$value

    # check param format
    data.table::set(vars, NULL, "is_chr", vapply(vars$new_value_num, anyNA, logical(1)))

    # add input rield
    vars[, value_rleid := .I]

    num <- vars[J(FALSE), on = "is_chr", nomatch = 0L]
    chr <- vars[J(TRUE), on = "is_chr", nomatch = 0L]

    # Number {{{
    # for numeric type, using position to determine min, max and levels
    # TODO: use element names instead of positions or use internal helper
    # `num_space()` and `chr_space()`. `patterns()` in data.table pkg can be a
    # good example of internal fun implementation
    if (!nrow(num)) {
        num_info <- data.table::data.table()
    } else {
        num_info <- num[, {
            id <- rleid[[1L]]
            range <- new_value_num[[1L]]
            if (length(range) != 3L) {
                abort("error_param_num_format",
                    paste0(
                        "For numeric field, a numeric vector of length 3 should be provided ",
                        "which defines parameter's minimum value, maximum value and number of total levels. ",
                        "Invalid input:\n", dot_string(l$dot[J(id), on = "rleid"])
                    )
                )
            }

            if (range[[1L]] >= range[[2L]]) {
                abort("error_param_num_format", paste0(
                    "For numeric field, minimum value (1st element) should be less than maximum value (2nd element). ",
                    "Invalid input:\n", dot_string(l$dot[J(id), on = "rleid"])
                ))
            }

            if (!is_count(range[[3L]])) {
                abort("error_param_num_format", paste0(
                    "For numeric field, number of total levels (3rd element) should be a positive integer. ",
                    "Invalid input:\n", dot_string(l$dot[J(id), on = "rleid"])
                ))
            }

            nm <- if (is.na(object_name)) "" else object_name
            object <- paste0(nm, "[", object_id, "]")
            field <- paste0(field_index, ":", field_name)
            list(path = paste(class_name, object, field, sep = "-->"),
                min = range[[1L]], max = range[[2L]], levels = range[[3L]]
            )
        }, by = "value_rleid"]
    }
    # }}}
    # Character {{{
    # TODO: convert to dummy variables
    if (!nrow(chr)) {
        chr_info <- data.table::data.table()
    } else {
        stop("Character fields are currently not supported.")
    }
    # }}}

    # use sensitivity::morris to generate input
    mo <- sensitivity::morris(model = NULL, factors = num_info$path, r = .rep,
        design = list(type = "oat", levels = num_info$levels, grid.jump = .grid_jump),
        binf = num_info$min, bsup = num_info$max, scale = FALSE
    )
    private$m_morris <- mo

    input <- data.table::as.data.table(mo$X)[, case := .I]
    data.table::setnames(input, as.character(c(num_info$value_rleid, "case")))
    input <- data.table::melt.data.table(input, id.vars = "case",
        variable.name = "value_rleid", value.name = "value",
        variable.factor = FALSE
    )[, `:=`(value_rleid = as.integer(value_rleid), value = as.list(value))]

    # format input for `Idf$update()`
    input <- num[, list(value_rleid, id = object_id, name = object_name,
        class = class_name, index = field_index, field = field_name)][
        input, on = "value_rleid"][, value_rleid := NULL]

    data.table::setorder(input, "case")
    data.table::setcolorder(input, "case")

    # store
    private$m_log$sample <- input
    dt <- split(input, by = "case", keep.by = FALSE)

    # create models
    private$m_param <- lapply(dt, function (upd) {
        idf <- private$m_idf$clone()
        idf$update(upd)
        idf
    })

    self
}
# }}}

# sen_samples {{{
sen_samples <- function (self, private) {
    private$m_log$sample
}
# }}}

# sen_evaluate {{{
sen_evaluate <- function (self, private, results) {
    assert(is.numeric(results), no_na(results))

    private$m_morris <- sensitivity::tell(private$m_morris, results)

    mu <- apply(private$m_morris$ee, 2, mean)
    mu.star <- apply(private$m_morris$ee, 2, function(x) mean(abs(x)))
    sigma <- apply(private$m_morris$ee, 2, sd)

    data <- data.table::data.table(
        index = seq_along(mu), name = private$m_morris$factors,
        mu = mu, mu.star = mu.star, sigma = sigma
    )

    attr(private$m_morris, "data") <- data

    private$m_morris
}
# }}}
