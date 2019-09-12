#' The 'epScan' package.
#'
#' @description Conduct sensitivity analysis and Bayesian calibration of
#' EnergyPlus models.
#'
#' @docType package
#' @name epScan-package
#' @aliases epScan
#' @useDynLib epScan, .registration = TRUE
#' @import methods
#' @import Rcpp
#' @import eplusr
#' @importFrom rstan sampling
#'
#' @references
#' A. Chong and K. Menberg, "Guidelines for the Bayesian calibration of building
#' energy models", Energy and Buildings, vol. 174, pp. 527–547. DOI:
#' 10.1016/j.enbuild.2018.06.028
NULL

# init var {{{
`.GRP` <- `.I` <- `.N` <- `.SD` <- `J` <- NULL

utils::globalVariables(c(
    "case", "class_id", "class_name", "day", "dot", "dot_nm", "field_index",
    "field_name", "full_name", "grouped", "hour", "J", "i.index", "i.index_par",
    "i.max", "i.min", "i.report_type", "i.value", "i.variable", "id", "idx",
    "index", "index_par", "input_object_rleid", "input_rleid", "is_sch_value",
    "key_value", "key_value_upper", "minute", "mixed", "month", "name",
    "new_value_num", "nm", "object_id", "object_name", "out", "report_type",
    "rleid", "simulation_days", "string", "value", "value_rleid", "variable",
    "variable_match", "variable_name"
))
# }}}
