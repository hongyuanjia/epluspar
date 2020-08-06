#' @importFrom lhs randomLHS
#' @importFrom data.table copy rbindlist setcolorder as.data.table dcast.data.table
#' @importFrom data.table setnames setorderv melt.data.table rleidv setattr
NULL

#' Conduct Bayesian Calibration on An EnergyPlus Model
#'
#' `BayesCalibJob` class provides a prototype of conducting Bayesian calibration
#' of EnergyPlus model.
#'
#' The basic workflow is basically:
#'
#' 1. Setting input and output variables using
#'    \href{../../epluspar/html/BayesCalibJob.html#method-input}{\code{$input()}}
#'    and
#'    \href{../../epluspar/html/BayesCalibJob.html#method-output}{\code{$output()}},
#'    respectively.
#'    Input variables should be variables listed in RDD while output variables
#'    should be variables listed in RDD and MDD.
#' 1. Adding parameters to calibrate using
#'    \href{../../epluspar/html/BayesCalibJob.html#method-param}{\code{$param()}}
#'    or
#'    \href{../../epluspar/html/BayesCalibJob.html#method-apply_measure}{\code{$apply_measure()}}.
#' 1. Check parameter sampled values and generated parametric models using
#'    \href{../../epluspar/html/BayesCalibJob.html#method-samples}{\code{$samples()}}
#'    and
#'    \href{../../epluspar/html/BayesCalibJob.html#method-models}{\code{$models()}},
#'    respectively.
#' 1. Run EnergyPlus simulations in parallel using
#'    \href{../../epluspar/html/BayesCalibJob.html#method-eplus_run}{\code{$eplus_run()}},
#' 1. Gather simulated data of input and output parameters using
#'    \href{../../epluspar/html/BayesCalibJob.html#method-data_sim}{\code{$data_sim()}}.
#' 1. Specify field measured data of input and output parameters using
#'    \href{../../epluspar/html/BayesCalibJob.html#method-data_field}{\code{$data_field()}}.
#' 1. Specify input data for Stan for Bayesian calibration using
#'    \href{../../epluspar/html/BayesCalibJob.html#method-data_bc}{\code{$data_bc()}}.
#' 1. Run bayesian calibration using stan using
#'    \href{../../epluspar/html/BayesCalibJob.html#method-stan_run}{\code{$stan_run()}}.
#'
#' @docType class
#' @name BayesCalibJob
#' @author Hongyuan Jia, Adrian Chong
#' @references
#' A. Chong and K. Menberg, "Guidelines for the Bayesian calibration of building
#' energy models", Energy and Buildings, vol. 174, pp. 527–547. DOI:
#' 10.1016/j.enbuild.2018.06.028
NULL

#' @export
# BayesCalibJob {{{
BayesCalibJob <- R6::R6Class(classname = "BayesCalibJob",
    inherit = eplusr::ParametricJob, cloneable = FALSE, lock_objects = FALSE,

    public = list(
        # INITIALIZE {{{
        #' @description
        #' Create a `BayesCalibJob` object
        #'
        #' @details
        #' When initialization, the objects of classes related in output variable
        #' reporting in the original [eplusr::Idf] will be deleted, in order to
        #' make sure all input and output variable specifications can be
        #' achieved using `Output:Variable` and `Output:Meter`. Classes to be
        #' deleted include:
        #'
        #' * `Output:Variable`
        #' * `Output:Meter`
        #' * `Output:Meter:MeterFileOnly`
        #' * `Output:Meter:Cumulative`
        #' * `Output:Meter:Cumulative:MeterFileOnly`
        #' * `Meter:Custom`
        #' * `Meter:CustomDecrement`
        #' * `Output:EnvironmentalImpactFactors`
        #'
        #' @param idf A path to an local EnergyPlus IDF file or an [eplusr::Idf] object.
        #' @param epw A path to an local EnergyPlus EPW file or an [eplusr::Epw] object.
        #'
        #' @return An `BayesCalibJob` object.
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
        #'     BayesCalibJob$new(idf_path, epw_path)
        #'
        #'     # create from an Idf and an Epw object
        #'     bc <- BayesCalibJob$new(eplusr::read_idf(idf_path), eplusr::read_epw(epw_path))
        #' }
        #' }
        #'
        initialize = function (idf, epw) {
            # do not allow NULL for epw
            if (is.null(epw)) abort("'epw' must be specified.")

            eplusr:::with_silent(super$initialize(idf, epw))

            # remove all output variables and meters
            private$m_seed <- bc_remove_output_class(super, self, private, all = FALSE, clone = TRUE)

            # init some logging variables
            private$m_log$run_ddy <- FALSE
            private$m_log$has_param <- FALSE
        },
        # }}}

        # PUBLIC FUNCTIONS {{{
        # read_rdd {{{
        #' @description
        #' Read EnergyPlus Report Data Dictionary (RDD) file
        #'
        #' @details
        #' `$read_rdd()` silently runs EnergyPlus using input seed model with
        #' design-day-only mode to create the `.rdd` file and returns the
        #' corresponding [RddFile][eplusr::read_rdd()] object.
        #'
        #' The `RddFile` object is stored internally and will be directly
        #' returned whenever you call `$read_rdd()` again. You can force to
        #' rerun the design-day-only simulation again to update the contents by
        #' setting `update` to `TRUE`.
        #'
        #' `$read_rdd()` and
        #' \href{../../epluspar/html/BayesCalibJob.html#method-read_mdd}{\code{$read_mdd()}}
        #' are useful when adding input and output parameters using
        #' \href{../../epluspar/html/BayesCalibJob.html#method-input}{\code{$input()}}
        #' and
        #' \href{../../epluspar/html/BayesCalibJob.html#method-output}{\code{$output()}},
        #' respectively.
        #'
        #' @param update Whether to run the design-day-only simulation and parse
        #'        `.rdd` and `.mdd` file again. Default: `FALSE`.
        #'
        #' @return An [RddFile][eplusr::read_rdd()] object.
        #'
        #' @examples
        #' \dontrun{
        #' bc$read_rdd()
        #'
        #' # force to rerun
        #' bc$read_rdd(update = TRUE)
        #' }
        #'
        read_rdd = function (update = FALSE)
            bc_read_rdd(super, self, private, update),
        # }}}

        # read_mdd {{{
        #' @description
        #' Read EnergyPlus Meter Data Dictionary (MDD) file
        #'
        #' @details
        #' `$read_mdd()` silently runs EnergyPlus using input seed model with
        #' design-day-only mode to create the `.mdd` file and returns the
        #' corresponding [MddFile][eplusr::read_mdd()] object.
        #'
        #' The `MddFile` object is stored internally and will be directly
        #' returned whenever you call `$read_mdd()` again. You can force to
        #' rerun the design-day-only simulation again to update the contents by
        #' setting `update` to `TRUE`.
        #'
        #' \href{../../epluspar/html/BayesCalibJob.html#method-read_rdd}{\code{$read_rdd()}}
        #' and
        #' `read_mdd()`
        #' are useful when adding input and output parameters using
        #' \href{../../epluspar/html/BayesCalibJob.html#method-input}{\code{$input()}}
        #' and
        #' \href{../../epluspar/html/BayesCalibJob.html#method-output}{\code{$output()}},
        #' respectively.
        #'
        #' @param update Whether to run the design-day-only simulation and parse
        #'        `.rdd` and `.mdd` file again. Default: `FALSE`.
        #'
        #' @return An [MddFile][eplusr::read_mdd()] object.
        #'
        #' @examples
        #' \dontrun{
        #' bc$read_mdd()
        #'
        #' # force to rerun
        #' bc$read_mdd(update = TRUE)
        #' }
        #'
        read_mdd = function (update = FALSE)
            bc_read_mdd(super, self, private, update),
        # }}}

        # input {{{
        #' @description
        #' Set input parameters
        #'
        #' @details
        #' `$input()` takes input parameter definitions in a similar pattern as
        #' you set output variables in `Output:Variable` and `Output:Meter`
        #' class and returns a [data.table::data.table()] containing the
        #' information of input parameters. Only variables in
        #' [RDD][eplusr::read_rdd()] are
        #' allowed.The returned [data.table::data.table()] has 5 columns:
        #'
        #' * `index`: Indices of input or output parameters.
        #' * `class`: The class that parameters belong to. Will be either
        #'   `Output:Variable` or `Output:Meter`.
        #' * `key_value`: Key value name for variables.
        #' * `variable_name`: Variable names listed in RDD or MDD.
        #' * `reporting_frequency`: Variable reporting frequency.
        #'
        #' If calling without any argument, the existing input parameters are
        #' directly returned, e.g. `bc$input()`.
        #'
        #' You can remove all existing input parameters by setting `append` to
        #' `NULL`, e.g. `bc$input(append = NULL)`.
        #'
        #' `key_value` accepts 3 different formats:
        #'
        #' * A character vector.
        #' * An [RddFile][eplusr::read_rdd()] object. It can be retrieved using
        #'   \href{../../epluspar/html/BayesCalibJob.html#method-read_rdd}{\code{$read_rdd()}}.
        #'   In this case, `name` argument will be ignored, as its values are
        #'   directly taken from variable names in input
        #'   [RddFile][eplusr::read_rdd()] object. For example:
        #'   ```
        #'   bc$input(bc$read_rdd()[1:5])
        #'   ```
        #' * A [data.frame()] with valid format for adding `Output:Variable` and
        #'  `Output:Meter` objects using [eplusr::Idf$load()][eplusr::Idf]. In
        #'  this case, `name` argument will be ignored. For example:
        #'  ```
        #'  bc$input(eplusr::rdd_to_load(bc$read_rdd()[1:5]))
        #'  ```
        #'
        #' @param key_value Key value name for variables. If not specified,
        #'        `"*"` are used for all variables. `key_value` can also be an
        #'        `RddFile`, `MddFile` or a [data.frame()]. Please see
        #'        description above.
        #' @param name Variable names listed in RDD or MDD.
        #' @param reporting_frequency Variable reporting frequency for **all**
        #'        variables.  If `NULL`, `"Timestep"` are used for all
        #'        variables. All possible values: `"Detailed"`, `"Timestep"`,
        #'        `"Hourly"`, `"Daily"`, `"Monthly"`, `"RunPeriod"`,
        #'        `"Environment"`, and `"Annual"`. Default: `NULL`.
        #' @param append Whether to append input variables at the end of
        #'        existing ones. A special value `NULL` can be given to remove
        #'        all existing parameters. Default: `FALSE`.
        #'
        #' @return A [data.table::data.table()].
        #'
        #' @examples
        #' \dontrun{
        #' # explicitly specify input variable name
        #' bc$input(name = "fan air mass flow rate", reporting_frequency = "hourly")
        #'
        #' # use an RddFile
        #' bc$input(bc$read_rdd()[1:5])
        #'
        #' # use a data.frame
        #' bc$input(eplusr::rdd_to_load(bc$read_rdd()[1:5]))
        #'
        #' # get existing input
        #' bc$input()
        #' }
        #'
        input = function (key_value = NULL, name = NULL, reporting_frequency = NULL, append = FALSE)
            bc_input(super, self, private, key_value, name, reporting_frequency, append),
        # }}}

        # output {{{
        #' @description
        #' Set output parameters
        #'
        #' @details
        #' `$output()` takes output parameter definitions in a
        #' similar pattern as you set output variables in `Output:Variable` and
        #' `Output:Meter` class and returns a [data.table::data.table()]
        #' containing the information of output parameters. Unlike
        #' \href{../../epluspar/html/BayesCalibJob.html#method-input}{\code{$input()}}
        #' both variables in [RDD][eplusr::read_rdd()] and
        #' [MDD][eplusr::read_mdd()] are allowd. The returned data.table has 5
        #' columns:
        #'
        #' * `index`: Indices of input or output parameters.
        #' * `class`: The class that parameters belong to. Will be either
        #'   `Output:Variable` or `Output:Meter`.
        #' * `key_value`: Key value name for variables.
        #' * `variable_name`: Variable names listed in RDD or MDD.
        #' * `reporting_frequency`: Variable reporting frequency.
        #'
        #' If calling without any argument, the existing output parameters are
        #' directly returned, e.g. `bc$output()`.
        #'
        #' You can remove all existing parameter by setting `append` to `NULL`,
        #' e.g. `bc$output(append = NULL)`.
        #'
        #' `key_value` accepts 3 different formats:
        #'
        #' * A character vector.
        #' * An [RddFile][eplusr::read_rdd()] object or an
        #'   [MddFile][eplusr::read_mdd()] object. They can be retrieved using
        #'   \href{../../epluspar/html/BayesCalibJob.html#method-read_rdd}{\code{$read_rdd()}}
        #'   and
        #'   \href{../../epluspar/html/BayesCalibJob.html#method-read_mdd}{\code{$read_mdd()}},
        #'   respectively.  In this case, `name` argument will be ignored, as
        #'   its values are directly taken from variable names in input
        #'   [RddFile][eplusr::read_rdd()] object or
        #'   [MddFile][eplusr::read_mdd()] object. For example:
        #'   ```
        #'   bc$output(bc$read_mdd()[1:5])
        #'   ```
        #' * A [data.frame()] with valid format for adding `Output:Variable` and
        #'  `Output:Meter` objects using [Idf$load()][eplusr::Idf]. In this
        #'  case, `name` argument will be ignored. For example:
        #'  ```
        #'  bc$output(eplusr::mdd_to_load(bc$read_mdd()[1:5]))
        #'  ```
        #'
        #' @param key_value Key value name for variables. If not specified,
        #'        `"*"` are used for all variables. `key_value` can also be an
        #'        `RddFile`, `MddFile` or a [data.frame()]. Please see
        #'        description above.
        #' @param name Variable names listed in RDD or MDD.
        #' @param reporting_frequency Variable reporting frequency for **all**
        #'        variables.  If `NULL`, `"Timestep"` are used for all
        #'        variables. All possible values: `"Detailed"`, `"Timestep"`,
        #'        `"Hourly"`, `"Daily"`, `"Monthly"`, `"RunPeriod"`,
        #'        `"Environment"`, and `"Annual"`. Default: `NULL`.
        #' @param append Whether to append input variables at the end of
        #'        existing ones. A special value `NULL` can be given to remove
        #'        all existing parameters. Default: `FALSE`.
        #'
        #' @return A [data.table::data.table()].
        #'
        #' @examples
        #' \dontrun{
        #' # explicitly specify input variable name
        #' bc$output(name = "fan electric power", reporting_frequency = "hourly")
        #'
        #' # use an RddFile or MddFile
        #' bc$output(bc$read_rdd()[6:10])
        #' bc$output(bc$read_mdd()[6:10])
        #'
        #' # use a data.frame
        #' bc$output(eplusr::rdd_to_load(bc$read_mdd()[6:10]))
        #'
        #' # get existing input
        #' bc$output()
        #' }
        #'
        output = function (key_value = NULL, name = NULL, reporting_frequency = NULL, append = FALSE)
            bc_output(super, self, private, key_value, name, reporting_frequency, append),
        # }}}

        # param {{{
        #' @description
        #' Set parameters for Bayesian calibration
        #'
        #' @details
        #' `$param()` takes parameter definitions in list format, which is
        #' similar to `$set()` in [eplusr::Idf] class except that each field is
        #' not assigned with a single value, but a numeric vector of length 2,
        #' indicating the minimum and maximum value of each
        #' parameter.
        #'
        #' Similar like the way of modifying object field values in
        #' [eplusr::Idf$set()][eplusr::Idf], there are 3 different ways of
        #' defining a parameter in epluspar:
        #'
        #' * `object = list(field = c(min, max))`: Where `object` is a
        #'   valid object ID or name. Note object ID should be denoted with two
        #'   periods `..`, e.g. `..10` indicates the object with ID `10`, It
        #'   will set that specific field in that object as one parameter.
        #' * `.(object, object) := list(field = c(min, max))`: Simimar like
        #'   above, but note the use of `.()` in the left hand side. You can put
        #'   multiple object ID or names in `.()`. It will set the field of all
        #'   specified objects as one parameter.
        #' * `class := list(field = c(min, max, levels))`: Note the use of `:=`
        #'   instead of `=`. The main difference is that, unlike `=`, the left
        #'   hand side of `:=` should be a valid class name in current
        #'   [eplusr::Idf]. It will set that field of all objects in specified
        #'   class as one parameter.
        #'
        #' For example, the code block below defines 4 calibration parameters:
        #'
        #' * Field `Fan Total Efficiency` in object named `Supply Fan 1` in
        #'   class `Fan:VariableVolume` class, with minimum and maximum being
        #'   0.1 and 1.0, respectively.
        #' * Field `Thickness` in all objects in class `Material`, with minimum
        #'   and maximum being 0.01 and 1.0, respectively.
        #' * Field `Conductivity` in all objects in class `Material`, with
        #'   minimum and maximum being 0.1 and 0.6, respectively.
        #' * Field `Watts per Zone Floor Area` in objects `Light1` and `Light2`
        #'   in class `Lights`, with minimum and maximum being 10 and 30,
        #'   respectively.
        #'
        #' ```
        #' bc$param(
        #'     `Supply Fan 1` = list(Fan_Total_Efficiency = c(min = 0.1, max = 1.0)),
        #'     Material := list(Thickness = c(0.01, 1), Conductivity = c(0.1, 0.6)),
        #'    .("Light1", "Light2") := list(Watts_per_Zone_Floor_Area = c(10, 30))
        #' )
        #' ```
        #'
        #' All models created using `$param()` will be named in the same
        #' pattern, i.e. `Case_ParameterName(ParamterValue)...`. Note that only
        #' paramter names will be abbreviated using [abbreviate()] with
        #' `minlength` being `5L` and `use.classes` being `TRUE`. If samples
        #' contain duplications, [make.unique()] will be called to make sure
        #' every model has a unique name.
        #'
        #' @param ... Lists of paramter definitions. Please see above on the
        #'        syntax.
        #' @param .names A character vector of the parameter names. If `NULL`,
        #'        the parameter will be named in format `t + number`, where
        #'        `number` is the index of parameter. Default: `NULL`.
        #' @param .num_sim An positive integer specifying the number of
        #'        simulations to run for each combination of calibration
        #'        parameter value. Default: `30L`.
        #'
        #' @return The modified `BayesCalibJob` object itself.
        #'
        #' @examples
        #' \dontrun{
        #' bc$param(
        #'     `Supply Fan 1` = list(Fan_Total_Efficiency = c(min = 0.1, max = 1.0)),
        #'     Material := list(Thickness = c(0.01, 1), Conductivity = c(0.1, 0.6)),
        #'    .("Light1", "Light2") := list(Watts_per_Zone_Floor_Area = c(10, 30))
        #' )
        #' }
        #'
        param = function (..., .names = NULL, .num_sim = 30L)
            bc_param(super, self, private, ..., .names = .names, .num_sim = .num_sim),
        # }}}

        # apply_measure {{{
        #' @description
        #' Set parameters for Bayesian calibration using function
        #'
        #' @details
        #' `$apply_measure()` works in a similar way as the `$apply_measure` in
        #' [eplusr::ParametricJob] class, with only exception that each argument
        #' supplied in `...` should be a numeric vector of length 2, indicating
        #' the minimum value and maximum value of each parameter.
        #'
        #' Basically `$apply_measure()` allows to apply a measure to an
        #' [eplusr::Idf]. A measure here is just a function that takes an
        #' [eplusr::Idf] object and other arguments as input, and returns a
        #' modified [eplusr::Idf] object as output.
        #'
        #' The names of function parameter will be used as the names of
        #' calibration parameter. For example, the equivalent version of
        #' specifying parameters described in
        #' \href{../../epluspar/html/SensitivityJob.html#method-param}{\code{$param()}}
        #' using `$apply_measure()` can be:
        #'
        #' ```
        #' # set calibration parameters using $apply_measure()
        #' # (a) first define a "measure"
        #' measure <- function (idf, efficiency, thickness, conducitivy, lpd) {
        #'     idf$set(
        #'         `Supply Fan 1` = list(Fan_Total_Efficiency = efficiency),
        #'         Material := list(Thickness = thickness, Conductivity = conducivity)
        #'         .("Light1", "Light2") := list(Watts_per_Zone_Floor_Area = lpd)
        #'     )
        #'
        #'     idf
        #' }
        #'
        #' # (b) then apply that measure with parameter space definitions as
        #' # function arguments
        #' bc$apply_measure(measure,
        #'     efficiency = c(min = 0.1, max = 1.0),
        #'     thickness = c(0.01, 1), conductivity = c(0.1, 0.6),
        #'     lpd = c(10, 30)
        #' )
        #' ```
        #'
        #' All models created using `$apply_measure()` will be named in the same
        #' pattern, i.e. `Case_ParameterName(ParamterValue)...`. Note that only
        #' paramter names will be abbreviated using [abbreviate()] with
        #' `minlength` being `5L` and `use.classes` being `TRUE`. If samples
        #' contain duplications, [make.unique()] will be called to make sure
        #' every model has a unique name.
        #'
        #' @param measure A function that takes an [eplusr::Idf] and other
        #'        arguments as input and returns an [eplusr::Idf] object as
        #'        output.
        #' @param ... Arguments **except first `Idf` argument** that are passed
        #'        to that `measure`.
        #' @param .num_sim An positive integer specifying the number of
        #'        simulations to run taking into account of all parameter
        #'        combinations. Default: `30L`.
        #'
        #' @return The modified `BayesCalibJob` object itself.
        #'
        #' @examples
        #' \dontrun{
        #' # set calibration parameters using $apply_measure()
        #' # (a) first define a "measure"
        #' measure <- function (idf, efficiency, thickness, conducitivy, lpd) {
        #'     idf$set(
        #'         `Supply Fan 1` = list(Fan_Total_Efficiency = efficiency),
        #'         Material := list(Thickness = thickness, Conductivity = conducivity)
        #'         .("Light1", "Light2") := list(Watts_per_Zone_Floor_Area = lpd)
        #'     )
        #'
        #'     idf
        #' }
        #'
        #' # (b) then apply that measure with parameter space definitions as
        #' # function arguments
        #' bc$apply_measure(measure,
        #'     efficiency = c(min = 0.1, max = 1.0),
        #'     thickness = c(0.01, 1), conductivity = c(0.1, 0.6),
        #'     lpd = c(10, 30)
        #' )
        #' }
        #'
        apply_measure = function (measure, ..., .num_sim = 30L)
            bc_apply_measure(super, self, private, measure, ..., .num_sim = .num_sim),
        # }}}

        # samples {{{
        #' @description
        #' Get sampled parameter values
        #'
        #' @details
        #' `$samples()` returns a [data.table::data.table()] which contains the
        #' sampled value for each parameter using [Random Latin Hypercube
        #' Sampling][lhs::randomLHS] method. The returned
        #' [data.table::data.table()] has `1 + n` columns, where `n` is the
        #' parameter number, and `1` indicates an extra column named `case`
        #' giving the index of each sample.
        #'
        #' Note that if `$samples()` is called before input and output
        #' parameters being set using
        #' \href{../../epluspar/html/BayesCalibJob.html#method-input}{\code{$input()}},
        #' and
        #' \href{../../epluspar/html/BayesCalibJob.html#method-output}{\code{$output()}},
        #' only the sampling will be performed and no parametric models will be
        #' created.  This is because information of input and output parameters
        #' are needed in order to make sure that corresponding variables will be
        #' reported during simulations. In this case, you can use
        #' \href{../../epluspar/html/BayesCalibJob.html#method-models}{\code{$models()}},
        #' to create those models.
        #'
        #' @return A [data.table::data.table()].
        #'
        #' @examples
        #' \dontrun{
        #' bc$samples()
        #' }
        #'
        samples = function ()
            bc_samples(super, self, private),
        # }}}

        # models {{{
        #' @description
        #' Get parametric models
        #'
        #' @details
        #' `$models()` returns a list of parametric [eplusr::Idf] objects
        #' created using calibration parameter values genereated using Random
        #' Latin Hypercube Sampling. As stated above, parametric models can only
        #' be created after input, output and calibration parameters have all be
        #' set using
        #' \href{../../epluspar/html/BayesCalibJob.html#method-input}{\code{$input()}},
        #' \href{../../epluspar/html/BayesCalibJob.html#method-output}{\code{$output()}}
        #' and
        #' \href{../../epluspar/html/BayesCalibJob.html#method-param}{\code{$param()}}
        #' (or
        #' \href{../../epluspar/html/BayesCalibJob.html#method-apply_measure}{\code{$apply_measure()}}
        #' ), respectively.
        #'
        #' All models will be named in the same pattern, i.e.
        #' `Case_ParameterName(ParamterValue)...`. Note that paramter names will
        #' be abbreviated using [abbreviate()] with `minlength` being `5L` and
        #' `use.classes` being `TRUE`.
        #'
        #' @return A named list of [eplusr::Idf] objects.
        #'
        #' @examples
        #' \dontrun{
        #' bc$models()
        #' }
        #'
        models = function ()
            bc_models(super, self, private),
        # }}}

        # data_sim {{{
        #' @description
        #' Collect simulation data
        #'
        #' @details
        #' `$data_sim()` returns a list of 2 [data.table::data.table()] which
        #' contains the simulated data of input and output parameters. These
        #' data will be stored internally and used during Bayesian calibration
        #' using Stan.
        #'
        #' The `resolution` parameter can be used to specify the time resolution
        #' of returned data. Note that input time resolution cannot be smaller
        #' than the reporting frequency, otherwise an error will be issued.
        #'
        #' The parameter is named in the same way as standard EnergyPlus csv
        #' output file, i.e. `KeyValue:VariableName [Unit](Frequency)`.
        #'
        #' By default, `$data_sim()` returns minimal columns, i.e. the
        #' `Date/Time` column together with all input and output parameters are
        #' returned.
        #'
        #' You can retrieve extra columns by setting `all` to `TRUE`.  Those
        #' column include:
        #'
        #' * `case`: Integer type. Indices of parametric simulations.
        #' * `environment_period_index`: Integer type. The indice of environment.
        #' * `environment_name`: Character type. A text string identifying the
        #'   simulation environment.
        #' * `simulation_days`: Integer type. Day of simulation.
        #' * `datetime`: DateTime type. The date time of simulation result. Note
        #'   that the year valueas are automatically calculated to meets the
        #'   start day of week restriction for each simulation environment.
        #' * `month`: Integer type. The month of reported date time.
        #' * `day`: Integer type. The day of month of reported date time.
        #' * `hour`: Integer type. The hour of reported date time.
        #' * `minute`: Integer type. The minute of reported date time.
        #' * `day_type`: Character type. The type of day, e.g. `Monday`,
        #'   `Tuesday` and etc. Note that `day_type` will always be `NA` if
        #'   `resolution` is specified.
        #'
        #' @param resolution A character string specifying a time unit or a
        #'        multiple of a unit to change the time resolution of returned
        #'        simulation data. Valid base units are `min`, `hour`, `day`,
        #'        `week`, `month`, and `year`.  Example: `10 mins`, `2 hours`,
        #'        `1 day`. If `NULL`, the variable reporting frequency is used.
        #'        Default: `NULL`.
        #' @param exclude_ddy Whether to exclude design day data. Default:
        #'        `TRUE`. Default: `FALSE`.
        #' @param all If `TRUE`, extra columns are also included in the returned
        #'        [data.table::data.table()] describing the simulation case and
        #'        datetime components. Default: `FALSE`.
        #'
        #' @return A list of 2 [data.table::data.table()].
        #'
        #' @examples
        #' \dontrun{
        #' bc$data_sim()
        #' }
        #'
        data_sim = function (resolution = NULL, exclude_ddy = TRUE, all = FALSE)
            bc_data_sim(super, self, private, resolution, exclude_ddy, all),
        # }}}

        # data_field {{{
        #' @description
        #' Specify field measured data
        #'
        #' @details
        #' `$data_field()` takes a [data.frame()] of measured value of output
        #' parameters and returns a list of [data.table::data.table()]s which
        #' contains the measured value of input and output parameters, and newly
        #' measured value of input if applicable.
        #'
        #' The specified `output` [data.frame()] is validated using criteria
        #' below:
        #'
        #' * The column number should be the same as the number of output
        #'   specified in
        #'   \href{../../epluspar/html/BayesCalibJob.html#method-output}{\code{$output()}}.
        #' * The row number should be the same as the number of simulated values
        #'   for each case extracted using
        #'   \href{../../epluspar/html/BayesCalibJob.html#method-data_sim}{\code{$data_sim()}}.
        #'
        #' For input parameters, the values of simulation data for the first
        #' case are directly used as the measured values.
        #'
        #' Parameter `new_input` can be used to give a [data.frame()] of newly
        #' measured value of input parameters. The column number of input
        #' [data.frame()] should be the same as the number of input parameters
        #' specified in
        #' \href{../../epluspar/html/BayesCalibJob.html#method-input}{\code{$input()}}.
        #' If not specified, the measured values of
        #' input parameters will be used for predictions.
        #'
        #' All the data will be stored internally and used during Bayesian
        #' calibration using Stan.
        #'
        #' Note that as `$data_field()` relies on the output of
        #' \href{../../epluspar/html/BayesCalibJob.html#method-data_sim}{\code{$data_sim()}}.
        #' to
        #' perform validation on the specified data, `$data_field()` cannot be
        #' called before
        #' \href{../../epluspar/html/BayesCalibJob.html#method-data_sim}{\code{$data_sim()}}.
        #' and internally stored data will be
        #' removed whenever
        #' \href{../../epluspar/html/BayesCalibJob.html#method-data_sim}{\code{$data_sim()}}.
        #' is called. This aims to make sure that
        #' simulated data and field data can be matched whenever the calibration
        #' is performed.
        #'
        #' @param output A [data.frame()] containing measured value of output
        #'        parameters.
        #' @param new_input A [data.frame()] containing newly measured value of
        #'        input parameters used for prediction. If `NULL`, values of the
        #'        first case in
        #'        \href{../../epluspar/html/BayesCalibJob.html#method-data_sim}{\code{$data_sim()}}
        #'        will be used.
        #' @param all If `TRUE`, extra columns are also included in the returned
        #'        [data.table::data.table()] describing the simulation case and
        #'        datetime components. For details, please see
        #'        \href{../../epluspar/html/BayesCalibJob.html#method-data_sim}{\code{$data_sim()}}.
        #'        Default: `FALSE`.
        #'
        #' @return A list of 3 elements:
        #'
        #' * `input`: a [data.table::data.table()] which is basically the input
        #'   variable values of the first case in
        #'   \href{../../epluspar/html/BayesCalibJob.html#method-data_sim}{\code{$data_sim()}}.
        #' * `output`: a [data.table::data.table()] of output variable values.
        #' * `new_output`: `NULL` or a [data.table::data.table()] of newly
        #'   measured input variable values.
        #'
        #' For details on the meaning of each columns, see
        #' \href{../../epluspar/html/BayesCalibJob.html#method-data_sim}{\code{$data_sim()}}.
        #'
        data_field = function (output, new_input = NULL, all = FALSE)
            bc_data_field(super, self, private, output, new_input, all),
        # }}}

        # data_bc {{{
        #' @description
        #' Combine simulation data and field measured data
        #'
        #' @details
        #' `$data_bc()` takes a list of field data and simulated data, and
        #' returns a list that contains data input for Bayesian calibration
        #' using the Stan model from Chong (2018):
        #'
        #' * `n`: Number of measured parameter observations.
        #' * `n_pred`: Number of newly design points for predictions.
        #' * `m`: Number of simulated observations.
        #' * `p`: Number of input parameters.
        #' * `q`: Number of calibration parameters.
        #' * `yf`: Data of measured output after z-score standardization using data of
        #'   simulated output.
        #' * `yc`: Data of simulated output after z-score standardization.
        #' * `xf`: Data of measured input after min-max normalization.
        #' * `xc`: Data of simulated input after min-max normalization.
        #' * `x_pred`: Data of new design points for predictions after min-max
        #'   normalization.
        #' * `tc`: Data of calibration parameters after min-max normalization.
        #'
        #' Input `data_field` and `data_sim` should have the same structure as the
        #' output from `$data_field()` and `$data_sim()`. If `data_field` and
        #' `data_sim` is not specified, the output from `$data_field()` and
        #' `$data_sim()` will be used.
        #'
        #' @param data_field A [data.frame()] specifying field measured data.
        #'        Should have the same structure as the output from
        #'        \href{../../epluspar/html/BayesCalibJob.html#method-data_field}{\code{$data_field()}}.
        #'        If `NULL`, the output from
        #'        \href{../../epluspar/html/BayesCalibJob.html#method-data_field}{\code{$data_field()}}
        #'        will be used. Default: `NULL`.
        #' @param data_sim A [data.frame()] specifying field measured data.
        #'        Should have the same structure as the output from
        #'        \href{../../epluspar/html/BayesCalibJob.html#method-data_sim}{\code{$data_sim()}}.
        #'        If `NULL`, the output from
        #'        \href{../../epluspar/html/BayesCalibJob.html#method-data_sim}{\code{$data_sim()}}
        #'        will be used. Default: `NULL`.
        #'
        #' @return A list of 11 elements.
        #'
        #' @examples
        #' \dontrun{
        #' bc$data_bc()
        #' }
        #'
        data_bc = function (data_field = NULL, data_sim = NULL)
            bc_data_bc(super, self, private, data_field, data_sim),
        # }}}

        # eplus_run {{{
        #' @description
        #' Run parametric simulations
        #'
        #' @details
        #' `$eplus_run()` runs all parametric models in parallel. Parameter
        #' `run_period` can be given to insert a new `RunPeriod` object. In this
        #' case, all existing `RunPeriod` objects in the seed model will be
        #' commented out.
        #'
        #' Note that when `run_period` is given, value of field `Run Simulation
        #' for Weather File Run Periods` in `SimulationControl` class will be
        #' reset to `Yes` to make sure input run period can take effect.
        #'
        #' @param dir The parent output directory for specified simulations.
        #'        Outputs of each simulation are placed in a separate folder
        #'        under the parent directory. If `NULL`, directory of seed
        #'        model will be used. Default: `NULL`.
        #' @param run_period A list giving a new `RunPeriod` object definition.
        #'        If not `NULL`, only this new RunPeriod will take effect with
        #'        all existing RunPeriod objects in the seed model being
        #'        commented out. If `NULL`, existing run period in the seed
        #'        model will be used. Default: `NULL`.
        #' @param wait If `TRUE`, R will hang on and wait all EnergyPlus simulations
        #'        finish. If `FALSE`, all EnergyPlus simulations are run in the
        #'        background. Default: `TRUE`.
        #' @param force Only applicable when the last simulation runs with
        #'        `wait` equals to `FALSE` and is still running. If `TRUE`,
        #'        current running job is forced to stop and a new one will
        #'        start. Default: `FALSE`.
        #' @param copy_external If `TRUE`, the external files that every `Idf`
        #'        object depends on will also be copied into the simulation
        #'        output directory. The values of file paths in the Idf will be
        #'        changed automatically. Currently, only `Schedule:File` class
        #'        is supported.  This ensures that the output directory will
        #'        have all files needed for the model to run. Default is
        #'        `FALSE`.
        #' @param echo Only applicable when `wait` is `TRUE`. Whether to print
        #'        simulation status. Default: same as the value of `wait`.
        #'
        #' @return The modified `BayesCalibJob` object itself.
        #'
        #' @examples
        #' \dontrun{
        #' # specify output directory and run period
        #' bc$eplus_run(dir = tempdir(), run_period = list("example", 1, 1, 1, 31))
        #'
        #' # run in the background
        #' bc$eplus_run(wait = TRUE)
        #' # see job status
        #' bc$status()
        #'
        #' # force to kill background job before running the new one
        #' bc$eplus_run(force = TRUE)
        #'
        #' # do not show anything in the console
        #' bc$eplus_run(echo = FALSE)
        #'
        #' # copy external files used in the model to simulation output directory
        #' bc$eplus_run(copy_external = TRUE)
        #' }
        #'
        eplus_run = function (dir = NULL, run_period = NULL, wait = TRUE, force = FALSE,
                              copy_external = FALSE, echo = wait)
            bc_eplus_run(super, self, private, dir, run_period, wait, force, copy_external, echo),
        # }}}

        # eplus_kill {{{
        #' @description
        #' Kill current running EnergyPlus simulations
        #'
        #' @details
        #' `$eplus_kill()` kills all background EnergyPlus processes that are
        #' current running if possible. It only works when simulations run in
        #' non-waiting mode.
        #'
        #' @return A single logical value of `TRUE` or `FALSE`, invisibly.
        #'
        #' @examples
        #' \dontrun{
        #' bc$eplus_kill()
        #' }
        #'
        eplus_kill = function ()
            super$kill(),
        # }}}

        # eplus_status {{{
        #' @description
        #' Get the EnergyPlus simulation status
        #'
        #' @details
        #' `$eplus_status()` returns a named list of values indicates the status
        #' of the job:
        #'
        #'   * `run_before`: `TRUE` if the job has been run before. `FALSE` otherwise.
        #'   * `alive`: `TRUE` if the job is still running in the background. `FALSE`
        #'     otherwise.
        #'   * `terminated`: `TRUE` if the job was terminated during last
        #'      simulation. `FALSE` otherwise. `NA` if the job has not been run yet.
        #'   * `successful`: `TRUE` if all simulations ended successfully. `FALSE` if
        #'     there is any simulation failed. `NA` if the job has not been run yet.
        #'   * `changed_after`: `TRUE` if the *seed model* has been modified since last
        #'      simulation. `FALSE` otherwise.
        #'   * `job_status`: A [data.table::data.table()] contains meta data
        #'     for each simulation job. For details, please see [run_multi()]. If the
        #'     job has not been run before, a [data.table::data.table()]
        #'     with 4 columns is returned:
        #'     - `index`: The index of simulation
        #'     - `status`: The status of simulation. As the simulation has not been run,
        #'       `status` will always be "idle".
        #'     - `idf`: The path of input IDF file.
        #'     - `epw`: The path of input EPW file. If not provided, `NA` will be
        #'       assigned.
        #'
        #' @return A named list of 6 elements.
        #'
        #' @examples
        #' \dontrun{
        #' bc$eplus_status()
        #' }
        #'
        eplus_status = function ()
            super$status(),
        # }}}

        # eplus_output_dir {{{
        #' @description
        #' Get EnergyPlus simulation output directory
        #'
        #' @details
        #' `$eplus_output_dir()` returns the output directory of EnergyPlus
        #' simulation results.
        #'
        #' @param which An integer vector of the indexes or a character vector
        #'        or names of parametric simulations. If `NULL`, results of all
        #'        parametric simulations are returned. Default: `NULL`.
        #'
        #' @return A character vector.
        #'
        #' @examples
        #' \dontrun{
        #' # get output directories of all simulations
        #' bc$eplus_output_dir()
        #'
        #' # get output directories of specified simulations
        #' bc$eplus_output_dir(c(1, 4))
        #' }
        #'
        eplus_output_dir = function (which = NULL)
            super$output_dir(which),
        # }}}

        # eplus_locate_output {{{
        #' @description
        #' Get paths of EnergyPlus output file
        #'
        #' @details
        #' `$eplus_locate_output()` returns the path of a single output file of
        #' specified simulations.
        #'
        #' @param which An integer vector of the indexes or a character vector
        #'        or names of parametric simulations. If `NULL`, results of all
        #'        parametric simulations are returned. Default: `NULL`.
        #' @param suffix A string that indicates the file extension of
        #'        simulation output. Default: `".err"`.
        #' @param strict If `TRUE`, it will check if the simulation was
        #'        terminated, is still running or the file exists or not.
        #'        Default: `TRUE`.
        #'
        #' @return A character vector.
        #'
        #' @examples
        #' \dontrun{
        #' # get the file path of the error file
        #' bc$eplus_locate_output(c(1, 4), ".err", strict = FALSE)
        #'
        #' # can use to detect if certain output file exists
        #' bc$eplus_locate_output(c(1, 4), ".expidf", strict = TRUE)
        #' }
        #'
        eplus_locate_output = function (which = NULL, suffix = ".err", strict = TRUE)
            super$locate_output(which, suffix, strict),
        # }}}

        # eplus_errors {{{
        #' @description
        #' Read EnergyPlus simulation errors
        #'
        #' @details
        #' $eplus_errors() returns a list of [ErrFile][eplusr::read_err()]
        #' objects which contain all contents of the simulation error files
        #' (`.err`). If `info` is `FALSE`, only warnings and errors are printed.
        #'
        #' @param which An integer vector of the indexes or a character vector
        #'        or names of parametric simulations. If `NULL`, results of all
        #'        parametric simulations are returned. Default: `NULL`.
        #' @param info If `FALSE`, only warnings and errors are printed.
        #'        Default: `FALSE`.
        #'
        #' @return A list of [ErrFile][eplusr::read_err()] objects.
        #'
        #' @examples
        #' \dontrun{
        #' bc$errors()
        #'
        #' # show all information
        #' bc$errors(info = TRUE)
        #' }
        #'
        eplus_errors = function (which = NULL, info = FALSE)
            super$errors(which, info),
        # }}}

        # eplus_report_data_dict {{{
        #' @description
        #' Read report data dictionary from EnergyPlus SQL outputs
        #'
        #' @details
        #' `$eplus_report_data_dict()` returns a [data.table::data.table()]
        #' which contains all information about report data.
        #'
        #' For details on the meaning of each columns, please see "2.20.2.1
        #' ReportDataDictionary Table" in EnergyPlus "Output Details and
        #' Examples" documentation.
        #'
        #' @param which An integer vector of the indexes or a character vector
        #'        or names of parametric simulations. If `NULL`, results of all
        #'        parametric simulations are returned. Default: `NULL`.
        #'
        #' @return A [data.table::data.table()] of 10 columns:
        #'
        #' * `case`: The model name. This column can be used to distinguish
        #'   output from different simulations
        #' * `report_data_dictionary_index`: The integer used to link the
        #'   dictionary data to the variable data. Mainly useful when joining
        #'   diferent tables
        #' * `is_meter`: Whether report data is a meter data. Possible values:
        #'   `0` and `1`
        #' * `timestep_type`: Type of data timestep. Possible values: `Zone` and
        #'   `HVAC System`
        #' * `key_value`: Key name of the data
        #' * `name`: Actual report data name
        #' * `reporting_frequency`:
        #' * `schedule_name`: Name of the the schedule that controls reporting
        #'     frequency.
        #' * `units`: The data units
        #'
        #' @examples
        #' \dontrun{
        #' bc$eplus_report_data_dict(c(1, 4))
        #' }
        #'
        eplus_report_data_dict = function (which = NULL)
            super$report_data_dict(which),
        # }}}

        # eplus_report_data {{{
        #' @description
        #' Read EnergyPlus report data
        #'
        #' @details
        #' `$eplus_report_data()` extracts the report data in a
        #' [data.table::data.table()] using key values, variable names and other
        #' specifications.
        #'
        #' `$eplus_report_data()` can also directly take all or subset output from
        #' \href{../../epluspar/html/BayesCalibJob.html#method-eplus_report_data_dict}{\code{$eplus_report_data_dict()}}
        #' as input, and extract all data specified.
        #'
        #' The returned column numbers varies depending on `all` argument.
        #'
        #' * `all` is `FALSE`, the returned [data.table::data.table()] has 6 columns:
        #'   * `case`: The model name. This column can be used to distinguish
        #'     output from different simulations
        #'   * `datetime`: The date time of simulation result
        #'   * `key_value`: Key name of the data
        #'   * `name`: Actual report data name
        #'   * `units`: The data units
        #'   * `value`: The data value
        #' * `all` is `TRUE`, besides columns described above, extra columns are also
        #'   included:
        #'   * `month`: The month of reported date time
        #'   * `day`: The day of month of reported date time
        #'   * `hour`: The hour of reported date time
        #'   * `minute`: The minute of reported date time
        #'   * `dst`: Daylight saving time indicator. Possible values: `0` and `1`
        #'   * `interval`: Length of reporting interval
        #'   * `simulation_days`: Day of simulation
        #'   * `day_type`: The type of day, e.g. `Monday`, `Tuesday` and etc.
        #'   * `environment_period_index`: The indice of environment.
        #'   * `environment_name`: A text string identifying the environment.
        #'   * `is_meter`: Whether report data is a meter data. Possible values: `0` and
        #'     `1`
        #'   * `type`: Nature of data type with respect to state. Possible values: `Sum`
        #'     and `Avg`
        #'   * `index_group`: The report group, e.g. `Zone`, `System`
        #'   * `timestep_type`: Type of data timestep. Possible values: `Zone` and `HVAC
        #'     System`
        #'   * `reporting_frequency`: The reporting frequency of the variable, e.g.
        #'   `HVAC System Timestep`, `Zone Timestep`.
        #'   * `schedule_name`: Name of the the schedule that controls reporting
        #'     frequency.
        #'
        #' With the `datetime` column, it is quite straightforward to apply time-series
        #' analysis on the simulation output. However, another painful thing is that
        #' every simulation run period has its own `Day of Week for Start Day`. Randomly
        #' setting the `year` may result in a date time series that does not have
        #' the same start day of week as specified in the RunPeriod objects.
        #'
        #' eplusr provides a simple solution for this. By setting `year` to `NULL`,
        #' which is the default behavior, eplusr will calculate a year value (from
        #' current year backwards) for each run period that compliances with the start
        #' day of week restriction.
        #'
        #' It is worth noting that EnergyPlus uses 24-hour clock system where 24 is only
        #' used to denote midnight at the end of a calendar day. In EnergyPlus output,
        #' "00:24:00" with a time interval being 15 mins represents a time period from
        #' "00:23:45" to "00:24:00", and similarly "00:15:00" represents a time period
        #' from "00:24:00" to "00:15:00" of the next day. This means that if current day
        #' is Friday, day of week rule applied in schedule time period "00:23:45" to
        #' "00:24:00" (presented as "00:24:00" in the output) is also Friday, but not
        #' Saturday. However, if you try to get the day of week of time "00:24:00" in R,
        #' you will get Saturday, but not Friday. This introduces inconsistency and may
        #' cause problems when doing data analysis considering day of week value.
        #'
        #' With `wide` equals `TRUE`, `$eplus_report_data()` will format the
        #' simulation output in the same way as standard EnergyPlus csv output
        #' file. Sometimes this can be useful as there may be existing
        #' tools/workflows that depend on this format.  When both `wide` and
        #' `all` are `TRUE`, columns of runperiod environment names and date
        #' time components are also returned, including:
        #' `environment_period_index", "environment_name`, `simulation_days`,
        #' `datetime`, `month`, `day`, `hour`, `minute`, `day_type`.
        #'
        #' For convenience, input character arguments matching in
        #' `$eplus_report_data()` are **case-insensitive**.
        #'
        #' @param which An integer vector of the indexes or a character vector
        #'        or names of parametric simulations. If `NULL`, results of all
        #'        parametric simulations are returned. Default: `NULL`.
        #'
        #' @param key_value A character vector to identify key values of the
        #'        data. If `NULL`, all keys of that variable will be returned.
        #'        `key_value` can also be data.frame that contains `key_value`
        #'        and `name` columns. In this case, `name` argument in
        #'        `$eplus_report_data()` is ignored. All available `key_value` for
        #'        current simulation output can be obtained using
        #'        \href{../../epluspar/html/BayesCalibJob.html#method-eplus_report_data_dict}{\code{$eplus_report_data_dict()}}.
        #'        Default: `NULL`.
        #'
        #' @param name A character vector to identify names of the data. If
        #'        `NULL`, all names of that variable will be returned. If
        #'        `key_value` is a data.frame, `name` is ignored. All available
        #'        `name` for current simulation output can be obtained using
        #'        \href{../../epluspar/html/BayesCalibJob.html#method-eplus_report_data_dict}{\code{$eplus_report_data_dict()}}.
        #'        Default: `NULL`.
        #'
        #' @param year Year of the date time in column `datetime`. If `NULL`, it
        #'        will calculate a year value that meets the start day of week
        #'        restriction for each environment. Default: `NULL`.
        #'
        #' @param tz Time zone of date time in column `datetime`. Default:
        #'        `"UTC"`.
        #'
        #' @param case If not `NULL`, a character column will be added indicates
        #'        the case of this simulation. If `"auto"`, the name of the IDF
        #'        file without extension is used.
        #'
        #' @param all If `TRUE`, extra columns are also included in the returned
        #'        [data.table::data.table()].
        #'
        #' @param wide If `TRUE`, the output is formated in the same way as
        #'        standard EnergyPlus csv output file.
        #'
        #' @param period A Date or POSIXt vector used to specify which time
        #'        period to return. The year value does not matter and only
        #'        month, day, hour and minute value will be used when
        #'        subsetting. If `NULL`, all time period of data is returned.
        #'        Default: `NULL`.
        #'
        #' @param month,day,hour,minute Each is an integer vector for month,
        #'        day, hour, minute subsetting of `datetime` column when
        #'        querying on the SQL database. If `NULL`, no subsetting is
        #'        performed on those components. All possible `month`, `day`,
        #'        `hour` and `minute` can be obtained using
        #'        \href{../../epluspar/html/BayesCalibJob.html#method-eplus_report_data_dict}{\code{$eplus_report_data_dict()}}.
        #'        Default: `NULL`.
        #'
        #' @param interval An integer vector used to specify which interval
        #'        length of report to extract. If `NULL`, all interval will be
        #'        used. Default: `NULL`.
        #'
        #' @param simulation_days An integer vector to specify which simulation
        #'        day data to extract. Note that this number resets after warmup
        #'        and at the beginning of an environment period. All possible
        #'        `simulation_days` can be obtained using
        #'        \href{../../epluspar/html/BayesCalibJob.html#method-eplus_report_data_dict}{\code{$eplus_report_data_dict()}}.
        #'        If `NULL`, all simulation days will be used. Default: `NULL`.
        #'
        #' @param day_type A character vector to specify which day type of data
        #'        to extract. All possible day types are: `Sunday`, `Monday`,
        #'        `Tuesday`, `Wednesday`, `Thursday`, `Friday`, `Saturday`,
        #'        `Holiday`, `SummerDesignDay`, `WinterDesignDay`, `CustomDay1`,
        #'        and `CustomDay2`. All possible values for current simulation
        #'        output can be obtained using
        #'        \href{../../epluspar/html/BayesCalibJob.html#method-eplus_report_data_dict}{\code{$eplus_report_data_dict()}}.
        #'
        #' @param environment_name A character vector to specify which
        #'        environment data to extract. If `NULL`, all environment data
        #'        are returned. Default: `NULL`. All possible
        #'        `environment_name` for current simulation output can be
        #'        obtained using:
        #' ```
        #' $read_table(NULL, "EnvironmentPeriods")
        #' ```
        #'
        #' @return A [data.table::data.table()].
        #'
        #' @examples
        #' \dontrun{
        #' # read report data
        #' bc$report_data(c(1, 4))
        #'
        #' # specify output variables using report data dictionary
        #' dict <- bc$report_data_dict(1)
        #' bc$report_data(c(1, 4), dict[units == "C"])
        #'
        #' # specify output variables using 'key_value' and 'name'
        #' bc$report_data(c(1, 4), "environment", "site outdoor air drybulb temperature")
        #'
        #' # explicitly specify year value and time zone
        #' bc$report_data(c(1, 4), dict[1], year = 2020, tz = "Etc/GMT+8")
        #'
        #' # get all possible columns
        #' bc$report_data(c(1, 4), dict[1], all = TRUE)
        #'
        #' # return in a format that is similar as EnergyPlus CSV output
        #' bc$report_data(c(1, 4), dict[1], wide = TRUE)
        #'
        #' # return in a format that is similar as EnergyPlus CSV output with
        #' # extra columns
        #' bc$report_data(c(1, 4), dict[1], wide = TRUE, all = TRUE)
        #'
        #' # only get data at the working hour on the first Monday
        #' bc$report_data(c(1, 4), dict[1], hour = 8:18, day_type = "monday", simulation_days = 1:7)
        #' }
        #'
        eplus_report_data = function (which = NULL, key_value = NULL, name = NULL,
                                year = NULL, tz = "UTC", all = FALSE, wide = FALSE,
                                period = NULL, month = NULL, day = NULL, hour = NULL, minute = NULL,
                                interval = NULL, simulation_days = NULL, day_type = NULL,
                                environment_name = NULL)
            super$report_data(which, key_value = key_value, name = name,
                year = year, tz = tz, all = all, wide = wide,
                period = period, month = month, day = day, hour = hour, minute = minute,
                interval = interval, simulation_days = simulation_days, day_type = day_type,
                environment_name = environment_name),
        # }}}

        # eplus_tabular_data {{{
        #' @description
        #' Read EnergyPlus tabular data
        #'
        #' @details
        #' `$eplus_tabular_data()` extracts the tabular data in a
        #' [data.table::data.table()] using report, table, column and row name
        #' specifications. The returned [data.table::data.table()] has
        #' 9 columns:
        #'
        #' * `case`: The model name. This column can be used to distinguish
        #'   output from different simulations
        #' * `index`: Tabular data index
        #' * `report_name`: The name of the report that the record belongs to
        #' * `report_for`: The `For` text that is associated with the record
        #' * `table_name`: The name of the table that the record belongs to
        #' * `column_name`: The name of the column that the record belongs to
        #' * `row_name`: The name of the row that the record belongs to
        #' * `units`: The units of the record
        #' * `value`: The value of the record **in string format**
        #'
        #' For convenience, input character arguments matching in
        #' `$eplus_tabular_data()` are **case-insensitive**.
        #'
        #' @param which An integer vector of the indexes or a character vector
        #'        or names of parametric simulations. If `NULL`, results of all
        #'        parametric simulations are returned. Default: `NULL`.
        #'
        #' @param report_name,report_for,table_name,column_name,row_name Each is
        #'        a character vector for subsetting when querying the SQL
        #'        database.  For the meaning of each argument, please see the
        #'        description above.
        #'
        #' @return A [data.table::data.table()] with 8 columns.
        #'
        #' @examples
        #' \dontrun{
        #' # read all tabular data
        #' bc$eplus_tabular_data(c(1, 4))
        #'
        #' # explicitly specify data you want
        #' str(bc$eplus_tabular_data(c(1, 4),
        #'     report_name = "AnnualBuildingUtilityPerformanceSummary",
        #'     table_name = "Site and Source Energy",
        #'     column_name = "Total Energy",
        #'     row_name = "Total Site Energy"
        #' ))
        #' }
        #'
        eplus_tabular_data = function (which = NULL, report_name = NULL, report_for = NULL,
                                table_name = NULL, column_name = NULL, row_name = NULL)
            super$tabular_data(which, report_name = report_name,
                report_for = report_for, table_name = table_name,
                column_name = column_name, row_name = row_name),
        # }}}

        # eplus_save {{{
        #' @description
        #' Save EnergyPlus parametric models
        #'
        #' @details
        #' `$eplus_save()` saves all parametric models in specified folder. An
        #' error will be issued if no measure has been applied.
        #'
        #' @param dir The parent output directory for models to be saved. If
        #'        `NULL`, the directory of the seed model will be used. Default:
        #'        `NULL`.
        #' @param separate If `TRUE`, all models are saved in a separate folder
        #'        with each model's name under specified directory. If `FALSE`,
        #'        all models are saved in the specified directory. Default:
        #'        `TRUE`.
        #' @param copy_external Only applicable when `separate` is `TRUE`. If
        #'        `TRUE`, the external files that every `Idf` object depends on
        #'        will also be copied into the saving directory. The values of
        #'        file paths in the Idf will be changed automatically.
        #'        Currently, only `Schedule:File` class is supported.  This
        #'        ensures that the output directory will have all files needed
        #'        for the model to run. Default: `FALSE`.
        #'
        #' @return A [data.table::data.table()] with two columns:
        #'
        #' * model: The path of saved parametric model files.
        #' * weather: The path of saved weather files.
        #'
        #' @examples
        #' \dontrun{
        #' # save all parametric models with each model in a separate folder
        #' bc$save(tempdir())
        #'
        #' # save all parametric models with all models in the same folder
        #' bc$save(tempdir(), separate = FALSE)
        #' }
        #'
        eplus_save = function (dir = NULL, separate = TRUE, copy_external = FALSE)
            super$save(dir, separate, copy_external),
        # }}}

        # stan_run {{{
        #' @description
        #' Run Bayesian calibration using Stan
        #'
        #' @details
        #' `$stan_run()` runs Bayesian calibration using [Stan][rstan::stan] and
        #' returns a list of 2 elements:
        #'
        #' * `fit`: An object of S4 class [rstan::stanfit].
        #' * `y_pred`: The output of
        #'   \href{../../epluspar/html/BayesCalibJob.html#method-prediction}{\code{$prediction()}}
        #'
        #' @note
        #' Currently, when using builtin Bayesian calibration algorithm, only
        #' one prediction output variable is supported. An error will be issued
        #' if multiple output variables found in `data`.
        #'
        #' @param file The path to the Stan program to use. If `NULL`, the
        #'        pre-compiled Stan code from Chong (2018) will be used.
        #'        Default: `NULL`.
        #' @param data Only applicable when `file` is not `NULL`. The data to be
        #'        used for Bayesian calibration. If `NULL`, the data that
        #'        `$data_bc()` returns is used. Default: `NULL`.
        #' @param iter A positive integer specifying the number of iterations
        #'        for each chain (including warmup). Default: `2000`.
        #' @param chains A positive integer specifying the number of Markov
        #'        chains. Default: `4`.
        #' @param echo Only applicable when `file` is NULL. Whether to print the
        #'        summary of Informational Messages to the screen after a chain
        #'        is finished or a character string naming a path where the
        #'        summary is stored. Default: `TRUE`.
        #' @param mc.cores An integer specifying how many cores to be used for
        #'        Stan. Default: `parallel::detectCores()`.
        #' @param all If `FALSE`, among above meta data columns, only `index`,
        #'        `type` and `Date/Time` will be returned. Default: `FALSE`.
        #' @param merge If `TRUE`, `y_pred` in returned list will merge all
        #'        \href{../../epluspar/html/BayesCalibJob.html#method-data_field}{\code{$data_field()}},
        #'        and predicted output into one [data.table::data.table()] with
        #'        all predicted values put in columns with a `\\[prediction\\]`
        #'        prefix. If `FALSE`, similar like above, but combine rows of
        #'        field measured output and predicted output together, with a
        #'        new column `type` added giving `field` indicating field
        #'        measured output and `prediction` indicating predicted output.
        #'        Default: `TRUE`.
        #' @param ... Additional arguments to pass to [rstan::sampling] (when
        #'        `file` is `NULL`) or [rstan::stan] (when `file` is not
        #'        `NULL`).
        #'
        #' @return A list of 2 elements.
        #'
        #' @examples
        #' \dontrun{
        #' bc$stan_run()
        #' }
        #'
        stan_run = function (file = NULL, data = NULL, iter = 2000L, chains = 4L, echo = TRUE,
                             mc.cores = parallel::detectCores(), all = FALSE, merge = TRUE, ...)
            bc_stan_run(super, self, private, file = file, data = data, iter = iter,
                        chains = chains, echo = echo, mc.cores = mc.cores, all = all,
                        merge = merge, ...),
        # }}}

        # stan_file {{{
        #' @description
        #' Extract Stan file for Bayesian calibration
        #'
        #' @details
        #' `$stan_file()` saves the Stan file used internally for Bayesian
        #' calibration. If no path is given, a character vector of the Stan
        #' code is returned. If given, the code will be save to the path and the
        #' file path is returned.
        #'
        #' @param path A path to save the Stan code. If `NULL`, a character
        #'        vector of the Stan code is returned.
        #'
        #' @examples
        #' \dontrun{
        #' bc$stan_file()
        #' }
        #'
        stan_file = function (path = NULL)
            bc_stan_file(super, self, private, path),
        # }}}

        # post_dist {{{
        #' @description
        #' Extract posterior distributions of calibrated parameters
        #'
        #' @details
        #' `$post_dist()` extracted calibrated parameter posterior distributions
        #' based on the results of
        #' \href{../../epluspar/html/BayesCalibJob.html#method-stan_run}{\code{$stan_run()}}
        #' and returns a [data.table::data.table()] with each parameter values
        #' filling one column. The parameter names are defined by the `.names`
        #' arguments in the
        #' \href{../../epluspar/html/BayesCalibJob.html#method-param}{\code{$param()}}.
        #'
        #' @return A [data.table::data.table()].
        #'
        #' @examples
        #' \dontrun{
        #' bc$post_dist()
        #' }
        #'
        post_dist = function ()
            bc_post_dist(super, self, private),
        # }}}

        # prediction {{{
        #' @description
        #' Extract predictions of output variables
        #'
        #' @details
        #' `$prediction()` calculates predicted output variable values based
        #' on the results of
        #' \href{../../epluspar/html/BayesCalibJob.html#method-stan_run}{\code{$stan_run()}}
        #' and returns a [data.table::data.table()] which combines the output of
        #' \href{../../epluspar/html/BayesCalibJob.html#method-data_field}{\code{$data_field()}}
        #' and predicted output values.
        #'
        #' Possible returned meta data columns:
        #'
        #' - `index`: Integer type. Row indices of field input data in
        #'   \href{../../epluspar/html/BayesCalibJob.html#method-data_field}{\code{$data_field()}}
        #' - `sample`: Integer type. Sample indices of the MCMC.
        #' - `type`: Character type. Only exists when `merge` is `FALSE`. The
        #'   type of output values. `field` indicates field measured output
        #'   values while `prediction` means predicted output values.
        #' - `Data/Time`: Character type. The date time in EnergyPlus-format.
        #' - `environment_period_index`: Integer type. The indice of environment.
        #' - `environment_name`: Character type. A text string identifying the
        #'   simulation environment.
        #' - `simulation_days`: Integer type. Day of simulation.
        #' - `datetime`: DateTime type. The date time of simulation result. Note
        #'   that the year valueas are automatically calculated to meets the
        #'   start day of week restriction for each simulation environment.
        #' - `month`: Integer type. The month of reported date time.
        #' - `day`: Integer type. The day of month of reported date time.
        #' - `hour`: Integer type. The hour of reported date time.
        #' - `minute`: Integer type. The minute of reported date time.
        #' - `day_type`: Character type. The type of day, e.g. `Monday`,
        #'   `Tuesday` and etc. Note that `day_type` will always be `NA` if
        #'   `resolution` is specified.
        #'
        #' @param all If `FALSE`, among above meta data columns, only `index`,
        #'        `type` and `Date/Time` will be returned. Default: `FALSE`.
        #' @param merge If `TRUE`, `y_pred` in returned list will merge all
        #'        \href{../../epluspar/html/BayesCalibJob.html#method-data_field}{\code{$data_field()}},
        #'        and predicted output into one [data.table::data.table()] with
        #'        all predicted values put in columns with a `\\[prediction\\]`
        #'        prefix. If `FALSE`, similar like above, but combine rows of
        #'        field measured output and predicted output together, with a
        #'        new column `type` added giving `field` indicating field
        #'        measured output and `prediction` indicating predicted output.
        #'        Default: `TRUE`.
        #'
        #' @return A [data.table::data.table()] with 1 column `sample` giving
        #' the sample indices from MCMC, plus the same number of columns as
        #' given calibrated parameters.
        #'
        #' @examples
        #' \dontrun{
        #' bc$prediction()
        #' }
        #'
        prediction = function (all = FALSE, merge = TRUE)
            bc_prediction(super, self, private, all = all, merge = merge),
        # }}}

        # evaluate {{{
        #' @description
        #' Calculate statistical indicators of output variable predictions
        #'
        #' @details
        #' `$evalute()` quantify the uncertainty of output variable predictions
        #' from each MCMC sample gathered from
        #' \href{../../epluspar/html/BayesCalibJob.html#method-prediction}{\code{$prediction()}}
        #' by calculating the statistical indicators.
        #'
        #' The default behavior is to evaluate the principal uncertainty indices
        #' used in ASHRAE Guideline 14 are Normalized Mean Bias Error (NMBE) and
        #' Coefficient of Variation of the Root Mean Square Error (CVRMSE).
        #'
        #' @param funs A list of functions that takes the simulation results as
        #'        the first argument and the measured results as the second
        #'        argument.  Default: `list(cvrmse, nmbe)`.
        #'
        #' @return A [data.table::data.table()] with 1 column `sample` giving
        #' the sample indices from MCMC, plus the same number of columns as
        #' given evaluation functions.
        #'
        #' @examples
        #' \dontrun{
        #' bc$evaluate()
        #' }
        #'
        evaluate = function (funs = list(nmbe, cvrmse))
            bc_evaluate(super, self, private, funs = funs, substitute(funs))
        # }}}
        # }}}
    ),

    private = list(
        # PRIVATE FIELDS {{{
        m_param = NULL,
        m_sample = NULL,
        m_input = NULL,
        m_output = NULL
        # }}}
    )
)
# }}}

#' Create a Bayesian Calibration Job
#'
#' `bayes_job()` takes an IDF and EPW as input, and returns an `BayesCalibJob`
#' object for conducting Bayesian calibration on an EnergyPlus model. For more
#' details, please see [BayesCalibJob].
#'
#' @param idf A path to an local EnergyPlus IDF file or an `Idf` object.
#' @param epw A path to an local EnergyPlus EPW file or an `Epw` object.
#' @return An `BayesCalibJob` object.
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
#'     bayes_job(idf_path, epw_path)
#'
#'     # create from an Idf and an Epw object
#'     bayes_job(read_idf(idf_path), read_epw(epw_path))
#' }
#' }
#' @seealso [sensi_job()] for creating a sensitivity analysis job.
#' @author Hongyuan Jia
#' @export
# bayes_job {{{
bayes_job <- function (idf, epw) {
    bc <- BayesCalibJob$new(idf, epw)
    # remove parent methods
    rm(list = c("run", "kill", "status", "output_dir", "locate_output",
        "errors", "report_data_dict", "report_data", "tabular_data"
    ), envir = bc)

    lockEnvironment(bc)
    bc
}
# }}}

# bc_remove_output_class {{{
bc_remove_output_class <- function (super, self, private, all = TRUE, clone = FALSE) {
    idf <- if (clone) private$m_seed$clone() else private$m_seed

    # clean all output classes to speed up
    out_cls <- idf$class_name(by_group = TRUE)[["Output Reporting"]]

    if (all) {
        out_cls <- setdiff(out_cls, c("Output:SQLite", "Output:VariableDictionary"))
    } else {
        out_cls <- intersect(out_cls, c("Output:Variable", "Output:Meter",
            "Output:Meter:MeterFileOnly", "Output:Meter:Cumulative",
            "Output:Meter:Cumulative:MeterFileOnly", "Meter:Custom",
            "Meter:CustomDecrement", "Output:EnvironmentalImpactFactors"
        ))
    }

    if (length(out_cls)) {
        eplusr:::with_silent(idf$del(idf$object_id(out_cls, simplify = TRUE), .force = TRUE))
    }

    idf
}
# }}}
# bc_run_ddy {{{
bc_run_ddy <- function (super, self, private) {
    if (!private$m_seed$is_valid_class("SizingPeriod:DesignDay")) {
        abort(paste0("In order to run design-day-only simulation, ",
            "at least one 'SizingPeriod:DesignDay' object should exist."
        ), "bc_no_ddy")
    }

    # clone the original and remove all output related classes
    idf <- bc_remove_output_class(super, self, private, clone = TRUE)

    # one single design day should be sufficient to get rdd and mdd
    # remove others to speed up
    if ((num <- idf$object_num("SizingPeriod:DesignDay")) > 1L) {
        eplusr:::with_silent(idf$del(idf$object_id("SizingPeriod:DesignDay", simplify = TRUE)[2L:num]))
    }

    eplusr:::with_silent(idf$save(tempfile(fileext = ".idf")))
    job <- eplusr:::with_silent(idf$run(NULL, dir = tempdir(), echo = FALSE))

    if (is.na(job$status()$successful) || !job$status()$successful) {
        abort("Failed to run design-day-only simulation.", "bc_ddy_run")
    }

    private$m_log$run_ddy <- TRUE

    private$m_log$rdd <- job$read_rdd()
    private$m_log$mdd <- job$read_mdd()
    # add lower case variable names
    set(private$m_log$rdd, NULL, "variable_lower", tolower(private$m_log$rdd$variable))
    set(private$m_log$mdd, NULL, "variable_lower", tolower(private$m_log$mdd$variable))

    self
}
# }}}
# bc_read_rdd {{{
bc_read_rdd <- function (super, self, private, update = FALSE) {
    if (update) {
        message("Updating RDD...")
        bc_run_ddy(super, self, private)
        message("Updating RDD... [SUCCESSFUL]")
    }

    if (!private$m_log$run_ddy) {
        message("Initializing RDD...")
        bc_run_ddy(super, self, private)
        message("Initializing RDD... [SUCCESSFUL]")
    }

    private$m_log$rdd[, -"variable_lower"]
}
# }}}
# bc_read_mdd {{{
bc_read_mdd <- function (super, self, private, update = FALSE) {
    if (update) {
        message("Updating MDD...")
        bc_run_ddy(super, self, private)
        message("Updating MDD... [SUCCESSFUL]")
    }

    if (!private$m_log$run_ddy) {
        message("Initializing MDD...")
        bc_run_ddy(super, self, private)
        message("Initializing MDD... [SUCCESSFUL]")
    }

    private$m_log$mdd[, -"variable_lower"]
}
# }}}
# bc_input {{{
bc_input <- function (super, self, private, key_value = NULL, name = NULL,
                      reporting_frequency = NULL, append = FALSE) {
    input <- bc_match_input_output(super, self, private, "input",
        key_value, name, reporting_frequency, append = append
    )

    if (is.null(input)) {
        input
    # remove the id column
    } else {
        input[, -"id"]
    }
}
# }}}
# bc_output {{{
bc_output <- function (super, self, private, key_value = NULL, name = NULL,
                       reporting_frequency = NULL, append = FALSE) {
    output <- bc_match_input_output(super, self, private, "output",
        key_value, name, reporting_frequency, append = append
    )

    if (is.null(output)) {
        output
    # remove the id column
    } else {
        output[, -"id"]
    }
}
# }}}
# bc_param {{{
bc_param <- function (super, self, private, ..., .names = NULL, .num_sim = 30L, .env = parent.frame()) {
    checkmate::assert_count(.num_sim)

    # clean measure created using $apply_measure() if any
    private$m_log$measure_wrapper <- NULL
    private$m_log$measure_name <- NULL

    l <- expand_param_specs(private$m_seed, ..., .env = .env, .names = .names, .specs_len = 2L)

    samples <- lhs_samples(l$param, .num_sim)

    private$m_param <- l$param
    private$m_sample <- samples
    private$m_log$matched <- l$value

    # only create models when input and output have been created
    if (is.null(private$m_input) || is.null(private$m_output)) {
        message("No parametric models have been created because input variables ",
            "or output variables are not set. Please set input and output variables ",
            "using '$input()' and '$output()' respectively. In this case, models ",
            "will be created when calling '$models()' or '$eplus_run()'."
        )
        return(self)
    }

    idfs <- create_par_models(private$m_seed, l$param, samples, l$value)

    private$m_idfs <- idfs

    # log
    private$log_new_uuid()
    private$log_idf_uuid()
    private$m_log$unsaved <- rep(TRUE, length(idfs))

    self
}
# }}}
# bc_param_specs {{{
bc_param_specs <- function (value_num, index) {
    if (!is.null(nm <- names(value_num))) {
        nm_valid <- nm[nm != ""]

        if (any(invld <- !nm_valid %in% c("min", "max"))) {
            abort(paste0("Parameter Range Specs should only contain ",
                "'min' and 'max'. Invalid element found: ",
                paste0("{", index, ":'", nm_valid[invld], "'}", collapse = ", ")
            ))
        }
        if (anyDuplicated(nm_valid)) {
            abort(paste0("Parameter Range Specs should contain only one ",
                "'min' and 'max'. Duplicated element found: ",
                paste0("{", index, ":'", nm_valid[duplicated(nm_valid)], "'}", collapse = ", ")
            ))
        }
        m <- match(c("min", "max"), nm)
        m[is.na(m)] <- setdiff(seq_along(value_num), m)
        value_num[m]
    }
    if (value_num[[1L]] >= value_num[[2L]]) {
        abort(paste0("For numeric field, minimum value should be less than ",
            "maximum value. Invalid input found for ",
            sprintf("{%i: %s(min), %s(max)}", index, value_num[[1L]], value_num[[2L]])
        ), "param_num_format")
    }
    list(min = value_num[[1L]], max = value_num[[2L]])
}
# }}}
# bc_apply_measure {{{
bc_apply_measure <- function (super, self, private, measure, ..., .num_sim = 30L, .env = parent.frame()) {
    l <- match_param_measure(measure, ..., .specs_len = 2L, .env = .env)

    samples <- lhs_samples(l$param, .num_sim)

    # store
    private$m_sample <- samples
    private$m_log$matched <- NULL
    private$m_log$measure <- l$measure
    private$m_log$measure_name <- l$name

    # only create models when input and output have been created
    if (is.null(private$m_input) || is.null(private$m_output)) {
        message("Input variables or output variables are not set. Please set ",
            "input and output variables using '$input()' and '$output()' ",
            "respectively. In this case, models will be created when calling ",
            "'$models()' or '$eplus_run()'."
        )
        return(self)
    }

    idfs <- create_par_models(private$m_seed, l$param, samples, measure = l$measure)
    private$m_idfs <- idfs

    # log
    private$log_new_uuid()
    private$log_idf_uuid()
    private$m_log$unsaved <- rep(TRUE, length(idfs))

    self
}
# }}}
# bc_samples {{{
bc_samples <- function (super, self, private) {
    bc_assert_has_sampled(super, self, private, stop = FALSE)
    private$m_sample
}
# }}}
# bc_models {{{
bc_models <- function (super, self, private, stop = FALSE) {
    if (!is.null(private$m_idfs)) return(private$m_idfs)

    if (!bc_assert_can_model(super, self, private, stop)) return()

    idfs <- create_par_models(private$m_seed, private$m_param, private$m_sample, private$m_log$matched, private$m_log$measure)
    private$m_idfs <- idfs

    # log
    private$log_new_uuid()
    private$log_idf_uuid()
    private$m_log$unsaved <- rep(TRUE, length(idfs))

    private$m_idfs
}
# }}}
# bc_eplus_run {{{
bc_eplus_run <- function (super, self, private, dir = NULL, run_period = NULL,
                          wait = TRUE, force = FALSE, copy_external = FALSE, echo = wait) {
    overwrite_runperiod <- function (idf, run_period) {
        idf <- use_runperiod(idf)
        if (!idf$is_valid_class("RunPeriod")) {
            idf$add(RunPeriod = run_period)
        } else {
            # delete the old run period
            old <- idf$to_string(class = "RunPeriod", header = FALSE, format = "new_top")
            idf$del(idf$object_id("RunPeriod", simplify = TRUE), .force = TRUE)
            idf$add(RunPeriod = c(run_period, list(.comment = old)))
        }
    }

    use_runperiod <- function (idf) {
        # check SimulationControl settings
        if (idf$is_valid_class("SimulationControl")) {
            simctrl <- idf$object_unique("SimulationControl")
            use_rp <- simctrl$value(5L, simplify = TRUE)
            if (!is.na(use_rp) && tolower(use_rp) == "no") {
                message("Reset 'Run Simulation for Weather File Run Periods' ",
                    "in 'SimulationControl' from 'No' to 'Yes' to make sure ",
                    "input run period can take effect."
                )
                simctrl$set("Run Simulation for Weather File Run Periods" = "Yes")
            }
        }
        idf
    }

    # remove all objects in output classes
    if (!is.null(run_period)) {
        checkmate::assert_list(run_period)

        # if no parameter models have been set
        if (is.null(private$m_idfs)) {
            overwrite_runperiod(private$m_seed, run_period)
            # should update logged seed UUID
            private$log_seed_uuid()
            # create models
            bc_models(super, self, private, TRUE)
        } else {
            # create models
            bc_models(super, self, private, TRUE)
            lapply(private$m_idfs, overwrite_runperiod, run_period = run_period)
        }
    } else {
        bc_models(super, self, private, TRUE)
    }

    # make sure input and output take effect in parametric models
    for (idf in private$m_idfs) {
        idf$Output_Variable <- NULL
        idf$Output_Variable <- private$m_seed$Output_Variable

        idf$Output_Meter <- NULL
        idf$Output_Meter <- private$m_seed$Output_Meter
    }
    # log
    private$log_new_uuid()
    private$log_idf_uuid()
    private$m_log$unsaved <- rep(TRUE, length(private$m_idfs))

    super$run(dir, wait = wait, force = force, copy_external = copy_external, echo = echo)
}
# }}}
# bc_data_sim {{{
bc_data_sim <- function (super, self, private, resolution = NULL, exclude_ddy = TRUE, all = FALSE) {
    bc_assert_can_collect(super, self, private, stop = TRUE)

    # remove logged data
    private$m_log$data_sim <- NULL
    # remove data field to make sure that data_sim and data_field is in pair
    private$m_log$data_field <- NULL

    input <- bc_extract_report_data(super, self, private, type = "input", exclude_ddy)
    output <- bc_extract_report_data(super, self, private, type = "output", exclude_ddy)

    if (!is.null(resolution)) {
        bc_assert_valid_resolution(super, self, private, resolution)

        input <- report_dt_aggregate(input, resolution)
        output <- report_dt_aggregate(output, resolution)
    }

    # format to wide
    input <- eplusr:::report_dt_to_wide(input, date_components = TRUE)
    output <- eplusr:::report_dt_to_wide(output, date_components = TRUE)

    # should keep the variable order
    input <- bc_retain_variable_order(super, self, private, input, "input")
    output <- bc_retain_variable_order(super, self, private, output, "output")

    private$m_log$data_sim$input <- copy(input)
    private$m_log$data_sim$output <- copy(output)
    private$m_log$data_sim$all <- all

    combine_input_output_data(input, output, all)
}
# }}}
# bc_data_field {{{
bc_data_field <- function (super, self, private, output, new_input = NULL, all = FALSE) {
    bc_assert_can_collect(super, self, private, stop = TRUE)

    # remove logged data
    private$m_log$data_field <- NULL

    bc_assert_valid_measured(super, self, private, output, "output", TRUE)

    # measured output
    output <- as.data.table(output)
    setnames(output, names(private$m_log$data_sim$output)[-(1L:11L)])
    set(output, NULL, names(private$m_log$data_sim$output)[1L:11L],
        private$m_log$data_sim$output[J(private$m_log$data_sim$output$case[1L]), on = "case"][
            , .SD, .SDcols = 1L:11L]
    )
    setcolorder(output, names(private$m_log$data_sim$output))

    # measured input: directly use sim data of the first case
    input <- private$m_log$data_sim$input[J(private$m_log$data_sim$input$case[[1L]]), on = "case"]

    # new measured input for prediction
    if (is.null(new_input)) {
        new_input <- input
    } else {
        bc_assert_valid_measured(super, self, private, new_input, "new_input", FALSE)

        new_input <- as.data.table(new_input)
        setnames(new_input, names(private$m_log$data_sim$input)[-(1L:11L)])
        set(new_input, NULL, names(input)[1L:11L], input[, .SD, .SDcols = 1L:11L])
    }
    setcolorder(new_input, names(input))

    # log
    private$m_log$data_field$input <- copy(input)
    private$m_log$data_field$output <- copy(output)
    private$m_log$data_field$new_input <- copy(new_input)
    private$m_log$data_field$all <- all

    # reset returned case to NA
    set(input, NULL, "case", NA_integer_)
    set(output, NULL, "case", NA_integer_)
    set(new_input, NULL, "case", NA_integer_)

    c(combine_input_output_data(input, output, all),
      list(new_input = combine_input_output_data(new_input, NULL, all)$input)
    )
}
# }}}
# bc_data_bc {{{
bc_data_bc <- function (super, self, private, data_field = NULL, data_sim = NULL) {
    bc_assert_can_stan(super, self, private, stop = TRUE)

    if (!is.null(data_sim)) {
        data_sim <- bc_check_data(super, self, private, data_sim, "sim")
    } else {
        data_sim <- private$m_log$data_sim
    }

    if (!is.null(data_field)) {
        data_field <- bc_check_data(super, self, private, data_field, "field")
    } else {
        data_field <- private$m_log$data_field
    }

    # data {{{
    # exclude 11 meta column: case, datetime, ...
    # measured output
    yf <- data_field$output[, .SD, .SDcols = -c(1L:11L)]
    # measured input
    xf <- data_field$input[, .SD, .SDcols = -c(1L:11L)]
    # simulated output
    yc <- data_sim$output[, .SD, .SDcols = -c(1L:11L)]
    # simulated input
    xc <- data_sim$input[, .SD, .SDcols = -c(1L:11L)]
    # newly measured input for prediction
    x_pred <- data_field$new_input[, .SD, .SDcols = -c(1L:11L)]
    # calibration parameters
    tc <- private$m_sample[rep(case, each = nrow(xf)), .SD, .SDcols = -1L]
    # }}}

    data_bc <- init_data_bc(yf, xf, x_pred, yc, xc, tc)

    private$m_log$stan$data <- data_bc$stan_data
    private$m_log$stan$yc_mean <- data_bc$yc_mean
    private$m_log$stan$yc_sd <- data_bc$yc_sd
    private$m_log$stan$tc_min <- data_bc$tc_min
    private$m_log$stan$tc_max <- data_bc$tc_max

    copy(data_bc$stan_data)
}
# }}}
# bc_post_dist {{{
bc_post_dist <- function (super, self, private) {
    bc_assert_can_stan(super, self, private, stop = TRUE)

    if (is.null(private$m_log$stan$fit)) {
        abort(paste0("Unable to calculate predictions ",
            "because Stan data is not available. Please use '$stan_run()' to ",
            "retrieve output of Bayesican calibration before caling '$prediction()'."
        ), "bc_stan_not_ready")
    }

    if (isTRUE(private$m_log$stan$custom_model)) {
        message("Customized Stan model was used during calibration, ",
            "instead of the built-in model. ",
            "epluspar may fail to extract calibrated parameter distributions. ",
            "Please do check the results to see if it is correct."
        )
    }

    if (!"tf" %in% names(private$m_log$stan$fit@par_dims)) {
        abort(paste0("Failed to get calibrated parameter ",
            "distributions because the 'tf' parameter is not found ",
            "in the stanfit object. This may be caused by using a customized Stan ",
            "model. Please check the input arguments when calling '$stan_run()'."
        ), "bc_tf_not_in_stan")
    }

    tf <- as.data.table(rstan::extract(private$m_log$stan$fit, pars = "tf")$tf)
    setnames(tf, unique(private$m_param$param_name))

    tf_min <- private$m_log$stan$tc_min
    tf_max <- private$m_log$stan$tc_max

    minmax_dnorm <- function (x, min, max) x * (max - min) + min

    for (i in seq_len(ncol(tf))) {
        set(tf, NULL, i, minmax_dnorm(tf[[i]], tf_min[[i]], tf_max[[i]]))
    }

    set(tf, NULL, "sample", seq_len(nrow(tf)))
    setcolorder(tf, "sample")
    private$m_log$stan$tf <- copy(tf)
    tf
}
# }}}
# bc_prediction {{{
bc_prediction <- function (super, self, private, all = FALSE, merge = TRUE) {
    bc_assert_can_stan(super, self, private, stop = TRUE)

    if (is.null(private$m_log$stan$fit)) {
        abort(paste0("Unable to calculate predictions ",
            "because Stan data is not available. Please use '$stan_run()' to ",
            "retrieve output of Bayesican calibration before caling '$prediction()'."
        ), "bc_stan_not_ready")
    }

    if (private$m_log$stan$custom_model) {
        message("Customized Stan model was used during calibration, ",
            "instead of the built-in model. ",
            "epluspar may fail to extract predicted output values. ",
            "Please do check the results to see if it is correct."
        )
    }

    if (!"y_pred" %in% names(private$m_log$stan$fit@par_dims)) {
        abort(paste0("Failed to get calibrated parameter ",
            "distributions because the 'tf' parameter is not found ",
            "in the stanfit object. This may be caused by using a customized Stan ",
            "model. Please check the input arguments when calling '$stan_run()'."
        ), "tf_not_in_stan")
    }

    y_pred <- rstan::extract(private$m_log$stan$fit, pars = "y_pred")$y_pred

    # get predictive inference y_pred and convert back to original scale
    y_pred <- cal_y_pred(y_pred,
        yc_mean = private$m_log$stan$yc_mean[[1L]],
        yc_sd = private$m_log$stan$yc_sd[[1L]],
        xf = private$m_log$data_field$input,
        yf = private$m_log$data_field$output,
        merge = merge
    )

    private$m_log$stan$y_pred <- copy(y_pred)
    private$m_log$stan$all <- all

    # remove case column since it only makes since for simulation data
    set(y_pred, NULL, "case", NULL)

    combine_input_output_data(output = y_pred, all = all)$output
}
# }}}
# bc_evaluate {{{
bc_evaluate <- function (super, self, private, funs = list(nmbe, cvrmse), sub_funs = substitute(funs)) {
    nm_y <- names(private$m_log$data_field$output)[-(1:11)]

    y_pred <- bc_prediction(super, self, private, merge = TRUE, all = FALSE)[
        , .SD, .SDcols = c("index", "sample", nm_y, paste(nm_y, "[Prediction]"))]

    # get function names
    nm_fun <- vapply(sub_funs[-1], deparse, character(1))

    # calculate stats per sample
    y_pred[, by = "sample", {
        stats <- lapply(funs, function (fun)
            match.fun(fun)(
                sim = get(paste(nm_y, "[Prediction]")),
                obs = get(nm_y)
            )
        )
        setattr(stats, "names", nm_fun)
    }]
}
# }}}
# bc_stan_run {{{
#' @importFrom stats sd
bc_stan_run <- function (super, self, private, file = NULL, data = NULL, iter = 2000L, chains = 4L,
                         echo = TRUE, mc.cores = parallel::detectCores(), all = FALSE, merge = TRUE, ...) {
    opts <- options(mc.cores = mc.cores)
    on.exit(options(opts), add = TRUE)

    data_bc <- bc_data_bc(super, self, private)

    if (!is.null(file)) {
        data <- if (is.null(data)) data_bc else data
        fit <- rstan::stan(file, data = data,
            chains = chains, iter = iter,
            ...
        )
        private$m_log$stan$custom_model <- TRUE
    } else {
        if (!is.numeric(data_bc$yf) || !is.numeric(data_bc$yc)) {
            abort(paste0(
                "When using builtin Bayesian calibration algorithm, ",
                "only one output variable is supported. ",
                "Invalid output variable number found: ", length(data_bc$yf)
            ), "bc_multi_output")
        }

        fit <- rstan::sampling(stanmodels$bc_with_pred, data = data_bc,
            chains = chains, iter = iter, show_messages = echo,
            ...
        )
        private$m_log$stan$custom_model <- FALSE
    }

    # store
    private$m_log$stan$fit <- fit

    list(fit = fit, y_pred = bc_prediction(super, self, private, all = all, merge = merge))
}
# }}}
# bc_stan_file {{{
bc_stan_file <- function (super, self, private, path = NULL) {
    lic <- system.file("stan/include/license.stan", package = "epluspar", mustWork = TRUE)
    bc <- system.file("stan/bc_with_pred.stan", package = "epluspar", mustWork = TRUE)

    code <- c(readLines(lic), "", readLines(bc))

    if (is.null(path)) return(code)

    if (!dir.exists(dirname(path))) dir.create(dirname(path), recursive = TRUE)

    opts <- options(encoding = "native.enc")
    on.exit(options(opts), add = TRUE)
    writeLines(enc2utf8(code), path, useBytes = TRUE)
    path
}
# }}}

# HELPERS
# input and output
# bc_match_input_output {{{
bc_match_input_output <- function (super, self, private, type = c("input", "output"),
                                   key_value = NULL, name = NULL, reporting_frequency = NULL,
                                   append = FALSE) {
    type <- match.arg(type)
    m_name <- paste("m", type, sep = "_")
    err_type <- paste0("bc_invalid_", type)

    if (is.null(key_value) && is.null(name) && is.null(reporting_frequency)) {
        if (is.null(append)) {
            if (NROW(private[[m_name]])) {
                eplusr:::with_silent(private$m_seed$del(private[[m_name]]$id, .force = TRUE))
            }
            private[[m_name]] <- NULL
        }
        return(private[[m_name]])
    }

    # make sure seed UUID is updated
    on.exit(private$log_seed_uuid(), add = TRUE)

    if (is.null(append)) append <- FALSE

    # check NA
    if (anyNA(key_value)) abort("'key_value' cannot contain any NA.", err_type)
    if (anyNA(name)) abort("'name' cannot contain any NA.", err_type)
    if (anyNA(reporting_frequency)) abort("'reporting_frequency' cannot contain any NA.", err_type)

    # get RDD and MDD
    if (!private$m_log$run_ddy) bc_run_ddy(super, self, private)

    # not specified
    if (is.null(key_value)) {
        key_value <- "*"
    # RddFile or MddFile
    } else if (inherits(key_value, c("RddFile", "MddFile"))) {
        if (type == "input" && inherits(key_value, "MddFile")) {
            abort(paste0("'$input()' only support RddFile. MddFile ",
                "can only be used in '$output()'."
            ), err_type)
        }
        bc_match_input_output_dict(super, self, private, type, append, reporting_frequency, key_value)
        return(private[[m_name]])
    # data.frame for $load()
    } else if (is.data.frame(key_value)) {
        bc_match_input_output_dt(super, self, private, type, append, reporting_frequency, key_value)
        return(private[[m_name]])
    # invalid format
    } else if (!is.character(key_value)) {
        if (type == "input") {
            dict <- "RddFile"
        } else {
            dict <- "RddFile or MddFile"
        }

        abort(paste0("'key_value' should be NULL, ",
            "a character vector, a data.frame or an ", dict, " object."
        ), err_type)
    }

    if (is.null(name)) {
        abort(paste0("Please give variable names via 'name'."), err_type)
    }

    if (type == "input") {
        rmdd <- private$m_log$rdd
    } else {
        rmdd <- rbindlist(list(private$m_log$rdd, private$m_log$mdd))
    }

    dt <- rmdd[J(tolower(name)), on = "variable_lower"]
    set(dt, NULL, "key_value", key_value)

    if (anyNA(dt$variable)) {
        abort(paste0("Invalid variable name found: ", paste0("'", name[is.na(dt$variable)], "'", collapse = ", ")), err_type)
    }

    # check reporting frequency
    if (is.null(reporting_frequency)) reporting_frequency <- "Timestep"
    reporting_frequency <- check_same_report_freq(type, reporting_frequency, bc_report_freq(super, self, private), append)
    set(dt, NULL, "reporting_frequency", reporting_frequency)

    # input cannot contain any duplications
    if (nrow(invld <- dt[duplicated(dt, by = c("variable_lower", "key_value"))])) {
        invld[report_type == "Meter", nm := paste0("'", variable, "'", collapse = ", ")]
        invld[report_type != "Meter", nm := paste0("'", key_value, ":", variable, "'", collapse = ", ")]
        abort(paste0("Duplications found in ", type, " variables: ", invld$nm), err_type)
    }

    # clone the original in case there are errors
    if (!append) ori_idf <- private$m_seed$clone()

    # remove existing if necessary
    bc_clean_existing_input_output(super, self, private, type, append, dt)

    dt[, index := .I]
    # now it's save to load it
    dt_var <- rdd_to_load(setattr(dt[report_type != "Meter"], "class", c("RddFile", class(dt))))
    if (nrow(dt_var)) {
        obj_var <- private$m_seed$load(dt_var, .unique = FALSE)
        dt_var <- private$m_seed$to_table(vapply(obj_var, function (x) x$id(), 1L), wide = TRUE)[, name := NULL]
        dt_var[dt, on = c("Variable Name" = "variable"), index := i.index]
    } else {
        dt_var <- data.table()
    }
    dt_mtr <- mdd_to_load(setattr(dt[report_type == "Meter"], "class", c("MddFile", class(dt))))
    if (nrow(dt_mtr)) {
        obj_mtr <- private$m_seed$load(dt_mtr, .unique = FALSE)
        dt_mtr <- private$m_seed$to_table(vapply(obj_mtr, function (x) x$id(), 1L), wide = TRUE)[, name := NULL]
        setnames(dt_mtr, names(dt_mtr)[[3L]], "Variable Name")
        set(dt_mtr, NULL, "Key Value", NA_character_)
        dt_mtr[dt, on = c("Variable Name" = "variable"), index := i.index]
    } else {
        dt_mtr <- data.table()
    }

    # add index
    dt <- tidy_names(rbindlist(list(dt_var, dt_mtr), use.names = TRUE))
    setcolorder(dt, "index")
    setorderv(dt, "index")

    if (append) {
        bc_combine_input_output(super, self, private, type, append, dt)
    } else {
        tryCatch(bc_combine_input_output(super, self, private, type, append, dt),
            epluspar_error_bc_invalid_input = function (e) {private$m_seed <- ori_idf; stop(e)},
            epluspar_error_bc_invalid_output = function (e) {private$m_seed <- ori_idf; stop(e)}
        )
    }

    private[[m_name]]
}
# }}}
# bc_match_input_output_dict {{{
bc_match_input_output_dict <- function (super, self, private, type, append, reporting_frequency, dict) {
    err_type <- paste0("bc_invalid_", type)
    other_type <- switch(type, input = "output", output = "input")

    if (!nrow(dict)) return()

    if (inherits(dict, "RddFile")) {
        full <- private$m_log$rdd
        load_fun <- eplusr::rdd_to_load
    } else {
        full <- private$m_log$mdd
        load_fun <- eplusr::mdd_to_load
    }

    # input only support RDD
    if (type == "input" && !inherits(dict, "RddFile")) {
        abort(("Input variables can only be output variables, not output meters."), err_type)
    }

    # keep the original untouched
    dict <- copy(dict)

    # init reporting frequency
    if (!"reporting_frequency" %in% names(dict)) {
        set(dict, NULL, "reporting_frequency", "Timestep")
    }
    if (!is.null(reporting_frequency)) {
        set(dict, NULL, "reporting_frequency", reporting_frequency)
    }

    if (!"variable_lower" %in% names(dict)) {
        set(dict, NULL, "variable_lower", tolower(dict$variable))
    }

    # Below should be the same for dt
    dict[full, on = "variable_lower", variable_match := i.variable]
    if (anyNA(dict$variable_match)) {
        abort(paste0("Invaid variable name found ",
            "in input 'key_value': ",
            paste0("'", dict[is.na(variable_match), variable], "'", collapse = "\n"), "."
        ), err_type)
    } else {
        dict[, `:=`(variable = variable_match, variable_match = NULL)]
    }

    input_freq <- unique(dict$reporting_frequency)
    input_freq <- check_same_report_freq(type, input_freq, bc_report_freq(super, self, private), append)
    set(dict, NULL, "reporting_frequency", input_freq)

    if (!"key_value" %in% names(dict)) {
        set(dict, NULL, "key_value", "*")
    } else {
        dict[is.na(key_value), key_value := "*"]
    }

    # clone the original Idf in case there are errors
    if (!append) ori_idf <- private$m_seed$clone()
    bc_clean_existing_input_output(super, self, private, type, append, dict)

    # now it's save to load it
    obj <- private$m_seed$load(load_fun(dict), .unique = FALSE)

    dt <- private$m_seed$to_table(vapply(obj, function (x) x$id(), 1L), wide = TRUE)[, name := NULL]
    if (inherits(dict, "MddFile")) {
        setnames(dt, "Key Name", "Variable Name")
        set(dt, NULL, "Key Value", NA_character_)
    }
    # add index
    dt <- tidy_names(dt[, index := .I])
    setcolorder(dt, "index")

    if (append) {
        bc_combine_input_output(super, self, private, type, append, dt)
    } else {
        tryCatch(bc_combine_input_output(super, self, private, type, append, dt),
            epluspar_error_bc_invalid_input = function (e) {private$m_seed <- ori_idf; stop(e)},
            epluspar_error_bc_invalid_output = function (e) {private$m_seed <- ori_idf; stop(e)}
        )
    }

    private[[paste0("m_", type)]]
}
# }}}
# bc_match_input_output_dt {{{
bc_match_input_output_dt <- function (super, self, private, type, append, reporting_frequency, dt) {
    err_type <- paste0("bc_invalid_", type)
    other_type <- switch(type, input = "output", output = "input")
    cls <- switch(type, input = "Output:Variable", output = c("Output:Variable", "Output:Meter"))
    cols <- c("class", "index", "value")

    if (!all(cols %in% names(dt))) {
        abort(paste0("When 'key_value' is a data.frame, ",
            "it should contains at least 3 columns named 'class', 'index' ",
            "and 'value'. Column ",
            paste0("'", cols[cols %in% names(dt)], "'", collapse = ", "), " ",
            "is/are missing in the input."
        ), err_type)
    }

    # here will copy the input
    dt <- as.data.table(dt)[, .SD, .SDcols = c(
        if ("id" %in% names(dt)) "id" else NULL, cols)
    ]

    if (any(invld <- !dt$class %in% cls)) {
        abort(paste0("When 'key_value' is a data.frame, ",
            "the 'class' column should always be ", paste0(cls, collapse = " and "),
            ". Invalid class name found: ",
            paste0("'", dt[invld, unique(class)], "'", collapse = ", "), "."
        ), err_type)
    }
    if (!is.integer(dt$index) || anyNA(dt$index)) {
        abort(paste0("When 'key_value' is a data.frame, ",
            "the 'index' column should be of integer type without any NA."
        ), err_type)
    }

    # index original input in order to keep the order
    dt[, idx := .GRP, by = c("id", "class")]

    cols <- c("class", "index")
    # check duplications
    if ("id" %in% names(dt)) cols <- c("id", cols)
    if (anyDuplicated(dt, by = cols)) {
        if ("id" %in% names(dt)) {
            cols <- "'id', 'class' and 'index'"
        } else {
            cols <- "'class' and 'index'"
        }
        abort(paste0("When 'key_value' is a data.frame, ", cols,
                " column combined should not contain any duplication."
            ),
            err_type
        )
    }

    if (type == "input") {
        var <- dt
        mtr <- dt[0L]
    } else {
        var <- dt[tolower(class) == "output:variable"]
        mtr <- dt[tolower(class) == "output:meter"]
    }

    var[J(1L, NA_character_), on = c("index", "value"), value := "*"]
    var[index == 1L, field := "key_value"]
    var[index == 2L, field := "variable"]
    var[index == 3L, field := "reporting_frequency"]
    if (var[index > 4L, .N]) {
        abort(paste0("Invalid field number for class Output:Variable: ",
            paste0(var[index > 4L, unique(index)], collapse = "\n"), "."
        ), err_type)
    }
    var <- var[index <= 3L]

    mtr[index == 1L, field := "variable"]
    mtr[index == 2L, field := "reporting_frequency"]
    if (mtr[index > 2L, .N]) {
        abort(paste0("Invalid field number for class Output:Meter: ",
            paste0(mtr[index > 4L, unique(index)], collapse = "\n"), "."
        ), err_type)
    }

    if (nrow(var)) {
        dict_var <- data.table::dcast.data.table(var, idx + id + class ~ field, value.var = "value")
    }
    if (nrow(mtr)) {
        dict_mtr <- data.table::dcast.data.table(mtr, idx + id + class ~ field, value.var = "value")
    }
    if (!nrow(var)) dict_var <- dict_mtr[0L]
    if (!nrow(mtr)) dict_mtr <- dict_var[0L]

    setattr(dict_var, "class", c("RddFile", "data.table", "data.frame"))
    setattr(dict_mtr, "class", c("MddFile", "data.table", "data.frame"))
    setnames(dict_var, "idx", "index")
    setnames(dict_mtr, "idx", "index")

    if (nrow(dict_var)) {
        bc_match_input_output_dict(super, self, private, type, append, reporting_frequency, dict_var)
        bc_match_input_output_dict(super, self, private, type, append = TRUE, reporting_frequency, dict_mtr)
    } else {
        bc_match_input_output_dict(super, self, private, type, append, reporting_frequency, dict_mtr)
    }

    # retain the original order
    (private[[paste0("m_", type)]] <- private[[paste0("m_", type)]][c(dict_var$index, dict_mtr$index)][, index := .I])
}
# }}}
# bc_clean_existing_input_output {{{
bc_clean_existing_input_output <- function (super, self, private, type, append, dt) {
    m_name <- paste("m", type, sep = "_")
    err_type <- paste0("bc_invalid_", type)

    # delete the old
    if (!append) {
        if (NROW(private[[m_name]])) {
            private$m_seed$del(private[[m_name]]$id, .force = TRUE)
        }
    # remove duplicated
    } else {
        if (NROW(private[[m_name]])) {
            all <- rbindlist(list(
                private[[m_name]][, list(index, key_value = tolower(key_value), variable = tolower(variable_name))][
                    , index := -index][is.na(key_value), key_value := "*"],
                dt[, list(index, key_value = tolower(key_value), variable = tolower(variable))]
            ))
        } else {
            all <- dt[, list(index, key_value = tolower(key_value), variable = tolower(variable))]
        }

        if (any(idx <- duplicated(all, by = c("key_value", "variable"), fromLast = TRUE))) {
            invld <- all[idx]
            # check if input variables have been already set
            if (NROW(private[[m_name]])) {
                if (nrow(invld <- invld[index < 0L])) {
                    abort(paste0("Variables specified ",
                            "have already been set as ", type, ". Invalid input found: ",
                        paste0("'", output_var_name(private[[m_name]][-invld$index]), "'", collapse = ", ")
                    ), err_type)
                }
            }

            # check if input variables have duplications
            abort(paste0("Duplications found in ", type, " variables: ", output_var_name(invld)), err_type)
        }

        # any key value being * should not overwrite others with specific names
        if (nrow(key_all <- all[key_value == "*"])) {
            key_all <- all[J(unique(key_all$variable)), on = "variable"]
            # input can not be inserted if there is one with key value being "*"
            if (nrow(invld <- dt[key_all[key_value != "*" & index > 0L, index]])) {
                abort(paste0("Cannot insert ",
                    "new variable when there is an existing one with key value ",
                    "being '*'. Invalid input found: ",
                    paste0("'", invld$key_value, ":", invld$variable, "'", collapse = ", ")
                ), paste0("bc_invalid_", type))
            }

            # input with key value being "*" can not be inserted if there is one
            # with specific key value
            if (NROW(invld <- private[[m_name]][key_all[key_value != "*" & index < 0L, -index]])) {
                abort(paste0("Cannot insert ",
                    "new variable with key value being '*' when there is an ",
                    "existing one with specific key value. Invalid input found: ",
                    paste0("'*:", invld$variable_name, "'", collapse = ", ")
                ), paste0("bc_invalid_", type))
            }
        }
    }
}
# }}}
# bc_combine_input_output {{{
bc_combine_input_output <- function (super, self, private, type, append, dt) {
    m_name <- paste("m", type, sep = "_")
    m_name2 <- paste0("m_", if (type == "input") "output" else "input")

    # check if there are same variables that also appear in the opposite
    if (NROW(private[[m_name2]])) {
        all <- rbindlist(list(
            dt[, list(id, key_value, variable_name)],
            private[[m_name2]][, list(id = -id, key_value, variable_name)]
        ))

        idx <- duplicated(all, by = c("key_value", "variable_name"), fromLast = TRUE)

        if (any(idx)) {
            invld <- dt[idx[1:nrow(dt)]]
            private$m_seed$del(invld$id)
            abort(paste0("Variables specified have already been set as ",
                    if (type == "input") "output" else "input", ": ",
                    paste0("'", output_var_name(invld), "'", collapse = ", ")
                ),
                paste0("bc_invalid_", type)
            )
        }

        if (length(var <- all[key_value == "*", variable_name])) {
            var <- all[J(var), on = "variable_name", list(mixed = id > 0L & id < 0L), by = "variable_name"][mixed == TRUE, variable_name]
            invld <- dt[J(var), on = "variable_name"]
            # input can not be inserted if there is one with key value being "*"
            if (nrow(invld <- invld[id > 0L])) {
                private$m_seed$del(invld$id)
                if (nrow(invld_star <- invld[key_value == "*"])) {
                    abort(paste0("Cannot insert ",
                        "new ", type, " variable with key value being '*' when ",
                        "there is an existing one in ",
                        if (type == "input") "output" else "input",
                        " variables with same variable. Invalid input found: ",
                        paste0("'", invld_star$key_value, ":", invld_star$variable_name, "'", collapse = ", ")
                    ), paste0("bc_invalid_", type))
                } else {
                    abort(paste0("Cannot insert ",
                        "new ", type, " variable when there is an existing one in ",
                        if (type == "input") "output" else "input",
                        " variables with key value being '*'. Invalid input found: ",
                        paste0("'", invld$key_value, ":", invld$variable_name, "'", collapse = ", ")
                    ), paste0("bc_invalid_", type))
                }
            }
        }
    }

    if (append) {
        private[[m_name]] <- rbindlist(list(
            private[[m_name]],
            dt[, index := index + NROW(private[[m_name]])]
        ), use.names = TRUE)
    } else {
        private[[m_name]] <- dt
    }

    setcolorder(private[[m_name]], c("index", "id", "class", "key_value",
        "variable_name", "reporting_frequency"))

    if (!NROW(private[[m_name]])) private[[m_name]] <- NULL
    private[[m_name]]
}
# }}}
# output_var_name {{{
output_var_name <- function (dt) {
    dt[is.na(key_value), out := variable_name]
    dt[!is.na(key_value), out := paste0(key_value, ":", variable_name)]
    on.exit(set(dt, NULL, "out", NULL), add = TRUE)
    dt$out
}
# }}}
# bc_report_freq {{{
bc_report_freq <- function (super, self, private) {
    res <- list(input = NULL, output = NULL)

    if (NROW(private$m_input)) {
        res$input <- eplusr:::validate_report_freq(unique(private$m_input$reporting_frequency))
    }

    if (NROW(private$m_output)) {
        res$output <- eplusr:::validate_report_freq(unique(private$m_output$reporting_frequency))
    }

    res
}
# }}}
# check_same_report_freq {{{
check_same_report_freq <- function (type, freq, old, append) {
    # check reporting frequency
    if (append && !is.null(old[[type]]) && !all(tolower(old[[type]]) == tolower(freq))) {
        abort(paste0(
            "Object specified does not have the same reporting ",
            "frequency as existing ", type, " ('", old[[type]], "'). ",
            "Invalid input reporting frequency: ",
            paste0("'", freq[freq != old[[type]]], "'")
        ), paste0("bc_invalid_", type))
    }
    other <- names(old)[names(old) != type]
    if (!is.null(old[[other]]) && !all(tolower(old[[other]]) == tolower(freq))) {
        abort(paste0(
            "Object specified does not have the same reporting ",
            "frequency as existing ", other, " ('", old[[other]], "'). ",
            "Invalid input reporting frequency: ",
            paste0("'", freq[freq != old[[other]]], "'")
        ), paste0("bc_invalid_", type))
    }
    eplusr:::validate_report_freq(freq)
}
# }}}

# param
# bc_assert_has_sampled {{{
bc_assert_has_sampled <- function (super, self, private, stop = TRUE) {
    if (is.null(private$m_sample)) {
        if (stop) {
            abort(paste0("No LHS samples have been generated. ",
                "Please use '$param()' or '$apply_measure()' to set parameters and ",
                "perform LHS sampling."
            ), "bc_not_ready")
        } else {
            message("No LHS samples have been generated. ",
                "Please use '$param()' or '$apply_measure()' to set parameters and ",
                "perform LHS sampling."
            )
        }
    }
    TRUE
}
# }}}
# lhs_samples {{{
lhs_samples <- function (param, num) {
    # use lhs::randomLHS to generate input
    samples <- as.data.table(lhs::randomLHS(num, nrow(param)))
    setnames(samples, param$param_name)

    # number the case
    samples[, case := .I]
    setcolorder(samples, "case")

    m <- melt.data.table(samples, id.vars = "case", variable.name = "param_name", variable.factor = FALSE)

    # calculate value
    m[param, on = "param_name", `:=`(value = i.min + (i.max - i.min) * value)]

    # recreate the sample
    samples <- dcast.data.table(m, case ~ param_name, value.var = "value")
    setcolorder(samples, c("case", param$param_name))

    samples
}
# }}}

# data
# bc_assert_can_model {{{
bc_assert_can_model <- function (super, self, private, stop = FALSE) {
    if (stop) {
        fun <- function (...) abort(paste0(...), "bc_not_ready")
    } else {
        fun <- message
    }

    # check if input and output variables are added after parameters
    if (is.null(private$m_input)) {
        fun("Unable to create parametric models ",
            "because input variables are not set. Please use '$input()' to set ",
            "input variables.")
        return(FALSE)
    }

    if (is.null(private$m_output)) {
        fun("Unable to create parametric models ",
            "because output variables are not set. Please use '$output()' to set ",
            "output variables.")
        return(FALSE)
    }

    if (is.null(private$m_sample)) {
        fun("Unable to create parametric models ",
            "because no LHS samples have been generated. ",
            "Please use '$param()' or '$apply_measure()' to set parameters and ",
            "perform LHS sampling."
        )
        return(FALSE)
    }

    TRUE
}
# }}}
# bc_assert_can_collect {{{
bc_assert_can_collect <- function (super, self, private, stop = FALSE) {
    if (stop) {
        fun <- function (...) abort(paste0(...), "bc_not_ready")
    } else {
        fun <- message
    }

    if (is.null(private$m_idfs)) {
        fun("No models have been created. Please use $model() to create parametric ",
            "models after input, output and parameters are set."
        )
        return(FALSE)
    }

    # use $output_dir() to perform other checking
    self$eplus_output_dir()

    TRUE
}
# }}}
# bc_assert_can_stan {{{
bc_assert_can_stan <- function (super, self, private, stop = FALSE) {
    if (stop) {
        fun <- function (...) abort(paste0(...), "bc_not_ready")
    } else {
        fun <- message
    }

    # check if input and output variables are added after parameters
    if (is.null(private$m_log$data_sim)) {
        fun("Unable to perform calibration ",
            "because simulated data are not set. Please use '$data_sim()' to retrieve ",
            "simulated input and output data before calling '$stan_run()'.")
        return(FALSE)
    }

    if (is.null(private$m_log$data_field)) {
        fun("Unable to perform calibration ",
            "because field data are not set. Please use '$data_field()' to specify ",
            "measured input and output data before calling '$stan_run()'.")
        return(FALSE)
    }

    TRUE
}
# }}}
# bc_assert_valid_resolution {{{
bc_assert_valid_resolution <- function (super, self, private, resolution) {
    freq <- bc_report_freq(super, self, private)$input

    # get current resolution
    if (freq == "Timestep") {
        cur_res <- as.integer(60 / private$m_seed$Timestep$Number_of_Timesteps_per_Hour)
        err_res <- paste0(cur_res, " min")
    } else if (freq == "Hourly") {
        cur_res <- 60
        err_res <- "1 hour"
    } else if (freq == "Daily") {
        cur_res <- 60 * 24
        err_res <- "1 day"
    } else if (freq == "Monthly") {
        cur_res <- 60 * 24 * 31
        err_res <- "1 month"
    } else {
        abort(paste0("Cannot change data resolution ",
            "when variable reporting frequency is '", freq, "'"
        ), "bc_cannot_resample")
    }

    in_res <- standardize_resolution(resolution)

    if (cur_res > in_res) {
        abort(paste0("Input resolution should ",
            "not be smaller than reporting frequency (", err_res, "). ",
            "Invalid resolution found: ", paste0("'", resolution, "'")
        ), "bc_invalid_resolution")
    }

    if (in_res %% cur_res) {
        abort(paste0("Input resolution should ",
            "be divisible by reporting frequency (", err_res, "). ",
            "Invalid resolution found: ", paste0("'", resolution, "'")
        ), "bc_invalid_resolution")
    }

    TRUE
}
# }}}
# bc_assert_valid_measured {{{
bc_assert_valid_measured <- function (super, self, private, dt, type = c("new_input", "output"), check_row = TRUE) {
    if (is.null(private$m_log$data_sim)) {
        abort(paste0("Field data should be specified ",
            "after collecting simulation data in order to perform validity checking. ",
            "Please run '$data_sim()' first."
        ), "bc_empty_data_sim")
    }

    type <- match.arg(type)
    m_name <- if (type == "new_input") "input" else "output"
    err_type <- paste0("bc_invalid_data_field_", type)

    if (!is.data.frame(dt)) {
        abort(paste0("'", type, "' should be ",
            "a data.frame. Invalid type: ", class(dt)[1L], "."
        ), err_type)
    }

    # should exclude 11 columns:
    # "case", "environment_period_index", "environment_name", "simulation_days",
    # "datetime", "month", "day", "hour", "minute", "day_type", "Date/Time"
    if (ncol(dt) != (ncol(private$m_log$data_sim[[m_name]]) - 11L)) {
        abort(paste0("The column number of ",
            "'", type, "' should be the same as ", m_name, " variables (",
            ncol(private$m_log$data_sim[[m_name]]) - 11L, "). ",
            "Invalid column number: ", ncol(dt), "."
        ), err_type)
    }

    if (check_row) {
        # count row number
        case_count <- private$m_log$data_sim[[m_name]][, list(n = .N), by = "case"]

        if (nrow(dt) != unique(case_count$n)) {
            abort(paste0("The row number of ",
                "'", type, "' should be the same as ", m_name, " variables (",
                unique(case_count$n), "). Invalid row number: ", nrow(dt), "."
            ), err_type)
        }
    }

    TRUE
}
# }}}
# bc_check_data {{{
bc_check_data <- function (super, self, private, data, type = c("sim", "field")) {
    type <- match.arg(type)
    m_name <- paste0("data_", type)
    err_type <- paste0("bc_invalid_", m_name)
    ori <- private$m_log[[m_name]]

    if (type == "sim") {
        len <- 2L
        nm <- c("input", "output")
    } else {
        len <- 3L
        nm <- c("input", "output", "new_input")
    }

    if (!is.list(data)) {
        abort(paste0("'", m_name, "' should be a list. ",
            "Invalid input class: '", class(data)[[1L]], "'."
        ), err_type)
    }
    if (length(data) != len) {
        abort(paste0("'", m_name, "' should be a list of ", len, ". ",
            "Invalid input length: '", length(data), "'."
        ), err_type)
    }
    if (!all(names(data) %in% nm)) {
        abort(paste0("'", m_name, "' should be a list of 2 ",
            "named element 'input' and 'output'. Invalid element found: ",
            paste0("'", names(data)[!names(data) %in% nm], "'", collapse = ", ")
        ), err_type)
    }

    for (name in nm) {
        if (!is.data.frame(data[[name]])) {
            abort(paste0("'", name, "' of '", m_name, "' should be a ",
                "data.frame. Invalid '", name, "' type: '", class(data[[name]])[[1L]], "'."
            ), err_type)
        }

        data[[name]] <- as.data.table(data[[name]])

        # remove meta columns
        meta <- names(ori[[name]])[1L:11L]
        if (length(meta_in <- intersect(names(data[[name]]), meta))) {
            set(data[[name]], NULL, meta_in, NULL)
        }

        if (ncol(data[[name]]) != (ncol(ori[[name]]) - 11L)) {
            abort(paste0("'", name, "' of '", m_name, "' should have the same variable ",
                "number (", ncol(ori[[name]]) - 11L, ") as in '$", m_name, "()$", name, "'. ",
                "Invalid variable number: ", ncol(data[[name]]), "."
            ), err_type)
        }
        if (nrow(data[[name]]) != nrow(ori[[name]])) {
            abort(paste0("'", name, "' of '", m_name, "' should have the same row ",
                "number (", nrow(ori[[name]]), ") as in '$", m_name, "()$", name, "'. ",
                "Invalid row number: ", nrow(data[[name]]), "."
            ), err_type)
        }

        type_in <- unlist(data[[name]][, lapply(.SD, function (x) typeof(x))])
        type <- unlist(ori[[name]][, lapply(.SD, function (x) typeof(x)), .SDcols = -(1L:11L)])
        if (any(invld <- type_in != type)) {
            idx <- which(invld)
            abort(paste0("'", name, "' of '", m_name, "' should have the same variable ",
                "type as in '$", m_name, "()$", name, "'. ",
                "Invalid column type: ",
                paste0("'", names(invld)[idx], "' type '", type_in[idx], "' (should be '", type[idx], "')", collapse = ", "), "."
            ), err_type)
        }

        # add meta columns
        set(data[[name]], NULL, meta, ori[[name]][, .SD, .SDcols = 1L:11L])
        setcolorder(data[[name]], meta)
    }

    data
}
# }}}
# bc_extract_report_data {{{
bc_extract_report_data <- function (super, self, private, type = c("input", "output"), exclude_ddy = TRUE) {
    m_name <- paste0("m_", type)

    key_all <- private[[m_name]][key_value == "*" | is.na(key_value)]
    key_spe <- private[[m_name]][!key_all, on = "index"]
    if (nrow(key_all)) {
        dt_all <- super$report_data(name = key_all$variable_name, all = TRUE)
        if (nrow(dt_all)) {
            set(dt_all, NULL, "case", as.integer(gsub("^Case(\\d+).*", "\\1", dt_all$case)))
        }
    } else {
        dt_all <- data.table()
    }
    # for specific
    if (nrow(key_spe)) {
        dt_spe <- super$report_data(NULL, key_spe$key_value, key_spe$variable_name, all = TRUE)
        # check if there are invalid key value
        # have to change to upper case becase there are some \retaincase
        # varaible, like `Environment:Site Outdoor Air Drybulb Temperature`.
        m <- dt_spe[, key_value_upper := toupper(key_value)][key_spe[, key_value_upper := toupper(key_value)], on = "key_value_upper", mult = "first"]
        if (anyNA(m$value)) {
            abort(paste0("Failed to extract ",
                "simulation data of ", type, " variables. Invalid variable specification found: ",
                paste0("'", output_var_name(m), "'", collapse = ", ")
            ), paste0("bc_", type, "_invalid_key_value"))
        }
        set(dt_spe, NULL, "key_value_upper", NULL)
        if (nrow(dt_spe)) {
            set(dt_spe, NULL, "case", as.integer(stringi::stri_extract_first_regex(dt_spe$case, "\\d+")))
        }
    } else {
        dt_spe <- data.table()
    }

    # combine
    dt <- rbindlist(list(dt_all, dt_spe))
    setorderv(dt, "case")

    # make sure each case gives same output rows
    count <- dt[, list(n = .N), by = "case"]
    if (nrow(dt) && length(unique(count$n)) != 1L) {
        abort(paste0("Internal error found when ",
            "extracting simulation data. Each case should give the same row number ",
            "of report variable data. If you use '$apply_measure()' to set parameters, ",
            "please make sure your measure does not result in different 'Timestep', ",
            "'RunPeriod' or other objects that can effect the report variable data."
        ), "bc_data_sim_row_not_same")
    }

    if (exclude_ddy) {
        dt[!J(c("SummerDesignDay", "WinterDesignDay")), on = "day_type"]
    } else {
        dt
    }
}
# }}}
# bc_retain_variable_order {{{
bc_retain_variable_order <- function (super, self, private, dt, type = c("input", "output")) {
    type <- match.arg(type)
    m_name <- paste0("m_", type)

    nm <- names(dt)
    nm_meta <- c("case", "environment_period_index", "environment_name",
        "simulation_days", "datetime", "month", "day",
        "hour", "minute", "day_type", "Date/Time"
    )

    nm_var <- setdiff(nm, nm_meta)
    nm_sp <- strsplit(nm_var, "[:\\[]")
    dt_nm <- data.table(full_name = nm_var,
        key_value = toupper(trimws(vapply(nm_sp, "[", "", 1L))),
        variable_name = trimws(vapply(nm_sp, "[", "", 2L))
    )

    dt_nm_all <- private[[m_name]][key_value == "*" | is.na(key_value)][dt_nm, on = "variable_name", nomatch = 0L]
    dt_nm_spe <- private[[m_name]][!dt_nm_all, on = "index"][, key_value := toupper(key_value)][
        dt_nm, on = c("key_value", "variable_name"), nomatch = 0L]

    dt_nm <- rbindlist(list(dt_nm_all[, list(index, full_name)], dt_nm_spe[, list(index, full_name)]))
    setorderv(dt_nm, "index")

    setcolorder(dt, c(nm_meta, dt_nm$full_name))
}
# }}}
# report_dt_aggregate {{{
#' @importFrom lubridate ceiling_date
report_dt_aggregate <- function (dt, resolution) {
    set(dt, NULL, "datetime", lubridate::ceiling_date(dt$datetime, resolution))

    dt_avg <- suppressWarnings(dt[J("Avg"), on = "type", nomatch = 0L,
        list(simulation_days = max(simulation_days), value = mean(value),
             month = month[.N], day = day[.N],
             hour = hour[.N], minute = minute[.N],
             day_type = NA_character_, units = units[.N]
        ),
        by = c("case", "datetime", "environment_period_index", "environment_name",
            "key_value", "name", "is_meter")
    ])
    dt_sum <- suppressWarnings(dt[!J("Avg"), on = "type",
        list(simulation_days = max(simulation_days), value = sum(value),
             month = month[.N], day = day[.N],
             hour = hour[.N], minute = minute[.N],
             day_type = NA_character_, units = units[.N]
        ),
        by = c("case", "datetime", "environment_period_index", "environment_name",
            "key_value", "name", "is_meter")
    ])
    dt <- rbindlist(list(dt_avg, dt_sum))

    spec <- parse_unit_spec(resolution)
    set(dt, NULL, "reporting_frequency", paste(spec$mult, tools::toTitleCase(spec$unit)))
}
# }}}
# combine_input_output_data {{{
combine_input_output_data <- function (input = NULL, output = NULL, all = FALSE) {
    if (!all) {
        cols <- c("environment_period_index", "environment_name", "simulation_days",
            "datetime", "month", "day", "hour", "minute", "day_type")

        if (!is.null(input) && length(cols_del <- intersect(names(input), cols))) {
            set(input, NULL, cols_del, NULL)
        }
        if (!is.null(output) && length(cols_del <- intersect(names(output), cols))) {
            set(output, NULL, cols_del, NULL)
        }
    }

    list(input = input, output = output)
}
# }}}
# standardize_resolution {{{
standardize_resolution <- function (resolution) {
    spec <- parse_unit_spec(resolution)

    up <- c("min" = 1, "hour" = 60,
        c("day" = 1, "week" = 7, "month" = 31, "year" = 365) * 60 * 24
    )

    spec$mult * up[spec$unit]
}
# }}}
# parse_unit_spec {{{
# borrowed from scales:::parse_unit_spec
parse_unit_spec <- function(unitspec) {
    parts <- strsplit(unitspec, " ")[[1L]]
    if (length(parts) == 1L) {
        mult <- 1
        unit <- unitspec
    } else {
        mult <- as.numeric(parts[[1L]])
        unit <- parts[[2]]
    }
    unit <- gsub("s$", "", unit)

    all_spec <- c("min", "hour", "day", "week", "month", "year")
    if (!unit %in% all_spec) {
        abort(paste0("Resolution unit should be one of ",
            paste0("'", all_spec, "'", collapse = ", "), ". Invalid resolution ",
            "unit found: ", "'", unit, "'"), "bc_invalid_resolution"
        )
    }

    list(unit = unit, mult = mult)
}
# }}}
# init_bc_data {{{
init_data_bc <- function (yf, xf, x_pred, yc, xc, tc) {

    # meta {{{
    # number of output parameters
    d <- ncol(yf)
    # number of input parameters
    p <- ncol(xf)
    # number of measured parameter observations
    n <- nrow(xf)
    # number of newly design points for predictions
    n_pred <- nrow(x_pred)
    # number of simulated observations
    m <- nrow(xc)
    # number of calibration parameters
    q <- ncol(tc)
    # }}}

    # z-score normalization on output parameter yf and eta {{{
    zscore_norm <- function (x, mean, sd) (x - mean) / sd

    yf_std <- copy(yf)
    yc_std <- copy(yc)
    yc_mean <- yc[, lapply(.SD, mean)]
    yc_sd <- yc[, lapply(.SD, sd)]
    for (i in seq.int(d)) {
        set(yf_std, NULL, i, zscore_norm(yf_std[[i]], yc_mean[[i]], yc_sd[[i]]))
        set(yc_std, NULL, i, zscore_norm(yc_std[[i]], yc_mean[[i]], yc_sd[[i]]))
    }
    # }}}

    # min-max normalization on input parameter xf, xc and x_pred {{{
    minmax_norm <- function (x, min, max) (x - min) / (max - min)

    xf_std <- copy(xf)
    xc_std <- copy(xc)
    x_pred_std <- copy(x_pred)

    x <- rbindlist(list(xf, xc))
    x_min <- x[, lapply(.SD, min)]
    x_max <- x[, lapply(.SD, max)]
    for (i in seq.int(p)) {
        set(xf_std, NULL, i, minmax_norm(xf_std[[i]], x_min[[i]], x_max[[i]]))
        set(xc_std, NULL, i, minmax_norm(xc_std[[i]], x_min[[i]], x_max[[i]]))
        set(x_pred_std, NULL, i, minmax_norm(x_pred_std[[i]], x_min[[i]], x_max[[i]]))
    }
    # }}}

    # min-max normalization on input parameter tc {{{
    tc_std <- copy(tc)
    tc_min <- tc[, lapply(.SD, min)]
    tc_max <- tc[, lapply(.SD, max)]
    for (i in seq.int(q)) {
        set(tc_std, NULL, i, minmax_norm(tc_std[[i]], tc_min[[i]], tc_max[[i]]))
    }
    # }}}

    # create data as list for input to Stan {{{
    stan_data <- list(
        # number of measured parameter observations
        n = n,
        # number of newly design points for predictions
        n_pred = n_pred,
        # number of simulated observations
        m = m,
        # number of input parameters
        p = p,
        # number of calibration parameters
        q = q,
        # measured output
        yf = yf_std,
        # simulated output
        yc = yc_std,
        # measured input
        xf = xf_std,
        # simulated input
        xc = xc_std,
        # new design points for predictions
        x_pred = x_pred_std,
        # calibration parameters
        tc = tc_std
    )

    # TODO: update this and also the doc when the multiple output code with
    # prediction is tested
    if (d > 1L) {
        # add number of output parameters
        stan_data <- c(stan_data, list(D = d))
        stan_data <- stan_data[!names(stan_data) %in% c("n_pred", "x_pred")]
    } else {
        stan_data$yf <- unlist(stan_data$yf, use.names = FALSE)
        stan_data$yc <- unlist(stan_data$yc, use.names = FALSE)
    }
    # }}}

    list(stan_data = stan_data, yc_mean = yc_mean, yc_sd = yc_sd, tc_min = tc_min, tc_max = tc_max)
}
# }}}
# cal_y_pred {{{
cal_y_pred <- function (yc_pred, yc_mean, yc_sd, xf, yf, merge = TRUE) {
    y_pred <- as.data.table(yc_pred * yc_sd + yc_mean)

    # add additional meta columns
    y_pred <- melt.data.table(y_pred, measure.vars = names(y_pred),
        variable.name = "index", value.name = "pred", variable.factor = FALSE
    )
    y_pred[, index := as.integer(gsub("V", "", index))]
    # add sample index
    y_pred[, sample := seq_len(.N), by = "index"]
    setcolorder(y_pred, c("index", "sample"))

    if (merge) {
        # rename prediction
        setnames(y_pred, c("index", "sample", paste(names(yf)[-(1L:11L)], "[Prediction]")))

        # combine field input, output and prediction
        y_pred <- copy(xf)[, index := .I ][
            yf[, .SD, .SDcols = -c(1L:11L)][, index := .I], on = "index"][
            y_pred, on = "index"]

        # fix column order
        setcolorder(y_pred, c("index", "sample", setdiff(names(y_pred), c("index", "sample"))))
    } else {
        # rename prediction
        setnames(y_pred, c("index", "sample", names(yf)[-(1L:11L)]))

        # add type to distinguish field and predicted
        y_pred <- rbindlist(use.names = TRUE, list(
            copy(xf)[, index := .I ][
                yf[, .SD, .SDcols = -c(1L:11L)][, index := .I], on = "index"][
                , `:=`(type = "field", sample = NA_integer_)],
            copy(xf)[, index := .I ][y_pred, on = "index"][
                , `:=`(type = "prediction")]
        ))
        # fix column order
        setcolorder(y_pred, c("index", "sample", "type", setdiff(names(y_pred), c("index", "sample", "type"))))
    }

    y_pred
}
# }}}
# stats {{{
rmse <- function (sim, obs) {
    sqrt(sum((sim - obs)^2, na.rm = TRUE) / (length(sim) - 1))
}

cvrmse <- function (sim, obs) {
    rmse(sim, obs) / mean(obs, na.rm = TRUE)
}

nmbe <- function(sim, obs){
  sum(sim - obs, na.rm = TRUE) / ((length(sim) - 1) * mean(obs, na.rm = TRUE))
}
# }}}
