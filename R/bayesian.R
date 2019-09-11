#' @importFrom lhs randomLHS
#' @importFrom data.table copy rbindlist setcolorder as.data.table dcast.data.table
#' @importFrom data.table setnames setorderv melt.data.table rleidv setattr
NULL

# sampleQuality {{{
sampleQuality <- function(sample, population, k) {
    # k: ncol(population)
    KL <- rep(NA,k)
    for(ii in (1:k)){
        Y1 <- sample[,ii]
        Y2 <- population[,ii]
        nr <- length(Y2)
        r <- range(Y2)
        Y1.dis <- entropy::discretize(Y1,numBins = nr^(1/3), r=r)
        Y2.dis <- entropy::discretize(Y2,numBins = nr^(1/3), r=r)
        #KL[ii] <- KL.Dirichlet(Y1.dis,Y2.dis,a1=1/length(Y1),a2=1/length(Y2)) # Schurmann-Grassberger (1996) entropy estimator
        KL[ii] <- KL.Dirichlet(Y1.dis,Y2.dis,a1=1,a2=1) # KL divergence with laplace prior
        #p1 <- as.data.frame(freqs.Dirichlet(Y1.dis+1, 0))$Freq
        #p2 <- as.data.frame(freqs.Dirichlet(Y2.dis+1, 0))$Freq
        #KL[ii] <- sum((p1-p2)*(log(p1)-log(p2)))
    }
    return(exp(-mean(KL)))
}
# }}}

#' Conduct Bayesian Calibration on an EnergyPlus Model
#'
#' `bayes_job()` takes an IDF and EPW as input and returns a `BayesCalibJob`,
#' which provides a prototype of conducting Bayesian calibration of EnergyPlus
#' model.
#'
#' @section Usage:
#' ```
#' bc <- bayes_job(idf, epw)
#' bc$version()
#' bc$seed()
#' bc$weather()
#' bc$read_rdd(update = FALSE)
#' bc$read_mdd(update = FALSE)
#' bc$input(key_value = NULL, name = NULL, reporting_frequency = NULL, append = FALSE)
#' bc$output(key_value = NULL, name = NULL, reporting_frequency = NULL, append = FALSE)
#' bc$param(..., .names = NULL, .num_sim = 30L)
#' bc$apply_measure(measure, ..., .names = NULL, .num_sim = 30L)
#' bc$samples()
#' bc$models()
#' bc$eplus_run(dir = NULL, run_period = NULL, wait = TRUE, force = FALSE,
#'              copy_external = FALSE, echo = wait)
#' bc$data_sim(resolution = NULL, exclude_ddy = TRUE, merge = FALSE, all = FALSE)
#' bc$data_field(output, new_input = NULL, merge = FALSE, all = FALSE)
#' bc$stan_run(iter = 2000L, chains = 4L, wait = TRUE, echo = wait, ...)
#' bc$stan_kill()
#' bc$stan_status()
#' bc$eplus_kill()
#' bc$eplus_status()
#' bc$eplus_output_dir(which = NULL)
#' bc$eplus_locate_output(which = NULL, suffix = ".err", strict = TRUE)
#' bc$eplus_errors(which = NULL, info = FALSE)
#' bc$eplus_report_data_dict(which = NULL)
#' bc$eplus_report_data(which = NULL, key_value = NULL, name = NULL,
#'                      year = NULL, tz = "UTC", all = FALSE, wide = FALSE,
#'                      period = NULL, month = NULL, day = NULL, hour = NULL, minute = NULL,
#'                      interval = NULL, simulation_days = NULL, day_type = NULL,
#'                      environment_name = NULL)
#' bc$eplus_tabular_data(which = NULL, report_name = NULL, report_for = NULL,
#'                       table_name = NULL, column_name = NULL, row_name = NULL)
#' bc$eplus_save(dir = NULL, separate = TRUE, copy_external = FALSE)
#' bc$print()
#' ```
#'
#' @section Create:
#' ```
#' bc <- bayes_job(idf, epw)
#' ```
#'
#' When calling `bayes_job()`, the objects of classes related in output variable
#' reporting will be deleted, in order to make sure all input and output
#' variable specifications can be achieved using `Output:Variable` and
#' `Output:Meter`. Classes to be deleted include:
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
#' **Arguments**
#'
#' * `idf`: Path to EnergyPlus IDF file or an `Idf` object.
#' * `epw`: Path to EnergyPlus EPW file or an `Epw` object.
#'
#' @section Get Seed Model and Weather:
#' ```
#' param$version()
#' param$seed()
#' param$weather()
#' ```
#'
#' `$version()` returns the version of input [Idf] object.
#'
#' `$seed()` returns the input [Idf] object.
#'
#' `$weather()` returns the input [Epw] object.
#'
#' @section Get RDD amd MDD Data of Seed Model:
#' ```
#' param$read_rdd(update = FALSE)
#' param$read_mdd(update = FALSE)
#' ```
#'
#' `$read_rdd()` and `$read_mdd()` silently runs EnergyPlus using input seed
#' model with design-day-only mode to create `.rdd` and `.mdd` file and returns
#' the corresponding [RddFile][eplusr::read_rdd()] and
#' [MddFile][eplusr::read_mdd()] object, respectively. The `RddFile` and
#' `MddFile` object is stored internally and will be directly returned whenever
#' you call `$read_rdd()` and `$read_mdd()` again. You can force to run the
#' design-day-only simulation again to update the contents by setting `update`
#' to `TRUE`.
#'
#' `$read_rdd()` and `read_mdd()` is useful when adding input and output
#' parameters.
#'
#' **Arguments**
#'
#' * `update`: Whether to run the design-day-only simulation and parse `.rdd`
#'   and `.mdd` file again. Default: `FALSE`.
#'
#' @section Set Input and Output Parameters:
#' ```
#' bc$input(key_value = NULL, name = NULL, reporting_frequency = NULL, append = FALSE)
#' bc$output(key_value = NULL, name = NULL, reporting_frequency = NULL, append = FALSE)
#' ```
#'
#' `$input()` and `$output()` takes input and output parameter definitions in a
#' similar pattern as you set output variables in `Output:Variable` and
#' `Output:Meter` class and returns a [data.table][data.table::data.table()]
#' containing the information of input and output parameters. For `$input()`,
#' only variables in RDD are allowed. For `$output()`, both variables in RDD and
#' MDD are allowd. The returned data.table has 5 columns:
#'
#' * `index`: Indices of input or output parameters.
#' * `class`: The class that parameters belong to. Will be either
#'   `Output:Variable` or `Output:Meter`.
#' * `key_value`: Key value name for variables.
#' * `variable_name`: Variable names listed in RDD or MDD.
#' * `reporting_frequency`: Variable reporting frequency.
#'
#' If calling without any argument, the existing input and output parameters are
#' directly returned, e.g. `bc$input()` and `bc$output()`.
#'
#' You can remove all existing input and output parameter by setting `append` to
#' `NULL`, e.g. `bc$input(append = NULL)` and `bc$output(append = NULL)`.
#'
#' `key_value` accepts 3 different formats:
#'
#' * A character vector.
#' * An `RddFile` or an `MddFile` object. They can be retrieved using
#'   `$read_rdd()` and `$read_mdd()`. In this case, `name` argument will be
#'   ignored, as its values are directly taken from variable names in input
#'   `RddFile` and `MddFile` object. For example:
#'   ```
#'   bc$input(bc$read_rdd()[1:5])
#'   bc$output(bc$read_mdd()[1:5])
#'   ```
#' * A [data.frame()] with valid format for adding `Output:Variable` and
#'  `Output:Meter` objects using [Idf$load()][eplusr::Idf]. In this case, `name`
#'  argument will be ignored. For example:
#'  ```
#'  bc$input(eplusr::rdd_to_load(bc$read_rdd()[1:5]))
#'  bc$output(eplusr::mdd_to_load(bc$read_mdd()[1:5]))
#'  ```
#'
#' **Arguments**
#'
#' * `key_value`: Key value name for variables. If not specified, `"*"` are used
#'   for all variables. `key_value` can also be an `RddFile`, `MddFile` or a
#'   [data.frame()]. Please see description above.
#' * `name`: Variable names listed in RDD or MDD.
#' * `reporting_frequency`: Variable reporting frequency for **all** variables.
#'   If `NULL`, `"Timestep"` are used for all variables. All possible
#'   values: `"Detailed"`, `"Timestep"`, `"Hourly"`, `"Daily"`, `"Monthly"`,
#'   `"RunPeriod"`, `"Environment"`, and `"Annual"`. Default: `NULL`.
#' * `append`: Whether to append input variables at the end of
#'   existing ones. A special value `NULL` can be given to remove all existing
#'   parameters. Default: `FALSE`.
#'
#' @section Set Parameters:
#' ```
#' bc$param(..., .names = NULL, .num_sim = 30L)
#' bc$apply_measure(measure, ..., .names = NULL, .num_sim = 30L)
#' bc$samples()
#' bc$models()
#' ```
#'
#' There are 2 ways to set calibration parameters in `BayesCalibJob` class,
#' i.e. `$param()` and `$apply_measure()`.
#'
#' `$param()` takes parameter definitions in list format, which is similar to
#' `$set()` in [eplusr::Idf] class except that each field is not assigned with a
#' single value, but a numeric vector of length 2, indicating the minimum and
#' maximum of the parameter. Every list in `$param()` should be named with a
#' valid object name. Object ID can also be used but have to be combined with
#' prevailing two periods `..`, e.g. `..10` indicates the object with ID `10`.
#'
#' There is two special syntax in `$param()`:
#'
#' * `class := list(field = value)`: Note the use of `:=` instead of `=`. The
#'   main difference is that, unlike `=`, the left hand side of `:=` should be a
#'   valid class name in the seed model. It will treat the specified field of
#'   all objects in specified class to as a single calibration parameter.
#' * `.(object, object) := list(field = value)`: Simimar like above, but note
#'   the use of `.()` in the left hand side. You can put multiple object ID or
#'   names in `.()`. It will treat the field of all specified objects to as a
#'   single calibration parameter.
#'
#' For example, the code block below defines 4 calibration parameters:
#'
#' * Field `Fan Total Efficiency` in object named `Supply Fan 1` in class
#'   `Fan:VariableVolume` class, with minimum and maximum being 0.1 and 1.0,
#'   respectively.
#' * Field `Thickness` in all objects in class `Material`, with minimum and
#'   maximum being 0.01 and 1.0, respectively.
#' * Field `Conductivity` in all objects in class `Material`, with minimum and
#'   maximum being 0.1 and 0.6, respectively.
#' * Field `Watts per Zone Floor Area` in objects `Light1` and `Light2` in class
#'   `Lights`, with minimum and maximum being 10 and 30, respectively.
#'
#' ```
#' bc$param(
#'     `Supply Fan 1` = list(Fan_Total_Efficiency = c(min = 0.1, max = 1.0)),
#'     Material := list(Thickness = c(0.01, 1), Conductivity = c(0.1, 0.6)),
#'    .("Light1", "Light2") := list(Watts_per_Zone_Floor_Area = c(10, 30))
#' )
#' ```
#'
#' **Arguments**
#'
#' * `...`: Lists of paramter definitions. Each list should be named with a valid
#'   object name or a valid object ID denoted in style `..1`, `..2` and etc. For
#'   specifying the fields for all objects in a class, the class name instead of
#'   the object name should be used, and a special notation `:=` should be used
#'   instead of `=`, e.g. `class := list(field = value)`. For grouping fields
#'   from different objects in the same class, use `.()` in the left hand side
#'   and put object ID or names inside., .e.g `.(object1, object2) := list(field
#'   = value)`.
#' * `.num_sim`: An positive integer specifying the number of simulations to run
#'   for each value of calibration parameter value. (NOT CORRECT). Default:
#'   `30L`.
#' * `.names`: A character vector of the parameter names. If `NULL`,
#'   the parameter will be named in format `t + number`. Default: `NULL`.
#'
#' `$apply_measure()` works in a similar way as the `$apply_measure` in
#' [eplusr::ParametricJob] class, with only exception that each argument
#' supplied in `...` should be a numeric vector of length 2, indicating the
#' minimum and maximum of the calibration parameter.
#' Basically `$apply_measure()` allows to apply a measure to an [Idf].
#' A measure here is just a function that takes an [Idf] object and other
#' arguments as input, and returns a modified [Idf] object as output. The names
#' of function parameter will be used as the names of calibration parameter. For
#' example, the equivalent version of specifying parameters described above
#' using `$apply_measure()` can be:
#'
#' ```
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
#' bc$apply_measure(measure,
#'     efficiency = c(min = 0.1, max = 1.0),
#'     thickness = c(0.01, 1), conductivity = c(0.1, 0.6),
#'     lpd = c(10, 30)
#' )
#' ```
#'
#' **Arguments**
#'
#' * `measure`: A function that takes an `Idf` and other arguments as input and
#'     returns an `Idf` object as output.
#' * `...`: Arguments **except first `Idf` argument** that are passed to that
#'   `measure`.
#' * `.num_sim`: An positive integer specifying the number of simulations to run
#'   for each value of calibration parameter value. (NOT CORRECT). Default:
#'   `30L`.
#' * `.names`: A character vector of the parameter names. If `NULL`,
#'   the parameter names will be the same as function parameters of `measure`.
#'
#' All models created using `$param()` and `$apply_measure()` will be named in
#' the same pattern, i.e. `Case_ParameterName(ParamterValue)...`. Note that only
#' paramter names will be abbreviated using [abbreviate()] with `minlength`
#' being `5L` and `use.classes` being `TRUE`. If samples contain duplications,
#' [make.unique()] will be called to make sure every model has a unique name.
#'
#' `$samples()` returns a [data.table][data.table::data.table()] which contains
#' the sampled value for each parameter using [Random Latin Hypercube
#' Sampling][lhs::randomLHS] method. The returned data.table has `1 + n`
#' columns, where `n` is the parameter number, and `1` indicates an extra column
#' named `case` giving the index of each sample. Note that if `$samples()` is
#' called before input and output parameters being set using `$input()` and
#' `$output()`, only the sampling will be performed and no parametric models
#' will be created. This is because information of input and output parameters
#' are needed in order to make sure that corresponding variables will be
#' reported during simulations. In this case, you can use `$models()` to create
#' those models.
#'
#' `$models()` returns a list of parametric [Idf][eplusr::Idf] objects created
#' using calibration parameter values genereated using Random Latin Hypercube
#' Sampling. As stated above, parametric models can only be created after input,
#' output and calibration parameters have all be set using `$input()`,
#' `$output()` and `$param()` (or `$apply_measure()`), respectively.
#'
#' All models will be named in the same pattern, i.e.
#' `Case_ParameterName(ParamterValue)...`. Note that paramter names will be
#' abbreviated using [abbreviate()] with `minlength` being `5L` and
#' `use.classes` being `TRUE`.
#'
#' @section Run Parametric Simulations:
#' ```
#' bc$eplus_run(dir = NULL, run_period = NULL, wait = TRUE, force = FALSE,
#'              copy_external = FALSE, echo = wait)
#' ```
#'
#' `$eplus_run()` runs all parametric models in parallel. Parameter `run_period`
#' can be given to insert a new `RunPeriod` object. In this case, all existing
#' `RunPeriod` objects in the seed model will be commented out.
#'
#' Note that when `run_period` is given, value of field `Run Simulation for
#' Weather File Run Periods` in `SimulationControl` class will be reset to `Yes`
#' to make sure input run period can take effect."
#'
#' **Arguments**
#'
#' * `dir`: The parent output directory for specified simulations. Outputs of
#'   each simulation are placed in a separate folder under the parent directory.
#'   If `NULL`, directory of seed model will be used. Default: `NULL`.
#' * `run_period`: A list giving a new `RunPeriod` object definition. If not
#'   `NULL`, only this new RunPeriod will take effect with all existing
#'   RunPeriod objects in the seed model being commented out. If `NULL`,
#'   existing run period in the seed model will be used. Default: `NULL`.
#' * `wait`: If `TRUE`, R will hang on and wait all EnergyPlus simulations
#'   finish. If `FALSE`, all EnergyPlus simulations are run in the background.
#'   Default: `TRUE`.
#' * `force`: Only applicable when the last simulation runs with `wait` equals
#'   to `FALSE` and is still running. If `TRUE`, current running job is
#'   forced to stop and a new one will start. Default: `FALSE`.
#' * `copy_external`: If `TRUE`, the external files that every `Idf` object
#'   depends on will also be copied into the simulation output directory. The
#'   values of file paths in the Idf will be changed automatically. Currently,
#'   only `Schedule:File` class is supported.  This ensures that the output
#'   directory will have all files needed for the model to run. Default is
#'   `FALSE`.
#' * `echo`: Only applicable when `wait` is `TRUE`. Whether to print simulation
#'   status. Default: the value of `wait`.
#'
#' @section Collect Simulation Data:
#' ```
#' bc$data_sim(resolution = NULL, exclude_ddy = TRUE, merge = FALSE, all = FALSE)
#' ```
#'
#' `$data_sim()` returns a [data.table][data.table::data.table()] (when `merge`
#' is `TRUE`) or a list of 2 [data.table][data.table::data.table()] (when
#' `merge` is `FALSE`) which contains the simulated data of input and output
#' parameters. These data will be stored internally and used during Bayesian
#' calibration using Stan.
#'
#' The `resolution` parameter can be used to specify the time resolution of
#' returned data. Note that input time resolution cannot be smaller than the
#' reporting frequency, otherwise an error will be issued.
#'
#' The parameter is named in the same way as standard EnergyPlus csv output
#' file, i.e. `KeyValue:VariableName[Unit](Frequency)`.
#'
#' By default, `$data_sim()` returns minimal columns, i.e. the `Date/Time`
#' column together with all input and output parameters are returned. You can
#' retrieve extra columns by setting `all` to `TRUE`. Those column include:
#'
#' * `case`: Character type. Same as the parametric model name in pattern
#'   `Case_ParameterName(ParamterValue)...`.
#' * `environment_period_index`: Integer type. The indice of environment.
#' * `environment_name`: Character type. A text string identifying the
#'   simulation environment.
#' * `simulation_days`: Integer type. Day of simulation.
#' * `datetime`: DateTime type. The date time of simulation result. Note that
#'   the year valueas are automatically calculated to meets the start day of
#'   week restriction for each simulation environment.
#' * `month`: Integer type. The month of reported date time.
#' * `day`: Integer type. The day of month of reported date time.
#' * `hour`: Integer type. The hour of reported date time.
#' * `minute`: Integer type. The minute of reported date time.
#' * `day_type`: Character type. The type of day, e.g. `Monday`, `Tuesday` and
#'   etc. Note that `day_type` will always be `NA` if `resolution` is specified.
#'
#' **Arguments**
#'
#' * `resolution`: A character string specifying a time unit or a multiple of a
#'   unit to change the time resolution of returned simulation data. Valid base
#'   units are `min`, `hour`, `day`, `week`, `month`, and `year`.
#'   Example: `10 mins`, `2 hours`, `1 day`. If `NULL`, the variable reporting
#'   frequency is used. Default: `NULL`.
#' * `exclude_ddy`: Whether to exclude design day data. Default: `TRUE`.
#' * `merge`: Whether to merge simulated data of input and output parameters. If
#'   `TRUE`, a [data.table][data.table::data.table()] is returned which contains
#'   datetime, data of output parameters, and then data of input parameters. If
#'   `FALSE`, a list will be returned of which `input` contains the data of
#'   input parameters, and `output` contains the data of output parameters.
#'   Default: `FALSE`.
#' * `all`: If `TRUE`, extra columns are also included in the returned
#'   [data.table][data.table::data.table()] describing the simulation case and
#'   datetime components. Default: `FALSE`.
#'
#' @section Specify Measured Data:
#' ```
#' bc$data_field(output, new_input = NULL, merge = FALSE, all = FALSE)
#' ```
#'
#' `$data_field()` takes a [data.frame()] of measured value of output
#' parameters and returns a list of [data.table][data.table::data.table()]s
#' which contains the measured value of input and output parameters, and newly
#' measured value of input if applicable.
#'
#' The specified `output` [data.frame()] is validated using criteria below:
#'
#' * The column number should be the same as the number of output specified in
#'   `$output()`.
#' * The row number should be the same as the number of simulated values for
#'   each case extracted using `$data_sim()`.
#'
#' Parameter `new_input` can be used to give a [data.frame()] of newly measured
#' value of input parameters. The column number of input [data.frame()] should
#' be the same as the number of input parameters specified in `$input()`.
#'
#' For input parameters, the values of simulation data for the first case are
#' directly used as the measured values.
#'
#' All the data will be stored internally and used during Bayesian calibration
#' using Stan.
#'
#' Note that as `$data_field()` relies on the output of `$data_sim()` to
#' perform validation on the specified data, `$data_field()` cannot be called
#' before `$data_sim()` and internally stored data will be removed whenever
#' `$data_sim()` is called. This aims to make sure that simulated data and field
#' data can be matched whenever the calibration is performed.
#'
#' **Arguments**
#'
#' * `output`: A [data.frame()] containing measured value of output parameters.
#' * `new_input`: A [data.frame()] containing newly measured value of input
#'   parameters.
#' * `merge`: Whether to merge simulated data of input and output parameters. If
#'   `TRUE`, a list of 2 [data.table][data.table::data.table()] is returned with
#'   first element named `merged` which contains datetime, data of output
#'   parameters, and then data of input parameters, and second element named
#'   `new_input` which contains formatted newly measured input parameter values.
#'   If `FALSE`, a list of 3 [data.table][data.table::data.table()] is returned
#'   with element `input`, `output` and `new_input` containing the data of
#'   input, output parameters and newly measured value of input pamameters.
#'   Default: `FALSE`.
#' * `all`: If `TRUE`, extra columns are also included in the returned
#'   [data.table][data.table::data.table()] describing the simulation case and
#'   datetime components. For details, please see `$data_sim()`. Default:
#'   `FALSE`.
#'
#' @section Run Bayesian Calibration Using Stan:
#' ```
#' bc$stan_run(iter = 2000L, chains = 4L, echo = TRUE, ...)
#' ```
#'
#' `$stan_run()` runs Bayesian calibration using Stan.
#'
#' **Arguments**
#'
#' * `iter`: A positive integer specifying the number of iterations for each
#'   chain (including warmup). Default: `2000`.
#' * `chains`: A positive integer specifying the number of Markov chains.
#'   Default: `4`.
#' * `echo`: Whether to print intermediate output from Stan on the console,
#'   which might be helpful for model debugging. Default: `TRUE`.
#' * `...`: Additional arguments to pass to [rstan::sampling].
#'
#' @section Inherited Methods from `ParametricJob`:
#' ```
#' bc$eplus_kill()
#' bc$eplus_status()
#' bc$eplus_output_dir(which = NULL)
#' bc$eplus_locate_output(which = NULL, suffix = ".err", strict = TRUE)
#' bc$eplus_errors(which = NULL, info = FALSE)
#' bc$eplus_report_data_dict(which = NULL)
#' bc$eplus_report_data(which = NULL, key_value = NULL, name = NULL,
#'                      year = NULL, tz = "UTC", all = FALSE, wide = FALSE,
#'                      period = NULL, month = NULL, day = NULL, hour = NULL, minute = NULL,
#'                      interval = NULL, simulation_days = NULL, day_type = NULL,
#'                      environment_name = NULL)
#' bc$eplus_tabular_data(which = NULL, report_name = NULL, report_for = NULL,
#'                       table_name = NULL, column_name = NULL, row_name = NULL)
#' bc$eplus_save(dir = NULL, separate = TRUE, copy_external = FALSE)
#' ```
#'
#' All methods listed above are inherited from eplusr's
#' [`ParametricJob`][eplusr::ParametricJob]. Each method has been renamed with a
#' prefix `eplus_`, e.g. `$output_dir()` in [eplusr::ParametricJob] becomes
#' `$eplus_output_dir()`. For detailed documentation on each
#' method, please see [eplusr's documentation][eplusr::ParametricJob].
#'
#' @examples
#' \dontrun{
#' if (is_avail_eplus(8.8)) {
#'     idf_name <- "5Zone_Transformer.idf"
#'     epw_name <-  "USA_CA_San.Francisco.Intl.AP.724940_TMY3.epw"
#'
#'     idf_path <- file.path(eplus_config(8.8)$dir, "ExampleFiles", idf_name)
#'     epw_path <- file.path(eplus_config(8.8)$dir, "WeatherData", epw_name)
#'
#'     # create from local files
#'     bayes_job(idf_path, epw_path)
#'
#'     # create from an Idf and an Epw object
#'     bc <- bayes_job(read_idf(idf_path), read_epw(epw_path))
#'
#'     # get the seed model
#'     bc$seed()
#'
#'     # get the weather
#'     bc$weather()
#'
#'     # read rdd
#'     bc$read_rdd()
#'
#'     # read mdd
#'     bc$read_mdd()
#'
#'     # set input and output parameters
#'     bc$input(name = "fan air mass flow rate", reporting_frequency = "hourly")
#'     # set input parameters
#'     bc$output(name = "fan electric power", reporting_frequency = "hourly")
#'
#'     # get existing input and output
#'     bc$input()
#'     bc$output()
#'
#'     # set sensitivity parameters using $param()
#'     bc$param(`Supply Fan 1` = list(Fan_Total_Efficiency = c(min = 0.1, max = 1.0)),
#'         .names = "faneff", .num_sim = 2
#'     )
#'
#'     # extract samples
#'     bc$samples()
#'
#'     # extract all models
#'     bc$models()
#'
#'     # run parametric simulations
#'     bc$eplus_run(dir = tempdir(), run_period = list("example", 1, 1, 1, 31))
#'
#'     # print simulation errors
#'     bc$eplus_errors()
#'
#'     # extract simulation data
#'     bc$data_sim()
#'
#'     # specify field data
#'     # here use the seed model as an example
#'     ## clone the seed model
#'     seed <- bc$seed()$clone()
#'     ## remove existing RunPeriod objects
#'     seed$RunPeriod <- NULL
#'     ## set run period as the same as in `$eplus_run()`
#'     seed$add(RunPeriod = list("test", 1, 1, 1, 31))
#'     ## save the model to tempdir
#'     seed$save(tempfile(fileext = ".idf"))
#'     job <- seed$run(epw_path, echo = FALSE)
#'     fan_power <- job$report_data(name = bc$output()$variable_name, wide = TRUE)
#'     bc$data_field(fan_power[, -c("case", "Date/Time")])
#'
#'     # run stan
#'     fit <- bc$stan_run()
#' }
#' }
#' @docType class
#' @name BayesCalibJob
#' @author Hongyuan Jia, Adrian Chong
#' @references
#' A. Chong and K. Menberg, "Guidelines for the Bayesian calibration of building
#' energy models", Energy and Buildings, vol. 174, pp. 527–547. DOI:
#' 10.1016/j.enbuild.2018.06.028
#' @export
# bayes_job {{{
bayes_job <- function (idf, epw) {
    bc <- BayesCalib$new(idf, epw)
    # remove parent methods
    rm(list = c("run", "kill", "status", "output_dir", "locate_output",
        "errors", "report_data_dict", "report_data", "tabular_data"
    ), envir = bc)

    lockEnvironment(bc)
    bc
}
# }}}

# BayesCalib {{{
BayesCalib <- R6::R6Class(classname = "BayesCalibJob",
    inherit = eplusr:::Parametric, cloneable = FALSE, lock_objects = FALSE,
    public = list(
        # INITIALIZE {{{
        initialize = function (idf, epw) {
            # do not allow NULL for epw
            assert(!is.null(epw))

            eplusr:::with_silent(super$initialize(idf, epw))

            # remove all output variables and meters
            private$m_idf <- bc_remove_output_class(super, self, private, all = FALSE, clone = TRUE)

            # init some logging variables
            private$m_log$run_ddy <- FALSE
            private$m_log$has_param <- FALSE
        },
        # }}}

        # PUBLIC FUNCTIONS {{{
        read_rdd = function (update = FALSE)
            bc_read_rdd(super, self, private, update),

        read_mdd = function (update = FALSE)
            bc_read_mdd(super, self, private, update),

        input = function (key_value = NULL, name = NULL, reporting_frequency = NULL, append = FALSE)
            bc_input(super, self, private, key_value, name, reporting_frequency, append),

        param = function (..., .names = NULL, .num_sim = 30L)
            bc_param(super, self, private, ..., .names = .names, .num_sim = .num_sim),

        apply_measure = function (measure, ..., .names = NULL, .num_sim = 30L)
            bc_apply_measure(super, self, private, ..., .names = .names, .num_sum = .num_sum),

        samples = function ()
            bc_samples(super, self, private),

        output = function (key_value = NULL, name = NULL, reporting_frequency = NULL, append = FALSE)
            bc_output(super, self, private, key_value, name, reporting_frequency, append),

        models = function ()
            bc_models(super, self, private),

        data_sim = function (resolution = NULL, exclude_ddy = TRUE, merge = FALSE, all = FALSE)
            bc_data_sim(super, self, private, resolution, exclude_ddy, merge, all),

        data_field = function (output, new_input = NULL, merge = FALSE, all = FALSE)
            bc_data_field(super, self, private, output, new_input, merge, all),

        eplus_run = function (dir = NULL, run_period = NULL, wait = TRUE, force = FALSE,
                              copy_external = FALSE, echo = wait)
            bc_eplus_run(super, self, private, dir, run_period, wait, force, copy_external, echo),

        eplus_kill = function ()
            super$kill(),

        eplus_status = function ()
            super$status(),

        stan_run = function (iter = 2000L, chains = 4L, echo = TRUE, ...)
            bc_stan_run(super, self, private, iter, chains, echo, ...),

        stan_kill = function ()
            bc_stan_kill(super, self, private),

        stan_status = function ()
            bc_stan_status(super, self, private),

        eplus_output_dir = function (which = NULL)
            super$output_dir(which),

        eplus_locate_output = function (which = NULL, suffix = ".err", strict = TRUE)
            super$locate_output(which, suffix, strict),

        eplus_errors = function (which = NULL, info = FALSE)
            super$errors(which, info),

        eplus_report_data_dict = function (which = NULL)
            super$report_data_dict(which),

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

        eplus_tabular_data = function (which = NULL, report_name = NULL, report_for = NULL,
                                table_name = NULL, column_name = NULL, row_name = NULL)
            super$tabular_data(which, report_name = report_name,
                report_for = report_for, table_name = table_name,
                column_name = column_name, row_name = row_name),

        eplus_save = function (dir = NULL, separate = TRUE, copy_external = FALSE)
            super$save(dir, separate, copy_external)
        # }}}
    ),

    private = list(
        # PRIVATE FIELDS {{{
        m_idf = NULL,
        m_epw = NULL,
        m_job = NULL,
        m_log = NULL,
        m_input = NULL,
        m_output = NULL,
        m_param = NULL
        # }}}
    )
)
# }}}

# bc_remove_output_class {{{
bc_remove_output_class <- function (super, self, private, all = TRUE, clone = FALSE) {
    idf <- if (clone) private$m_idf$clone() else private$m_idf

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
    if (!private$m_idf$is_valid_class("SizingPeriod:DesignDay")) {
        abort("error_no_ddy", paste0("In order to run design-day-only simulation, ",
            "at least one `SizingPeriod:DesignDay` should exist."
        ))
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
        abort("error_ddy_run", "Failed to run design-day-only simulation.")
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
    assert(is_count(.num_sim))
    # clean measure created using $apply_measure() if any
    private$m_log$measure_wrapper <- NULL

    l <- tryCatch(eplusr:::sep_value_dots(..., .empty = FALSE, .scalar = FALSE, .null = FALSE, .env = .env),
        error_empty_input = function (e) {
            abort("error_bc_empty_param_input", "Please give parameters to set.")
        }
    )

    obj_val <- eplusr:::match_set_idf_data(
        ._get_private(private$m_idf)$idd_env(),
        ._get_private(private$m_idf)$idf_env(),
        l
    )

    # handle whole-class case
    # class equals to TRUE or dot_nm length larger than 1 should be treated as a whole
    l$value <- data.table::rbindlist(list(
        obj_val$value[J(l$dot[class == TRUE | vapply(dot_nm, length, 1L) > 1L, rleid]), on = "input_rleid", .SD[1L], by = c("class_id", "field_index")],
        obj_val$value[!J(l$dot[class == TRUE | vapply(dot_nm, length, 1L) > 1L, rleid]), on = "input_rleid"]
    ), use.names = TRUE)

    # validate input
    par <- validate_par_space(l, private$m_idf, "bc")

    # get sample value
    sam <- lhs_samples(par, obj_val$value, .names, .num_sim)
    private$m_log$sample <- sam

    # only create models when input and output have been created
    if (is.null(private$m_input) || is.null(private$m_output)) {
        message("No parametric models have been created because input variables ",
            "or output variables are not set. Please set input and output variables ",
            "using `$input()` and `$output()` respectively. In this case, models ",
            "will be created when calling `$models()` or `$run()`."
        )
        return(self)
    }

    create_par_models(self, private, type = "bc")

    self
}
# }}}
# bc_apply_measure {{{
bc_apply_measure <- function (super, self, private, measure, ..., .names = NULL, .num_sim = 30L) {
    # measure name
    mea_nm <- deparse(substitute(measure, parent.frame()))

    assert(is.function(measure), msg = "`measure` should be a function.")
    if (length(formals(measure)) < 2L) {
        abort("error_measure_no_arg", "`measure` function must have at least two argument.")
    }

    # match fun arg
    mc <- match.call(measure, quote(measure(private$m_idf, ...)))[-1L]
    l <- vector("list", length(mc[-1L]))
    names(l) <- names(mc[-1L])
    # get value
    for (nm in names(l)) l[[nm]] <- eval(mc[-1L][[nm]])

    # check input format
    par <- validate_par_space(l, type = "bc")

    sam <- lhs_samples(par, NULL, .names, .num_sim)

    # store
    private$m_log$sample <- sam

    measure_wrapper <- function (idf, ...) {
        assert(eplusr::is_idf(idf), msg = paste0("Measure should take an `Idf` object as input, not `", class(idf)[[1]], "`."))
        idf <- idf$clone(deep = TRUE)
        idf <- measure(idf, ...)
        assert(eplusr::is_idf(idf), msg = paste0("Measure should return an `Idf` object, not `", class(idf)[[1]], "`."))
        idf
    }

    private$m_log$measure_wrapper <- measure_wrapper
    private$m_log$measure_name <- mea_nm

    # only create models when input and output have been created
    if (is.null(private$m_input) || is.null(private$m_output)) {
        message("Input variables or output variables are not set. Please set ",
            "input and output variables using `$input()` and `$output()` ",
            "respectively. In this case, models will be created when calling ",
            "`$models()` or `$run()`."
        )
        return(self)
    }

    create_par_models(self, private, type = "bc")

    self
}
# }}}
# bc_samples {{{
bc_samples <- function (super, self, private) {
    bc_assert_has_sampled(super, self, private, stop = FALSE)
    private$m_log$sample$sample
}
# }}}
# bc_models {{{
bc_models <- function (super, self, private) {
    if (!is.null(private$m_param)) return(private$m_param)

    create_par_models(self, private, verbose = TRUE, type = "bc")
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
                message("Reset `Run Simulation for Weather File Run Periods` ",
                    "in `SimulationControl` from `No` to `Yes` to make sure ",
                    "input run period can take effect."
                )
                simctrl$set(`Run Simulation for Weather File Run Periods` = "Yes")
            }
        }
        idf
    }

    # remove all objects in output classes
    if (!is.null(run_period)) {
        assert(is.list(run_period))

        # if no parameter models have been set
        if (is.null(private$m_param)) {
            overwrite_runperiod(private$m_idf, run_period)
            # should update logged seed UUID
            private$m_log$seed_uuid <- ._get_private(private$m_idf)$m_log$uuid
            # create models
            private$m_param <- create_par_models(self, private, stop = TRUE, type = "bc")
        } else {
            # create models
            private$m_param <- create_par_models(self, private, stop = TRUE, type = "bc")
            lapply(private$m_param, overwrite_runperiod, run_period = run_period)
            # should update logged parametric model UUIDs
            private$m_log$uuid <- vapply(private$m_param, function (idf) ._get_private(idf)$m_log$uuid, character(1L))
        }
    } else {
        private$m_param <- create_par_models(self, private, stop = TRUE, type = "bc")
    }

    super$run(dir, wait = wait, force = force, copy_external = copy_external, echo = echo)
}
# }}}
# bc_data_sim {{{
bc_data_sim <- function (super, self, private, resolution = NULL, exclude_ddy = TRUE, merge = FALSE, all = FALSE) {
    bc_assert_can_collect(super, self, private, stop = TRUE)

    # remove logged data
    private$m_log$data_sim <- NULL
    # remove data field to make sure that data_sim and data_field is in pair
    private$m_log$data_field <- NULL

    input <- bc_extract_report_data(super, self, private, type = "input", exclude_ddy)
    output <- bc_extract_report_data(super, self, private, type = "output", exclude_ddy)

    if (!is.null(resolution)) {
        bc_assert_valid_resolution(super, self, private, resolution)

        input <- bc_change_data_resolution(super, self, private, input, resolution)
        output <- bc_change_data_resolution(super, self, private, output, resolution)
    }

    # format to wide
    input <- eplusr:::report_dt_to_wide(input, date_components = TRUE)
    output <- eplusr:::report_dt_to_wide(output, date_components = TRUE)

    # should keep the variable order
    input <- bc_retain_variable_order(super, self, private, input, "input")
    output <- bc_retain_variable_order(super, self, private, output, "output")

    private$m_log$data_sim$input <- copy(input)
    private$m_log$data_sim$output <- copy(output)

    combine_input_output_data(input, output, merge, all)
}
# }}}
# bc_data_field {{{
bc_data_field <- function (super, self, private, output, new_input = NULL, merge = FALSE, all = FALSE) {
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
    if (!is.null(new_input)) {
        bc_assert_valid_measured(super, self, private, new_input, "new_input", FALSE)

        new_input <- as.data.table(new_input)
        setnames(new_input, names(private$m_log$data_sim$input)[-(1L:11L)])

        private$m_log$data_field$new_input <- copy(new_input)
    }

    # log
    private$m_log$data_field$input <- copy(input)
    private$m_log$data_field$output <- copy(output)

    # reset returned case to NA
    set(input, NULL, "case", NA_character_)
    set(output, NULL, "case", NA_character_)

    if (merge) {
        list(merged = combine_input_output_data(input, output, merge, all), new_input = new_input)
    } else {
        c(combine_input_output_data(input, output, merge, all), list(new_input = new_input))
    }
}
# }}}
# bc_stan_run {{{
bc_stan_run <- function (super, self, private, iter = 2000L, chains = 4L, echo = TRUE, ...) {
    bc_assert_can_stan(super, self, private, stop = TRUE)

    # data {{{
    # exclude 11 meta column: case, datetime, ...
    # measured output
    y <- private$m_log$data_field$output[, .SD, .SDcols = -c(1L:11L)]
    # measured input
    xf <- private$m_log$data_field$input[, .SD, .SDcols = -c(1L:11L)]
    # simulated output
    eta <- private$m_log$data_sim$output[, .SD, .SDcols = -c(1L:11L)]
    # simulated input
    xc <- private$m_log$data_sim$input[, .SD, .SDcols = -c(1L:11L)]
    # calibration parameters
    tc <- private$m_log$sample$sample[rep(case, each = nrow(xf)), .SD, .SDcols = -1L]

    # newly measured input for prediction
    if (is.null(private$m_log$data_field$new_input)) {
        xpred <- xc
    } else {
        xpred <- private$m_log$data_field$new_input
    }
    # }}}

    # meta {{{
    # number of output parameters
    d <- ncol(y)
    # number of input parameters
    p <- ncol(xf)
    # number of measured parameter observations
    n <- nrow(xf)
    # number of newly design points for predictions
    n_pred <- nrow(xpred)
    # number of simulated observations
    m <- nrow(xc)
    # number of calibration parameters
    q <- ncol(tc)
    # }}}

    # z-score normalization on output parameter y and eta {{{
    zscore_norm <- function (x, mu, sd) (x - mu) / sd
    un_zscore_norm <- function (x, mu, sd) x * sd + mu

    eta_std <- copy(eta)
    y_std <- copy(y)
    eta_mu <- eta[, lapply(.SD, mean)]
    eta_sd <- eta[, lapply(.SD, sd)]
    for (i in seq.int(d)) {
        set(y_std, NULL, i, zscore_norm(y_std[[i]], eta_mu[[i]], eta_sd[[i]]))
        set(eta_std, NULL, i, zscore_norm(eta_std[[i]], eta_mu[[i]], eta_sd[[i]]))
    }
    # }}}

    # min-max normalization on input parameter xf, xc and xpred {{{
    minmax_norm <- function (x, min, max) (x - min) / (max - min)
    un_minmax_norm <- function (x, min, max) x * (max - min) + min

    xf_std <- copy(xf)
    xc_std <- copy(xc)
    xpred_std <- copy(xpred)

    x <- rbindlist(list(xf, xc))
    x_min <- x[, lapply(.SD, min)]
    x_max <- x[, lapply(.SD, max)]
    for (i in seq.int(p)) {
        set(xf_std, NULL, i, minmax_norm(xf_std[[i]], x_min[[i]], x_max[[i]]))
        set(xc_std, NULL, i, minmax_norm(xc_std[[i]], x_min[[i]], x_max[[i]]))
        set(xpred_std, NULL, i, minmax_norm(xpred_std[[i]], x_min[[i]], x_max[[i]]))
    }
    # }}}

    # create data as list for input to Stan {{{
    stan_data <- list(
        # number of measured parameter observations
        n = n,
        # number of simulated observations
        m = m,
        # number of newly design points for predictions
        n_pred = n_pred,
        # number of input parameters
        p = p,
        # number of calibration parameters
        q = q,
        # number of output parameters
        D = d,
        # measured output
        y = y,
        # simulated output
        eta = eta,
        # measured input
        xf = xf,
        # simulated input
        xc = xc,
        # calibration parameters
        tc = tc,
        # new design points for predictions
        x_pred = xpred
    )
    # }}}

    fit <- rstan::sampling(stanmodels$bc_with_pred, data = stan_data,
        chains = chains, iter = iter, open_progress = echo, show_messages = echo,
        ...
    )
    (private$m_log$stan$fit <- fit)
}
# }}}
# bc_stan_kill {{{
bc_stan_kill <- function (super, self, private) {
}
# }}}
# bc_stan_status {{{
bc_stan_status <- function (super, self, private) {
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
    err_type <- paste0("error_bc_invalid_", type)

    if (is.null(key_value) && is.null(name) && is.null(reporting_frequency)) {
        if (is.null(append)) {
            if (NROW(private[[m_name]])) {
                eplusr:::with_silent(private$m_idf$del(private[[m_name]]$id, .force = TRUE))
            }
            private[[m_name]] <- NULL
        }
        return(private[[m_name]])
    }

    # make sure seed UUID is updated
    on.exit(private$m_log$seed_uuid <- ._get_private(private$m_idf)$m_log$uuid, add = TRUE)

    if (is.null(append)) append <- FALSE

    # check NA
    if (anyNA(key_value)) abort(err_type, "`key_value` cannot contain any NA.")
    if (anyNA(name)) abort(err_type, "`name` cannot contain any NA.")
    if (anyNA(reporting_frequency)) abort(err_type, "`reporting_frequency` cannot contain any NA.")

    # get RDD and MDD
    if (!private$m_log$run_ddy) bc_run_ddy(super, self, private)

    # not specified
    if (is.null(key_value)) {
        key_value <- "*"
    # RddFile or MddFile
    } else if (inherits(key_value, c("RddFile", "MddFile"))) {
        if (type == "input" && inherits(key_value, "MddFile")) {
            abort(err_type, paste0("`$input()` only support RddFile. MddFile ",
                "can only be used in `$output()`."
            ))
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

        abort(err_type, paste0("`key_value` should be NULL, ",
            "a character vector, a data.frame or an ", dict, " object."
        ))
    }

    if (is.null(name)) {
        abort(err_type, paste0("Please give variable names via `name`."))
    }

    if (type == "input") {
        rmdd <- private$m_log$rdd
    } else {
        rmdd <- rbindlist(list(private$m_log$rdd, private$m_log$mdd))
    }

    dt <- rmdd[J(tolower(name)), on = "variable_lower"]
    set(dt, NULL, "key_value", key_value)

    if (anyNA(dt$variable)) {
        abort(err_type, paste0("Invalid variable name found: ", paste0("`", name[is.na(dt$variable)], "`", collapse = ", ")))
    }

    # check reporting frequency
    if (is.null(reporting_frequency)) reporting_frequency <- "Timestep"
    reporting_frequency <- check_same_report_freq(type, reporting_frequency, bc_report_freq(super, self, private), append)
    set(dt, NULL, "reporting_frequency", reporting_frequency)

    # input cannot contain any duplications
    if (nrow(invld <- dt[duplicated(dt, by = c("variable_lower", "key_value"))])) {
        invld[report_type == "Meter", nm := paste0("`", variable, "`", collapse = ", ")]
        invld[report_type != "Meter", nm := paste0("`", key_value, ":", variable, "`", collapse = ", ")]
        abort(err_type, paste0("Duplications found in ", type, " variables: ", invld$nm))
    }

    # remove existing if necessary
    bc_clean_existing_input_output(super, self, private, type, append, dt)

    dt[, index := .I]
    # now it's save to load it
    dt_var <- rdd_to_load(setattr(dt[report_type != "Meter"], "class", c("RddFile", class(dt))))
    if (nrow(dt_var)) {
        obj_var <- private$m_idf$load(dt_var, .unique = FALSE)
        dt_var <- private$m_idf$to_table(vapply(obj_var, function (x) x$id(), 1L), wide = TRUE)[, name := NULL]
        dt_var[dt, on = c("Variable Name" = "variable"), index := i.index]
    } else {
        dt_var <- data.table()
    }
    dt_mtr <- mdd_to_load(setattr(dt[report_type == "Meter"], "class", c("MddFile", class(dt))))
    if (nrow(dt_mtr)) {
        obj_mtr <- private$m_idf$load(dt_mtr, .unique = FALSE)
        dt_mtr <- private$m_idf$to_table(vapply(obj_mtr, function (x) x$id(), 1L), wide = TRUE)[, name := NULL]
        setnames(dt_mtr, "Key Name", "Variable Name")
        set(dt_mtr, NULL, "Key Value", NA_character_)
        dt_mtr[dt, on = c("Variable Name" = "variable"), index := i.index]
    } else {
        dt_mtr <- data.table()
    }

    # add index
    dt <- tidy_names(rbindlist(list(dt_var, dt_mtr), use.names = TRUE))
    setcolorder(dt, "index")
    setorderv(dt, "index")

    bc_combine_input_output(super, self, private, type, append, dt)

    private[[m_name]]
}
# }}}
# bc_match_input_output_dict {{{
bc_match_input_output_dict <- function (super, self, private, type, append, reporting_frequency, dict) {
    err_type <- paste0("error_bc_invalid_", type)
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
        abort(err_type, ("Input variables can only be output variables, not output meters."))
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
        abort(err_type, paste0("Invaid variable name found ",
            "in input `key_value`: ",
            paste0("`", dict[is.na(variable_match), variable], "`", collapse = "\n"), "."
        ))
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

    bc_clean_existing_input_output(super, self, private, type, append, dict)

    # now it's save to load it
    obj <- private$m_idf$load(load_fun(dict), .unique = FALSE)

    dt <- private$m_idf$to_table(vapply(obj, function (x) x$id(), 1L), wide = TRUE)[, name := NULL]
    if (inherits(dict, "MddFile")) {
        setnames(dt, "Key Name", "Variable Name")
        set(dt, NULL, "Key Value", NA_character_)
    }
    # add index
    dt <- tidy_names(dt[, index := .I])
    setcolorder(dt, "index")

    bc_combine_input_output(super, self, private, type, append, dt)

    private[[paste0("m_", type)]]
}
# }}}
# bc_match_input_output_dt {{{
bc_match_input_output_dt <- function (super, self, private, type, append, reporting_frequency, dt) {
    err_type <- paste0("error_bc_invalid_", type)
    other_type <- switch(type, input = "output", output = "input")
    cls <- switch(type, input = "Output:Variable", output = c("Output:Variable", "Output:Meter"))
    cols <- c("class", "index", "value")

    if (!all(cols %in% names(dt))) {
        abort(err_type, paste0("When `key_value` is a data.frame, ",
            "it should contains at least 3 columns named `class`, `index` ",
            "and `value`. Column `",
            paste0("`", cols[cols %in% names(dt)], "`", collapse = ", "), " ",
            "is/are missing in the input."
        ))
    }

    # here will copy the input
    dt <- as.data.table(dt)[, .SD, .SDcols = c(
        if ("id" %in% names(dt)) "id" else NULL, cols)
    ]

    if (any(invld <- !dt$class %in% cls)) {
        abort(err_type, paste0("When `key_value` is a data.frame, ",
            "the `class` column should always be ", paste0(cls, collapse = " and "),
            ". Invalid class name found: ",
            paste0("`", dt[invld, unique(class)], "`", collapse = ", "), "."
        ))
    }
    if (!is.integer(dt$index) || anyNA(dt$index)) {
        abort(err_type, paste0("When `key_value` is a data.frame, ",
            "the `index` column should be of integer type without any NA."
        ))
    }

    # index original input in order to keep the order
    dt[, idx := .GRP, by = c("id", "class")]

    cols <- c("class", "index")
    # check duplications
    if ("id" %in% names(dt)) cols <- c("id", cols)
    if (anyDuplicated(dt, by = cols)) {
        if ("id" %in% names(dt)) {
            cols <- "`id`, `class` and `index`"
        } else {
            cols <- "`class` and `index`"
        }
        abort(err_type,
            paste0("When `key_value` is a data.frame, ", cols,
                " column combined should not contain any duplication."
            )
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
        abort(err_type, paste0("Invalid field number for class Output:Variable: ",
            paste0(var[index > 4L, unique(index)], collapse = "\n"), "."
        ))
    }
    var <- var[index <= 3L]

    mtr[index == 1L, field := "variable"]
    mtr[index == 2L, field := "reporting_frequency"]
    if (mtr[index > 2L, .N]) {
        abort(err_type, paste0("Invalid field number for class Output:Meter: ",
            paste0(mtr[index > 4L, unique(index)], collapse = "\n"), "."
        ))
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

    bc_match_input_output_dict(super, self, private, type, append, reporting_frequency, dict_var)
    bc_match_input_output_dict(super, self, private, type, append = TRUE, reporting_frequency, dict_mtr)

    # retain the original order
    private[[paste0("m_", type)]] <- private[[paste0("m_", type)]][c(dict_var$index, dict_mtr$index)][, index := .I]
    private[[paste0("m_", type)]]
}
# }}}
# bc_clean_existing_input_output {{{
bc_clean_existing_input_output <- function (super, self, private, type, append, dt) {
    m_name <- paste("m", type, sep = "_")
    err_type <- paste0("error_bc_invalid_", type)

    # delete the old
    if (!append) {
        if (NROW(private[[m_name]])) {
            private$m_idf$del(private[[m_name]]$id, .force = TRUE)
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
                    abort(err_type, paste0("Variables specified ",
                            "have already been set as ", type, ". Invalid input found: ",
                        paste0("`", output_var_name(private[[m_name]][-invld$index]), "`", collapse = ", ")
                    ))
                }
            }

            # check if input variables have duplications
            abort(err_type, paste0("Duplications found in ", type, " variables: ", output_var_name(invld)))
        }

        # any key value being * should not overwrite others with specific names
        if (nrow(key_all <- all[key_value == "*"])) {
            key_all <- all[J(unique(key_all$variable)), on = "variable"]
            # input can not be inserted if there is one with key value being "*"
            if (nrow(invld <- dt[key_all[key_value != "*" & index > 0L, index]])) {
                abort(paste0("error_bc_invalid_", type), paste0("Cannot insert ",
                    "new variable when there is an existing one with key value ",
                    "being `*`. Invalid input found: ",
                    paste0("`", invld$key_value, ":", invld$variable, "`", collapse = ", ")
                ))
            }

            # input with key value being "*" can not be inserted if there is one
            # with specific key value
            if (NROW(invld <- private[[m_name]][key_all[key_value != "*" & index < 0L, -index]])) {
                abort(paste0("error_bc_invalid_", type), paste0("Cannot insert ",
                    "new variable with key value being `*` when there is an ",
                    "existing one with specific key value. Invalid input found: ",
                    paste0("`*:", invld$variable_name, "`", collapse = ", ")
                ))
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
            private[[m_name2]][, list(-id, key_value, variable_name)]
        ))

        idx <- duplicated(all, by = c("key_value", "variable_name"), fromLast = TRUE)

        if (any(idx)) {
            invld <- dt[idx[1:nrow(dt)]]
            private$m_idf$del(invld$id)
            abort(paste0("error_bc_invalid_", type),
                paste0("Variables specified have already been set as ",
                    if (type == "input") "output" else "input", ": ",
                    paste0("`", output_var_name(invld), "`", collapse = ", ")
                )
            )
        }

        if (length(var <- all[key_value == "*", variable_name])) {
            var <- all[J(var), on = "variable_name", list(mixed = id > 0L & id < 0L), by = "variable_name"][mixed == TRUE, variable_name]
            invld <- dt[J(var), on = "variable_name"]
            # input can not be inserted if there is one with key value being "*"
            if (nrow(invld <- invld[id > 0L])) {
                private$m_idf$del(invld$id)
                if (nrow(invld_star <- invld[key_value == "*"])) {
                    abort(paste0("error_bc_invalid_", type), paste0("Cannot insert ",
                        "new ", type, " variable with key value being `*` when ",
                        "there is an existing one in ",
                        if (type == "input") "output" else "input",
                        " variables with same variable. Invalid input found: ",
                        paste0("`", invld_star$key_value, ":", invld_star$variable_name, "`", collapse = ", ")
                    ))
                } else {
                    abort(paste0("error_bc_invalid_", type), paste0("Cannot insert ",
                        "new ", type, " variable when there is an existing one in ",
                        if (type == "input") "output" else "input",
                        " variables with key value being `*`. Invalid input found: ",
                        paste0("`", invld$key_value, ":", invld$variable_name, "`", collapse = ", ")
                    ))
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
        abort(paste0("error_bc_invalid_", type), paste0(
            "Object specified does not have the same reporting ",
            "frequency as existing ", type, " (`", old[[type]], "`). ",
            "Invalid input reporting frequency: ",
            paste0("`", freq[freq != old[[type]]], "`")
        ))
    }
    other <- names(old)[names(old) != type]
    if (!is.null(old[[other]]) && !all(tolower(old[[other]]) == tolower(freq))) {
        abort(paste0("error_bc_invalid_", type), paste0(
            "Object specified does not have the same reporting ",
            "frequency as existing ", other, " (`", old[[other]], "`). ",
            "Invalid input reporting frequency: ",
            paste0("`", freq[freq != old[[other]]], "`")
        ))
    }
    eplusr:::validate_report_freq(freq)
}
# }}}

# param
# bc_assert_has_sampled {{{
bc_assert_has_sampled <- function (super, self, private, stop = TRUE) {
    if (is.null(private$m_log$sample)) {
        if (stop) {
            abort("error_bc_not_ready", paste0("No LHS samples have been generated. ",
                "Please use `$param()` or `$apply_measure()` to set parameters and ",
                "perform LHS sampling."
            ))
        } else {
            message("No LHS samples have been generated. ",
                "Please use `$param()` or `$apply_measure()` to set parameters and ",
                "perform LHS sampling."
            )
        }
    }
    TRUE
}
# }}}
# lhs_samples {{{
lhs_samples <- function (par, value, names = NULL, num) {
    fctr <- par_names(par, names, type = "bc")

    # use lhs::randomLHS to generate input
    val <- as.data.table(lhs::randomLHS(num, nrow(par$num$meta)))
    setnames(val, fctr)

    # number the case
    val[, case := .I]
    data.table::setcolorder(val, "case")

    # melt
    val_m <- data.table::melt.data.table(val, id.vars = "case",
        variable.name = "name_par", value.name = "value",
        variable.factor = FALSE
    )

    # add parameter index
    val_m[data.table(index_par = seq_along(fctr), name_par = fctr),
        on = "name_par", index_par := i.index_par
    ]

    # calculate value
    val_m[par$num$meta, on = c("index_par" = "value_rleid"),
        `:=`(value = i.min + (i.max - i.min) * value)
    ]

    # recreate the sample
    val <- dcast.data.table(val_m, case ~ name_par, value.var = "value")

    # get case name
    nms <- case_names(val[, -"case"])

    # format val for `Idf$update()`
    val_m <- par$num$data[, list(value_rleid, name = object_name, class = class_name,
        index = field_index, field = field_name, is_sch_value, class_id
    )][val_m, on = c("value_rleid" = "index_par")]
    # change value column to list
    set(val_m, NULL, "value", as.list(val_m$value))
    setnames(val_m, "value_rleid", "index_par")

    # if schedule value detected, change it to character
    val_m[J(TRUE), on = "is_sch_value", value := lapply(value, as.character)][
        , is_sch_value := NULL]

    # this is necessary to get the right order of val
    data.table::setorder(val_m, "case")
    data.table::setcolorder(val_m, c("case", "index_par", "name_par", "class",
        "name", "index", "field", "value"
    ))

    # combine
    if (!is.null(value)) {
        # handle grouped parameters
        grp <- par$dot[, list(grouped = class || vapply(dot_nm, length, 1L) > 1L), by = c("rleid")][
            grouped == TRUE
        ]

        # get parameter index
        target <- value[, list(id = object_id, input_rleid, input_object_rleid)]
        target[J(grp$rleid), on = "input_rleid", input_object_rleid := 1L]
        set(target, NULL, "index_par", rleidv(target, c("input_rleid", "input_object_rleid")))
        set(target, NULL, c("input_rleid", "input_object_rleid"), NULL)

        # merge
        val_m <- target[val_m, on = c("index_par"), allow.cartesian = TRUE]
        data.table::setcolorder(val_m, c("case", "index_par", "name_par", "class",
            "id", "name", "index", "field", "value"
        ))
    }

    list(names = nms, sample = val, value = val_m)
}
# }}}

# data sim
# bc_assert_can_model {{{
bc_assert_can_model <- function (self, private, stop = FALSE) {
    if (stop) {
        fun <- function (...) abort("error_bc_not_ready", paste0(...))
    } else {
        fun <- message
    }

    # check if input and output variables are added after parameters
    if (is.null(private$m_input)) {
        fun("Unable to create parametric models ",
            "because input variables are not set. Please use `$input()` to set ",
            "input variables.")
        return(FALSE)
    }

    if (is.null(private$m_output)) {
        fun("Unable to create parametric models ",
            "because output variables are not set. Please use `$output()` to set ",
            "output variables.")
        return(FALSE)
    }

    if (is.null(private$m_log$sample)) {
        fun("Unable to create parametric models ",
            "because no LHS samples have been generated. ",
            "Please use `$param()` or `$apply_measure()` to set parameters and ",
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
        fun <- function (...) abort("error_bc_not_ready", paste0(...))
    } else {
        fun <- message
    }

    if (is.null(private$m_param)) {
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
        fun <- function (...) abort("error_bc_not_ready", paste0(...))
    } else {
        fun <- message
    }

    # check if input and output variables are added after parameters
    if (is.null(private$m_log$data_sim)) {
        fun("Unable to perform calibration ",
            "because simulated data are not set. Please use `$data_sim()` to retrieve ",
            "simulated input and output data before calling `$stan_run()`.")
        return(FALSE)
    }

    if (is.null(private$m_log$data_field)) {
        fun("Unable to perform calibration ",
            "because field data are not set. Please use `$data_field()` to specify ",
            "measured input and output data before calling `$stan_run()`.")
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
        cur_res <- as.integer(60 / private$m_idf$Timestep$Number_of_Timesteps_per_Hour)
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
        abort("error_bc_cannot_resample", paste0("Cannot change data resolution ",
            "when variable reporting frequency is `", freq, "`"
        ))
    }

    in_res <- standardize_resolution(resolution)

    if (cur_res > in_res) {
        abort("error_bc_invalid_resolution", paste0("Input resolution should ",
            "not be smaller than reporting frequency (", err_res, "). ",
            "Invalid resolution found: ", paste0("`", resolution, "`")
        ))
    }

    if (in_res %% cur_res) {
        abort("error_bc_invalid_resolution", paste0("Input resolution should ",
            "be divisible by reporting frequency (", err_res, "). ",
            "Invalid resolution found: ", paste0("`", resolution, "`")
        ))
    }

    TRUE
}
# }}}
# bc_assert_valid_measured {{{
bc_assert_valid_measured <- function (super, self, private, dt, type = c("new_input", "output"), check_row = TRUE) {
    if (is.null(private$m_log$data_sim)) {
        abort("error_bc_empty_data_sim", paste0("Field data should be specified ",
            "after collecting simulation data in order to perform validity checking. ",
            "Please run `$data_sim()` first."
        ))
    }

    type <- match.arg(type)
    m_name <- if (type == "new_input") "input" else "output"
    err_type <- paste0("error_bc_invalid_data_field_", type)

    if (!is.data.frame(dt)) {
        abort(err_type, paste0("`", type, "` should be ",
            "a data.frame. Invalid type: ", class(dt)[1L], "."
        ))
    }

    # should exclude 11 columns:
    # "case", "environment_period_index", "environment_name", "simulation_days",
    # "datetime", "month", "day", "hour", "minute", "day_type", "Date/Time"
    if (ncol(dt) != (ncol(private$m_log$data_sim[[m_name]]) - 11L)) {
        abort(err_type, paste0("The column number of ",
            "`", type, "` should be the same as ", m_name, " variables (",
            ncol(private$m_log$data_sim[[m_name]]) - 11L, "). ",
            "Invalid column number: ", ncol(dt), "."
        ))
    }

    if (check_row) {
        # count row number
        case_count <- private$m_log$data_sim[[m_name]][, list(n = .N), by = "case"]

        if (nrow(dt) != unique(case_count$n)) {
            abort(err_type, paste0("The row number of ",
                "`", type, "` should be the same as ", m_name, " variables (",
                unique(case_count$n), "). Invalid row number: ", nrow(dt), "."
            ))
        }
    }

    TRUE
}
# }}}
# bc_extract_report_data {{{
bc_extract_report_data <- function (super, self, private, type = c("input", "output"), exclude_ddy = TRUE) {
    m_name <- paste0("m_", type)

    key_all <- private[[m_name]][key_value == "*" | is.na(key_value)]
    key_spe <- private[[m_name]][!key_all, on = "index"]
    if (nrow(key_all)) {
        dt_all <- super$report_data(NULL, name = key_all$variable_name, all = TRUE)
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
            abort(paste0("error_bc_", type, "_invalid_key_value"), paste0("Failed to extract ",
                "simulation data of ", type, " variables. Invalid variable specification found: ",
                paste0("`", output_var_name(m), "`", collapse = ", ")
            ))
        }
        set(dt_spe, NULL, "key_value_upper", NULL)
    } else {
        dt_spe <- data.table()
    }

    # combine
    dt <- rbindlist(list(dt_all, dt_spe))

    # make sure each case gives same output rows
    count <- dt[, list(n = .N), by = "case"]
    if (length(unique(count$n)) != 1L) {
        abort("error_bc_data_sim_row_not_same", paste0("Internal error found when ",
            "extracting simulation data. Each case should give the same row number ",
            "of report variable data. If you use `$apply_measure()` to set parameters, ",
            "please make sure your measure does not result in different `Timestep`, ",
            "`RunPeriod` or other objects that can effect the report variable data."
        ))
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

    dt_nm_all <- private[[m_name]][key_value == "*" | is.na(key_value)][dt_nm, on = "variable_name"]
    dt_nm_spe <- private[[m_name]][!dt_nm_all, on = "index"][, key_value := toupper(key_value)][
        dt_nm, on = c("key_value", "variable_name"), nomatch = 0L]

    dt_nm <- rbindlist(list(dt_nm_all[, list(index, full_name)], dt_nm_spe[, list(index, full_name)]))
    setorderv(dt_nm, "index")

    setcolorder(dt, c(nm_meta, dt_nm$full_name))
}
# }}}
# bc_change_data_resolution {{{
#' @importFrom lubridate ceiling_date
bc_change_data_resolution <- function (super, self, private, dt, resolution) {
    set(dt, NULL, "datetime", lubridate::ceiling_date(dt$datetime, resolution))

    dt[self$read_rdd(), on = c("name" = "variable"), report_type := i.report_type]
    dt[self$read_mdd(), on = c("name" = "variable"), report_type := i.report_type]

    dt_avg <- suppressWarnings(dt[J("Average"), on = "report_type", nomatch = 0L,
        list(simulation_days = max(simulation_days), value = mean(value),
             month = month[.N], day = day[.N],
             hour = hour[.N], minute = minute[.N],
             day_type = NA_character_, units = units[.N]
        ),
        by = c("case", "datetime", "environment_period_index", "environment_name",
            "key_value", "name", "is_meter")
    ])
    dt_sum <- suppressWarnings(dt[!J("Average"), on = "report_type",
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
combine_input_output_data <- function (input, output, merge = FALSE, all = FALSE) {
    if (merge) {
        set(input, NULL, c("case", "environment_period_index", "environment_name",
            "simulation_days", "datetime", "month", "day", "hour", "minute",
            "day_type", "Date/Time"), NULL
        )
        if (!all) {
            set(output, NULL, c("case", "environment_period_index", "environment_name",
                "simulation_days", "datetime", "month", "day", "hour", "minute",
                "day_type"), NULL
            )
        }
    } else if (!all) {
        set(input, NULL, c("case", "environment_period_index", "environment_name",
            "simulation_days", "datetime", "month", "day", "hour", "minute", "day_type"), NULL
        )
        set(output, NULL, c("case", "environment_period_index", "environment_name",
            "simulation_days", "datetime", "month", "day", "hour", "minute", "day_type"), NULL
        )
    }

    if (merge) {
        cbind(output, input)
    } else {
        list(input = input, output = output)
    }
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
        abort("error_bc_invalid_resolution", paste0("Resolution unit should be one of ",
            paste0("`", all_spec, "`", collapse = ", "), ". Invalid resolution ",
            "unit found: ", "`", unit, "`")
        )
    }

    list(unit = unit, mult = mult)
}
# }}}
