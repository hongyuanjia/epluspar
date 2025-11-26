#' @include utils.R
NULL

#' Conduct Multi-Objective Optimization on An EnergyPlus Model
#'
#' @description
#' `GAOptimJob` class provides a framework for conducting single- or multi-
#' objective optimization on EnergyPlus models using Genetic Algorithms (GA).
#' It implements the NSGA-II algorithm for multi-objective optimization and
#' supports various genetic operators for mutation, recombination, and selection.
#'
#' @details
#' ## Basic Workflow
#'
#' The typical workflow for using `GAOptimJob` is:
#'
#' 1. **Create a job**: Initialize with an IDF and EPW file using [gaoptim_job()]
#'    or `GAOptimJob$new()`
#' 2. **Define parameters**: Set optimization parameters using `$apply_measure()`
#'    with a measure function and parameter spaces
#' 3. **Set objectives**: Define objective function(s) using `$objective()`
#' 4. **Configure GA operators** (optional): Customize genetic algorithm behavior:
#'    - `$mutator()`: Configure mutation operators
#'    - `$recombinator()`: Configure recombination (crossover) operators
#'    - `$selector()`: Configure selection operators
#'    - `$terminator()`: Set termination conditions
#' 5. **Run optimization**: Execute the optimization using `$run()`
#' 6. **Extract results**: Get optimal solutions using:
#'    - `$best_set()` for single-objective optimization
#'    - `$pareto_set()` for multi-objective optimization
#'    - `$population()` for complete population history
#'
#' ## Genetic Algorithm Implementation
#'
#' The class uses the [miesmuschel](https://CRAN.R-project.org/package=miesmuschel)
#' package for genetic algorithm operators and
#' [bbotk](https://CRAN.R-project.org/package=bbotk) for optimization infrastructure.
#' Parameter spaces are defined using [paradox](https://CRAN.R-project.org/package=paradox).
#'
#' For multi-objective optimization, the implementation follows the NSGA-II
#' algorithm (Deb et al., 2002), which uses:
#' - Non-dominated sorting for ranking solutions
#' - Crowding distance for diversity preservation
#' - Elitism through (mu + lambda) survival strategy
#'
#' ## Parallel Execution
#'
#' The optimization supports parallel execution of EnergyPlus simulations using
#' the [mirai](https://CRAN.R-project.org/package=mirai) package. This can
#' significantly reduce optimization time when multiple CPU cores are available.
#'
#' @docType class
#' @name GAOptimJob
#' @author Hongyuan Jia
#' @seealso
#' - [gaoptim_job()] for creating a `GAOptimJob` object
#' - [paradox::p_dbl()], [paradox::p_int()], [paradox::p_fct()] for defining parameter spaces
#' - [miesmuschel::mut()], [miesmuschel::rec()], [miesmuschel::sel()] for genetic operators
#' - [bbotk::trm()] for custom termination conditions
#' @references
#' Deb, K., Pratap, A., Agarwal, S., & Meyarivan, T. (2002). A fast and elitist
#' multiobjective genetic algorithm: NSGA-II. IEEE Transactions on Evolutionary
#' Computation, 6(2), 182-197. DOI: 10.1109/4235.996017
NULL

# GAOptimJob {{{
GAOptimJob <- R6::R6Class(
    classname = "GAOptimJob",
    inherit = eplusr::ParametricJob,
    cloneable = FALSE,
    lock_objects = FALSE,

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
        initialize = function(idf, epw) {
            if (!requireNamespace("miesmuschel", quietly = TRUE)) {
                stop(sprintf(
                    paste(
                        "Package 'miesmuschel' is required for genetic algorithm optimization.\n",
                        "Please install it via 'install.packages(\"miesmuschel\")'."
                    )
                ))
            }
            if (!requireNamespace("bbotk", quietly = TRUE)) {
                stop(sprintf(
                    paste(
                        "Package 'bbotk' is required for genetic algorithm optimization.\n",
                        "Please install it via 'install.packages(\"bbotk\")'."
                    )
                ))
            }
            if (!requireNamespace("paradox", quietly = TRUE)) {
                stop(sprintf(
                    paste(
                        "Package 'paradox' is required for genetic algorithm optimization.\n",
                        "Please install it via 'install.packages(\"paradox\")'."
                    )
                ))
            }
            eplusr::with_silent(super$initialize(idf, epw))
            self
        },
        # }}}

        # PUBLIC FUNCTIONS {{{
        # param {{{
        #' @description
        #' Set parameters for genetic algorithm optimization
        #'
        #' @details
        #' `$param()` takes parameter definitions in list format, which is
        #' similar to `$set()` in [eplusr::Idf] class except that each field is
        #' not assigned with a single value, but a parameter domain object
        #' created using paradox functions such as [paradox::p_dbl()],
        #' [paradox::p_int()], or [paradox::p_fct()], indicating the search
        #' space of each parameter.
        #'
        #' Similar like the way of modifying object field values in
        #' [eplusr::Idf$set()][eplusr::Idf], there are 3 different ways of
        #' defining a parameter in epluspar:
        #'
        #' * `object = list(field = param_space)`: Where `object` is a
        #'   valid object ID or name. Note object ID should be denoted with two
        #'   periods `..`, e.g. `..10` indicates the object with ID `10`. It
        #'   will set that specific field in that object as one parameter.
        #' * `.(object, object) := list(field = param_space)`: Similar like
        #'   above, but note the use of `.()` in the left hand side. You can put
        #'   multiple object ID or names in `.()`. It will set the field of all
        #'   specified objects as one parameter.
        #' * `class := list(field = param_space)`: Note the use of `:=`
        #'   instead of `=`. The main difference is that, unlike `=`, the left
        #'   hand side of `:=` should be a valid class name in current
        #'   [eplusr::Idf]. It will set that field of all objects in specified
        #'   class as one parameter.
        #'
        #' For example, the code block below defines 4 optimization parameters:
        #'
        #' * Field `Fan Total Efficiency` in object named `Supply Fan 1` in
        #'   class `Fan:VariableVolume`, with search space being \[0.1, 1.0\].
        #' * Field `Thickness` in all objects in class `Material`, with search
        #'   space being \[0.01, 1.0\].
        #' * Field `Conductivity` in all objects in class `Material`, with
        #'   search space being \[0.1, 0.6\].
        #' * Field `Watts per Zone Floor Area` in objects `Light1` and `Light2`
        #'   in class `Lights`, with search space being \[10, 30\].
        #'
        #' ```
        #' opt$param(
        #'     `Supply Fan 1` = list(Fan_Total_Efficiency = paradox::p_dbl(0.1, 1.0)),
        #'     Material := list(Thickness = paradox::p_dbl(0.01, 1), Conductivity = paradox::p_dbl(0.1, 0.6)),
        #'    .("Light1", "Light2") := list(Watts_per_Zone_Floor_Area = paradox::p_dbl(10, 30))
        #' )
        #' ```
        #'
        #' @param ... Lists of parameter definitions. Please see above on the
        #'        syntax.
        #' @param .names A character vector of the parameter names. If `NULL`,
        #'        the parameter will be named in format `p + number`, where
        #'        `number` is the index of parameter. Default: `NULL`.
        #'
        #' @return The modified `GAOptimJob` object itself.
        #'
        #' @examples
        #' \dontrun{
        #' opt$param(
        #'     `Supply Fan 1` = list(Fan_Total_Efficiency = paradox::p_dbl(0.1, 1.0)),
        #'     Material := list(Thickness = paradox::p_dbl(0.01, 1), Conductivity = paradox::p_dbl(0.1, 0.6)),
        #'    .("Light1", "Light2") := list(Watts_per_Zone_Floor_Area = paradox::p_dbl(10, 30))
        #' )
        #' }
        #'
        param = function(..., .names = NULL) {
            gaopt__param(super, self, private, ..., .names = .names, .env = parent.frame())
        },
        # }}}

        # apply_measure {{{
        #' @description
        #' Set optimization parameters using a measure function
        #'
        #' @details
        #' `$apply_measure()` takes a measure function and its parameter
        #' specifications to define the optimization parameters. The measure
        #' function should take an [eplusr::Idf] object as the first argument
        #' and return a modified [eplusr::Idf] object.
        #'
        #' Each parameter should be specified using paradox domain functions:
        #'
        #' * [paradox::p_dbl()]: For continuous numeric parameters within a range
        #' * [paradox::p_int()]: For discrete integer parameters
        #' * [paradox::p_fct()]: For categorical parameters with predefined choices
        #' * [paradox::p_lgl()]: For logical (TRUE/FALSE) parameters
        #' * [paradox::p_uty()]: For untyped parameters (advanced usage)
        #'
        #' The measure function will be called during optimization with the seed
        #' IDF and the sampled parameter values to generate variant models.
        #'
        #' @param measure A function that takes an [eplusr::Idf] object as the
        #'        first argument and returns a modified [eplusr::Idf] object.
        #'        The function should have at least two arguments: the first for
        #'        the IDF object and the rest for optimization parameters.
        #' @param ... Named arguments specifying parameter spaces. Each argument
        #'        should be a parameter domain object created using paradox
        #'        functions such as [paradox::p_dbl()] (continuous numeric),
        #'        [paradox::p_int()] (integer), [paradox::p_fct()] (categorical),
        #'        [paradox::p_lgl()] (logical), or [paradox::p_uty()] (untyped).
        #' @param .names A function or lambda function that takes parameter values
        #'        as arguments and returns a character string for naming the
        #'        generated model variant. If `NULL`, models will be named
        #'        automatically in format `Gen[gen]-Ind[ind]`, where `gen` is the
        #'        generation number and `ind` is the index of parameter combination.
        #'        Default: `NULL`.
        #'
        #' @return The modified `GAOptimJob` object itself.
        #'
        #' @examples
        #' \dontrun{
        #' # define a measure function
        #' my_measure <- function(idf, wall_thickness, roof_insulation) {
        #'     # modify wall construction
        #'     # ...
        #'     # modify roof insulation
        #'     # ...
        #'
        #'     # return modified IDF
        #'     idf
        #' }
        #'
        #' # apply the measure with parameter spaces
        #' opt$apply_measure(my_measure,
        #'     wall_thickness = paradox::p_dbl(0.1, 0.5),
        #'     roof_insulation = paradox::p_dbl(20, 50),
        #'     .names = function(wall_thickness, roof_insulation) {
        #'         sprintf("wall_%.2f_roof_%.0f", wall_thickness, roof_insulation)
        #'     }
        #' )
        #' }
        #'
        apply_measure = function(measure, ..., .names = NULL) {
            gaopt__apply_measure(super, self, private, measure, ..., .names = .names)
        },
        # }}}

        # objective {{{
        #' @description
        #' Set objective function(s) for optimization
        #'
        #' @details
        #' `$objective()` takes one or more **named** objective functions to be
        #' optimized. Each objective function should take an [eplusr::EplusJob]
        #' object as the first argument and return either:
        #'
        #' * A single numeric value
        #' * A named numeric vector
        #' * A named list with numeric values
        #' * A single-row data.table with numeric columns
        #'
        #' ## Syntax Options
        #'
        #' **All objectives must be named.** There are two ways to specify
        #' objectives:
        #'
        #' **1. Named syntax** (using `=`):
        #'
        #' ```
        #' opt$objective(energy = my_energy_function)
        #' ```
        #'
        #' The objective will use the specified name and be **maximized** by
        #' default. This syntax does **not** support prefixes (`-`/`+`).
        #'
        #' **2. Assignment syntax** (using `:=`):
        #'
        #' ```
        #' opt$objective("obj1" := my_function)
        #' opt$objective(c("obj1", "obj2") := my_multi_objective_function)
        #' ```
        #'
        #' Use this when you want to:
        #' - Specify optimization direction using prefixes (`-`/`+`)
        #' - Have a single function return multiple objective values
        #'
        #' The function should return a named vector, list, or data.table. Each
        #' objective will be **maximized** by default.
        #'
        #' ## Optimization Direction
        #'
        #' **By default, all objectives are MAXIMIZED.** To change the
        #' optimization direction, use prefixes **only with the `:=` syntax**:
        #'
        #' * Prefix the **objective name** with `-` to **minimize**:
        #'   ```
        #'   opt$objective(-"energy" := my_function)
        #'   ```
        #'
        #' * Prefix the **objective name** with `+` to **maximize** (explicit):
        #'   ```
        #'   opt$objective(+"comfort" := my_function)
        #'   ```
        #'
        #' * For multiple objectives from one function:
        #'   ```
        #'   opt$objective(c(-"hot", -"cold", +"comfort") := get_zone_comfort)
        #'   ```
        #'   This minimizes "hot" and "cold" while maximizing "comfort".
        #'
        #' **Note**: The `=` syntax does **not** support prefixes. To control
        #' direction with `=` syntax, use the `.dir` parameter.
        #'
        #' ## The `.dir` Parameter
        #'
        #' The `.dir` parameter provides a way to **override** optimization
        #' directions specified by prefixes or defaults.
        #'
        #' **When `.dir = NULL` (default)**:
        #' * Objectives follow their prefix direction (`-` for minimize, `+` for maximize)
        #' * Objectives without prefix are maximized by default
        #'
        #' **When `.dir` is specified** (`"min"` or `"max"`):
        #' * `.dir` **overrides all prefix directions**
        #' * All objectives use the direction specified by `.dir`
        #'
        #' For example:
        #'
        #' ```
        #' # Without .dir: follows prefix
        #' opt$objective(-"energy" := total_energy)  # minimizes
        #'
        #' # With .dir: overrides prefix
        #' opt$objective(-"energy" := total_energy, .dir = "max")  # maximizes (overridden)
        #' opt$objective(+"comfort" := comfort_hours, .dir = "min")  # minimizes (overridden)
        #' ```
        #'
        #' This is useful when you want to quickly reverse all optimization
        #' directions without changing the code.
        #'
        #' @param ... One or more **named** objective functions. All objectives must
        #'        be named using one of two syntax forms:
        #'        - Named: `name = my_function` (maximized by default, no prefix support)
        #'        - Assignment: `"name" := my_function` or `c("name1", "name2") := my_function`
        #'          (for functions returning multiple values)
        #'        **Prefixes (`-`/`+`) are only supported with `:=` syntax**:
        #'        prefix objective names with `-` to minimize or `+` to explicitly
        #'        maximize (e.g., `-"energy" := my_function` or `c(-"hot", +"comfort") := my_function`).
        #' @param .dir Direction of optimization. Can be:
        #'        - `NULL` (default): All objectives follow their prefix direction
        #'          (maximize by default, or as specified by `-`/`+` prefixes)
        #'        - `"min"` or `"max"`: A single direction that **overrides** all
        #'          prefix directions
        #'        - A character vector with one element per objective: Individual
        #'          directions that **override** each objective's prefix direction
        #'        When specified, `.dir` takes precedence over any prefix (`-`/`+`)
        #'        settings.
        #'
        #' @return The modified `GAOptimJob` object itself.
        #'
        #' @examples
        #' \dontrun{
        #' # Define objective functions
        #' total_energy <- function(job) {
        #'     as.numeric(
        #'         job$tabular_data(
        #'             table_name = "Site and Source Energy",
        #'             column_name = "Total Energy",
        #'             row_name = "Total Site Energy"
        #'         )$value
        #'     )
        #' }
        #'
        #' comfort_hours <- function(job) {
        #'     8760 - as.numeric(
        #'         job$tabular_data(
        #'             table_name = "Comfort and Setpoint Not Met Summary",
        #'             column_name = "Facility",
        #'             row_name = "Time Not Comfortable Based on Simple ASHRAE 55-2004"
        #'         )$value
        #'     )
        #' }
        #'
        #' # Function returning multiple objectives
        #' get_zone_comfort <- function(job) {
        #'     temp <- job$report_data(
        #'         key_value = c("Core_top", "Core_bottom"),
        #'         name = "Zone Mean Air Temperature"
        #'     )
        #'     temp[, list(
        #'         hot = sum(value > 28) / .N,
        #'         cold = sum(value < 18) / .N,
        #'         comfort = sum(value >= 20 & value <= 26) / .N
        #'     )]
        #' }
        #'
        #' # ---- Named syntax (= syntax, no prefix support) ----
        #' # Maximize total_energy (default behavior)
        #' opt$objective(energy = total_energy)
        #'
        #' # Maximize comfort_hours
        #' opt$objective(comfort = comfort_hours)
        #'
        #' # Multi-objective: both maximized by default
        #' opt$objective(
        #'     energy = total_energy,
        #'     comfort = comfort_hours
        #' )
        #'
        #' # ---- Assignment syntax (:= syntax, supports prefixes) ----
        #' # Minimize total_energy using - prefix
        #' opt$objective(-"energy" := total_energy)
        #'
        #' # Explicitly maximize using + prefix
        #' opt$objective(+"comfort" := comfort_hours)
        #'
        #' # Multi-objective with different directions
        #' opt$objective(
        #'     -"energy" := total_energy,
        #'     +"comfort" := comfort_hours
        #' )
        #'
        #' # ---- One function returning multiple objectives ----
        #' # Minimize hot and cold, maximize comfort
        #' opt$objective(
        #'     c(-"hot", -"cold", +"comfort") := get_zone_comfort
        #' )
        #'
        #' # ---- Combined example (mixing = and := syntax) ----
        #' opt$objective(
        #'     energy = total_energy,              # = syntax: maximize (use .dir to change)
        #'     comfort = comfort_hours,            # = syntax: maximize (use .dir to change)
        #'     c(-"hot", -"cold") := get_zone_comfort  # := syntax: minimize both
        #' )
        #'
        #' # ---- Using .dir parameter ----
        #' # Default behavior (.dir = NULL): follow prefix or default to maximize
        #' opt$objective(energy = total_energy)  # maximizes energy
        #' opt$objective(-"energy" := total_energy)  # minimizes energy
        #'
        #' # Using .dir to set direction (works with both = and := syntax)
        #' opt$objective(energy = total_energy, .dir = "min")  # minimizes energy
        #'
        #' # .dir OVERRIDES prefix directions
        #' opt$objective(-"energy" := total_energy, .dir = "max")  # maximizes (overridden!)
        #' opt$objective(+"comfort" := comfort_hours, .dir = "min")  # minimizes (overridden!)
        #'
        #' # Using .dir with multiple objectives
        #' opt$objective(
        #'     energy = total_energy,
        #'     comfort = comfort_hours,
        #'     .dir = c("min", "max")  # minimize energy, maximize comfort
        #' )
        #'
        #' # Useful for quickly reversing all directions
        #' opt$objective(
        #'     -"energy" := total_energy,
        #'     -"cost" := total_cost,
        #'     +"comfort" := comfort_hours,
        #'     .dir = "max"  # All become maximize (overrides all prefixes)
        #' )
        #' }
        #'
        #'
        #'
        objective = function(..., .dir = NULL) {
            gaopt__objective(super, self, private, ..., .dir = .dir, .env = parent.frame())
        },
        # }}}

        # recombinator {{{
        #' @description
        #' Configure recombination operators for genetic algorithm
        #'
        #' @details
        #' `$recombinator()` sets the recombination (crossover) operators used
        #' in the genetic algorithm. Different operators can be specified for
        #' different parameter types.
        #'
        #' ## Default Operators
        #'
        #' The following default recombination operators are used for each
        #' parameter type:
        #'
        #' * `.double`: `miesmuschel::rec("sbx", n = 15)` - Simulated Binary
        #'   Crossover with distribution index 15
        #' * `.integer`: `miesmuschel::rec("xounif", p = 0.7)` - Uniform
        #'   crossover with probability 0.7
        #' * `.choice`: `miesmuschel::rec("xounif", p = 0.7)` - Uniform
        #'   crossover with probability 0.7
        #' * `.logical`: `miesmuschel::rec("xounif", p = 0.7)` - Uniform
        #'   crossover with probability 0.7
        #' * `.untyped`: `miesmuschel::rec("xounif", p = 0.7)` - Uniform
        #'   crossover with probability 0.7
        #'
        #' ## Parameter-Specific vs Type-Wide Operators
        #'
        #' There are two ways to configure recombination operators:
        #'
        #' **1. Type-wide operators** (using `.double`, `.integer`, etc.):
        #' These set the default operator for **all parameters** of a given
        #' type. For example, `.double` applies to all double-type parameters.
        #'
        #' **2. Parameter-specific operators** (using `...`):
        #' These set operators for **individual parameters** by name, overriding
        #' the type-wide defaults. Use the syntax `parameter_name = rec(...)`.
        #'
        #' Parameter-specific operators take precedence over type-wide operators.
        #'
        #' @param ... Named recombination operators for specific parameters. Use
        #'        the syntax `parameter_name = rec(...)` to assign a recombination
        #'        operator to a specific parameter. These override the type-wide
        #'        operators (`.double`, `.integer`, etc.) for the specified
        #'        parameters.
        #' @param .double Recombination operator for all double-type parameters.
        #'        Default: `miesmuschel::rec("sbx", n = 15)`.
        #' @param .integer Recombination operator for all integer-type parameters.
        #'        Default: `miesmuschel::rec("xounif", p = 0.7)`.
        #' @param .choice Recombination operator for all choice-type parameters.
        #'        Default: `miesmuschel::rec("xounif", p = 0.7)`.
        #' @param .logical Recombination operator for all logical-type parameters.
        #'        Default: `miesmuschel::rec("xounif", p = 0.7)`.
        #' @param .untyped Recombination operator for all untyped parameters.
        #'        Default: `miesmuschel::rec("xounif", p = 0.7)`.
        #'
        #' @return The modified `GAOptimJob` object itself.
        #'
        #' @examples
        #' \dontrun{
        #' # Use default recombinators for all parameters
        #' opt$recombinator()
        #'
        #' # Set type-wide recombinator: all double parameters use SBX with n = 20
        #' opt$recombinator(.double = miesmuschel::rec("sbx", n = 20, p = 0.9))
        #'
        #' # Set parameter-specific recombinator: only "wall_thickness" uses SBX with n = 30
        #' opt$recombinator(wall_thickness = miesmuschel::rec("sbx", n = 30))
        #'
        #' # Combine both: set type-wide default, then override specific parameters
        #' opt$recombinator(
        #'     .double = miesmuschel::rec("sbx", n = 20),  # All doubles use this
        #'     wall_thickness = miesmuschel::rec("sbx", n = 30),  # Except this one
        #'     window_ratio = miesmuschel::rec("xounif", p = 0.5)  # And this one
        #' )
        #' }
        #'
        recombinator = function(
            ...,
            .double = miesmuschel::rec("sbx", n = 15),
            .integer = miesmuschel::rec("xounif", p = 0.7),
            .choice = miesmuschel::rec("xounif", p = 0.7),
            .logical = miesmuschel::rec("xounif", p = 0.7),
            .untyped = miesmuschel::rec("xounif", p = 0.7)
        ) {
            gaopt__recombinator(
                super,
                self,
                private,
                ...,
                .double = .double,
                .integer = .integer,
                .choice = .choice,
                .logical = .logical,
                .untyped = .untyped
            )
        },
        # }}}

        # mutator {{{
        #' @description
        #' Configure mutation operators for genetic algorithm
        #'
        #' @details
        #' `$mutator()` sets the mutation operators used in the genetic
        #' algorithm. Different operators can be specified for different
        #' parameter types.
        #'
        #' ## Default Operators
        #'
        #' The following default mutation operators are used for each parameter
        #' type:
        #'
        #' * `.double`: `miesmuschel::mut("gauss", sdev = 0.05)` - Gaussian
        #'   mutation with standard deviation 0.05
        #' * `.integer`: `miesmuschel::mut("gauss", sdev = 0.05)` - Gaussian
        #'   mutation with standard deviation 0.05
        #' * `.choice`: `miesmuschel::mut("unif")` - Uniform mutation
        #' * `.logical`: `miesmuschel::mut("unif")` - Uniform mutation
        #' * `.untyped`: `miesmuschel::mut("unif")` - Uniform mutation
        #'
        #' ## Parameter-Specific vs Type-Wide Operators
        #'
        #' There are two ways to configure mutation operators:
        #'
        #' **1. Type-wide operators** (using `.double`, `.integer`, etc.):
        #' These set the default operator for **all parameters** of a given
        #' type. For example, `.double` applies to all double-type parameters.
        #'
        #' **2. Parameter-specific operators** (using `...`):
        #' These set operators for **individual parameters** by name, overriding
        #' the type-wide defaults. Use the syntax `parameter_name = mut(...)`.
        #'
        #' Parameter-specific operators take precedence over type-wide operators.
        #'
        #' @param ... Named mutation operators for specific parameters. Use the
        #'        syntax `parameter_name = mut(...)` to assign a mutation operator
        #'        to a specific parameter. These override the type-wide operators
        #'        (`.double`, `.integer`, etc.) for the specified parameters.
        #' @param .double Mutation operator for all double-type parameters.
        #'        Default: `miesmuschel::mut("gauss", sdev = 0.05)`.
        #' @param .integer Mutation operator for all integer-type parameters.
        #'        Default: `miesmuschel::mut("gauss", sdev = 0.05)`.
        #' @param .choice Mutation operator for all choice-type parameters.
        #'        Default: `miesmuschel::mut("unif")`.
        #' @param .logical Mutation operator for all logical-type parameters.
        #'        Default: `miesmuschel::mut("unif")`.
        #' @param .untyped Mutation operator for all untyped parameters.
        #'        Default: `miesmuschel::mut("unif")`.
        #'
        #' @return The modified `GAOptimJob` object itself.
        #'
        #' @examples
        #' \dontrun{
        #' # Use default mutators for all parameters
        #' opt$mutator()
        #'
        #' # Set type-wide mutator: all double parameters use polynomial mutation
        #' opt$mutator(.double = miesmuschel::mut("poly", eta = 30, p = 0.2))
        #'
        #' # Set parameter-specific mutator: only "wall_thickness" uses polynomial mutation
        #' opt$mutator(wall_thickness = miesmuschel::mut("poly", eta = 30, p = 0.2))
        #'
        #' # Combine both: set type-wide default, then override specific parameters
        #' opt$mutator(
        #'     .double = miesmuschel::mut("gauss", sdev = 0.1),  # All doubles use this
        #'     wall_thickness = miesmuschel::mut("poly", eta = 30),  # Except this one
        #'     window_ratio = miesmuschel::mut("poly", eta = 20)    # And this one
        #' )
        #' }
        #'
        mutator = function(
            ...,
            .double = miesmuschel::mut("gauss", sdev = 0.05),
            .integer = miesmuschel::mut("gauss", sdev = 0.05),
            .choice = miesmuschel::mut("unif"),
            .logical = miesmuschel::mut("unif"),
            .untyped = miesmuschel::mut("unif")
        ) {
            gaopt__mutator(
                super,
                self,
                private,
                ...,
                .double = .double,
                .integer = .integer,
                .choice = .choice,
                .logical = .logical,
                .untyped = .untyped
            )
        },
        # }}}

        # selector {{{
        #' @description
        #' Configure selection operators for genetic algorithm
        #'
        #' @details
        #' `$selector()` sets the selection operators and survival strategy
        #' used in the genetic algorithm.
        #'
        #' ## Selection Operators
        #'
        #' **Parent selection** (`parent`): Determines how individuals are
        #' selected from the current population for mating (recombination).
        #' Common strategies include random selection, tournament selection, etc.
        #'
        #' **Survival selection** (`survival`): Determines which individuals
        #' survive to the next generation after offspring are created. The
        #' default uses non-dominated sorting with crowding distance as a
        #' tiebreaker, which is suitable for multi-objective optimization.
        #'
        #' ## Survival Strategy
        #'
        #' The `strategy` parameter controls how parents and offspring compete:
        #'
        #' * `"plus"`: (mu + lambda) strategy - Both parents and offspring
        #'   compete for survival. The best `mu` individuals from the combined
        #'   pool of parents and offspring are selected.
        #' * `"comma"`: (mu, lambda) strategy - Only offspring are considered
        #'   for survival. Parents are discarded. Requires `lambda >= mu`.
        #'
        #' ## Default Values
        #'
        #' * `parent`: `miesmuschel::sel("random")` - Random parent selection
        #' * `survival`: `miesmuschel::sel("best", miesmuschel::scl("nondom", tiebreak = "crowdingdist"))`
        #'   - Non-dominated sorting with crowding distance tiebreaker (for
        #'   multi-objective optimization)
        #' * `strategy`: `"plus"` - (mu + lambda) strategy
        #'
        #' @param parent Parent selection operator. Default:
        #'        `miesmuschel::sel("random")`.
        #' @param survival Survival selection operator. Default:
        #'        `miesmuschel::sel("best", miesmuschel::scl("nondom", tiebreak = "crowdingdist"))`.
        #' @param strategy Survival strategy, either `"plus"` or `"comma"`.
        #'        Default: `"plus"`.
        #'
        #' @return The modified `GAOptimJob` object itself.
        #'
        #' @examples
        #' \dontrun{
        #' # Use default selectors (random parent, non-dominated survival, plus strategy)
        #' opt$selector()
        #'
        #' # Use tournament selection for parents
        #' opt$selector(parent = miesmuschel::sel("tournament", k = 3))
        #'
        #' # Use comma strategy (only offspring survive)
        #' opt$selector(strategy = "comma")
        #'
        #' # Customize both parent and survival selection
        #' opt$selector(
        #'     parent = miesmuschel::sel("tournament", k = 5),
        #'     survival = miesmuschel::sel("best"),
        #'     strategy = "plus"
        #' )
        #' }
        #'
        selector = function(
            parent = miesmuschel::sel("random"),
            survival = miesmuschel::sel("best", miesmuschel::scl("nondom", tiebreak = "crowdingdist")),
            strategy = "plus"
        ) {
            gaopt__selector(super, self, private, parent = parent, survival = survival, strategy = strategy)
        },
        # }}}

        # terminator {{{
        #' @description
        #' Configure termination conditions for genetic algorithm
        #'
        #' @details
        #' `$terminator()` sets the conditions under which the optimization
        #' will stop. Multiple conditions can be specified, and the algorithm
        #' will terminate when any condition is met.
        #'
        #' Available termination conditions:
        #'
        #' * `max_gen`: Maximum number of generations
        #' * `max_eval`: Maximum number of function evaluations
        #' * `max_time`: Maximum time in seconds
        #' * Custom terminators: Pass [bbotk::Terminator] objects via `...`
        #'
        #' @param ... One or more custom terminator objects created using
        #'        [bbotk::trm()] or other bbotk terminator constructors.
        #' @param max_gen Maximum number of generations. Default: `NULL` (no
        #'        limit).
        #' @param max_eval Maximum number of evaluations. Default: `NULL` (no
        #'        limit).
        #' @param max_time Maximum time in seconds. Default: `NULL` (no limit).
        #'
        #' @return The modified `GAOptimJob` object itself.
        #'
        #' @examples
        #' \dontrun{
        #' # terminate after 100 generations
        #' opt$terminator(max_gen = 100)
        #'
        #' # terminate after 1000 evaluations or 1 hour
        #' opt$terminator(max_eval = 1000, max_time = 3600)
        #'
        #' # use custom terminator from bbotk
        #' opt$terminator(bbotk::trm("stagnation", iters = 10))
        #'
        #' # combine multiple terminators
        #' opt$terminator(
        #'     bbotk::trm("stagnation", iters = 10),
        #'     max_gen = 100,
        #'     max_time = 3600
        #' )
        #' }
        #'
        terminator = function(..., max_gen = NULL, max_eval = NULL, max_time = NULL) {
            gaopt__terminator(
                super,
                self,
                private,
                ...,
                max_gen = max_gen,
                max_eval = max_eval,
                max_time = max_time
            )
        },
        # }}}

        # run {{{
        #' @description
        #' Run genetic algorithm optimization
        #'
        #' @details
        #' `$run()` executes the genetic algorithm optimization process. The
        #' algorithm follows these steps in each generation:
        #'
        #' 1. Select parents from the current population
        #' 1. Generate offspring through recombination (crossover)
        #' 1. Apply mutation to offspring
        #' 1. Evaluate fitness of offspring by running EnergyPlus simulations
        #' 1. Select survivors for the next generation
        #' 1. Check termination conditions
        #'
        #' The optimization continues until one of the termination conditions
        #' set by `$terminator()` is met.
        #'
        #' ## Prerequisites
        #'
        #' Before calling `$run()`, you must configure the following components:
        #'
        #' - **Measure**: At least one parametric measure must be set via
        #'   `$apply_measure()`. This defines which IDF parameters will be
        #'   optimized.
        #' - **Objective**: At least one objective function must be set via
        #'   `$objective()`. This defines what to optimize (minimize or
        #'   maximize).
        #' - **Genetic operators**: The following operators must be configured:
        #'   - Mutator via `$mutator()`
        #'   - Recombinator via `$recombinator()`
        #'   - Selector via `$selector()`
        #' - **Terminator**: At least one termination condition must be set via
        #'   `$terminator()`. This determines when the optimization stops.
        #'
        #' If any of these prerequisites are missing, `$run()` will throw an
        #' error indicating which configuration is missing.
        #'
        #' ## Population and Offspring Size
        #'
        #' The `mu` parameter controls the population size (number of
        #' individuals maintained in each generation), while `lambda` controls
        #' the offspring size (number of new individuals generated per
        #' generation).
        #'
        #' The current implementation uses the **(mu + lambda)** survival
        #' strategy by default, where survivors are selected from the combined
        #' pool of `mu` parents and `lambda` offspring. This allows offspring
        #' to compete directly with their parents, which can help preserve good
        #' solutions.
        #'
        #' Common configurations:
        #' - `lambda = mu` (default): Balanced exploration and exploitation
        #' - `lambda > mu`: More exploration, higher diversity
        #' - `lambda < mu`: More exploitation, faster convergence
        #'
        #' Use `$best_set()` for single-objective or
        #' `$pareto_set()` for multi-objective optimization to extract the
        #' final results.
        #'
        #' @param mu Population size (number of individuals in each
        #'        generation). Default: `20L`.
        #' @param lambda Offspring size (number of new individuals generated
        #'        per generation). Default: `mu` (same as population size).
        #' @param dir Output directory for simulation results. If `NULL`, uses
        #'        the directory of the seed IDF file. Default: `NULL`.
        #' @param separate If `TRUE`, each individual's simulation results are
        #'        saved in a separate folder. If `FALSE`, all results are saved
        #'        in the same folder. Default: `TRUE`.
        #' @param parallel Controls parallel execution of EnergyPlus simulations
        #'        via the `mirai` package. Accepts three types of values:
        #'        - **Logical** (`TRUE`/`FALSE`): If `TRUE` and no daemons are
        #'          currently running, automatically starts daemons using 80% of
        #'          available CPU cores. If `FALSE`, does not modify daemon
        #'          settings (uses existing daemons if available, or runs
        #'          sequentially if none are set).
        #'        - **Positive integer**: Specifies the exact number of CPU cores
        #'          (daemons) to use for parallel execution.
        #'        - **List**: Advanced configuration passed directly to
        #'          `mirai::daemons()` for custom daemon setup.
        #'        Default: `TRUE`.
        #'
        #' @return The modified `GAOptimJob` object itself.
        #'
        #' @examples
        #' \dontrun{
        #' # run with default settings (mu = 20, lambda = 20, auto parallel)
        #' opt$run()
        #'
        #' # run with larger population
        #' opt$run(mu = 50)
        #'
        #' # run with different offspring size (more exploration)
        #' opt$run(mu = 20, lambda = 40)
        #'
        #' # run with custom output directory
        #' opt$run(mu = 30, dir = "~/optimization_results")
        #'
        #' # run with specific number of CPU cores
        #' opt$run(parallel = 4)
        #'
        #' # run without modifying existing daemon settings
        #' # (useful if you've already set up daemons manually)
        #' opt$run(parallel = FALSE)
        #'
        #' # advanced: custom daemon configuration
        #' opt$run(parallel = list(n = 8, dispatcher = TRUE))
        #'
        #' # combine multiple parameters
        #' opt$run(mu = 50, lambda = 100, dir = tempdir(), parallel = 6)
        #' }
        #'
        run = function(mu = 20L, lambda = mu, dir = NULL, separate = TRUE, parallel = TRUE) {
            gaopt__run(super, self, private, mu, lambda, dir, separate, parallel)
        },
        # }}}

        # best_set {{{
        #' @description
        #' Extract best solution for single-objective optimization
        #'
        #' @details
        #' `$best_set()` returns the best solution(s) found during
        #' single-objective optimization. This method only works when a single
        #' objective function has been set.
        #'
        #' The returned [data.table::data.table()] contains:
        #'
        #' * Parameter values for the best solution(s)
        #' * Objective function value(s)
        #' * Generation and individual indices (if `unique = FALSE`)
        #'
        #' When `unique = TRUE`, only the best parameter combination is
        #' returned. When `unique = FALSE`, all occurrences of the best
        #' solution across generations are returned.
        #'
        #' @param unique If `TRUE`, return only unique best parameter
        #'        combinations. If `FALSE`, return all occurrences of the best
        #'        solution across generations. Default: `TRUE`.
        #'
        #' @return A [data.table::data.table()] containing the best solution(s).
        #'
        #' @examples
        #' \dontrun{
        #' # get best solution
        #' best <- opt$best_set()
        #'
        #' # get all occurrences of best solution
        #' best_all <- opt$best_set(unique = FALSE)
        #' }
        #'
        best_set = function(unique = TRUE) {
            gaopt__best_set(super, self, private, unique = unique)
        },
        # }}}

        # pareto_set {{{
        #' @description
        #' Extract Pareto-optimal solutions for multi-objective optimization
        #'
        #' @details
        #' `$pareto_set()` returns the Pareto-optimal (non-dominated)
        #' solutions found during multi-objective optimization. This method
        #' only works when multiple objective functions have been set.
        #'
        #' A solution is Pareto-optimal if no other solution is better in all
        #' objectives simultaneously. The returned [data.table::data.table()]
        #' contains:
        #'
        #' * Parameter values for each Pareto-optimal solution
        #' * Objective function values for each solution
        #' * Generation and individual indices (if `unique = FALSE`)
        #'
        #' When `unique = TRUE`, only unique Pareto-optimal parameter
        #' combinations are returned. When `unique = FALSE`, all non-dominated
        #' solutions from all generations are returned.
        #'
        #' @param unique If `TRUE`, return only unique Pareto-optimal parameter
        #'        combinations. If `FALSE`, return all non-dominated solutions
        #'        from all generations. Default: `TRUE`.
        #'
        #' @return A [data.table::data.table()] containing Pareto-optimal
        #'         solutions.
        #'
        #' @examples
        #' \dontrun{
        #' # get Pareto-optimal solutions
        #' pareto <- opt$pareto_set()
        #'
        #' # get all non-dominated solutions from all generations
        #' pareto_all <- opt$pareto_set(unique = FALSE)
        #' }
        #'
        pareto_set = function(unique = TRUE) {
            gaopt__pareto_set(super, self, private, unique = unique)
        },
        # }}}

        # population {{{
        #' @description
        #' Extract complete population history
        #'
        #' @details
        #' `$population()` returns the complete history of all individuals
        #' evaluated during the optimization process across all generations.
        #'
        #' The returned [data.table::data.table()] contains:
        #'
        #' * `index_gen`: Generation number
        #' * `index_ind`: Individual number within generation
        #' * Parameter values for each individual
        #' * Objective function values for each individual
        #'
        #' This is useful for analyzing the optimization progress and
        #' understanding how the population evolved over generations.
        #'
        #' @return A [data.table::data.table()] containing all evaluated
        #'         individuals.
        #'
        #' @examples
        #' \dontrun{
        #' # get complete population history
        #' pop <- opt$population()
        #'
        #' # analyze optimization progress
        #' library(ggplot2)
        #' ggplot(pop, aes(x = index_gen, y = objective_value)) +
        #'     geom_point() +
        #'     geom_smooth()
        #' }
        #'
        population = function() {
            gaopt__population(super, self, private)
        },
        # }}}

        # print {{{
        #' @description
        #' Print GAOptimJob object
        #'
        #' @details
        #' `$print()` prints a summary of the `GAOptimJob` object, including:
        #'
        #' * Seed IDF and EPW file information
        #' * Optimization parameters and their ranges
        #' * Objective function(s)
        #' * Genetic algorithm configuration (operators, termination conditions)
        #' * Optimization status and progress (if optimization has been run)
        #'
        #' This method is automatically called when you type the object name in
        #' the console.
        #'
        #' @return The `GAOptimJob` object itself, invisibly.
        #'
        #' @examples
        #' \dontrun{
        #' # print object summary
        #' opt$print()
        #'
        #' # or simply
        #' opt
        #' }
        #'
        print = function() {
            gaopt__print(super, self, private)
        }
        # }}}
        # }}}
    ),

    private = list(
        # PRIVATE FIELDS {{{
        m_seed = NULL,
        m_idfs = NULL,
        m_job = NULL,
        m_log = NULL,
        m_opt = NULL,
        m_ctrl = NULL
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
gaoptim_job <- function(idf, epw) {
    bc <- GAOptimJob$new(idf, epw)
    lockEnvironment(bc)
    bc
}
# }}}

# gaopt_apply_measure {{{
gaopt__apply_measure <- function(super, self, private, measure, ..., .names = NULL, .env = parent.frame()) {
    # clean up previous optimization
    private$m_opt <- NULL
    # clean up previous measure
    private$m_log$measure <- NULL

    checkmate::assert_function(measure)
    fmls <- formals(measure)
    if (length(fmls) < 2L) {
        stop("'measure' function must have at least two argument")
    }

    nms_fmls <- names(fmls)
    if (nms_fmls[[1L]] != "idf") {
        stop("First argument of measure function must be 'idf'.")
    }

    # check ... are all named and are Domain objects
    if (...length() == 0L) {
        stop("No parameter has been provided.")
    }
    dots <- list(...)
    nms_dots <- names(dots)
    if (is.null(nms_dots) || any(!nzchar(nms_dots))) {
        if (is.null(nms_dots)) {
            nms_dots <- rep("", length(dots))
        }
        stop(sprintf(
            paste(
                "Parameters must be all named.",
                "But the following parameters are not named: [%s]."
            ),
            paste0(nms_dots[!nzchar(nms_dots)], collapse = ", ")
        ))
    }
    if (any(!vapply(dots, inherits, logical(1L), "Domain"))) {
        stop(sprintf(
            paste(
                "Parameters must be all 'Domain' objects.",
                "But the following parameters are not: [%s]."
            ),
            paste0(nms_dots[!vapply(dots, inherits, logical(1L), "Domain")], collapse = ", ")
        ))
    }
    if (any(!nms_dots %in% nms_fmls)) {
        stop(sprintf(
            paste(
                "Parameters must be arguments of measure function.",
                "But the following parameters are not: [%s]."
            ),
            paste0(nms_dots[!nms_dots %in% nms_fmls], collapse = ", ")
        ))
    }

    # in case 'function(idf, ...)' is specified as a measure
    if (is.name(substitute(measure, .env))) {
        bare <- FALSE
        mea_nm <- deparse(substitute(measure, .env))
    } else {
        bare <- TRUE
        mea_nm <- "case"
    }

    if (!is.null(.names)) {
        fun_name <- as_function(.names, .env)
        if (is.null(fun_name)) {
            stop("'.names' must be a function or a lambda function.")
        }
        .names <- fun_name
    }

    private$m_log$bare <- bare
    private$m_log$simple <- FALSE
    private$m_log$measure$name <- mea_nm
    private$m_log$measure$func <- measure
    private$m_log$measure$deps <- find_dependencies(measure, env = .env)
    private$m_log$measure$naming <- .names
    private$m_log$parameter <- do.call(paradox::ps, dots)

    self
}
# }}}

# gaopt_param {{{
gaopt__param <- function(super, self, private, ..., .names = NULL, .env = parent.frame()) {
    stop("'$param()' is not implemented for 'GAOptimJob'. Please use '$apply_measure()' instead.")
    # clean up previous optimization
    private$m_opt <- NULL
    # clean up previous measure
    private$m_log$measure <- NULL

    # clean measure created using $apply_measure() if any
    private$m_log$measure$name <- NULL
    private$m_log$measure$fun <- NULL
    private$m_log$matched <- NULL

    expanded <- eplusr::expand_idf_dots_value(
        get_priv_env(private$m_seed)$idd_env(),
        get_priv_env(private$m_seed)$idf_env(),
        ...,
        .type = "object",
        .complete = FALSE,
        .unique = TRUE,
        .empty = FALSE,
        .default = FALSE,
        .scalar = FALSE,
        .pair = FALSE,
        .env = .env
    )
    if (vapply(expanded$value, inherits, logical(1L), "Domain")) {
        stop(sprintf(
            paste(
                "All parameters must be 'Domain' objects.",
                "But the following parameters are not: [%s]."
            ),
            paste0(expanded$param_name[!vapply(expanded$value, inherits, logical(1L), "Domain")], collapse = ", ")
        ))
    }

    self
}
# }}}

# gaopt__objective {{{
gaopt__objective <- function(super, self, private, ..., .dir = NULL, .env = parent.frame()) {
    if (...length() == 0L) {
        stop("No objective function has been provided.")
    }
    # clean up previous optimization
    private$m_opt <- NULL
    # clean up previous objectives
    private$m_log$objective <- NULL

    elems <- substitute(list(...))[-1L]
    nms <- names(elems)
    if (is.null(nms)) {
        nms <- character(length(elems))
    }

    nmdir_obj <- vector("list", length(elems))
    func_obj <- vector("list", length(elems))
    names(func_obj) <- ""

    for (i in seq_along(elems)) {
        elem <- elems[[i]]
        if (nms[[i]] == "") {
            if (is.symbol(elem) || (is.call(elem) && !identical(elem[[1L]], as.symbol(":=")))) {
                stop(sprintf("Objective must be named. But objective [%i] is not", i))
            }

            deparsed <- gaopt__objective_dir(elem[[2L]])
            if (!is.null(names(deparsed))) {
                deparsed <- list(deparsed)
            }
            nmdir_obj[[i]] <- deparsed
            sym_obj <- elem[[3L]]
        } else {
            # e.g. "name" = fun or name = fun, all maximize
            nmdir_obj[[i]] <- list(list(name = nms[[i]], dir = "maximize"))
            sym_obj <- elem
        }

        func <- eval(sym_obj, .env)
        deps <- find_dependencies(func, env = .env)
        func_fmls <- formals(func)
        if (is.null(func_fmls)) {
            stop(sprintf(
                paste(
                    "Objective function must have at least one argument.",
                    "But objective function %s has no argument."
                ),
                if (is.symbol(sym_obj)) {
                    sprintf("'%s'", as.character(sym_obj))
                } else {
                    sprintf("#%i", i)
                }
            ))
        }

        func_fmls_nms <- names(func_fmls)
        if (func_fmls_nms[[1L]] != "job") {
            stop(sprintf(
                paste(
                    "First argument of objective function must be 'job'.",
                    "But the one for objective function %s is '%s'."
                ),
                if (is.symbol(sym_obj)) {
                    sprintf("'%s'", as.character(sym_obj))
                } else {
                    sprintf("#%i", i)
                },
                func_fmls_nms[[1L]]
            ))
        }

        if (is.symbol(sym_obj)) {
            names(func_obj)[[i]] <- as.character(sym_obj)
        }
        func_obj[[i]] <- list(func = func, deps = deps, param = func_fmls_nms[-1L])
    }

    # check for duplicated names
    nm_obj <- lapply(nmdir_obj, function(name_dir) {
        vapply(name_dir, .subset2, character(1L), "name")
    })
    dir_obj <- lapply(nmdir_obj, function(name_dir) {
        vapply(name_dir, .subset2, character(1L), "dir")
    })
    if (anyDuplicated.default(unlist(nm_obj, FALSE, FALSE))) {
        nm_obj <- unlist(nm_obj, FALSE, FALSE)
        stop(sprintf(
            "Duplicated objective names found: '%s'.",
            nm_obj[duplicated(nm_obj)][[1L]]
        ))
    }
    num_obj <- sum(lengths(nm_obj))
    dir_obj <- unlist(dir_obj, FALSE, FALSE)
    if (!is.null(.dir)) {
        checkmate::assert_subset(.dir, c("min", "max"), empty.ok = FALSE)

        if (length(.dir) == 1L) {
            .dir <- rep(.dir, num_obj)
        } else if (length(.dir) != num_obj) {
            stop(sprintf(
                "Number of directions (%i) in '.dir' does not match number of objectives (%i).",
                length(.dir),
                num_obj
            ))
        }
        # .dir overrides prefix directions
        dir_obj <- ifelse(.dir == "max", "maximize", "minimize")
    }
    object <- data.table::data.table(
        index = seq_len(num_obj),
        name = unlist(nm_obj, FALSE, FALSE),
        measure = rep(names(func_obj), lengths(nm_obj)),
        direction = dir_obj
    )

    # merge parameter names
    param <- lapply(func_obj, .subset2, "param")
    param <- data.table::data.table(
        index = rep(seq_along(param), lengths(param)),
        measure = rep(names(func_obj), lengths(param)),
        param = unlist(param, FALSE, FALSE)
    )

    # merge objective functions
    func <- lapply(func_obj, .subset2, "func")
    deps <- unlist(lapply(unname(func_obj), .subset2, "deps"))
    deps <- deps[!duplicated(deps)]

    private$m_log$objective$name <- object
    private$m_log$objective$func <- func
    private$m_log$objective$deps <- deps
    private$m_log$objective$param <- param

    self
}

# gaopt__objective_dir {{{
gaopt__objective_dir <- function(name_dir, cur_dir = NULL) {
    if (is.null(cur_dir)) {
        cur_dir <- "maximize"
    }
    if (is.character(name_dir)) {
        # e.g. "name" = fun
        return(list(name = name_dir, dir = cur_dir))
    }

    if (is.symbol(name_dir)) {
        if (identical(name_dir, as.symbol(":="))) {
            # e.g. "name" := fun, or name := fun, or [-+]name := fun
            gaopt__objective_dir(name_dir[-1L])
        } else {
            # e.g. name = fun
            name <- deparse(name_dir, width.cutoff = 500L, backtick = FALSE)
            return(list(name = name, dir = cur_dir))
        }
    }

    if (is.call(name_dir)) {
        # e.g. c(...) := fun
        if (identical(name_dir[[1L]], as.symbol("c"))) {
            name_dir <- name_dir[-1L]
            lapply(name_dir, gaopt__objective_dir)
        } else if (identical(name_dir[[1L]], as.symbol("-"))) {
            # e.g. -name := fun
            gaopt__objective_dir(name_dir[[2L]], "minimize")
        } else if (identical(name_dir[[1L]], as.symbol("+"))) {
            # e.g. +name := fun
            gaopt__objective_dir(name_dir[[2L]], "maximize")
        } else {
            stop("Invalid objective name: ", name_dir)
        }
    } else {
        stop("Invalid objective name: ", name_dir)
    }
}
# }}}

# gaopt_recombinator {{{
gaopt__recombinator <- function(
    super,
    self,
    private,
    ...,
    .double = miesmuschel::rec("sbx", n = 15),
    .integer = miesmuschel::rec("xounif", p = 0.7),
    .choice = miesmuschel::rec("xounif", p = 0.7),
    .logical = miesmuschel::rec("xounif", p = 0.7),
    .untyped = miesmuschel::rec("xounif", p = 0.7)
) {
    # clean up previous optimization
    private$m_opt <- NULL
    gaopt__register_operator(super, self, private, "rec", "specific", ...)
    gaopt__register_operator(
        super,
        self,
        private,
        "rec",
        "default",
        ParamDbl = .double,
        ParamInt = .integer,
        ParamFct = .choice,
        ParamLgl = .logical,
        ParamAny = .untyped
    )

    self
}
# }}}

# gaopt_mutator {{{
gaopt__mutator <- function(
    super,
    self,
    private,
    ...,
    .double = miesmuschel::mut("gauss", sdev = 0.05),
    .integer = miesmuschel::mut("gauss", sdev = 0.05),
    .choice = miesmuschel::mut("unif"),
    .logical = miesmuschel::mut("unif"),
    .untyped = miesmuschel::mut("unif")
) {
    # clean up previous optimization
    private$m_opt <- NULL
    gaopt__register_operator(super, self, private, "mut", "specific", ...)
    gaopt__register_operator(
        super,
        self,
        private,
        "mut",
        "default",
        ParamDbl = .double,
        ParamInt = .integer,
        ParamFct = .choice,
        ParamLgl = .logical,
        ParamAny = .untyped
    )
    self
}
# }}}

# gaopt_selector {{{
gaopt__selector <- function(
    super,
    self,
    private,
    parent = miesmuschel::sel("random"),
    survival = miesmuschel::sel("best", miesmuschel::scl("nondom", tiebreak = "crowdingdist")),
    strategy = "plus"
) {
    # clean up previous optimization
    private$m_opt <- NULL
    gaopt__register_operator(super, self, private, "sel", "parent", parent)
    gaopt__register_operator(super, self, private, "sel", "survival", survival)
    private$m_ctrl$survival_strategy <- match.arg(strategy, c("plus", "comma"))
    self
}
# }}}

# gaopt_terminator {{{
gaopt__terminator <- function(super, self, private, ..., max_eval = NULL, max_gen = NULL, max_time = NULL) {
    # clean up previous optimization
    private$m_opt <- NULL
    if (!is.null(max_eval)) {
        gaopt__register_operator(super, self, private, "trm", "evals", bbotk::trm("evals", n_evals = max_eval))
    }
    if (!is.null(max_gen)) {
        gaopt__register_operator(super, self, private, "trm", "gens", bbotk::trm("gens", generations = max_gen))
    }
    if (!is.null(max_time)) {
        gaopt__register_operator(super, self, private, "trm", "run_time", bbotk::trm("run_time", secs = max_time))
    }
    terms <- list(...)
    if (length(terms) > 0L) {
        for (i in seq_along(terms)) {
            if (!inherits(terms[[i]], "Terminator")) {
                stop(sprintf(
                    paste(
                        "All input must be 'Terminator' objects.",
                        "But the %ith input is not."
                    ),
                    i
                ))
            }
            gaopt__register_operator(super, self, private, "trm", terms[[i]]$id, terms[[i]])
        }
    }

    self
}
# }}}

# gaopt__collect_objectives {{{
gaopt__collect_objectives <- function(job, objectives) {
    name_split <- split(objectives$name, by = "measure")
    out <- vector("list", length(objectives$func))

    for (i in seq_along(objectives$func)) {
        res <- objectives$func[[i]](job)
        if (is.numeric(res)) {
            # validate length
            if (length(res) != nrow(name_split[[i]])) {
                stop(sprintf(
                    paste(
                        "Objective function '%s' should return %i value(s).",
                        "But got %i value(s)."
                    ),
                    names(objectives$func[i]),
                    nrow(name_split[[i]]),
                    length(res)
                ))
            }
            # validate names
            if (is.null(names(res))) {
                names(res) <- name_split[[i]]$name
            } else {
                m <- match(name_split[[i]]$name, names(res))
                if (anyNA(m)) {
                    stop(sprintf(
                        paste(
                            "Objective function '%s' should return value(s) with the following name(s): %s.",
                            "But the following name(s) are not found: %s."
                        ),
                        names(objectives$func[i]),
                        paste0("'", name_split[[i]]$name, "'", collapse = ", "),
                        paste0("'", name_split[[i]]$name[is.na(m)], "'", collapse = ", ")
                    ))
                }
                res <- res[m]
            }
            # validate NA
            if (anyNA(res)) {
                warning(sprintf(
                    "Objective function '%s' returns NA for %s.",
                    names(objectives$func[i]),
                    paste0("'", name_split[[i]]$name[is.na(res)], "'", collapse = ", ")
                ))
            }
            res <- data.table::as.data.table(as.list(res))
        } else if (is.data.frame(res)) {
            # validate columns
            if (ncol(res) != nrow(name_split[[i]])) {
                stop(sprintf(
                    paste(
                        "Objective function '%s' should return %i column(s).",
                        "But got %i column(s)."
                    ),
                    names(objectives$func[i]),
                    nrow(name_split[[i]]),
                    ncol(res)
                ))
            }
            if (nrow(res) != 1L) {
                stop(sprintf(
                    paste(
                        "Objective function '%s' should return a single row.",
                        "But got %i rows."
                    ),
                    names(objectives$func[i]),
                    nrow(res)
                ))
            }

            data.table::setDT(res)
            # validate names
            m <- match(name_split[[i]]$name, names(res))
            if (anyNA(m)) {
                stop(sprintf(
                    paste(
                        "Objective function '%s' should return a data.frame",
                        "containing the following column(s): %s.",
                        "But the following column(s) are not found: %s."
                    ),
                    names(objectives$func[i]),
                    paste0("'", name_split[[i]]$name, "'", collapse = ", "),
                    paste0("'", name_split[[i]]$name[is.na(m)], "'", collapse = ", ")
                ))
            }
            data.table::setcolorder(res, m)
        } else {
            stop(sprintf(
                paste(
                    "Objective function '%s' should return a numeric vector or a data.frame.",
                    "But got '%s'."
                ),
                names(objectives$func[i]),
                class(res)[[1L]]
            ))
        }
        out[[i]] <- res
    }
    do.call(cbind, out)
}
# }}}

# gaopt__optim_instance {{{
gaopt__optim_instance <- function(super, self, private) {
    # clean up previous optimization
    private$m_opt <- NULL
    assert_ready_optim(super, self, private)

    domain <- private$m_log$parameter
    codomain <- vector("list", nrow(private$m_log$objective$name))
    for (i in private$m_log$objective$name$index) {
        codomain[[i]] <- paradox::p_dbl(tags = private$m_log$objective$name$direction[[i]])
    }
    names(codomain) <- private$m_log$objective$name$name
    codomain <- do.call(paradox::ps, codomain)

    obj <- bbotk::ObjectiveRFunDt$new(
        # we only get a data.table as input
        fun = function(
            xdt,
            path_idf,
            path_epw,
            path_dir = NULL,
            sep_dir = FALSE,
            measure,
            objectives,
            naming,
            index_gen
        ) {
            # generate names
            if (is.null(naming)) {
                vec_names <- sprintf("Gen%s-Ind%s", index_gen, seq_len(nrow(xdt)))
            } else {
                vec_names <- vapply(
                    seq_len(nrow(xdt)),
                    FUN.VALUE = character(1L),
                    function(i) {
                        inputs <- as.list(xdt[i, ])
                        nms_fmls <- names(formals(naming))
                        if ("index_gen" %in% nms_fmls) {
                            inputs[["index_gen"]] <- index_gen
                        }
                        if ("index_ind" %in% nms_fmls) {
                            inputs[["index_ind"]] <- i
                        }
                        do.call(naming, inputs)
                    }
                )
            }
            checkmate::assert_string(path_dir, null.ok = TRUE)
            if (is.null(path_dir)) {
                path_dir <- dirname(path_idf)
            }
            if (sep_dir) {
                path_dir <- file.path(path_dir, basename(vec_names))
            }
            path_outs <- file.path(path_dir, paste0(vec_names, ".idf"))

            # mirai::mirai_map supports data.frame as input and will automatically
            # loop over rows
            inputs <- data.table::set(data.table::copy(xdt), NULL, ".__path_out__", path_outs)
            args <- list(
                .x = inputs,
                .f = function(..., .__path_out__, path_idf, path_epw, measure, objectives, names_obj, func_merge) {
                    eplusr::eplusr_option(verbose_info = FALSE)
                    idf <- eplusr::read_idf(path_idf)
                    idf <- do.call(measure, c(idf = idf, ...))
                    if (!eplusr::is_idf(idf)) {
                        stop(sprintf(
                            "Measure function should return an 'Idf' object, not '%s'.",
                            class(idf)[[1L]]
                        ))
                    }
                    idf$save(.__path_out__, overwrite = TRUE)
                    job <- idf$run(path_epw, wait = TRUE, echo = FALSE, copy_external = TRUE, readvars = FALSE)
                    func_merge(job, objectives)
                },
                .args = list(
                    path_idf = path_idf,
                    path_epw = path_epw,
                    measure = measure$func,
                    objectives = objectives,
                    names_obj = codomain$ids(),
                    func_merge = gaopt__collect_objectives
                )
            )
            if (length(measure$deps)) {
                args <- c(args, measure$deps)
            }
            if (length(objectives$deps)) {
                args <- c(args, objectives$deps)
            }
            mirais <- do.call(mirai::mirai_map, args = args)
            fitness <- mirais[mirai::.stop]
            data.table::rbindlist(fitness)
        },
        domain = domain,
        codomain = codomain,
        constants = paradox::ps(
            path_idf = paradox::p_uty(),
            path_epw = paradox::p_uty(),
            path_dir = paradox::p_uty(),
            sep_dir = paradox::p_lgl(),
            measure = paradox::p_uty(),
            objectives = paradox::p_uty(),
            naming = paradox::p_uty(),
            index_gen = paradox::p_int()
        )
    )

    obj$constants$values <- list(
        path_idf = private$m_seed$path(),
        path_epw = private$m_epws_path,
        measure = private$m_log$measure,
        objectives = private$m_log$objective,
        naming = private$m_log$measure$naming
    )

    trms <- bbotk::trm("combo", unlist(private$m_ctrl$trm, use.names = FALSE))
    if (nrow(private$m_log$objective$name) == 1L) {
        bbotk::OptimInstanceBatchSingleCrit$new(obj, terminator = trms)
    } else {
        bbotk::OptimInstanceBatchMultiCrit$new(obj, terminator = trms)
    }
}
# }}}

# gaopt_run {{{
gaopt__run <- function(
    super,
    self,
    private,
    mu = 20L,
    lambda = mu,
    dir = NULL,
    separate = TRUE,
    parallel = TRUE
) {
    assert_ready_optim(super, self, private)
    checkmate::assert_flag(separate)
    if (checkmate::test_flag(parallel)) {
        if (parallel) {
            # if no daemon is running
            if (!mirai::daemons_set()) {
                # start daemons with 80% of available cores
                mirai::daemons(trunc(parallel::detectCores() * 0.8))
            }
        }
    } else if (checkmate::test_count(parallel, positive = TRUE)) {
        mirai::daemons(parallel)
    } else {
        do.call(mirai::daemons, parallel)
    }

    # initialize optimization instance
    optinst <- gaopt__optim_instance(super, self, private)

    # set output directory
    optinst$objective$constants$values$path_dir <- dir
    optinst$objective$constants$values$sep_dir <- separate

    # only include active parameters
    cls_params <- unique(unname(private$m_log$parameter$class))
    mutators <- miesmuschel::mut(
        "combine",
        operators = c(
            private$m_ctrl$mut$specific,
            private$m_ctrl$mut$default[cls_params]
        )
    )
    recombinators <- miesmuschel::rec(
        "combine",
        operators = c(
            private$m_ctrl$rec$specific,
            private$m_ctrl$rec$default[cls_params]
        )
    )
    selector_parent <- private$m_ctrl$sel$parent
    selector_survival <- private$m_ctrl$sel$survival

    # register operators
    miesmuschel::mies_prime_operators(
        search_space = optinst$search_space,
        mutators = list(mutators),
        recombinators = list(recombinators),
        selectors = list(selector_parent, selector_survival)
    )

    mies_survival <- if (private$m_ctrl$survival_strategy == "plus") {
        miesmuschel::mies_survival_plus
    } else {
        miesmuschel::mies_survival_comma
    }

    cli::cat_rule("Initialization | Generation [1]")
    private$m_log$start_time <- Sys.time()

    # get initial population of parameters
    index_gen <- 1L
    cli::cli_progress_step(
        "Creating initial population",
        "Created initial population",
        "Creating initial population failed"
    )
    optinst$objective$constants$values$index_gen <- index_gen
    miesmuschel::mies_init_population(optinst, mu)

    cli::cli_progress_step(
        "Generating offspring",
        "Generated offspring",
        "Generating offspring failed"
    )
    offspring <- miesmuschel::mies_generate_offspring(
        optinst,
        lambda,
        parent_selector = selector_parent,
        mutator = mutators,
        recombinator = recombinators
    )

    cli::cli_progress_step(
        "Evaluating fitness values",
        "Evaluated fitness values",
        "Evaluating fitness values failed"
    )
    is_terminated <- tryCatch(
        miesmuschel::mies_evaluate_offspring(optinst, offspring),
        terminated_error = function(e) e
    )
    if (inherits(is_terminated, "terminated_error")) {
        cli::cli_alert_info("Termination condition reached. Optimization terminated.")
    }

    cli::cli_progress_step(
        "Selecting survivors",
        "Selected survivors",
        "Selecting survivors failed"
    )
    mies_survival(optinst, mu, selector_survival)

    repeat {
        index_gen <- index_gen + 1L
        optinst$objective$constants$values$index_gen <- index_gen
        cli::cat_rule(sprintf("Generation [%i]", index_gen))
        # generate offspring
        cli::cli_progress_step(
            "Generating offspring",
            "Generated offspring",
            "Generating offspring failed"
        )
        offspring <- miesmuschel::mies_generate_offspring(
            optinst,
            lambda,
            parent_selector = selector_parent,
            mutator = mutators,
            recombinator = recombinators
        )

        cli::cli_progress_step(
            "Evaluating fitness values",
            "Evaluated fitness values",
            "Evaluating fitness values failed"
        )
        is_terminated <- tryCatch(
            miesmuschel::mies_evaluate_offspring(optinst, offspring),
            terminated_error = function(e) e
        )
        if (inherits(is_terminated, "terminated_error")) {
            cli::cli_progress_done(result = "done")
            break
        }
        cli::cli_progress_step(
            "Selecting survivors",
            "Selected survivors",
            "Selecting survivors failed",
            spinner = TRUE
        )
        mies_survival(optinst, mu, selector_survival)
    }
    cli::cli_alert_info("Termination condition reached. Optimization terminated.")

    private$m_opt <- optinst
    private$m_log$end_time <- Sys.time()

    self
}
# }}}

# gaopt_register_operator {{{
gaopt__register_operator <- function(super, self, private, type, slot, ...) {
    cls <- switch(type, "rec" = "Recombinator", "mut" = "Mutator", "sel" = "Selector", "trm" = "Terminator")

    # remove existing
    private$m_ctrl[[type]][[slot]] <- NULL

    opts <- list(...)
    if (any(!vapply(opts, inherits, logical(1L), cls))) {
        stop(sprintf(
            paste(
                "All input must be '%s' objects.",
                "But the followings are not: [%s]."
            ),
            cls,
            paste0(which(!vapply(opts, inherits, logical(1L), cls)), collapse = ", ")
        ))
    }
    if (length(opts) == 1L && is.null(names(opts))) {
        opts <- opts[[1L]]
    }
    private$m_ctrl[[type]][[slot]] <- opts

    private$m_ctrl[[type]]
}
# }}}

# gaopt_format_population {{{
gaopt__format_population <- function(result, x, y) {
    data.table::setnames(result, c("dob", "x_id"), c("index_gen", "index_ind"))
    data.table::set(result, NULL, "index_gen", as.integer(result$index_gen))
    data.table::set(result, NULL, "index_ind", as.integer(result$index_ind))
    result[, .SD, .SDcols = c("index_gen", "index_ind", x, y)]
}
# }}}

# gaopt_population {{{
gaopt__population <- function(super, self, private) {
    opt <- private$m_opt

    if (is.null(opt)) {
        message("Optimization has not been run before.")
        return(invisible())
    }

    pop <- data.table::copy(opt$archive$data)
    gaopt__format_population(pop, opt$archive$cols_x, opt$archive$cols_y)
}
# }}}

# gaopt_pareto_set {{{
gaopt__pareto_set <- function(super, self, private, unique = TRUE) {
    opt <- private$m_opt

    if (is.null(opt)) {
        message("Optimization has not been run before.")
        return(invisible())
    }

    if (opt$objective$codomain$length < 2L) {
        message("'$pareto_set()' only works for multi-objective optimization problem.")
        message("Please use '$best_set()' for single-objective optimization.")
        return(invisible())
    }

    pset <- gaopt__format_population(opt$archive$best(), opt$archive$cols_x, opt$archive$cols_y)

    if (unique) {
        pset <- unique(pset, by = c(opt$archive$cols_x, opt$archive$cols_y))
    }

    pset
}
# }}}

# gaopt_best_set {{{
gaopt__best_set <- function(super, self, private, unique = TRUE) {
    opt <- private$m_opt

    if (is.null(opt)) {
        message("Optimization has not been run before.")
        return(invisible())
    }

    if (opt$objective$codomain$length != 1L) {
        message("'$best_set()' only works for single-objective optimization problem.")
        message("Please use '$pareto_set()' for multi-objective optimization.")
        return(invisible())
    }

    pset <- gaopt__format_population(opt$archive$best(), opt$archive$cols_x, opt$archive$cols_y)

    if (unique) {
        pset <- unique(pset, by = c(opt$archive$cols_x, opt$archive$cols_y))
    }

    pset
}
# }}}

# assert_ready_optim {{{
assert_ready_optim <- function(super, self, private) {
    assert_ready_parameter(super, self, private)
    assert_ready_objective(super, self, private)
    assert_ready_controller(super, self, private)

    TRUE
}
# }}}

# assert_ready_parameter {{{
assert_ready_parameter <- function(super, self, private) {
    if (is.null(private$m_log$parameter)) {
        stop("No parameter has been set. Please run '$apply_measure()' first.")
    }

    TRUE
}
# }}}

# assert_ready_objective {{{
assert_ready_objective <- function(super, self, private) {
    if (is.null(private$m_log$objective)) {
        stop("No objecive has been set. Please run '$objective()' first.")
    }

    TRUE
}
# }}}

# assert_ready_controller {{{
assert_ready_controller <- function(super, self, private) {
    if (is.null(private$m_ctrl$mut)) {
        stop("No mutator has been set. Please run '$mutator()' first.")
    }
    if (is.null(private$m_ctrl$rec)) {
        stop("No recombinator has been set. Please run '$recombinator()' first.")
    }
    if (is.null(private$m_ctrl$sel)) {
        stop("No selector has been set. Please run '$selector()' first.")
    }
    if (is.null(private$m_ctrl$trm)) {
        stop("No terminator has been set. Please run '$terminator()' first.")
    }

    TRUE
}
# }}}

# extract_population {{{
extract_population <- function(population, objective, pareto = FALSE) {
    combine_results <- function(result) {
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

    for (i in seq_along(results)) {
        set(results[[i]], NULL, "index_gen", i)
    }
    results <- rbindlist(results)

    # get objective names
    nm <- unlist(mapply(name = objective$name, dim = objective$dim, SIMPLIFY = FALSE, FUN = function(name, dim) {
        if (dim == 1L) {
            return(name)
        }
        paste(name, seq_len(dim))
    }))

    setnames(results, names(results)[seq_along(nm)], nm)
    setcolorder(results, c("index_gen", "index_ind"))
    setcolorder(results, setdiff(names(results), nm))
    results
}
# }}}

# gaopt_print {{{
gaopt__print <- function(super, self, private) {
    gaopt__print_header(super, self, private)
    gaopt__print_measure_and_params(super, self, private)
    gaopt__print_objectives(super, self, private)
    gaopt__print_config(super, self, private)
    gaopt__print_result(super, self, private)
    invisible(self)
}
# }}}

# str.GAOptimJob {{{
# Provide a lightweight str() method for GAOptimJob that does not call
# object$print(). This prevents IDE workspace inspectors (such as the VSCode
# R extension) from repeatedly triggering the full cli-based print output
# whenever they call str() on objects in the global environment.
#' @export
str.GAOptimJob <- function(object, ...) {
    cat("<GAOptimJob> Use print(gaopt) for a full summary.\n")
    invisible(object)
}
# }}}

# gaopt_print_header {{{
gaopt__print_header <- function(super, self, private) {
    # Use a double-line rule for the main job header
    cli::cli_div(theme = list(rule = list("line-type" = "double")))
    cli::cli_rule(left = "{.strong EnergPlus Optimization Simulation Job}")

    cli::cli_ul()
    cli::cli_li("Seed IDF: {.file {private$m_seed$path()}}")
    if (!is.null(private$m_epws_path)) {
        cli::cli_li("Weather: {.file {private$m_epws_path}}")
    } else {
        cli::cli_li("Weather: [Not set]")
    }
    cli::cli_li("EnergyPlus Version: {.val {private$m_seed$version()}}")
    cli::cli_end()

    cli::cli_end()
}
# }}}

# gaopt_print_measure_and_parameters {{{
gaopt__print_measure_and_params <- function(super, self, private) {
    cli::cli_rule("{.strong Measure & Parameters}")
    if (is.null(private$m_log$measure)) {
        cli::cli_alert_warning("Measure: [Not set]")
        cli::cli_alert_warning("Parameters: [Not set]")
    } else {
        cli::cli_text("Measure: {.strong {private$m_log$measure$name}}")
        if (!is.null(private$m_log$parameter) && private$m_log$parameter$length > 0L) {
            param_ids <- private$m_log$parameter$ids()
            cli::cli_text("Parameters ({length(param_ids)}):")
            cli::cli_ul()
            for (i in seq_along(param_ids)) {
                param_id <- param_ids[[i]]
                param_cls <- private$m_log$parameter$params$cls[[i]]
                param_desc <- if (param_cls == "ParamDbl") {
                    sprintf(
                        "Double [%.6g, %.6g]",
                        private$m_log$parameter$params$lower[[i]],
                        private$m_log$parameter$params$upper[[i]]
                    )
                } else if (param_cls == "ParamInt") {
                    sprintf(
                        "Integer [%i, %i]",
                        private$m_log$parameter$params$lower[[i]],
                        private$m_log$parameter$params$upper[[i]]
                    )
                } else if (param_cls == "ParamFct") {
                    levels <- private$m_log$parameter$params$levels[[i]]
                    if (length(levels) > 3L) {
                        sprintf("{%s, ... (%i levels)}", paste(levels[1:3], collapse = ", "), length(levels))
                    } else {
                        sprintf("{%s}", paste(levels, collapse = ", "))
                    }
                } else if (param_cls == "ParamLgl") {
                    "Logical {TRUE, FALSE}"
                } else {
                    "Untyped"
                }
                cli::cli_li("{.field {param_id}}: {param_desc}")
            }
            cli::cli_end()
        } else {
            cli::cli_alert_warning("Parameters: [Not set]")
        }
    }
}
# }}}

# gaopt_print_objectives {{{
gaopt__print_objectives <- function(super, self, private) {
    cli::cli_rule("{.strong Objectives}")
    if (is.null(private$m_log$objective) || is.null(private$m_log$objective$name)) {
        cli::cli_alert_warning("[No objectives set]")
    } else {
        obj_names <- private$m_log$objective$name
        cli::cli_text("Optimization objectives ({nrow(obj_names)}):")
        cli::cli_ul()
        for (i in seq_len(nrow(obj_names))) {
            direction_symbol <- if (obj_names$direction[i] == "minimize") {
                cli::symbol$arrow_down
            } else {
                cli::symbol$arrow_up
            }
            direction_text <- obj_names$direction[i]
            cli::cli_li("{.field {obj_names$name[i]}}: {direction_symbol} {direction_text}")
        }
        cli::cli_end()
    }
}
# }}}

# gaopt_print_config {{{
gaopt__print_config <- function(super, self, private) {
    cli::cli_rule("{.strong Genetic Algorithm Configuration}")

    if (is.null(private$m_ctrl)) {
        cli::cli_alert_warning("[Not configured]")
        return(invisible())
    }

    # Mutator
    cli::cli_div(theme = list(h2 = list("margin-top" = 0, "margin-bottom" = 0)))
    cli::cli_h2("Mutator")
    cli::cli_end()
    if (!is.null(private$m_ctrl$mut)) {
        mut_info <- gaopt__format_operator_info(private$m_ctrl$mut, "mut")
        if (length(mut_info$type_wide) > 0L) {
            cli::cli_text("{.emph Type-wide}:")
            cli::cli_ul()
            for (line in mut_info$type_wide) {
                cli::cli_li(line)
            }
            cli::cli_end()
        }
        if (length(mut_info$param_specific) > 0L) {
            cli::cli_text("{.emph Parameter-specific}:")
            cli::cli_ul()
            for (line in mut_info$param_specific) {
                cli::cli_li(line)
            }
            cli::cli_end()
        }
        if (length(mut_info$type_wide) == 0L && length(mut_info$param_specific) == 0L) {
            cli::cli_alert_warning("[Not configured]")
        }
    } else {
        cli::cli_alert_warning("[Not set]")
    }

    # Recombinator
    cli::cli_div(theme = list(h2 = list("margin-top" = 0, "margin-bottom" = 0)))
    cli::cli_h2("Recombinator")
    cli::cli_end()
    if (!is.null(private$m_ctrl$rec)) {
        rec_info <- gaopt__format_operator_info(private$m_ctrl$rec, "rec")
        if (length(rec_info$type_wide) > 0L) {
            cli::cli_text("{.emph Type-wide}:")
            cli::cli_ul()
            for (line in rec_info$type_wide) {
                cli::cli_li(line)
            }
            cli::cli_end()
        }
        if (length(rec_info$param_specific) > 0L) {
            cli::cli_text("{.emph Parameter-specific}:")
            cli::cli_ul()
            for (line in rec_info$param_specific) {
                cli::cli_li(line)
            }
            cli::cli_end()
        }
        if (length(rec_info$type_wide) == 0L && length(rec_info$param_specific) == 0L) {
            cli::cli_alert_warning("[Not configured]")
        }
    } else {
        cli::cli_alert_warning("[Not set]")
    }

    # Selector
    cli::cli_div(theme = list(h2 = list("margin-top" = 0, "margin-bottom" = 0)))
    cli::cli_h2("Selector")
    cli::cli_end()
    if (!is.null(private$m_ctrl$sel)) {
        parent_str <- if (!is.null(private$m_ctrl$sel$parent)) {
            gaopt__format_selector(private$m_ctrl$sel$parent)
        } else {
            "[Not set]"
        }
        survival_str <- if (!is.null(private$m_ctrl$sel$survival)) {
            gaopt__format_selector(private$m_ctrl$sel$survival)
        } else {
            "[Not set]"
        }
        strategy <- if (!is.null(private$m_ctrl$survival_strategy)) {
            if (private$m_ctrl$survival_strategy == "plus") "(mu + lambda)" else "(mu, lambda)"
        } else {
            "[Not set]"
        }
        cli::cli_ul()
        cli::cli_li("Parent: {parent_str}")
        cli::cli_li("Survival: {survival_str}")
        cli::cli_li("Strategy: {strategy}")
        cli::cli_end()
    } else {
        cli::cli_alert_warning("[Not set]")
    }

    # Terminator
    cli::cli_div(theme = list(h2 = list("margin-top" = 0, "margin-bottom" = 0)))
    cli::cli_h2("Terminator")
    cli::cli_end()
    if (!is.null(private$m_ctrl$trm) && length(private$m_ctrl$trm) > 0L) {
        cli::cli_ul()
        for (name in names(private$m_ctrl$trm)) {
            trm <- private$m_ctrl$trm[[name]]
            trm_desc <- gaopt__format_terminator(trm)
            cli::cli_li(trm_desc)
        }
        cli::cli_end()
    } else {
        cli::cli_alert_warning("[Not set]")
    }
}
# }}}

# gaopt_print_result {{{
gaopt__print_result <- function(super, self, private) {
    cli::cli_rule("Optimization Result")
    if (is.null(private$m_opt)) {
        cli::cli_alert_warning("[No optimization has been run]")
        return(invisible())
    }

    run_time <- format(round(
        difftime(
            private$m_log$end_time,
            private$m_log$start_time
        ),
        digits = 2L
    ))
    cli::cli_alert_success(
        sprintf(
            "Simulation started at %s and completed successfully after %s.",
            format(private$m_log$start_time, "%Y-%m-%d %H:%M:%S"),
            run_time
        )
    )
}
# }}}

# gaopt_format_operator_info {{{
gaopt__format_operator_info <- function(operator_list, type) {
    info <- list(type_wide = character(0L), param_specific = character(0L))

    # Format type-wide operators
    if (!is.null(operator_list$default) && length(operator_list$default) > 0L) {
        for (param_type in names(operator_list$default)) {
            op <- operator_list$default[[param_type]]
            op_desc <- gaopt__format_operator(op, type)
            type_name <- switch(
                param_type,
                ParamDbl = "double",
                ParamInt = "integer",
                ParamFct = "choice",
                ParamLgl = "logical",
                ParamAny = "untyped",
                param_type
            )
            info$type_wide <- c(info$type_wide, sprintf("%s: %s", type_name, op_desc))
        }
    }

    # Format parameter-specific operators
    if (!is.null(operator_list$specific) && length(operator_list$specific) > 0L) {
        for (param_name in names(operator_list$specific)) {
            op <- operator_list$specific[[param_name]]
            op_desc <- gaopt__format_operator(op, type)
            info$param_specific <- c(info$param_specific, sprintf("%s -> %s", param_name, op_desc))
        }
    }

    info
}
# }}}

# gaopt_format_operator {{{
gaopt__format_operator <- function(op, type) {
    if (is.null(op)) {
        return("NULL")
    }

    # Get operator ID, fallback to class name if ID is NULL
    op_id <- if (inherits(op, "MiesOperator")) {
        if (!is.null(op$id)) {
            op$id
        } else {
            # Use the most specific class name (first in the class vector)
            class(op)[[1L]]
        }
    } else {
        "unknown"
    }

    # Extract parameters if available
    params <- character(0L)
    if (inherits(op, "MiesOperator") && !is.null(op$param_set)) {
        for (param_name in op$param_set$ids()) {
            param_val <- op$param_set$values[[param_name]]
            if (!is.null(param_val)) {
                params <- c(params, sprintf("%s=%s", param_name, format(param_val)))
            }
        }
    }

    if (length(params) > 0L) {
        sprintf("%s(%s)", op_id, paste(params, collapse = ", "))
    } else {
        op_id
    }
}
# }}}

# gaopt_format_selector {{{
gaopt__format_selector <- function(sel) {
    if (is.null(sel)) {
        return("NULL")
    }

    if (!inherits(sel, "Selector")) {
        return(format(sel))
    }

    # Get selector ID, fallback to class name if ID is NULL
    sel_id <- if (!is.null(sel$id)) {
        sel$id
    } else {
        # Use the most specific class name (first in the class vector)
        class(sel)[[1L]]
    }

    # Extract parameters if available
    params <- character(0L)
    if (!is.null(sel$param_set)) {
        for (param_name in sel$param_set$ids()) {
            param_val <- sel$param_set$values[[param_name]]
            if (!is.null(param_val)) {
                # Handle nested scalers
                if (inherits(param_val, "Scaler")) {
                    scaler_desc <- gaopt__format_scaler(param_val)
                    params <- c(params, sprintf("%s=%s", param_name, scaler_desc))
                } else {
                    params <- c(params, sprintf("%s=%s", param_name, format(param_val)))
                }
            }
        }
    }

    if (length(params) > 0L) {
        sprintf("%s(%s)", sel_id, paste(params, collapse = ", "))
    } else {
        sel_id
    }
}
# }}}

# gaopt_format_scaler {{{
gaopt__format_scaler <- function(scl) {
    if (is.null(scl)) {
        return("NULL")
    }

    if (!inherits(scl, "Scaler")) {
        return(format(scl))
    }

    # Get scaler ID, fallback to class name if ID is NULL
    scl_id <- if (!is.null(scl$id)) {
        scl$id
    } else {
        # Use the most specific class name (first in the class vector)
        class(scl)[[1L]]
    }

    # Extract parameters if available
    params <- character(0L)
    if (!is.null(scl$param_set)) {
        for (param_name in scl$param_set$ids()) {
            param_val <- scl$param_set$values[[param_name]]
            if (!is.null(param_val)) {
                params <- c(params, sprintf("%s=%s", param_name, format(param_val)))
            }
        }
    }

    if (length(params) > 0L) {
        sprintf("%s(%s)", scl_id, paste(params, collapse = ", "))
    } else {
        scl_id
    }
}
# }}}

# gaopt_format_terminator {{{
gaopt__format_terminator <- function(trm) {
    if (is.null(trm)) {
        return("NULL")
    }

    if (!inherits(trm, "Terminator")) {
        return(format(trm))
    }

    trm_id <- trm$id

    # Extract parameters if available
    params <- character(0L)
    if (!is.null(trm$param_set)) {
        for (param_name in trm$param_set$ids()) {
            param_val <- trm$param_set$values[[param_name]]
            if (!is.null(param_val)) {
                params <- c(params, sprintf("%s=%s", param_name, format(param_val)))
            }
        }
    }

    if (length(params) > 0L) {
        sprintf("%s(%s)", trm_id, paste(params, collapse = ", "))
    } else {
        trm_id
    }
}
# }}}
