# $print() works with minimal configuration

    Code
      gaopt$print()
    Message
      == EnergPlus Optimization Simulation Job =======================================
      * Seed IDF:
      '/Users/hongyuanjia/Applications/EnergyPlus-23-1-0/ExampleFiles/1ZoneUncontrolled.idf'
      * Weather:
      '/Users/hongyuanjia/Applications/EnergyPlus-23-1-0/WeatherData/USA_CA_San.Francisco.Intl.AP.724940_TMY3.epw'
      * EnergyPlus Version: 23.1.0
      -- Measure & Parameters --------------------------------------------------------
      ! Measure: [Not set]
      ! Parameters: [Not set]
      -- Objectives ------------------------------------------------------------------
      ! [No objectives set]
      -- Genetic Algorithm Configuration ---------------------------------------------
      ! [Not configured]
      -- Optimization Result ---------------------------------------------------------
      ! [No optimization has been run]

# $print() works with measure and parameters

    Code
      gaopt$print()
    Message
      == EnergPlus Optimization Simulation Job =======================================
      * Seed IDF:
      '/Users/hongyuanjia/Applications/EnergyPlus-23-1-0/ExampleFiles/1ZoneUncontrolled.idf'
      * Weather:
      '/Users/hongyuanjia/Applications/EnergyPlus-23-1-0/WeatherData/USA_CA_San.Francisco.Intl.AP.724940_TMY3.epw'
      * EnergyPlus Version: 23.1.0
      -- Measure & Parameters --------------------------------------------------------
      Measure: rotate_building
      Parameters (1):
      * degree: Double [0, 360]
      -- Objectives ------------------------------------------------------------------
      ! [No objectives set]
      -- Genetic Algorithm Configuration ---------------------------------------------
      ! [Not configured]
      -- Optimization Result ---------------------------------------------------------
      ! [No optimization has been run]

# $print() works with objectives

    Code
      gaopt$print()
    Message
      == EnergPlus Optimization Simulation Job =======================================
      * Seed IDF:
      '/Users/hongyuanjia/Applications/EnergyPlus-23-1-0/ExampleFiles/1ZoneUncontrolled.idf'
      * Weather:
      '/Users/hongyuanjia/Applications/EnergyPlus-23-1-0/WeatherData/USA_CA_San.Francisco.Intl.AP.724940_TMY3.epw'
      * EnergyPlus Version: 23.1.0
      -- Measure & Parameters --------------------------------------------------------
      Measure: rotate_building
      Parameters (1):
      * degree: Double [0, 360]
      -- Objectives ------------------------------------------------------------------
      Optimization objectives (2):
      * energy: ^ maximize
      * comfort: ^ maximize
      -- Genetic Algorithm Configuration ---------------------------------------------
      ! [Not configured]
      -- Optimization Result ---------------------------------------------------------
      ! [No optimization has been run]

# $print() works with GA operators

    Code
      gaopt$print()
    Message
      == EnergPlus Optimization Simulation Job =======================================
      * Seed IDF:
      '/Users/hongyuanjia/Applications/EnergyPlus-23-1-0/ExampleFiles/1ZoneUncontrolled.idf'
      * Weather:
      '/Users/hongyuanjia/Applications/EnergyPlus-23-1-0/WeatherData/USA_CA_San.Francisco.Intl.AP.724940_TMY3.epw'
      * EnergyPlus Version: 23.1.0
      -- Measure & Parameters --------------------------------------------------------
      ! Measure: [Not set]
      ! Parameters: [Not set]
      -- Objectives ------------------------------------------------------------------
      ! [No objectives set]
      -- Genetic Algorithm Configuration ---------------------------------------------
      -- Mutator --
      Type-wide:
      * double: MutatorGauss(sdev=0.05, sdev_is_relative=TRUE,
      truncated_normal=FALSE)
      * integer: MutatorGauss(sdev=0.05, sdev_is_relative=TRUE,
      truncated_normal=FALSE)
      * choice: MutatorDiscreteUniform(can_mutate_to_same=TRUE)
      * logical: MutatorDiscreteUniform(can_mutate_to_same=TRUE)
      * untyped: MutatorDiscreteUniform(can_mutate_to_same=TRUE)
      -- Recombinator --
      Type-wide:
        * double: RecombinatorSimulatedBinaryCrossover(n=15)
        * integer: RecombinatorCmpMaybe(p=0.7)
        * choice: RecombinatorCmpMaybe(p=0.7)
        * logical: RecombinatorCmpMaybe(p=0.7)
        * untyped: RecombinatorCmpMaybe(p=0.7)
      -- Selector --
        * Parent: SelectorRandom(sample_unique=groups)
        * Survival: SelectorBest(shuffle_selection=TRUE, scale.epsilon=0,
        scale.jitter=TRUE, scale.scale_output=TRUE, scale.tiebreak=crowdingdist)
        * Strategy: (mu + lambda)
      -- Terminator --
        * evals(n_evals=1000, k=0)
        * gens(generations=100)
      -- Optimization Result ---------------------------------------------------------
      ! [No optimization has been run]

# $print() shows parameter-specific operators

    Code
      gaopt$print()
    Message
      == EnergPlus Optimization Simulation Job =======================================
      * Seed IDF:
      '/Users/hongyuanjia/Applications/EnergyPlus-23-1-0/ExampleFiles/1ZoneUncontrolled.idf'
      * Weather:
      '/Users/hongyuanjia/Applications/EnergyPlus-23-1-0/WeatherData/USA_CA_San.Francisco.Intl.AP.724940_TMY3.epw'
      * EnergyPlus Version: 23.1.0
      -- Measure & Parameters --------------------------------------------------------
      Measure: modify_building
      Parameters (2):
      * degree: Double [0, 360]
      * thickness: Double [0.1, 0.5]
      -- Objectives ------------------------------------------------------------------
      ! [No objectives set]
      -- Genetic Algorithm Configuration ---------------------------------------------
      -- Mutator --
      Type-wide:
      * double: MutatorGauss(sdev=0.05, sdev_is_relative=TRUE,
      truncated_normal=FALSE)
      * integer: MutatorGauss(sdev=0.05, sdev_is_relative=TRUE,
      truncated_normal=FALSE)
      * choice: MutatorDiscreteUniform(can_mutate_to_same=TRUE)
      * logical: MutatorDiscreteUniform(can_mutate_to_same=TRUE)
      * untyped: MutatorDiscreteUniform(can_mutate_to_same=TRUE)
      Parameter-specific:
        * degree -> MutatorGauss(sdev=0.1, sdev_is_relative=TRUE,
        truncated_normal=FALSE)
      -- Recombinator --
      ! [Not set]
      -- Selector --
      ! [Not set]
      -- Terminator --
      ! [Not set]
      -- Optimization Result ---------------------------------------------------------
      ! [No optimization has been run]

