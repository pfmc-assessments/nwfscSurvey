# nwfscSurvey 2.8.0
  * Add replacement functions for a number of existing functions with standardized 
  naming structure and add soft deprecation warning to the older functions. The 
  new functions are: `create_strata()` to replace `creatStrataDF.fn()`, 
  `get_spp_list()` to replace `GetSppDefault.fn()`, 
  `get_strata_areas()` to replace `createStrataDF.fn()`, `get_strata_areas_default()` 
  to replace `GetStrata.fn()`, `get_survey_names_abb()` to replace `GetSurveyAbb.fn()`, 
  `get_survey_names_long()` to replace `createMatrix()`, `plot_cpue_map()` to replace 
  `PlotMap.fn()`, `plot_sex_ratio()` to replace `PlotSexRatio.fn()`, 
  `plot_sex_ratio_strata()` to replace `PlotSexRatioStrata.fn()`, and 
  `strata_factors()` to replace `StrataAreas.fn()`.
  * Hard deprecate functions that should no longer be used: `ReadInLengths()`, 
  `ReadInAges()`, and `Format.AKSlope.fn()`.
  * Eliminate the hardwired functionality of all plots being saved to 
  `file.path(dir, "plots")` and move to save files to the user specified `dir`.
  * Eliminate the hardwired functionality of all ss3 output being saved to 
  `file.path(dir, printfolder)` with the default of `printfolder = forss3` and 
  move to save files to the user specified `dir`.
  * Make `survey = NWFSC.Combo` the default argument for all `pull_` functions.
  * Move to `@inheritParams` from `@template`
  * Removes 17 deprecated functions (and their corresponding man pages) including 
  `Biomass.fn`, `PullCatch.fn`, `PullBio.fn`, `SurveyLFs.fn`, `SurveyAFs.fn`, 
  `GetN.fn`, and others, along with cleaning up the NAMESPACE of their exports and unused imports.
  * Converts `stop()` and warning() calls to `cli::cli_abort()` and `cli::cli_alert_warning()`.

# nwfscSurvey 2.2
    * Correct unit issues throughout the package

# nwfscSurvey 2.1
    * Tagged version associated with the 2021 West Coast groundfish assessments

# nwfscSurvey 2.0
    * Cleaned up all documentation and errors so package checks run cleanly.
    * Documented data
    * Added Github action

# nwfscSurvey 1.63_2019
    * Tagged version associated with 2019 West Coast groundfish assessments

# nwfscSurvey 1.6.1
 * Add extensive new documentation




