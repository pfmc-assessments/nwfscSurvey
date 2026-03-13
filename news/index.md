# Changelog

## nwfscSurvey 2.8.0

- Add replacement functions for a number of existing functions with
  standardized naming structure and add soft deprecation warning to the
  older functions. The new functions are:
  [`create_strata()`](../reference/create_strata.md) to replace
  `creatStrataDF.fn()`, [`get_spp_list()`](../reference/get_spp_list.md)
  to replace [`GetSppDefault.fn()`](../reference/GetSppDefault.fn.md),
  [`get_strata_areas()`](../reference/get_strata_areas.md) to replace
  `createStrataDF.fn()`, `get_strata_areas_default()` to replace
  [`GetStrata.fn()`](../reference/GetStrata.fn.md),
  [`get_survey_names_abb()`](../reference/get_survey_names_abb.md) to
  replace [`GetSurveyAbb.fn()`](../reference/GetSurveyAbb.fn.md),
  [`get_survey_names_long()`](../reference/get_survey_names_long.md) to
  replace [`createMatrix()`](../reference/createMatrix.md),
  [`plot_cpue_map()`](../reference/plot_cpue_map.md) to replace
  [`PlotMap.fn()`](../reference/PlotMap.fn.md),
  [`plot_sex_ratio()`](../reference/plot_sex_ratio.md) to replace
  [`PlotSexRatio.fn()`](../reference/PlotSexRatio.fn.md),
  [`plot_sex_ratio_strata()`](../reference/plot_sex_ratio_strata.md) to
  replace
  [`PlotSexRatioStrata.fn()`](../reference/PlotSexRatioStrata.fn.md),
  and [`strata_factors()`](../reference/strata_factors.md) to replace
  [`StrataAreas.fn()`](../reference/StrataAreas.fn.md).
- Hard deprecate functions that should no longer be used:
  `ReadInLengths()`, `ReadInAges()`, and
  [`Format.AKSlope.fn()`](../reference/Format.AKSlope.fn.md).
- Eliminate the hardwired functionality of all plots being saved to
  `file.path(dir, "plots")` and move to save files to the user specified
  `dir`.
- Eliminate the hardwired functionality of all ss3 output being saved to
  `file.path(dir, printfolder)` with the default of
  `printfolder = forss3` and move to save files to the user specified
  `dir`.
- Make `survey = NWFSC.Combo` the default argument for all `pull_`
  functions.
- Move to `@inheritParams` from `@template`
- Removes 17 deprecated functions (and their corresponding man pages)
  including `Biomass.fn`, `PullCatch.fn`, `PullBio.fn`, `SurveyLFs.fn`,
  `SurveyAFs.fn`, `GetN.fn`, and others, along with cleaning up the
  NAMESPACE of their exports and unused imports.
- Converts [`stop()`](https://rdrr.io/r/base/stop.html) and warning()
  calls to
  [`cli::cli_abort()`](https://cli.r-lib.org/reference/cli_abort.html)
  and
  [`cli::cli_alert_warning()`](https://cli.r-lib.org/reference/cli_alert.html).

## nwfscSurvey 2.2

``` R
* Correct unit issues throughout the package
```

## nwfscSurvey 2.1

``` R
* Tagged version associated with the 2021 West Coast groundfish assessments
```

## nwfscSurvey 2.0

``` R
* Cleaned up all documentation and errors so package checks run cleanly.
* Documented data
* Added Github action
```

## nwfscSurvey 1.63_2019

``` R
* Tagged version associated with 2019 West Coast groundfish assessments
```

## nwfscSurvey 1.6.1

- Add extensive new documentation
