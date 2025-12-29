# Assign sex to unsexed fish based on fish with similar traits

Assign sex to unsexed fish based on fish with similar traits

## Usage

``` r
SexRatio.fn(
  x,
  sexRatioStage,
  sexRatioUnsexed,
  maxSizeUnsexed,
  bins = NULL,
  verbose = TRUE
)
```

## Arguments

- x:

  A data frame or a nested list depending on `sexRatioStage` which will
  dictate where this function is called inside code that expands the
  data such as [`SurveyLFs.fn()`](SurveyLFs.fn.md) or
  [`SurveyAFs.fn()`](SurveyAFs.fn.md).

- sexRatioStage:

  An integer specifying the stage of the expansion in which the sex
  ratio should be applied. There is no default value and only values of
  ``` 1L`` or  ```2L\` are appropriate.

- sexRatioUnsexed:

  A numerical value within `[0.0, 1.0]` that will be used as the sex
  ratio for measured individuals less than `maxSizeUnsexed`. If
  `NA_real_`, then the sex ratio for stage-1 expansion will not be
  conducted.

- maxSizeUnsexed:

  A numerical value specifying the right side of the following bin
  `[0, maxSizeUnsexed]`, where all fish measured in this bin are
  assigned a sex based on sexRatioUnsexed. Fish with a measurement
  larger than this value will be assigned a sex based on the calculated
  sex ratio in the data.

- bins:

  A vector of measurement bins that were used to bin the measurement of
  interest. Passing the bins as an argument ensures that appropriate
  "like" fish are found, i.e., if a bin is missing from the data because
  no fish were measured that fell in that bin the code will be aware the
  bin is missing. TODO: This argument could be avoided if the bins were
  factors where all of the levels are documented.

- verbose:

  A logical that specifies if you want to print messages and warnings to
  the console. The default is `TRUE`.

## Details

Assign sex to fish that were sampled but not sexed based on the
proportion of like fish that were female out of sexed fish. After
assigning unsexed fish to males and females, new sample sizes of each
sex are calculated and the expansion factors are augmented. The
definition of what classifies as "like fish" depends on the stage of the
expansion, see the Details section.

The sex ratio is calculated as the number of females divided by the sum
of the number of females and males. So, the value being reported is
actually the proportion of females.

`SexRatio.fn()` has different behavior depending on the expansion being
performed, which is controlled using the `sexRatioStage` argument.

The expansion factors in the returned data frame are unaltered for
unsexed fish. TODO: Decide if the previous method is intended or if the
expansion factors should be changed for unsexed fish too but to zero?
Also, the stage-2 expansion does alter the number of unsexed fish but
the stage-1 does not. TODO: Think about how the function alters the
expansion sample sizes and did not just report a sex ratio for each
stage that could be used in the expansion functions themselves. TODO:
Change the message for stage-2 expansion to better reflect what the code
is doing TODO: align the stage-1 and stage-2 expansion details so they
are written in a similar fashion.

- Stage-1 expansion:

  - Sex ratio from observations within a tow/measurement bin combination
    is applied to all unsexed fish within that same tow/measurement bin;

  - Sex ratio from observations within a measurement bin across all
    years is applied to all unsexed fish within that same measurement
    bin that did not have any sex ratio information available at the tow
    level;

  - Sex ratio from nearby measurement bins across all years is applied
    to all unsexed fish within a measurement bin with zero sexed fish.
    Here, nearby bins are classified as those that are one measurement
    unit smaller and larger than the current bin.

- Stage-2 expansion:

  - Sex ratio of sexed fish is within a measurement bin is calculated
    for every strata/year combination;

  - Missing sex ratios for a measurement bin are filled in based on the
    sex ratio of all measured fish within that bin across all years and
    strata;

  - Still missing sex ratios for a measurement bin are based on the sex
    ratio of sexed fish for near (i.e., plus or minus one) measurement
    bins away across all years and strata.

## Author

Allan C. Hicks and Chantel R. Wetzel
