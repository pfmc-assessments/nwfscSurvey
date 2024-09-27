#' Label tow expanded compositions
#'
#' @param x Dataframe of tow expanded composition data created by the
#' [get_expanded_comps()].
#'
#' @author Chantel Wetzel
#' @return A labelled data frame.
#' @export
#'
label_tow_expansion <- function(x) {
  data_labelled <- x |>
    labelled::set_variable_labels(
      trawl_id = "Unique tow id.",
      comp_column = "Length or age.",
      year = "Year of the tow.",
      area_swept = "Estimated area swept by the tow in catch per km2.",
      strata = "User defined strata name of the tow by depth and laitude.",
      strata_area = "Estimated area of the strata in km2.",
      tows = "The total number of positive tows by year.",
      total_catch_numbers = "The total number of fish of selected species within the tow.",
      bin = "User-specified composition bin based on the comp_column.",
      all_fish = "Total number of fish sampled in the unique trawl_id.",
      n_female = "Number of female fish samples by bin.",
      n_male = "Number of male fish samples by bin.",
      n_unsexed = "Number of unsexed fish samples by bin.",
      multiplier = "The total_catch_numbers divided by all fish sampled (tow expansion multiplier).",
      exp_f = "Multiplier multiplied by the n_female.",
      exp_m = "Multiplier multiplied by the n_male.",
      exp_u = "Multiplier multiplied by the n_unsexed."
    )
  return(data_labelled)
}
