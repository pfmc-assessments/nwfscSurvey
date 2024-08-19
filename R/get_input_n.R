#' Calculate input sample sizes
#'
#' @references
#' Stewart, I.J. and O.S. Hamel. 2014.
#' Bootstrapping of sample size for length- or age-composition data used in
#' stock assessment.
#' Canadian Journal of Fishery and Aquatic Science, 71(4): 581--588.
#' [10.1139/cjfas-2013-0289](https://doi.org/10.1139/cjfas-2013-0289).
#'
#' @template dir
#' @param data A data frame of composition data created using [pull_bio()].
#' @param comp_column_name The column name to create composition data for. This column can be
#' is used to determine whether to format the composition data for length or age
#' compositions by looking for either age (e.g., age_years, Age, best_age) or length
#' (e.g., Length, length, Length_cm) in the comp_column_name. Default Length_cm.
#' @param input_sample_size_method Determines the default input sample size to add to
#' the composition data for SS3. There are three options: c("stewart_hamel", "tows",
#' "total_samples") where the default is "stewart_hamel".
#' @param species_group A string specifying the species group of interest, which
#'   will lead to the use of the correct species-specific value for
#'   the number of unique samples per tow. See the function call for
#'   allowed values, where the default is `"all"`.
#' @template printfolder
#' @template verbose
#'
#' @author Chantel R. Wetzel
#' @export
#'
#' @examples
#' \dontrun{
#' bio <- pull_bio(
#'   common_name = "petrale sole",
#'   survey = "NWFSC.Combo"
#' )
#'
#' n <- get_input_n(
#'   data = bio,
#'   comp_column_name = "length_cm",
#'   input_sample_size_method = "stewart_hamel",
#'   species_group = "flatfish"
#' )
#' }
#'
get_input_n <- function(
    dir = NULL,
    data,
    comp_column_name = c("length_cm", "age")[1],
    input_sample_size_method = c("stewart_hamel", "tows", "total_samples")[1],
    species_group = c(
      "all",
      "flatfish",
      "shelfrock",
      "sloperock",
      "thorny",
      "other")[1],
    printfolder = "forSS3",
    verbose = TRUE) {

  plotdir <- file.path(dir, printfolder)
  check_dir(dir = plotdir, verbose = verbose)

  colnames(data) <- tolower(colnames(data))
  comp_column_name <- tolower(comp_column_name)
  if (!comp_column_name %in% colnames(data)) {
    stop("The comp_column_name is not a column name in the data.")
  }

  multiplier <- ifelse(
    species_group == "flatfish", 3.09, ifelse(
      species_group %in% c("shelfrock", "sloperock"), 2.43, ifelse(
        species_group == "thorny", 6.91, ifelse(
          species_group == "other", 2.38, ifelse(
            species_group == "all", 2.73, 1
          )
        )
      )
    )
  )

  data[, "multiplier"] <- multiplier
  data[, "sex_grouped"] <- "sexed"
  data[which(data[,"sex"] == c("U")), "sex_grouped"] <- "unsexed"

  data_with_counts <- data |>
    dplyr::filter(!is.na(comp_column_name)) |>
    dplyr::group_by(year) |>
    dplyr::mutate(
      n_all_fish = n(),
      n_sexed_fish = sum(sex %in% c("F", "M")),
      n_unsexed_fish = sum(sex == "U")
    )

  samples_by_sex <- data_with_counts |>
    dplyr::group_by(year, sex_grouped) |>
    dplyr::summarise(
      tows = length(unique(trawl_id)),
      n = n(),
      stewart_hamel = floor(unique(multiplier) * tows)
    ) |>
    dplyr::ungroup()

  samples_all <- data_with_counts |>
    dplyr::group_by(year) |>
    dplyr::summarise(
      sex_grouped = "all",
      tows = length(unique(trawl_id)),
      n = n(),
      stewart_hamel = floor(unique(multiplier) * tows)
    ) |>
    dplyr::ungroup()

  samples <- rbind(samples_by_sex, samples_all)

  if (input_sample_size_method == "stewart_hamel") {
    samples[, "input_n"] <- samples[, "stewart_hamel"]
    replace <- which(samples[, "input_n"] > samples[, "n"])
    if (length(replace) > 0) {
      samples[replace, "input_n"] <- samples[replace, "n"]
    }
  }
  if (input_sample_size_method == "tows") {
    samples[, "input_n"] <- samples[, "tows"]
  }
  if (input_sample_size_method == "total_samples") {
    samples[, "input_n"] <- samples[, "n"]
  }

  if (!is.null(dir)) {
    project <- ifelse("project" %in% colnames(data),
                      gsub(" ", "_", tolower(unique(data[, "project"]))), "")
    species <- ifelse("common_name" %in% colnames(data), unique(data[, "common_name"]), "")
    write.csv(
      x = samples,
      file = file.path(plotdir, paste0(species, "_", comp_column_name, "_samples_", project, ".csv")),
      row.names = FALSE
    )
  }
  return(samples)
}
