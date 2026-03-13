#' Creates a vector of strata factors by request column names
#'
#' @param data Catch dataframe created by \[pull_catch()]
#' @param strata_vars Column names in `data` to define the stratas by.  The
#'   default columns are `Depth_m` or `depth_m` and `Latitude_dd` or `latitude_dd`.
#' @param strata_df Strata dataframe created by [create_strata()]
#'
#' @author Chantel Wetzel
#' @export
#'
strata_factors <- function(
  data,
  strata_df,
  strata_vars = c("Latitude_dd", "Depth_m")
) {
  strata_vars <- tolower(strata_vars)
  strata_df <- strata_df |>
    dplyr::rename_all(tolower) |>
    as.data.frame()

  data <- data |>
    dplyr::rename_all(tolower) |>
    dplyr::mutate(
      stratum = NA
    ) |>
    as.data.frame()

  data$stratum <- NA
  for (s in 1:nrow(strata_df)) {
    ind <- rep(TRUE, nrow(data))
    for (i in 1:length(strata_vars)) {
      ind <- ind &
        data[, strata_vars[i]] >=
          strata_df[s, paste(strata_vars[i], ".1", sep = "")] &
        data[, strata_vars[i]] <
          strata_df[s, paste(strata_vars[i], ".2", sep = "")]
    }
    data[ind, "stratum"] <- as.character(strata_df[s, "name"])
  }
  data[, "stratum"] <- factor(
    data[, "stratum"],
    levels = as.character(strata_df[, "name"])
  )
  return(data$stratum)
}
