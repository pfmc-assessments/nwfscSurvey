#' Combine catch data by trawl_id for data pulled using the
#' pull_catch function. Generally, only a single tow is returned
#' by species and in those instances this function is not needed.
#' This function is only needed in unique data situations. For
#' example, if data are pulled with all sample_types included then
#' there will often be data from the same trawl_id included as
#' multiple records. In these instances this function allows for
#' the sample data to be combined for unique trawl_id.
#'
#' @details
#'
#'
#' @template dir
#' @param data A data frame of catches obtained by using the pull_catch
#' function.
#' @template verbose
#'
#' @author Chantel Wetzel
#' @export
#'
#' @importFrom dplyr group_by distinct
#'
#' @examples
#' \dontrun{
#'
#'
combine_tows <- function(dir = NULL, data, verbose = TRUE){

  check_dir(dir = dir, verbose = verbose)

  if(!"total_catch_numbers" %in% colnames(data)){
    stop("The data object needs to be a data frame of pulled catches from the `pull_catch` function.")
  }

  original_colname <- colnames(data)
  colnames(data) <- tolower(colnames(data))

  find <- grep("trawl_id", colnames(data), ignore.case = TRUE)
  n_id <- table(data[, find])
  if(sum(n_id == 2) == 0){
    stop("All trawl_ids are unique and there is no need to combine data.")
  }

  catch <- data |>
    dplyr::group_by(common_name, trawl_id) |>
    mutate(
      total_catch_numbers = sum(total_catch_numbers),
      total_catch_wt_kg = sum(total_catch_wt_kg),
      subsample_count = sum(subsample_count),
      subsample_wt_kg = sum(subsample_wt_kg),
      cpue_kg_per_ha_der = sum(cpue_kg_per_ha_der),
      cpue_kg_km2 = sum(cpue_kg_km2),
      partition_sample_types = NA,
      partition = NA
    ) |>
    dplyr::distinct(trawl_id, .keep_all = TRUE)

  catch <- as.data.frame(catch)
  colnames(catch) <- original_colname

  return(catch)
}
