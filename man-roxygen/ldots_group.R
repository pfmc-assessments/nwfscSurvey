#' @param ... <[dplyr::dplyr_data_masking]>
#'   Variables to group by, i.e., passed to [dplyr::group_by()].
#'   Must be valid, unquoted column names present in `data`.
#'   These groups will be added to existing groups. If this behavior is not
#'   desired, then run [dplyr::ungroup()] before passing the data frame to
#'   `data`.
