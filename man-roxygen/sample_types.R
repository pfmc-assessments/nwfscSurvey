#' @param sample_types A character vector of sample types, i.e.,
#' `"statistical_partition_dim"`, that you would like to keep. The default is
#' to only keep `NA` values, both real and character NA. But, for some
#' instances you may want to keep Life Stage and Size samples. The majority
#' of samples with `"statistical_partition_dim"` of Size and Life Stage are
#' Pacific hake and should not be considered different than regular survey
#' samples. The other types of samples that may be designated Life Stage are
#' egg cases that can be caught and identified for select elasmobranch
#' species. These type of samples should not be included in the data used
#' to estimate indices of abundance and are omitted by default.
