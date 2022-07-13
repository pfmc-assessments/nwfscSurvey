check_grouping <- function(data_in, data_out) {
  if (is.data.frame(data_in)) {
    # Ensure grouping is transient
    data_out <- dplyr::dplyr_reconstruct(data_out, data_in)
  }
  return(data_out)
}
