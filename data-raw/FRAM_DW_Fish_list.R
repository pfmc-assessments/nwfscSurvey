#### Create a list of species names using FRAM_DW_Fish_list
PullSpp <- utils::read.csv(
  file = dir(pattern = "FRAM_DW_Fish_list\\.csv", recursive = TRUE)
) %>%
  dplyr::select(dplyr::matches("common|sci.+_name$")) |>
  dplyr::rename(common = common_name, latin = scientific_name) |>
  dplyr::mutate(
    common_name = gsub("\\s", "_", common),
    scientific_name = gsub("\\s", "_", latin)
  )

usethis::use_data(PullSpp, overwrite = TRUE)
