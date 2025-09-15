#### Create a data frame of available survey cells
# @Curt-Whitmire-NOAA is working on providing this table within the data warehouse
# itself with the additional data of year of decommissioning if a cell is
# no longer in the lottery for future years

availablecells <- readxl::read_excel(
  path = dir(pattern = "Selection Set 2018 with Cell Corners", recursive = TRUE)
) |> data.frame()

usethis::use_data(availablecells, overwrite = TRUE)
