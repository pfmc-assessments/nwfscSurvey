#' Survey name matching function used when pull from the data warehouse
#' The website is: https://www.webapp.nwfsc.noaa.gov/data
#'
#'
#' @author Chantel Wetzel based on code by John Wallace
#' @export


createMatrix <- function() {
  # Survey options available in the data warehouse
  # Triennial - Groundfish Triennial Shelf Survey - Conducted between 1977 - 2004 every 3rd year
  # AFSC.slope - not yet in warehouse - Conducted between 1988 - 2001, full sampling in the 1997, 1999, 2000, and 2001 years
  # NWFSC.Combo - Groundfish Slope and Shelf Combination Survey - Conducted starting in 2003 - present
  # NWFSC.Shelf - Groundfish Shelf Survey - Only conducted in 2001 (not used in West Coast groundfish assessments)
  matrix(
    data = c(
      "Triennial", "Groundfish Triennial Shelf Survey", "Triennial Survey",
      "AFSC.Slope", "AFSC/RACE Slope Survey", "AFSC Slope Survey",
      "NWFSC.Combo", "Groundfish Slope and Shelf Combination Survey", "NWFSC WCGBTS",
      "NWFSC.Slope", "Groundfish Slope Survey", "NWFSC Slope Survey",
      "NWFSC.Shelf", "Groundfish Shelf Survey", "NWFSC Shelf Survey",
      "NWFSC.Hypoxia", "Hypoxia Study", "",
      "NWFSC.Santa.Barb.Basin", "Santa Barbara Basin Study", "",
      "NWFSC.Shelf.Rockfish", "Shelf Rockfish [2004-2015]", "",
      "NWFSC.Hook.Line", "Hook and Line", "NWFSC Hook and Line Survey",
      "NWFSC.Video", "Video Study", ""
    ),
    ncol = 3,
    byrow = TRUE
  )
}
