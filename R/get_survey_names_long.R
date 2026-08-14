#' Survey name matching function used when pull from the data warehouse
#'
#'
#' @author Chantel Wetzel based on code by John Wallace
#' @export
#'
get_survey_names_long <- function() {
  # Survey options available in the data warehouse
  # Triennial - Groundfish Triennial Shelf Survey - Conducted between 1977 - 2004 every 3rd year
  # AFSC.slope - not yet in warehouse - Conducted between 1988 - 2001, full sampling in the 1997, 1999, 2000, and 2001 years
  # NWFSC.Combo - Groundfish Slope and Shelf Combination Survey - Conducted starting in 2003 - present
  # NWFSC.Shelf - Groundfish Shelf Survey - Only conducted in 2001 (not used in West Coast groundfish assessments)
  a <- matrix(
    data = c(
      "Triennial",
      "AFSC/RACE Triennial Groundfish Shelf Survey",

      "Triennial",
      "AFSC/RACE Triennial Groundfish Shelf Survey (by NWFSC)",

      "AFSC.Slope",
      "AFSC/RACE Slope Survey",

      "NWFSC.Slope",
      "West Coast Groundfish Bottom Trawl Slope Survey",

      "NWFSC.Combo",
      "West Coast Groundfish Bottom Trawl Slope/Shelf Combination Survey",

      "NWFSC.Shelf",
      "West Coast Groundfish Bottom Trawl Shelf Survey",

      "NWFSC.Hypoxia",
      "West Coast Groundfish Bottom Trawl Hypoxia Study",

      "NWFSC.Santa.Barb.Basin",
      "West Coast Groundfish Bottom Trawl Santa Barbara Basin Study",

      "NWFSC.Shelf.Rockfish",
      "Shelf Rockfish [2004-2015]",

      "NWFSC.Hook.Line",
      "Hook and Line",

      "NWFSC.Video",
      "West Coast Groundfish Bottom Trawl Video Study",

      "Triennial.Canada",
      "AFSC/RACE Triennial Groundfish Shelf Survey: Canada"
    ),
    ncol = 2,
    byrow = TRUE
  )
  colnames(a) <- c("old_names", "new_names")
  return(a)
}
