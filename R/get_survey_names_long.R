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
      "triennial",

      "Triennial",
      "AFSC/RACE Triennial Groundfish Shelf Survey (by NWFSC)",
      "triennial",

      "AFSC.Slope",
      "AFSC/RACE Slope Survey",
      "afsc_slope",

      "NWFSC.Slope",
      "West Coast Groundfish Bottom Trawl Slope Survey",
      "nwfsc_slope",

      "NWFSC.Combo",
      "West Coast Groundfish Bottom Trawl Slope/Shelf Combination Survey",
      "wcgbt",

      "NWFSC.Shelf",
      "West Coast Groundfish Bottom Trawl Shelf Survey",
      "nwfsc_shelf",

      "NWFSC.Hypoxia",
      "West Coast Groundfish Bottom Trawl Hypoxia Study",
      "nwfsc_hypoxia",

      "NWFSC.Santa.Barb.Basin",
      "West Coast Groundfish Bottom Trawl Santa Barbara Basin Study",
      "nwfsc_sb_basin",

      "NWFSC.Hook.Line",
      "Hook and Line",
      "nwfsc_hkl",

      "NWFSC.Video",
      "West Coast Groundfish Bottom Trawl Video Study",
      "nwfsc_video",

      "Triennial.Canada",
      "AFSC/RACE Triennial Groundfish Shelf Survey: Canada",
      "triennial_canada"
    ),
    ncol = 3,
    byrow = TRUE
  )
  colnames(a) <- c("old_names", "new_names", "alt_names")
  return(a)
}
