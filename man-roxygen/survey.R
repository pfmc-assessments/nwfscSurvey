#' @param survey A character entry from one of the following options that
#' specifies which survey to pull the data for. The input options are:
#'   * Triennial,
#'   * AFSC.Slope,
#'   * NWFSC.Combo,
#'   * NWFSC.Slope,
#'   * NWFSC.Shelf,
#'   * NWFSC.Hypoxia,
#'   * NWFSC.Santa.Barb.Basin,
#'   * NWFSC.Shelf.Rockfish (not yet working),
#'   * NWFSC.Hook.Line (not yet working),
#'   * NWFSC.Video,
#'   * Triennial.Canada
#' 
#' The National Marine Fishery Service Alaska Fisheries Science Center (AFSC)
#' Triennial survey was conducted between 1977 - 2004 occurring every 3rd year.
#' The initial year, 1977, survey is not traditionally used in calculating 
#' indices of abundance. The Triennial survey sampled areas within the Canadian 
#' EEZ on the West Coast of Vancouver Island in 1980 - 2001 but these data are 
#' associated with a different survey name "Triennial.Canada".
#' The AFSC Slope Survey (AFSC.Slope) along the west coast of the U.S. began in 1984 and occurred 
#' annually from 1988-2001, with the exception of 1994 and 1998, when surveys were not conducted. 
#' Prior to 1997, only a limited portion of the coast was covered in each year. 
#' U.S. West Coast groundfish stock assessments only use the four years of consistent 
#' and complete survey coverage (1997, 1999-2001). The Northwest Fisheries Science
#' Center (NWFSC) Slope survey (NWFSC.Slope) was conducted between 1998 - 2001. 
#' The NWFSC West Coast Groundfish Bottom Trawl survey (NWFSC.Combo) is conducted 
#' annualy starting in 2003 (excluding 2020) and samples both the U.S. west coast
#' shelf and slope between 55 - 1,280 meters. 
#' Data can only be pulled from one survey at a time, though we are working on
#' allowing for a vector of survey names.
#' Currently, `NWFSC.Shelf.Rockfish` and `NWFSC.Hook.Line` are not supported.
#' The default of `NULL` is a placeholder that must be replaced with an entry.
