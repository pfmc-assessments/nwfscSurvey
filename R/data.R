#' Example data for AFSCforeign_hauls
#'
#' @format A data frame.
"AFSCforeign_hauls"

#' Example data for NWFSC bio data
#'
#' @format A data frame.
"bio_nwfsc_combo"

#' Example data for NWFSC catch data
#'
#' @format A data frame.
"catch_nwfsc_combo"

#' A data set for determining strata areas (hectares)
#'
#' Data necessary for determining the amount of area a strata covers to
#' expand estimates of biomass to be absolute.
#' The data includes the following columns:
#' \itemize{
#'   \item SUBAREA_ID
#'   \item MIN_DEPTH_M
#'   \item MAX_DEPTH_M
#'   \item AREA_HECTARES
#'   \item SUBAREA_SET_ID
#'   \item MIN_LAT_DD
#'   \item MAX_LAT_DD
#' }
#'
#' @author Curt Whitmire
#' @format A data frame.
"SA3"

#' A data set for determining strata areas (hectares)
#'
#' Data necessary for determining the amount of area a strata covers to
#' expand estimates of biomass to be absolute. The \code{SA3} data set
#' was updated in March of 2021 to allow for splitting areas at the
#' 40.16667, a common management line.
#' The data includes the following columns:
#' \itemize{
#'   \item OBJECT_ID
#'   \item MIN_DEPTH_M
#'   \item MAX_DEPTH_M
#'   \item MIN_LAT_DD
#'   \item MAX_LAT_DD
#'   \item AREA_HECTARES
#'   \item INPFC_AREA
#' }
#' @author Curt Whitmire
#' @format A data frame.
"SA3_v2021.1"

#' Sql pull of species names
#'
#' A static version of the sql call to pull species taxonomic information
#' from the data warehouse.
#'
#' @author Curt Whitmire
#'
#' @format A data frame with four columns,
#' latin, common, common_name, and scientific_name.
#' @seealso \code{\link{PullSpp.fn}} for more information.
#'
"PullSpp"
