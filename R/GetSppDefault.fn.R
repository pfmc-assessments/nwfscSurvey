#' Get Vector of Default Species Names for US West Coast
#'
#' @details
#' A standard list of common names for species of interest to the
#' US West Coast. This list does not reflect ecosystem or financial
#' importance, and instead it is strictly a list of species for which
#' analyses take place.
#'
#' @export
#' @return A vector of strings specifying common names with words separated
#' using underscores.
#' @examples
#' GetSppDefault.fn()
GetSppDefault.fn <- function() {
  sp.list <- c(
    "arrowtooth_flounder",
    "aurora_rockfish",
    "big_skate",
    "blackgill_rockfish",
    "bocaccio",
    "canary_rockfish",
    "chilipepper",
    "curlfin_sole",
    "darkblotched_rockfish",
    "dover_sole",
    "english_sole",
    "flathead_sole",
    "greenspotted_rockfish",
    "greenstriped_rockfish",
    "lingcod",
    "longnose_skate",
    "longspine_thornyhead",
    "pacific_cod",
    "pacific_ocean_perch",
    "pacific_sanddab",
    "pacific_spiny_dogfish",
    "petrale_sole",
    "redbanded_rockfish",
    "rex_sole",
    "rosethorn_rockfish",
    "rougheye_rockfish",
    "sablefish",
    "sharpchin_rockfish",
    "shortspine_thornyhead",
    "splitnose_rockfish",
    "stripetail_rockfish",
    "widow_rockfish",
    "yelloweye_rockfish",
    "yellowtail_rockfish"
  )
  sp.list <- sp.list[order(sp.list)]

  return(sp.list)
}
