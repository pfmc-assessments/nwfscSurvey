#' Get Vector of Default Species Names for US West Coast
#'
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
    "shortbelly_rockfish",
    "pacific_cod",
    "brown_rockfish",
    "copper_rockfish",
    "dover_sole",
    "sablefish",
    "petrale_sole",
    "bank_rockfish",
    "pacific_sanddab",
    "arrowtooth_flounder",
    "aurora_rockfish",
    "blackgill_rockfish",
    "bocaccio",
    "canary_rockfish",
    "chilipepper",
    "darkblotched_rockfish",
    "english_sole",
    "flathead_sole",
    "greenspotted_rockfish",
    "greenstriped_rockfish",
    "lingcod",
    "longspine_thornyhead",
    "pacific_spiny_dogfish",
    "redbanded_rockfish",
    "rex_sole",
    "rougheye_rockfish",
    "sharpchin_rockfish",
    "shortspine_thornyhead",
    "splitnose_rockfish",
    "yellowtail_rockfish",
    "big_skate",
    "longnose_skate",
    "pacific_ocean_perch",
    "widow_rockfish",
    "copper_rockfish",
    "squarespot_rockfish",
    "quillback_rockfish",
    "yelloweye_rockfish"
  )
  sp.list <- sp.list[order(sp.list)]
  return(sp.list)
}
