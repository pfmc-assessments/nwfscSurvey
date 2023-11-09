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
    "arrowtooth_flounder",
    "aurora_rockfish",
    "bank_rockfish",
    "big_skate",
    "blackgill_rockfish",
    "bocaccio",
    "brown_rockfish",
    "canary_rockfish",
    "chilipepper",
    "copper_rockfish",
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
    "quillback_rockfish",
    "redbanded_rockfish",
    "rex_sole",
    "rosethorn_rockfish",
    "rougheye_rockfish",
    "sablefish",
    "sharpchin_rockfish",
    "shortbelly_rockfish",
    "shortspine_thornyhead",
    "splitnose_rockfish",
    "squarespot_rockfish",
    "stripetail_rockfish",
    "widow_rockfish",
    "yellowtail_rockfish"
  )
  sp.list <- sp.list[order(sp.list)]

  return(sp.list)
}
