#' Get Species Information
#' 
#' Get the scientific name, common name, and strata group for
#' a vector of species.
#'
#' @param species A vector of species names. The words can be separated
#' using spaces or underscores. Full names are not required but they will
#' increase the consistency of the results should partial matches return
#' multiple matches.
#'
#' @export
#' @return A data frame with the scientific name, common name, and strata
#' group for each species.
#' @author Kelli Faye Johnson
#' @examples
#' \dontrun{
#' GetSpp.fn(c("sablefish", "petrale"))
#' }

GetSpp.fn <- function(species) {

  # background information
  sppnames <- PullSpp.fn()
  spplist <- t(data.frame(
    c("north_south", "shortbelly_rockfish"),
    c("wa_or", "pacific_cod"),
    c("ca", "brown_rockfish"),
    c("ca", "copper_rockfish"),
    c("coast", "dover_sole"),
    c("north_south", "sablefish"),
    c("coast", "petrale_sole"),
    c("ca", "bank_rockfish"),
    c("coast", "pacific_sanddab"),
    c("coast", "arrowtooth_flounder"),
    c("coast", "aurora_rockfish"),
    c("ca", "blackgill_rockfish"),
    c("north_south", "bocaccio"),
    c("coast", "canary_rockfish"),
    c("north_south", "chilipepper"),
    c("coast", "darkblotched_rockfish"),
    c("coast", "english_sole"),
    c("wa_or", "flathead_sole"),
    c("coast", "greenspotted_rockfish"),
    c("coast", "greenstriped_rockfish"),
    c("coast", "lingcod"),
    c("coast", "longspine_thornyhead"),
    c("coast", "pacific_spiny_dogfish"),
    c("coast", "redbanded_rockfish"),
    c("coast", "rex_sole"),
    c("coast", "rougheye_rockfish"),
    c("coast", "sharpchin_rockfish"),
    c("coast", "shortspine_thornyhead"),
    c("north_south", "splitnose_rockfish"),
    c("north_south", "yellowtail_rockfish"),
    c("coast", "big_skate"),
    c("coast", "longnose_skate"),
    c("north_south", "pacific_ocean_perch"),
    c("coast", "widow_rockfish")
    ))
  row.names(spplist) <- NULL

  # Match species name
  index <- lapply(species, function(y) {
    out <- which(apply(apply(sppnames, 1, grepl, pattern = y), 2, any))
    names(out) <- NULL
    return(out)
  })
  if (any(lapply(index, length) == 0)) {
    bad <- which(lapply(index, length) == 0)
    warning("The following species were not found in the look up table\n",
      "generated from the data warehouse (webapps.nwfsc.noaa.gov)\n",
      "and only information for species in the table will be returned:\n",
      paste(species[bad], collapse = ", "), call. = FALSE)
    species <- species[-1 * bad]
    index <- index[-1 * bad]
  }
  if (length(species) == 0) {
    stop("No matches were found for your input species.")
  }
  out <- sppnames[unlist(index), ]
  out[, "input"] <- species

  # Match strata
  index <- match(out[, "common_name"], spplist[, 2])
  if (any(is.na(index))) {
    bad <- which(is.na(index))
    warning("The following species were not found in the look up table\n",
      "stored in GetSpp for default strata, please self-assign strata:\n",
      paste(species[bad], collapse = ", "), call. = FALSE)
  }
  out[, "strata"] <- spplist[index, 1]

  return(out)
}
