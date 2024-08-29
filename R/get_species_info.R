#' Get Species Information
#'
#' @details
#' Get the scientific name, common name, and strata group for
#' a vector of species.
#'
#' @param species A vector of species names. The words can be separated
#' using spaces or underscores. Full names are not required but they will
#' increase the consistency of the results should partial matches return
#' multiple matches.
#' @param unident A logical entry with the default value of \code{FALSE},
#' to match historical output that did not include unidentified groups.
#' @template verbose
#'
#' @export
#' @return A data frame with the
#' scientific name in the latin column and in the scientific_name column
#' that has spaces replaced with underscores;
#' common name in the common column and in the common_name column
#' that has spaces replaced with underscores;
#' species values used as input in the input column; and
#' strata used to assess its status in the strata column.
#' @seealso See
#' \code{\link{PullSpp.fn}} for information on common and scientific names;
#' and \code{\link{GetStrata.fn}} for the different stratifications.
#' @author Kelli Faye Johnson
#' @examples
#' get_species_info(c("sablefish", "petrale"))
#' # Expect a warning because vermilion doesn't have an strata assigned to it
#' testthat::expect_message(
#'   get_species_info(c("vermilion"))
#' )
#' # Dusky returns multiple entries
#' testthat::expect_message(
#'   get_species_info(c("dusky"))
#' )
#'
get_species_info <- function(species, unident = FALSE, verbose = TRUE) {
  # background information
  sppnames <- nwfscSurvey::PullSpp.fn()
  if (!unident) {
    sppnames <- sppnames[!grepl("unident", sppnames[["common"]]), ]
  }
  species <- tolower(gsub(" ", "_", species))

  spplist <- t(data.frame(
    c("coast", "arrowtooth_flounder", "flatfish"),
    c("coast", "aurora_rockfish", "sloperock"),
    c("coast", "big_skate", "other"),
    c("ca", "blackgill_rockfish", "sloperock"),
    c("north_south", "bocaccio", "shelfrock"),
    c("coast", "canary_rockfish", "shelfrock"),
    c("north_south", "chilipepper", "shelfrock"),
    c("coast", "copper_rockfish", "other"),
    c("coast", "curlfin_sole", "flatfish"),
    c("coast", "darkblotched_rockfish", "shelfrock"),
    c("coast", "dover_sole", "flatfish"),
    c("coast", "english_sole", "flatfish"),
    c("wa_or", "flathead_sole", "flatfish"),
    c("coast", "greenspotted_rockfish", "shelfrock"),
    c("coast", "greenstriped_rockfish", "shelfrock"),
    c("coast", "lingcod", "other"),
    c("coast", "longnose_skate", "other"),
    c("coast", "longspine_thornyhead", "thorny"),
    c("wa_or", "pacific_cod", "other"),
    c("north_south", "pacific_ocean_perch", "shelfrock"),
    c("coast", "pacific_sanddab", "flatfish"),
    c("coast", "pacific_spiny_dogfish", "other"),
    c("coast", "petrale_sole", "flatfish"),
    c("coast", "quillback_rockfish", "other"),
    c("coast", "redbanded_rockfish", "sloperock"),
    c("coast", "rex_sole", "flatfish"),
    c("coast", "rosethorn_rockfish", "shelfrock"),
    c("coast", "rougheye_rockfish", "sloperock"),
    c("sablefish", "sablefish", "other"),
    c("coast", "sharpchin_rockfish", "sloperockf"),
    c("north_south", "shortbelly_rockfish", "shelfrock"),
    c("coast", "shortspine_thornyhead", "thorny"),
    c("north_south", "splitnose_rockfish", "sloperock"),
    c("coast", "stripetail_rockfish", "shelfrock"),
    c("coast", "widow_rockfish", "shelfrock"),
    c("coast", "yelloweye_rockfish", "shelfrock"),
    c("north_south", "yellowtail_rockfish", "shelfrock")
  ))
  row.names(spplist) <- NULL

  # Match species name
  index <- lapply(species, function(y) {
    out <- which(apply(apply(sppnames, 1, grepl, pattern = y, ignore.case = TRUE), 2, any))
    names(out) <- NULL
    return(out)
  })

  if (any(lapply(index, length) == 0)) {
    bad <- which(lapply(index, length) == 0)
    bad_names <- paste(species[bad], collapse = ", ")
    cli::cli_alert_info(
      "The following species were not found in the look up table from the data warehouse
       (webapps.nwfsc.noaa.gov), and only information for species in the table will be returned:
       {bad_names}."
    )
    species <- species[-1 * bad]
    index <- index[-1 * bad]
  }

  if (length(species) == 0) {
    cli::cli_abort(
      "No matches were found for your input species: {species}."
    )
  }
  out <- sppnames[unlist(index), ]
  out[, "input"] <- rep(species, times = vapply(index, length, FUN.VALUE = 1L))

  # Match strata
  index <- match(tolower(out[, "common_name"]), tolower(spplist[, 2]))
  if (any(is.na(index))) {
    bad <- which(is.na(index))
    bad_strata <- paste(unique(out[bad, "input"]), collapse = ", ")
    if (any(is.na(index))) {
      cli::cli_alert_warning(
        "The following species were not found in the look up table stored in
        get_species_info(), please self-assign strata and species_type:
        {bad_strata}."
      )
    }

    if (length(index) != 1) {
      if (verbose) {
        multi_matches <- paste(unique(out[bad, "common_name"]), collapse = ", ")
        cli::cli_alert_warning(
          "Multiple matches were found for the {species} in the look up table
          stored in PullSpp.fn(). Only one match is returned.
          The common_name for the removed match is: {multi_matches}."
        )
      }
    }
    if (sum(!is.na(index)) == length(index)) {
      out <- out[!is.na(index), ]
      index <- index[!is.na(index)]
    }
  }

  out[, "strata"] <- ifelse(
    test = is.na(index),
    yes = "coast",
    no = spplist[index, 1]
  )
  out[, "species_type"] <- ifelse(
    test = is.na(index),
    yes = "all",
    no = spplist[index, 3]
  )

  return(out)
}
