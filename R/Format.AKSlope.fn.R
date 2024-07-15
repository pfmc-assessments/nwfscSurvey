#' Rename AFSC slope survey columns from data pulled before 2017
#'
#' @details
#' Rename columns in the AFSC slope survey data file
#' received prior to the creation of the NWFSC data
#' warehouse. This function converts the older data files
#' to create the needed column names to work within survey
#' package functions. Output from this function will be list
#' of containing catch, length, and age data.
#'
#' @template dir
#' @param datTows A data frame of catch data for the
#' AKFSC slope survey with incorrect column names.
#' prior to the creation of the data warehouse.
#' @param datL A list of biological data (lengths and ages)
#' for the AKFSC slope survey with incorrect column
#' names prior to the creation of the data warehouse.
#' @param start.year The first year of data to retain within
#' the data frame. The first year typically used from this
#' survey is 1997.
#' @template verbose
#'
#' @author Chantel Wetzel
#' @export
#'
#' @examples
#' \dontrun{
#' # load data files for catch and biological data
#' load("Tri.Shelf.and.AFSC.Slope.canary.Catch.24.May.11.dmp")
#' catch <- Tri.Shelf.and.AFSC.Slope.canary.Catch.24.May.11
#' load("AFSC.Slope.Shelf.sable.bio.5.24.11.dmp")
#' bio <- AK.Surveys.Bio.sablefish.24.May.11
#' # call function and reformat the data
#' filter.dat <- Format.AKSlope.fn(
#'   datTows = catch,
#'   datL = bio,
#'   start.year = 1997
#' )
#' }
#'
Format.AKSlope.fn <- function(
    dir = NULL,
    datTows,
    datL = NA,
    start.year = 1997,
    verbose = TRUE) {
  check_dir(dir = dir, verbose)

  # Filter for only the AKFSC Slope survey
  if ("SURVEY" %in% colnames(datTows)) {
    datTows <- datTows[datTows$SURVEY == "AFSC.Slope", ]
  } else {
    datTows$SURVEY <- "AFSC.Slope"
  }
  datTows <- datTows[datTows$YEAR >= start.year, ]

  # Deal with the catch data file
  names(datTows)[names(datTows) == "HAULJOIN"] <- "Trawl_id"
  names(datTows)[names(datTows) == "YEAR"] <- "Year"
  names(datTows)[names(datTows) == "BOTTOM_DEPTH"] <- "Depth_m"
  names(datTows)[names(datTows) == "WEIGHT"] <- "Subsample_wt_kg" # Need to double check units
  names(datTows)[names(datTows) == "NUMBER_FISH"] <- "Subsample_count"
  names(datTows)[names(datTows) == "VESSEL"] <- "Vessel"

  datTows$Project <- "AK.Slope"
  datTows$Pass <- datTows$Tow <- NA
  datTows$Date <- paste0(datTows$Year, "-", datTows$MONTH, "-", datTows$DAY)
  datTows$Latitude_dd <- (datTows$START_LATITUDE + datTows$END_LATITUDE) / 2
  datTows$Longitude_dd <- (datTows$START_LONGITUDE + datTows$END_LONGITUDE) / 2
  datTows$total_catch_numbers <- datTows$Subsample_count
  datTows$total_catch_wt_kg <- datTows$Subsample_wt_kg
  datTows$Area_swept_ha <- (datTows$DISTANCE_FISHED * datTows$NET_WIDTH) / 10 # area swept for each tow in hectare
  datTows$cpue_kg_km2 <- datTows$Subsample_wt_kg / (0.01 * datTows$Area_swept_ha)

  datTows <- datTows[, c(
    "Project", "Trawl_id", "Year", "Pass", "Vessel", "Tow", "Date", "Depth_m", "Longitude_dd", "Latitude_dd",
    "Area_swept_ha", "cpue_kg_km2", "Subsample_count", "Subsample_wt_kg",
    "total_catch_numbers", "total_catch_wt_kg"
  )]

  # Deal with the biological length data file

  if ("SURVEY" %in% colnames(datL$Lengths)) {
    tmp1 <- datL$Lengths[datL$Lengths$SURVEY == "AFSC.Slope", ]
  } else {
    tmp1 <- datL$Lengths
    tmp1$Lengths$SURVEY <- "AFSC.Slope"
  }
  tmp1 <- tmp1[tmp1$YEAR >= start.year, ]

  names(tmp1)[names(tmp1) == "HAULJOIN"] <- "Trawl_id"
  names(tmp1)[names(tmp1) == "YEAR"] <- "Year"
  names(tmp1)[names(tmp1) == "VESSEL"] <- "Vessel"
  names(tmp1)[names(tmp1) == "BOTTOM_DEPTH"] <- "Depth_m"
  names(tmp1)[names(tmp1) == "START_LATITUDE"] <- "Latitude_dd"
  names(tmp1)[names(tmp1) == "START_LONGITUDE"] <- "Longitude_dd"
  names(tmp1)[names(tmp1) == "SP_TOW_WGHT_KG"] <- "Weight" # Need to double check units
  names(tmp1)[names(tmp1) == "LENGTH"] <- "Length_cm"

  tmp1$Age <- tmp1$Weight <- NA
  # The akfsc slope sexes were specified 1 = males and 2 = females
  tmp1$Sex <- codify_sex(tmp1[["SEX"]])
  tmp1$Length_cm <- tmp1$Length_cm / 10

  tmp1$Project <- "AK.Slope"
  tmp1$Pass <- tmp1$Tow <- NA
  tmp1$Date <- paste0(tmp1$Year, "-", tmp1$MONTH, "-", tmp1$DAY)


  tmp1 <- tmp1[, c(
    "Project", "Trawl_id", "Year", "Pass", "Vessel", "Tow", "Date", "Depth_m", "Longitude_dd", "Latitude_dd",
    "Weight", "Length_cm", "Sex", "Age"
  )]

  # Deal with the biological age data file
  if ("SURVEY" %in% colnames(datL$Ages)) {
    tmp2 <- datL$Ages[datL$Ages$SURVEY == "AFSC.Slope", ]
  } else {
    tmp2 <- datL$Ages
    tmp2$Ages$SURVEY <- "AFSC.Slope"
  }

  tmp2 <- datL$Ages[datL$Ages$SURVEY == "AFSC.Slope", ]

  if (dim(tmp2)[1] > 0) {
    tmp2 <- tmp2[tmp2$YEAR >= start.year, ]

    names(tmp2)[names(tmp2) == "HAULJOIN"] <- "Trawl_id"
    names(tmp2)[names(tmp2) == "YEAR"] <- "Year"
    names(tmp2)[names(tmp2) == "VESSEL"] <- "Vessel"
    names(tmp2)[names(tmp2) == "BOTTOM_DEPTH"] <- "Depth_m"
    names(tmp2)[names(tmp2) == "START_LATITUDE"] <- "Latitude_dd"
    names(tmp2)[names(tmp2) == "START_LONGITUDE"] <- "Longitude_dd"
    names(tmp2)[names(tmp2) == "SP_TOW_WGHT_KG"] <- "Weight" # Need to double check units
    names(tmp2)[names(tmp2) == "LENGTH"] <- "Length_cm"
    names(tmp2)[names(tmp2) == "AGE"] <- "Age"

    tmp2$Weight <- NA
    # The akfsc slope sexes were specified 1 = males and 2 = females
    tmp2$Sex <- codify_sex(tmp2[["SEX"]])
    tmp2$Length_cm <- as.numeric(tmp2$Length_cm) / 10

    tmp2$Project <- "AK.Slope"
    tmp2$Pass <- tmp2$Tow <- NA
    tmp2$Date <- paste0(tmp2$Year, "-", tmp2$MONTH, "-", tmp2$DAY)

    tmp2 <- tmp2[, c(
      "Project", "Trawl_id", "Year", "Pass", "Vessel", "Tow", "Date", "Depth_m", "Longitude_dd", "Latitude_dd",
      "Weight", "Length_cm", "Sex", "Age"
    )]
  }

  out <- list()
  out$datTows <- datTows
  out$length <- tmp1
  out$age <- NULL
  if (!is.null(tmp2)) {
    if (dim(tmp2)[1] > 0) {
      out$age <- tmp2
    }
  }

  save_rdata(
    x = out,
    dir = dir,
    name_base = "afsc_slope_survey_converted_data",
    verbose = verbose
  )

  return(out)
}
