#' Calculate strata area
#'
#' Calculate the area of your strata desired strata using the SA3_v2021.1.rda file
#' (included with this package and accessible via \code{utils::data("SA3_v2021.1")}).
#' Default areas are available for each of 1756 combinations of
#' latitude and depth.
#' Latitude breaks must fall on 0.5 degree breaks from 32.5 to 49.0 and
#' depth breaks must be from the set (55, 75, 100, 125, 155,
#' 183, 200, 250, 300, 350, 400, 450, 500, 549, 600, 700, 800, 900, 1000,
#' 1100, 1200, 1280), where the unevenly spaced values are due to depth
#' boundaries associated with the strata used in the survey sampling design.
#' A latitude break at 40.166667 has also been added to allow strata split
#' at that common management boundary near Cape Mendocino.
#'
#' @examples
#' areaexample <- StrataAreas.fn(data.frame(
#'   name = LETTERS[1:8],
#'   Latitude_dd.2 = c(49, 49, 49, 45, 45, 40.5,40.5, 40.5),
#'   Latitude_dd.1 = c(45, 45, 40.5, 40.5, 40.5, 34.5, 34.5, 34.5),
#'   Depth_m.1 = c(183, 549, 900, 183, 549, 183, 549, 900),
#'   Depth_m.2 = c(549, 900, 1280, 549, 900, 549, 900, 1280)
#' ))
#' # setNames(round(areaexample[["area"]], 0), areaexample[["name"]])
#' #    A    B    C    D    E    F    G    H
#' # 5829 4024 9259 6211 5264 6952 7801 8059
#' @param strat.df A data frame describing the strata names and boundaries.
#' Boundaries are determined by latitude and bottom depth with
#' Latitude_dd.1 and Latitude_dd.2 being the
#' low and high bound for north to south and
#' Depth_m.1 and Depth_m.2 being the
#' low and high bound for east to west.
#' A column of \code{area} can also be included if users want this column to
#' be located in a certain order within the data frame, but it is not necessary.
#' @param df The stored data frame or a personally created data frame that
#' is used to calculate areas for the West Coast stratifications. The default
#' data frame can be accessed using \code{utils::data("SA3_v2021.1")}.
#' @return Returns the \code{strat.df} with entries in the area column containing
#' the area (square km) for each strata.
#' @author Chantel Wetzel and Kelli Johnson
#' @seealso
#' See \code{\link{CreateStrataDF.fn}} for a wrapper to this function.
#' @export
StrataAreas.fn <- function(
  strat.df,
  df = get(utils::data("SA3_v2021.1", overwrite = TRUE, package = "nwfscSurvey"))) {

  S <- strat.df
  S$area <- NA

  for (i in 1:nrow(S)) {
    # check that latitudes are from the available set
    maxLat <- max(c(S$Latitude_dd.1[i], S$Latitude_dd.2[i])) == df$MAX_LAT_DD
    minLat <- min(c(S$Latitude_dd.1[i], S$Latitude_dd.2[i])) == df$MIN_LAT_DD
    if (sum(maxLat) == 0 | sum(minLat) == 0) {
      stop(
        "A latitude in your strata is not available.\n",
        "  Either use an available latitude or supply your own area.\n",
        "  Your latitude: ",
        S$Latitude_dd.1[i],
        " ",
        S$Latitude_dd.2[i],
        "\n  Available latitudes:",
        paste(sort(unique(c(df$MAX_LAT_DD, df$MIN_LAT_DD))), collapse = " ")
      )
    }
    # check that depths are from the available set
    maxDep <- max(c(S$Depth_m.1[i], S$Depth_m.2[i])) == df$MAX_DEPTH_M
    minDep <- min(c(S$Depth_m.1[i], S$Depth_m.2[i])) == df$MIN_DEPTH_M
    if (sum(maxDep) == 0 | sum(minDep) == 0) {
      stop(
        "A depth in your strata is not available.\n",
        "  Either use an available depth or supply your own area.\n",
        "  Your depths: ",
        S$Depth_m.1[i],
        " ",
        S$Depth_m.2[i],
        "\n  Available depths: ",
        paste(sort(unique(c(df$MAX_DEPTH_M, df$MIN_DEPTH_M))), collapse = " ")
      )
    }
    # now index all rows that meet criteria for subsetting to determine area
    maxLat <- max(c(S$Latitude_dd.1[i], S$Latitude_dd.2[i])) >= df$MAX_LAT_DD
    minLat <- min(c(S$Latitude_dd.1[i], S$Latitude_dd.2[i])) <= df$MIN_LAT_DD
    maxDep <- max(c(S$Depth_m.1[i], S$Depth_m.2[i])) >= df$MAX_DEPTH_M
    minDep <- min(c(S$Depth_m.1[i], S$Depth_m.2[i])) <= df$MIN_DEPTH_M

    R <- df[maxLat & minLat & maxDep & minDep, ]
    S$area[i] <- sum(R$AREA_HECTARES) * 0.01 #km2, alternative *10000 sq meter
  }
  return(S)
}
