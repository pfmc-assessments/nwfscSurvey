#' Determine strata area.
#'
#' Calculates the area of your strata using the SA3.rda file (included with
#' this package) which contains the area for each of 1756 combinations of
#' latitude and depth.
#' Latitude breaks must fall on 0.5 degree breaks from 32.5 to 49.0 and
#' depth breaks must be from the set (55, 75, 100, 125, 155,
#' 183, 200, 250, 300, 350, 400, 450, 500, 549, 600, 700, 800, 900, 1000,
#' 1100, 1200, 1280), where the unevenly spaced values are due to depth
#' boundaries associated with the strata used in the survey sampling design.
#'
#' A convertFactor of 0.01 convert hectares to km2.
#'
#' strat.df is a dataframe with a column called "name" and two columns for
#' each stratum variable (START_LATITUDE and BOTTOM_DEPTH) indicating the low
#' and high bounds. The columns for each stratum variable should be named with
#' the stratum variable name followed by .1 for the low bound and .2 for the
#' high bound. For example, this is what a strat.df would look like (as called
#' in the example below).
#' \preformatted{
#'   name  area    START_LATITUDE.2 START_LATITUDE.1 BOTTOM_DEPTH.1 BOTTOM_DEPTH.2
#'   A     5829    49.0             45.0             183            549
#'   B     4024    49.0             45.0             549            900
#'   C     9259    49.0             40.5             900            1280
#'   D     6211    45.0             40.5             183            549
#'   E     5264    45.0             40.5             549            900
#'   F     6952    40.5             34.5             183            549
#'   G     7801    40.5             34.5             549            900
#'   H     8059    40.5             34.5             900            1280
#' }
#'
#' SA3 is provided in this package.
#'
#' @param strat.df A dataframe describing the strata name, area, and
#' boundaries.  Boundaries are determined by latitude and bottom depth. See
#' Details.
#' @param df the dataframe to calculate areas for the West Coast
#' @param convertFactor Multiplier on the area in SA3. Default = 0.01 to
#' convert hectares to square km.
#' @return Returns the dataframe input with a column called area.
#' @author Allan Hicks and Chantel Wetzel with help from John Wallace.
#' @export


StrataAreas.fn <- function(strat.df, df = SA3, convertFactor = 0.01) {
  # calculates the area of your strata using John Wallace's SA3 file
  # fpath = system.file("data", "SA3.rda", package="nwfscSurvey")
  # load(fpath)
  # this code is stolen from within John Wallace's 2011 GLMM code
  ## -8/19/2013: ACH modified it to report error when a supplied latitude or depth is not exactly matched
  # a convertFactor of 0.01 convert hectares to km2
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
    S$area[i] <- sum(R$AREA_HECTARES) * convertFactor
  }
  return(S)
}
