#' Function that is able to create strata data frame and will calculate strata areas using the StrataAreas.fn
#' 
#' Example function call
#' createStratDF.fn (names = c('shallow', 'deep'), depths = c(183, 300, 500), lat = c(45, 49), SA3) 
#' would create the data frame:
#'
#'   name      area          BOTTOM_DEPTH.1 BOTTOM_DEPTH.2 START_LATITUDE.1 START_LATITUDE.2
#'   shallow   2547.066      183            300            45.0             49.0          
#'   deep      2809.660      300            500            45.0             49.0            
#'
#' @param names  vector of names for strata, if not specified the default with be A, B, ...
#' @param depth  vector depth splits for each strata
#' @param lats vector of latitude splits for each strata
#' @param SA3 A dataframe of min and max latitude and min and max depth with area for each defined square.  See Details.
#'
#' @return Returns the dataframe input with a column called area.
#' @author Allan Hicks and Chantel Wetzel
#' @export
#' @seealso \code{\link{StrataAreas.fn}}

CreateStrataDF.fn <- function(names=NA, depths, lats) {
    fpath = system.file("data", "SA3.rda", package="nwfscSurvey")
    load(fpath)    
    out <- data.frame(
        name=NA,
        area=NA,
        Depth_m.1=rep(depths[-length(depths)],length(lats)-1),
        Depth_m.2=rep(depths[-1],length(lats)-1),
        Latitude_dd.1=rep(lats[-length(lats)],each=length(depths)-1),
        Latitude_dd.2=rep(lats[-1],each=length(depths)-1)
    )

    if (is.na(names[1])) {
        out$name=LETTERS[1:nrow(out)] }
    if (!is.na(names[1])) {
        out$name=names[1:nrow(out)] }
    out <- StrataAreas.fn(strat.df = out, df = SA3)
}
