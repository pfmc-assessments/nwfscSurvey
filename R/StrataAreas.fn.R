StrataAreas.fn <- function(strat.df,SA3,convertFactor=0.01) {
    #calculates the area of your strata using John Wallace's SA3 file
    #this code is stolen from within John Wallace's 2011 GLMM code
    ##-8/19/2013: ACH modified it to report error when a supplied latitude or depth is not exactly matched
    #a convertFactor of 0.01 convert hectares to km2
    S <- strat.df
    S$area <- NA

    for ( i in 1:nrow(S)) {
        #first check that depths are exactly defined
        maxLat <- max(c(S$START_LATITUDE.1[i],S$START_LATITUDE.2[i])) == SA3$MAX_LAT_DD
        minLat <- min(c(S$START_LATITUDE.1[i],S$START_LATITUDE.2[i])) == SA3$MIN_LAT_DD
        if(sum(maxLat) == 0 | sum(minLat) == 0) stop(cat("A latitude in your strata is not available in SA3.\nEither use an available latitude or supply your own area.\nYour latitude:",
                                                         c(S$START_LATITUDE.1[i],S$START_LATITUDE.2[i]),
                                                         "\nAvailable latitudes:",sort(unique(c(SA3$MAX_LAT_DD,SA3$MIN_LAT_DD))),"\n"))
        maxDep <- max(c(S$BOTTOM_DEPTH.1[i],S$BOTTOM_DEPTH.2[i])) == SA3$MAX_DEPTH_M
        minDep <- min(c(S$BOTTOM_DEPTH.1[i],S$BOTTOM_DEPTH.2[i])) == SA3$MIN_DEPTH_M
        if(sum(maxDep) == 0 | sum(minDep) == 0) stop(cat("A depth in your strata is not available in SA3.\nEither use an available depth or supply your own area.\nYour depths:",
                                                         c(S$BOTTOM_DEPTH.1[i],S$BOTTOM_DEPTH.2[i]),
                                                         "\nAvailable depths:",sort(unique(c(SA3$MAX_DEPTH_M,SA3$MIN_DEPTH_M))),"\n"))

        #now index all rows that meet criteria for subsetting to determine area
        maxLat <- max(c(S$START_LATITUDE.1[i],S$START_LATITUDE.2[i])) >= SA3$MAX_LAT_DD
        minLat <- min(c(S$START_LATITUDE.1[i],S$START_LATITUDE.2[i])) <= SA3$MIN_LAT_DD
        maxDep <- max(c(S$BOTTOM_DEPTH.1[i],S$BOTTOM_DEPTH.2[i])) >= SA3$MAX_DEPTH_M
        minDep <- min(c(S$BOTTOM_DEPTH.1[i],S$BOTTOM_DEPTH.2[i])) <= SA3$MIN_DEPTH_M

        R <- SA3[maxLat & minLat & maxDep & minDep,]
        S$area[i] <- sum(R$AREA_HECTARES)*convertFactor
    }
    S
}
