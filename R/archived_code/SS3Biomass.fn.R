#' This function outputs a dataframe in the abundance index format needed for SS3
#' it calculates the standard error of log(B) by first finding the cv of B
#' It can output the median biomass or the mean biomass. Median biomass is bias corrected so that log(Bmedian)=mean(log(B))
#'
#' @param bio object created by the readInBiomass.fn
#' @param fleet fleet number
#' @param season season
#' @param outputMedian TRUE/FALSE
#'
#' @author Allan Hicks & Chantel Wetzel
#' @export

SS3Biomass.fn <-function(bio, fleet="EnterFleet", season=1, outputMedian=T) {

    #This function outputs a dataframe in the abundance index format needed for SS3
    #it calculates the standard error of log(B) by first finding the cv of B
    #It can output the median biomass or the mean biomass. Median biomass is bias corrected so that log(Bmedian)=mean(log(B))
    # xxx <- split(bio[,c("AreaName","MinStratumDepth","MaxStratumDepth","Biomass","BiomassVar")],bio$Year)
    # years <- names(xxx)
    # bio <- unlist(lapply(xxx,function(x){sum(x$Biomass,na.rm=T)}))
    # variance <- unlist(lapply(xxx,function(x){sum(x$BiomassVar,na.rm=T)}))
    # cv <- sqrt(variance)/bio
    # seLogB <- sqrt(log(cv^2+1))
    # if(outputMedian==T) {
    #     med <- bio*exp(-0.5*seLogB^2)
    #     out <- data.frame(Year=years,Season=season,Fleet=fleet,Value=med,seLogB=seLogB)
    # }
    # else {
    #     out <- data.frame(Year=years,Season=season,Fleet=fleet,Value=bio,seLogB=seLogB)
    # }
    # row.names(out) <- NULL

    xxx <- split(bio[,c("Area_Name", "Min_Stratum_Depth_(m)", "Max_Stratum_Depth_(m)", "Biomass_(kg)", "Biomass_Variance")], bio[,"Survey_Year"])
    years <- names(xxx)
    bio <- unlist(lapply(xxx,function(x){sum(x[,"Biomass_(kg)"], na.rm=T)}))
    variance <- unlist(lapply(xxx,function(x){sum(x[,"Biomass_Variance"],na.rm=T)}))

    cv <- sqrt(variance)/bio
    seLogB <- sqrt(log(cv^2+1))
    if(outputMedian==T) {
        med <- bio*exp(-0.5*seLogB^2)
        out <- data.frame(Year=years, Season=season, Fleet=fleet, Value=med, seLogB=seLogB)
    }
    else {
        out <- data.frame(Year=years,Season=season,Fleet=fleet,Value=bio,seLogB=seLogB)
    }
    row.names(out) <- NULL
    return(out)

}
