#' Calculates design based estimates from survey data for early West Coast surveys (e.g., Triennial, AFSC slope)
#'
#' The variables defining the strata must begin with the name in strat.vars and end with ".1" or ".2" (i.e., BEST_DEPTH.1)
#' the strata are assumed to be continuous variables, thus have a lower and upper value defining them. The lower value does not necessarily have to be the same as the previous upper value.
#' the stat.df dataframe is difficult to build up with more than one variable becuase it turns into a design where you have to define all areas, thus repeat the variables for one (like a design)
#' 
#' An example strat.df
#' data.frame(name=c("shallowS","shallowN","deepS","deepN"),area=c(1200,1400,2000,2500),BEST_DEPTH.1=c(rep(55,2),rep(100,2)),BEST_DEPTH.2=c(rep(100,2),rep(183,2)),INPFC=c(32,42,32,42),BEST_LATITUDE.2=c(42,49,42,49))
#' I should think about adding in a routine that automatically puts in the latitude if stratifying by INPFC (or simply finds the strata by INPFC)
#' The code below splits out the data by stratum and calculates the average density in each stratum. It then expands up by area to give an estimated weight in each stratum.
#' The stratum weights are added together to get a total index for that year
#' I calculate the variance given stratified sampling theory
#' I work in normal space, then calculate the statistics if B is lognormal
#' This is the Mean Ratio Estimate
#' 
#' @param dir directory of the excel file
#' @param dat Dataframe of the data. It must have catch rate called catchPerArea and columns corresponding to strata variable names. It must also have a column called year.
#' @param stat.vars A vector of the strata variable names (i.e., c("BEST_LATITUDE","BEST_DEPTH"))
#' @param strat.df a dataframe with the first column the name of the stratum, the second column the area of the stratum, and the remaining columns are the high and low variables defining the strata.
#' @param printfolder the folder where files will be saved
#' @param outputMedian T/F output median or the mean biomass estimate
#' @param convert convert biomass 
#' @param season season for SS
#' @param fleet fleet number for SS
#' @author Allan Hicks and Chantel Wetzel
#' @export

Biomass.fn <- function(dir, dat, strat.vars = c("Depth_m","Latitude_dd"), strat.df, printfolder = "forSS", outputMedian = T, 
                                     verbose = TRUE, convert = 100, season = NA, fleet = NA)  {

    #if(removeCAN) {
    #    # This filters by HAULJOIN which no longer is provided in the database
    #    #fpath = system.file("data", "AFSCforeign_hauls.rda", package="nwfscSurvey")
    #    #load(fpath)
    #    #load("AFSCforeign_hauls.rda")
    #    #foreignHauls = AFSCforeign_hauls
    #    #foreignHauls <- read.csv(file.path(directory,foreignfile))
    #    #foreignInd <- !(dat$HAULJOIN %in% foreignHauls$HAULJOIN)
    #    #dat <- dat[foreignInd,]
    #    # if(verbose) {cat(sum(foreignInd),"rows kept (or",sum(!foreignInd),"removed) out of",totRows,"after removing foreign hauls\n")}

    #    # Simple split based on latitude
    #    totRows = dim(dat)[1]
    #    dat = dat[dat$Latitude_dd <= 49,]
    #    if(verbose) {cat(dim(dat)[1],"rows kept (or",totRows - dim(dat)[1] ,"removed) out of", totRows,"after removing foreign hauls\n")}
    #}     

    if(is.null(dat$cpue_kg_km2)) stop("There must be a column called cpue_kg_km2 in the dataframe")
    row.names(strat.df) <- strat.df[,1]     #put in rownmaes to make easier to index later
    numStrata <- nrow(strat.df)

    numStrata <- nrow(strat.df)   
    #create strata factors
    stratum <- rep(NA,nrow(dat))        #the stratum factor
    for(strat in 1:numStrata) {
        ind <- rep(T,nrow(dat))
        for(i in 1:length(strat.vars)) {
            ind <- ind & dat[,strat.vars[i]]>=strat.df[strat,paste(strat.vars[i],".1",sep="")] & dat[,strat.vars[i]]<strat.df[strat,paste(strat.vars[i],".2",sep="")]
        }
        stratum[ind] <- as.character(strat.df[strat,1])
    }
    stratum <- factor(stratum,levels=as.character(strat.df[,1]))
    dat <- data.frame(dat, stratum)  
    dat.yr <- split(dat,dat$Year)

    yr.fn <- function(x) {
        x <- split(x,x$stratum)
        namesStrat <- names(x)
        nobs <- unlist(lapply(x,function(x){nrow(x)}))
        if(any(nobs<=1)) {
            cat("*****\nWARNING: At least one stratum in year",x[[1]][1,"year"],"has fewer than one observation.\n*****\n")
        }
        meanCatchRateInStrata <- unlist(lapply(x,function(x){mean(x$cpue_kg_km2)}))
        varCatchRateInStrata  <- unlist(lapply(x,function(x){var (x$cpue_kg_km2)}))
        stratStats <- data.frame(name=namesStrat,area=strat.df[namesStrat,2],ntows=nobs,meanCatchRate=meanCatchRateInStrata,varCatchRate=varCatchRateInStrata)
        stratStats$Bhat <- stratStats$area*stratStats$meanCatchRate
        stratStats$varBhat <- stratStats$varCatchRate*(stratStats$area*stratStats$area)/stratStats$ntows
        stratStats
    }
    yearlyStrataEsts <- lapply(dat.yr,yr.fn)
    names(yearlyStrataEsts) <- paste("Year",names(yearlyStrataEsts),sep="")
    
    yrTotal.fn <- function(x) {
        data.frame(Bhat=sum(x$Bhat),seBhat=sqrt(sum(x$varBhat)),cv=sqrt(sum(x$varBhat))/sum(x$Bhat))
    }
    ests <- as.data.frame(t(as.data.frame(lapply(lapply(yearlyStrataEsts, yrTotal.fn),t)))) #some crazy stuff to put into a dataframe with years as rows
    logVar <- log(ests$cv^2+1)
    ln <- data.frame(year=substring(row.names(ests),5),meanBhat=ests$Bhat/convert,medianBhat=ests$Bhat*exp(-0.5*logVar)/convert,SElogBhat=sqrt(logVar))

    
    df.list = list()
    df  <- list(Strata=yearlyStrataEsts, Total=ests, LNtons=ln)
    if(outputMedian) { bio <- data.frame(Year=df$LNtons$year, Season = season, Fleet = fleet, Value=df$LNtons$medianBhat, seLogB=df$LNtons$SElogBhat) }
    if(!outputMedian){ bio <- data.frame(Year=df$LNtons$year, Season = season, Fleet = fleet, Value=df$LNtons$meanBhat,   seLogB=df$LNtons$SElogBhat) }

    plotdir <- file.path(dir,printfolder)
    plotdir.isdir <- file.info(plotdir)$isdir
    if(is.na(plotdir.isdir) | !plotdir.isdir){
      dir.create(plotdir)
    }
    write.csv(bio, file = paste0(plotdir,"/design_based_indices.csv"), row.names = FALSE)
    
    df.list[[1]] <- df
    df.list[[2]] <- bio
    return(df.list)
}