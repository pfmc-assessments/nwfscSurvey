#' Calculates the number of observations by strata
#'
#'
#' @param dir directory where the output file will be saved
#' @param dat data-frame of the data that has been by the PullCatch.fn
#' @param strat.vars A vector of the strata variable names (i.e., c("Depth_m","Latitude_dd"))
#' @param strat.df a dataframe with the first column the name of the stratum, the second column the area of the stratum, and the remaining columns are the high and low variables defining the strata created by the CreateStrataDF.fn
#' @param printfolder the folder where files will be saved
#' @param verbose opt to print out message statements
#'
#' @author Chantel Wetzel
#' @export


CheckStrata.fn <- function(dir = NULL, dat, strat.vars = c("Depth_m","Latitude_dd"), strat.df, printfolder = "forSS",  verbose = TRUE)
{

    row.names(strat.df) <- strat.df[,1]     #put in rownmaes to make easier to index later
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
    dat.stratum <- split(dat, dat$stratum)

    yr.fn <- function(x) {
        x <- split(x,x$stratum)
        namesStrat <- names(x)
        nobs <- unlist(lapply(x, function(x){nrow(x)}))
        pos  <- unlist(lapply(x, function(x){ sum(x$total_catch_wt_kg > 0) }))
        if(any(nobs<=1)) {
            if (verbose){
            cat("*****\nWARNING: At least one stratum in year",x[[1]][1,"year"],"has fewer than one observation.\n*****\n")}
        }

        stratStats <- data.frame(name = namesStrat, area = strat.df[namesStrat,2], ntows = nobs, ptows = pos)
        stratStats
    }

    yearlyStrataEsts <- lapply(dat.yr, yr.fn)
    #names(yearlyStrataEsts) <- paste("Year",names(yearlyStrataEsts),sep="")

    NumberTows = as.data.frame(yearlyStrataEsts[[1]][,c("name", "ntows")])
    for(a in 2:length(yearlyStrataEsts)){
        NumberTows = cbind(NumberTows, yearlyStrataEsts[[a]]$ntows)
    }
    colnames(NumberTows) = c("Area_Name", names(yearlyStrataEsts))
    rownames(NumberTows) = c()

    PositiveTows = as.data.frame(yearlyStrataEsts[[1]][,c("name", "ptows")])
    for(a in 2:length(yearlyStrataEsts)){
        PositiveTows = cbind(PositiveTows, yearlyStrataEsts[[a]]$ptows)
    }
    colnames(PositiveTows) = c("Area_Name", names(yearlyStrataEsts))
    rownames(PositiveTows) = c()

    out = list()
    out$NumberTows = NumberTows
    out$PositiveTows = PositiveTows

    if(!is.null(dir)){
        plotdir <- file.path(dir,printfolder)
        plotdir.isdir <- file.info(plotdir)$isdir
        if(is.na(plotdir.isdir) | !plotdir.isdir){
          dir.create(plotdir)
        }
        write.csv(out, file = file.path(plotdir, paste("strata_observations.csv", sep="")), row.names = FALSE)
    }
    return(out)
}
