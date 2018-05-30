#Written by Allan Hicks
#allan.hicks@noaa.gov
#206-302-2435
#05/03/2011
#Version 0.23
#changed filename to EarlyWestCoastSurveyFunctions to avoid confusion with Alaska surveys
#the code is good for the Triennial survey and the AK slope survey

#05/31/2011
#Version 0.24
#added age-at-length function

ReadInBiomass.EWC.fn <- function(dataFile,directory,species=c(NA),removeCAN=T,verbose=F) {
###############################################################################################
#Reads in the triennial survey data and filters the data into what is necessary
#It reads in catch and station data and makes sure only the species necessary are kept
#### may want to keep NA (blank in Excel) to select the zero tows
#removeCAN is a flag if you want tows in Canadian waters removed
#   need the file called foreign_hauls.csv
#
#Written by Allan Hicks
#Necessary column names
#   SPECIES_CODE
#   WEIGHT in kg
#   DISTANCE_FISHED is in km
#   NET_WIDTH is in m
###############################################################################

    dat <- read.csv(paste(directory,dataFile,sep="\\"))
    totRows <- nrow(dat)
    dat <- dat[dat$SPECIES_CODE %in% species,]
    if(verbose) {cat(nrow(dat),"rows kept out of",totRows,"after filtering by species\n")}
    totRows <- nrow(dat)
    
    if(removeCAN) {
        foreignHauls <- read.csv(paste(directory,"foreign_hauls.csv",sep="\\"))
        foreignInd <- !(dat$HAULJOIN %in% foreignHauls$HAULJOIN)
        dat <- dat[foreignInd,]
        if(verbose) {cat(sum(foreignInd),"rows kept (or",sum(!foreignInd),"removed) out of",totRows,"after removing foreign hauls\n")}
    }            
    
    #calculate the density (kg/km^2) using net width and distance fished
    dat$areaFished <- dat$DISTANCE_FISHED*(dat$NET_WIDTH/1000)
    tmp <- sum(is.na(dat$areaFished))
    if(tmp>0 | verbose) {cat("There are",tmp,"instances where area swept could not be calculated due to missing values.\n")}
    dat$kgPerKm2 <- dat$WEIGHT/dat$areaFished
    dat$kgPerKm2[is.na(dat$kgPerKm2)&!is.na(dat$areaFished)] <- 0                                  #the tows with no observation of the species
    return(dat)
    
}



ReadInLengths.EWC.fn <- function(dataFile,directory,species=c(NA),removeCAN=T,verbose=F) {
###############################################################################################
#Reads in the triennial survey data and filters the data into what is necessary
#It reads in catch and station data and makes sure only the species necessary are kept
#### may want to keep NA (blank in Excel) to select the zero tows
#removeCAN is a flag if you want tows in Canadian waters removed
#   need the file called foreign_hauls.csv
#
#
#Necessary column names
#   SPECIES_CODE
#   LENGTH
###############################################################################

    dat <- read.csv(paste(directory,dataFile,sep="\\"))
    totRows <- nrow(dat)
    dat <- dat[dat$SPECIES_CODE %in% species,]
    if(verbose) {cat(nrow(dat),"rows kept out of",totRows,"after filtering by species\n")}
    totRows <- nrow(dat)
    
    if(removeCAN) {
        foreignHauls <- read.csv(paste(directory,"foreign_hauls.csv",sep="\\"))
        foreignInd <- !(dat$HAULJOIN %in% foreignHauls$HAULJOIN)
        dat <- dat[foreignInd,]
        if(verbose) {cat(sum(foreignInd),"rows kept (or",sum(!foreignInd),"removed) out of",totRows,"after removing foreign hauls\n")}
        totRows <- nrow(dat)
    }        
    
    dat <- dat[!is.na(dat$LENGTH),]
    if(verbose) {cat(nrow(dat),"rows kept out of",totRows,"after removing missing lengths\n")}
    
    return(dat)   
}





StrataFactors.fn <- function(dat,strat.vars,strat.df) {
#creates a vector of strata factors by name
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
    return(stratum)
}
#dat <- data.frame(BOTTOM_DEPTH=c(90,90,90,90,200,200,200,200,90,90,90,90,200,200,200,200),START_LATITUDE=c(36.5,36.5,38,38,36.5,36.5,38,38,36.5,36.5,38,38,36.5,36.5,38,38),year=c(rep(2000,8),rep(2001,8)),
#                    catchPerArea=c(1,3,5,7,9,11,13,13,21,23,25,27,29,111,113,113))
#xs <- data.frame(name=c("shallowS","shallowN","deepS","deepN"),area=c(1200,1400,2000,2500),BOTTOM_DEPTH.1=c(rep(55,2),rep(100,2)),BOTTOM_DEPTH.2=c(rep(100,2),rep(1000,2)),START_LATITUDE.1=c(36,37,36,37),START_LATITUDE.2=c(37,49,37,49))
#data.frame(dat,StrataFactors.fn(dat,strat.vars=c("START_LATITUDE","BOTTOM_DEPTH"),strat.df=xs))


StrataAreas.fn <- function(strat.df,SA3,convertFactor=0.01) {
    #calculates the area of your strata using John Wallace's SA3 file
    #this code is stolen from within John Wallace's 2011 GLMM code
    #a convertFactor of 0.01 convert hectares to km2
    S <- strat.df
    S$area <- NA

    for ( i in 1:nrow(S)) {
        maxLat <- max(c(S$START_LATITUDE.1[i],S$START_LATITUDE.2[i])) >= SA3$MAX_LAT_DD
        minLat <- min(c(S$START_LATITUDE.1[i],S$START_LATITUDE.2[i])) <= SA3$MIN_LAT_DD
        if(sum(maxLat) == 0 | sum(minLat) == 0) stop("A latitude in your strata is not available in SA3.\nEither use ana available latitude or supply your own area.") 
        maxDep <- max(c(S$BOTTOM_DEPTH.1[i],S$BOTTOM_DEPTH.2[i])) >= SA3$MAX_DEPTH_M
        minDep <- min(c(S$BOTTOM_DEPTH.1[i],S$BOTTOM_DEPTH.2[i])) <= SA3$MIN_DEPTH_M
        if(sum(maxDep) == 0 | sum(minDep) == 0) stop("A depth in your strata is not available in SA3.\nEither use an available depth or supply your own area.") 
        R <- SA3[maxLat & minLat & maxDep & minDep,]
        S$area[i] <- sum(R$AREA_HECTARES)*convertFactor
    }
    S
}



DesignBasedEstBiomass.EWC.fn <- function(dat,strat.vars,strat.df)  {
#Calculates design based estimates from survey data
#dat is a dataframe of the data. It must have catchrate called catchPerArea and columns corresponding to strata variable names. It must also have a column called year.
#strat.vars is a vector of the strata variable names (i.e., c("BEST_LATITUDE","BEST_DEPTH"))
#strat.df is a dataframe with the first column the name of the stratum, the second column the area of the stratum, and the remaining columns are the high and low variables defining the strata
#The variables defining the strata must begin with the name in strat.vars and end with ".1" or ".2" (i.e., BEST_DEPTH.1)
#the strata are assumed to be continuous variables, thus have a lower and upper value defining them. The lower value does not necessarily have to be the same as the previous upper value.
#the stat.df dataframe is difficult to build up with more than one variable becuase it turns into a design where you have to define all areas, thus repeat the variables for one (like a design)
#An example strat.df
### data.frame(name=c("shallowS","shallowN","deepS","deepN"),area=c(1200,1400,2000,2500),BEST_DEPTH.1=c(rep(55,2),rep(100,2)),BEST_DEPTH.2=c(rep(100,2),rep(183,2)),INPFC=c(32,42,32,42),BEST_LATITUDE.2=c(42,49,42,49))
### I should think about adding in a routine that automatically puts in the latitude if stratifying by INPFC (or simply finds the strata by INPFC)
#The code below splits out the data by stratum and calculates the average density in each stratum. It then expands up by area to give an estimated weight in each stratum.
#The stratum weights are added together to get a total index for that year
#I calculate the variance given stratified sampling theory
#I work in normal space, then calculate the statistics if B is lognormal
#This is the Mean Ratio Estimate

    if(is.null(dat$catchPerArea)) stop("There must be a column called catchPerArea in the dataframe")
    row.names(strat.df) <- strat.df[,1]     #put in rownmaes to make easier to index later
    numStrata <- nrow(strat.df)
    
    #first create strata factors
    dat <- data.frame(dat,stratum=StrataFactors.fn(dat,strat.vars,strat.df))        #create a new column for the stratum factor
    dat.yr <- split(dat,dat$year)

    yr.fn <- function(x) {
        x <- split(x,x$stratum)
        namesStrat <- names(x)
        nobs <- unlist(lapply(x,function(x){nrow(x)}))
        if(any(nobs<=1)) {
            cat("*****\nWARNING: At least one stratum in year",x[[1]][1,"year"],"has fewer than one observation.\n*****\n")
        }
        meanCatchRateInStrata <- unlist(lapply(x,function(x){mean(x$catchPerArea)}))
        varCatchRateInStrata <- unlist(lapply(x,function(x){var(x$catchPerArea)}))
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
    ests <- as.data.frame(t(as.data.frame(lapply(lapply(yearlyStrataEsts,yrTotal.fn),t))))             #some crazy stuff to put into a dataframe with years as rows
    logVar <- log(ests$cv^2+1)
    ln <- data.frame(year=substring(row.names(ests),5),meanBhat=ests$Bhat/1000,medianBhat=ests$Bhat*exp(-0.5*logVar)/1000,SElogBhat=sqrt(logVar))
    
    list(Strata=yearlyStrataEsts,Total=ests,LNtons=ln)
}

#dat <- data.frame(BOTTOM_DEPTH=c(90,90,90,90,200,200,200,200,90,90,90,90,200,200,200,200),START_LATITUDE=c(36.5,36.5,38,38,36.5,36.5,38,38,36.5,36.5,38,38,36.5,36.5,38,38),year=c(rep(2000,8),rep(2001,8)),
#                    catchPerArea=c(1,3,5,7,9,11,13,13,21,23,25,27,29,111,113,113))
#xs <- data.frame(name=c("shallowS","shallowN","deepS","deepN"),area=c(1200,1400,2000,2500),BOTTOM_DEPTH.1=c(rep(55,2),rep(100,2)),BOTTOM_DEPTH.2=c(rep(100,2),rep(1000,2)),START_LATITUDE.1=c(36,37,36,37),START_LATITUDE.2=c(37,49,37,49))
#TriennialDesignBasedEstBiomass.fn(dat,strat.vars=c("START_LATITUDE","BOTTOM_DEPTH"),strat.df=xs)



SurveyLFs.EWC.fn <- function(datL,datTows,strat.vars=NULL,strat.df=NULL,femaleMale=c(2,1),lgthBins=1,SS3out=F,meanRatioMethod=T,gender=3,NAs2zero=T,sexRatioUnsexed=NA,maxSizeUnsexed=NA)  {
#expands the lengths up to the total stratum area then sums over strata
#Written by Allan Hicks 16 March 2009
#modified to incorporate unsexed fish using sex ratios in May 2011
#weighted by sample size and area
#datL should have a column called "year" indicating year
#femaleMale is a vector of codes for female then male (in that order)
#lgthBin is the increment of each length bin or a vector of the actual bins
    #NOTE: The length bin called F0 or M0 is retained to show proportion of lengths smaller than smallest bin
    #      You will want to likely add this to your first length bin and delete this before putting in SS3, or
    #       start the lgthBins argument at the 2nd length bin and F0 will be all fish smaller (hence the first length bin)
#SS3out: if True the output is in a format pastable into SS3 dat file

    row.names(strat.df) <- strat.df[,1]     #put in rownames to make easier to index later
    numStrata <- nrow(strat.df)
    ind <- !duplicated(datL$HAULJOIN)
    datB <- datL[ind,c("HAULJOIN","WEIGHT","NUMBER_FISH",strat.vars,"DISTANCE_FISHED","NET_WIDTH","year")]    #individual tow data
    datB$areaFished <- datB$DISTANCE_FISHED*datB$NET_WIDTH/1000   #area swept for each tow in km2

    datL$sex2 <- rep(NA,nrow(datL))
    datL[datL$SEX==femaleMale[1],"sex2"] <- "f"
    datL[datL$SEX==femaleMale[2],"sex2"] <- "m"
    
    #set up length bins
    if(length(lgthBins)==1) {
        Lengths <- c(0,seq(floor(min(datL$LENGTH)),ceiling(max(datL$LENGTH)),lgthBins),Inf)
    }
    else{
        Lengths <- c(0,lgthBins,Inf)
    }
    #print(Lengths)
    datL$allLs <- Lengths[findInterval(datL$LENGTH,Lengths,all.inside=T)]
    #print(head(datL))

    
    #first create strata factors
    datB <- data.frame(datB,stratum=StrataFactors.fn(datB,strat.vars,strat.df))        #create a new column for the stratum factor
    numTows <- table(datTows$year,StrataFactors.fn(datTows,strat.vars,strat.df))        #strata for each individual tow

    #calculate expansion factor per tow
    #for all sexes
    TdatL.tows <- as.data.frame(table(datL$HAULJOIN))
    datB <- data.frame(datB[match(as.character(TdatL.tows$Var1),as.character(datB$HAULJOIN)),],TowExpFactorU=TdatL.tows$Freq)
    datB$TowExpFactorU <- datB$NUMBER_FISH/datB$TowExpFactorU
    #for females and males only
    TdatL.tows <- as.data.frame(table(datL$HAULJOIN,datL$SEX%in%femaleMale))
    TdatL.tows <- TdatL.tows[TdatL.tows$Var2==TRUE,]
    datB <- data.frame(datB[match(as.character(TdatL.tows$Var1),as.character(datB$HAULJOIN)),],TowExpFactorMF=TdatL.tows$Freq)
    datB$TowExpFactorMF <- datB$NUMBER_FISH/datB$TowExpFactorMF
    datB$TowExpFactorMF[datB$TowExpFactorMF==Inf] <- NA
    
    #find frequency of number of lengths
    TdatL.lengths <- as.data.frame(table(datL$HAULJOIN,datL$LENGTH))  #first do all lengths, unsexed
    names(TdatL.lengths) <- c("HAULJOIN","LENGTH","numU")
    TdatL.lengths <- TdatL.lengths[TdatL.lengths$numU>0,]
    datB <- merge(datB,TdatL.lengths,by="HAULJOIN",all=T)
    #Females and males
    TdatL.lengths <- as.data.frame(table(datL$HAULJOIN,datL$LENGTH,datL$sex2))
    names(TdatL.lengths) <- c("HAULJOIN","LENGTH","SEX","num")
    TdatL.lengths <- TdatL.lengths[TdatL.lengths$num>0,]
    TdatL.lengths <- split(TdatL.lengths,TdatL.lengths$SEX)
    temp <- TdatL.lengths[["f"]][,c("HAULJOIN","LENGTH","num")]
    names(temp) <- c("HAULJOIN","LENGTH","numF")
    datB <- merge(datB,temp,by=c("HAULJOIN","LENGTH"),all=T)
    datB[is.na(datB$numF),"numF"] <- 0
    temp <- TdatL.lengths[["m"]][,c("HAULJOIN","LENGTH","num")]
    names(temp) <- c("HAULJOIN","LENGTH","numM")
    datB <- merge(datB,temp,by=c("HAULJOIN","LENGTH"),all=T)
    datB[is.na(datB$numM),"numM"] <- 0

    #now calculate the expanded lengths per tow
    datB$expU <- datB$numU*datB$TowExpFactorU
    datB$expF <- datB$numF*datB$TowExpFactorMF
    datB$expM <- datB$numM*datB$TowExpFactorMF
    
    datB$LENGTH <- as.numeric(as.character(datB$LENGTH))
    datB$allLs <- Lengths[findInterval(datB$LENGTH,Lengths,all.inside=T)]
    #print(head(datB))
   
    #incorporate unsexed fish using sex ratios
    if(length(sexRatioUnsexed)==1 & !is.na(sexRatioUnsexed)) {
        datB$sexRatio <- datB$expF/(datB$expF+datB$expM)
        datB$sexRatio[datB$LENGTH <= maxSizeUnsexed] <- sexRatioUnsexed
        #now fill in any missing ratios with ratios of that bin from other years and strata (can probably be done more efficiently)
        noRatio <- which(is.na(datB$sexRatio))
        if(length(noRatio)>0) cat("\nThese are sex ratios that were filled in using observations from the same lengths from different strata and years\n")
        for(i in noRatio) {
            inds <- datB$allLs==datB$allLs[i]
            tmpF <- sum(datB$expF[inds])
            tmpM <- sum(datB$expM[inds])
            datB$sexRatio[i] <- tmpF/(tmpF+tmpM)
            print(datB[i,c("LENGTH","allLs","expF","expM","sexRatio")])
        }

        noRatio <- which(is.na(datB$sexRatio))
        if(length(noRatio)>0) cat("\nThese are sex ratios that were filled in using observations from nearby lengths\n")
        for(i in noRatio) {
            nearLens <- Lengths[c(which(Lengths==datB$allLs[i])-1,which(Lengths==datB$allLs[i])+1)]
            inds <- datB$allLs %in% nearLens
            tmpF <- sum(datB$expF[inds])
            tmpM <- sum(datB$expM[inds])
            datB$sexRatio[i] <- tmpF/(tmpF+tmpM)
            print(datB[i,c("LENGTH","allLs","expF","expM","sexRatio")])
        }
        noRatio <- which(is.na(datB$sexRatio))
        if(length(noRatio)>0) cat("Some sex ratios were left unknown and omitted\n\n")
        if(length(noRatio)==0) cat("Done filling in sex ratios\n\n")

        tmpFemUnsex <- round(datB$sexRatio*(datB$expU-datB$expF-datB$expM))
        tmpMaleUnsex <- datB$expU - tmpFemUnsex
        datB$NumF <- datB$expF + tmpFemUnsex
        datB$NumM <- datB$expM + tmpMaleUnsex
        #print(unique(round(datB$sexRatio,1)))
    }
    
    #sum over strata within year
    datB.yrstr <- split(datB,as.character(datB$year))
    datB.yrstr <- lapply(datB.yrstr,function(x){split(x,as.character(x$stratum))})
    names(datB.yrstr)
    lengthTotalRatio.fn <- function(x,strat) {
        #function to sum lengths within a stratum and a year
        #Uses the Total Ratio estimate and Mean ratio estimate
        theYear <- unique(x$year)
        theStratum <- unique(x$stratum)
        if(length(theYear)!=1) stop("there is a problem in length.fn. There should be exactly one year in each list item.\n")
        if(length(theStratum)!=1) stop("there is a problem in length.fn. There should be exactly one stratum in each list item.\n")
        a.hi <- unlist(lapply(split(x,x$OP_CODE),function(x){x$areaFished[1]}))  #area swept per tow in stratum
        a.h <- sum(a.hi)  #total area swept in stratum
        A.h <- strat[strat$name==theStratum,"area"]
        x$LENGTH_cm <- as.numeric(as.character(x$LENGTH_cm))
        noDups <- !duplicated(x$LENGTH_cm)
        xcols <- c("year","stratum") #must be two or more columns to keep the selection a dataframe
        lgths <- split(x,x$LENGTH_cm)
        LjhU <- unlist(lapply(lgths,function(x){sum(x$expU)}))
        x <- data.frame(x[rep(1,length(LjhU)),xcols],area=A.h,areaSwept=a.h,LENGTH_cm=as.numeric(names(LjhU)),LjhU=LjhU,TotalLjhU=A.h*LjhU/a.h)
        LjhF <- unlist(lapply(lgths,function(x){sum(x$expF)}))
        x <- data.frame(x,LjhF=LjhF,TotalLjhF=A.h*LjhF/a.h)
        LjhM <- unlist(lapply(lgths,function(x){sum(x$expM)}))
        x <- data.frame(x,LjhM=LjhM,TotalLjhM=A.h*LjhM/a.h)
        return(x)
    }
    lengthMeanRatio.fn <- function(x,strat,numTows) {
        #function to sum lengths within a stratum and a year
        #Uses the Mean ratio estimate
        theYear <- unique(x$year)
        theStratum <- unique(x$stratum)
        if(length(theYear)!=1) stop("there is a problem in length.fn. There should be exactly one year in each list item.\n")
        if(length(theStratum)!=1) stop("there is a problem in length.fn. There should be exactly one stratum in each list item.\n")
        if(!(as.character(theYear)%in%row.names(numTows))) stop(paste("The year",theYear,"is in the lengths file but is not in the tow file"))
        ntows <- numTows[as.character(theYear),theStratum]
        A.h <- strat[strat$name==theStratum,"area"]
        x$LENGTH_cm <- as.numeric(as.character(x$LENGTH))
        noDups <- !duplicated(x$LENGTH)
        xcols <- c("year","stratum") #must be two or more columns to keep the selection a dataframe
        lgths <- split(x,x$LENGTH)
        LjhU <- unlist(lapply(lgths,function(x){sum(x$expU/x$areaFished)}))
        x <- data.frame(x[rep(1,length(LjhU)),xcols],area=A.h,LENGTH=as.numeric(names(LjhU)),LjhU=LjhU/ntows,TotalLjhU=A.h*LjhU/ntows)
        LjhF <- unlist(lapply(lgths,function(x){sum(x$expF/x$areaFished)}))
        x <- data.frame(x,LjhF=LjhF/ntows,TotalLjhF=A.h*LjhF/ntows)
        LjhM <- unlist(lapply(lgths,function(x){sum(x$expM/x$areaFished)})) 
        x <- data.frame(x,LjhM=LjhM/ntows,TotalLjhM=A.h*LjhM/ntows)
        return(x)
    }

    if(meanRatioMethod) {
        L.year.str <- lapply(datB.yrstr,function(x){lapply(x,lengthMeanRatio.fn,strat=strat.df,numTows)})
    }
    else{
        L.year.str <- lapply(datB.yrstr,function(x){lapply(x,lengthTotalRatio.fn,strat=strat.df)})
    }

     year.fn <- function(x,Lengths) {   #calculate the LFs by year
        theLs.yr <- unlist(lapply(x,function(x){as.numeric(as.character(x$LENGTH))}))
        TotalLjhU <- unlist(lapply(x,function(x){as.numeric(as.character(x$TotalLjhU))}))
        TotalLjhF <- unlist(lapply(x,function(x){as.numeric(as.character(x$TotalLjhF))}))
        TotalLjhM <- unlist(lapply(x,function(x){as.numeric(as.character(x$TotalLjhM))}))
        allLs <- Lengths[findInterval(theLs.yr,Lengths,all.inside=T)]    #finds the interval that the length falls in and floors it (so 23.2 would be in 23 if 23 was a level in Lengths, all.inside puts maximum age group into N-1 group, thus I padded with Inf.)
        TotalLjU <- tapply(TotalLjhU,allLs,sum,na.rm=T)
        TotalLjF <- tapply(TotalLjhF,allLs,sum,na.rm=T)
        TotalLjM <- tapply(TotalLjhM,allLs,sum,na.rm=T)
        out <- data.frame(Length=Lengths,TotalLjU=rep(NA,length(Lengths)),TotalLjF=rep(NA,length(Lengths)),TotalLjM=rep(NA,length(Lengths)))
        row.names(out) <- out$Length
        out[names(TotalLjU),"TotalLjU"] <- 100*(TotalLjU+TotalLjF+TotalLjM)/sum(TotalLjU+TotalLjF+TotalLjM,na.rm=T)
        out[names(TotalLjF),"TotalLjF"] <- 100*TotalLjF/(sum(TotalLjF,na.rm=T)+sum(TotalLjM,na.rm=T))
        out[names(TotalLjM),"TotalLjM"] <- 100*TotalLjM/(sum(TotalLjF,na.rm=T)+sum(TotalLjM,na.rm=T))
        out <- out[-nrow(out),]   #remove last row because Inf and always NA due to inside.all=T
        return(out)
    }
    L.year <- lapply(L.year.str,year.fn,Lengths=Lengths)
    if(!SS3out) {
        return(list(L.year=L.year,L.year.str=L.year.str))
    }
    
    #otherwise return SS3 output for gender type
    if(gender==0) {
        lgths <- as.character(L.year[[1]]$Length)
        Ls <- unlist(lapply(L.year,function(x){c(x$TotalLjU)}))
        if(NAs2zero){Ls[is.na(Ls)] <- 0}
        Ls <- matrix(Ls,nrow=length(L.year),byrow=T,
            dimnames=list(NULL,paste(rep("U",length(lgths)),lgths,sep="")))
        out <- data.frame(year=as.numeric(names(L.year)),season=rep(NA,length(L.year)),fleet=rep(NA,length(L.year)),gender=rep(0,length(L.year)),
            partition=rep(0,length(L.year)),Nsamp=rep(NA,length(L.year)),Ls)    
    }
    if(gender==3) {
        #females then males
        lgths <- as.character(L.year[[1]]$Length)
        Ls <- unlist(lapply(L.year,function(x){c(x$TotalLjF,x$TotalLjM)}))
        if(NAs2zero){Ls[is.na(Ls)] <- 0}
        Ls <- matrix(Ls,nrow=length(L.year),byrow=T,
            dimnames=list(NULL,paste(c(rep("F",length(lgths)),rep("M",length(lgths))),lgths,sep="")))
        out <- data.frame(year=as.numeric(names(L.year)),season=rep(NA,length(L.year)),fleet=rep(NA,length(L.year)),gender=rep(3,length(L.year)),
            partition=rep(0,length(L.year)),Nsamp=rep(NA,length(L.year)),Ls)
    }

    cat("\nNOTE: You may need to add the column called F0 and/or M0 to your first length bin\n\tand delete that column.\n\tThese are the proportion of lengths smaller than the first length bin\n\n")
    return(out)
}

#strataDef <- read.table("C:\\NOAA2009\\Petrale\\Data\\Survey\\Triennial survey\\strata.csv",header=T,sep=",")
#datB <- PetraleTriennialBiomass.fn(verbose=T)
#datB <- datB[datB$year!=1977,]
#datL <- PetraleTriennialLengths.fn(verbose=T)
#tmp <- TriennialSurveyLFs.fn(datL,datB,strat.vars=c("BOTTOM_DEPTH","START_LATITUDE"),strat.df=strataDef,femaleMale=c(2,1),lgthBins=20)#seq(120,620,20))
#tmp <- TriennialSurveyLFs.fn(datL,strat.vars=c("BOTTOM_DEPTH","START_LATITUDE"),strat.df=strataDef,femaleMale=c(2,1),lgthBins=20,SS3out=T)




SurveyAgeAtLen.EWC.fn <- function(datAL,datTows,strat.vars=NULL,strat.df=NULL,femaleMale=c(2,1),lgthBins=1,ageBins=1,SS3out=F,meanRatioMethod=T,raw=T,NAs2zero=T,season="ENTER",fleet="ENTER",partition="ETNER",ageerr="ENTER",returnSamps=F)  {
    #calculates proportion of age at length and reformats into SS3 format
    #Uses raw numbers at length, assuming that is a random sample conditioned on length and sex.
    #To use expanded numbers (up to strata areas), set raw=F
    #Only gender codes 1 and 2 and puts males and females on separate lines because the age@L is conditioned on sex (a sample of females of length 25cm, for example)
    #Gender=1: females only. Male values ignored
    #Gender=2: males only. Female values ignored.
    #lgthBins is either the interval between length bins or the actual length bins
    #note that 0 and Inf are tacked on the ends to account for lengths and ages outside the interval. You may want to add these in to first and last bin.
    #I assume all fish are sexed for age data, thus do not apply sex ratios for unsexed fish

    row.names(strat.df) <- strat.df[,1]     #put in rownames to make easier to index later
    numStrata <- nrow(strat.df)

    ind <- !duplicated(datAL$HAULJOIN)
    datB <- datAL[ind,c("HAULJOIN","WEIGHT","NUMBER_FISH",strat.vars,"DISTANCE_FISHED","NET_WIDTH","year")]    #individual tow data
    datB$areaFished <- datB$DISTANCE_FISHED*datB$NET_WIDTH/1000   #area swept for each tow in km2

    datAL$sex2 <- rep(NA,nrow(datAL))
    datAL[datAL$SEX==femaleMale[1],"sex2"] <- "f"
    datAL[datAL$SEX==femaleMale[2],"sex2"] <- "m"
    
    #set up length bins
    if(length(lgthBins)==1) {
        Lengths <- c(0,seq(floor(min(datAL$LENGTH)),ceiling(max(datAL$LENGTH)),lgthBins),Inf)
    }
    else{
        Lengths <- c(0,lgthBins,Inf)
    }
    #print(Lengths)
    if(length(ageBins)==1) {
        Ages <- c(0,seq(floor(min(datAL$AGE)),ceiling(max(datAL$AGE)),ageBins),Inf)
    } else{
        Ages <- c(0,ageBins,Inf)        #put 0 and Inf on ends because all.inside=T in findInterval below. Treats these as minus and plus groups
    }

    datAL$allLs <- Lengths[findInterval(datAL$LENGTH,Lengths,all.inside=T)]
    datAL$allAs <- Ages[findInterval(datAL$AGE,Ages,all.inside=T)]
    #print(head(datAL))

    #first create strata factors
    datB <- data.frame(datB,stratum=StrataFactors.fn(datB,strat.vars,strat.df))        #create a new column for the stratum factor
    #print(head(datTows))
    numTows <- table(datTows$year,StrataFactors.fn(datTows,strat.vars,strat.df))        #strata for each individual tow

    
    #calculate expansion factor per tow
    #for all sexes
    TdatL.tows <- as.data.frame(table(datAL$HAULJOIN))
    datB <- data.frame(datB[match(as.character(TdatL.tows$Var1),as.character(datB$HAULJOIN)),],TowExpFactorU=TdatL.tows$Freq)
    datB$TowExpFactorU <- datB$NUMBER_FISH/datB$TowExpFactorU
    #for females and males only
    TdatL.tows <- as.data.frame(table(datAL$HAULJOIN,datAL$SEX%in%femaleMale))
    TdatL.tows <- TdatL.tows[TdatL.tows$Var2==TRUE,]
    datB <- data.frame(datB[match(as.character(TdatL.tows$Var1),as.character(datB$HAULJOIN)),],TowExpFactorMF=TdatL.tows$Freq)
    datB$TowExpFactorMF <- datB$NUMBER_FISH/datB$TowExpFactorMF
    datB$TowExpFactorMF[datB$TowExpFactorMF==Inf] <- NA

    #find frequency of number of age at lengths
    TdatL.al <- as.data.frame(table(datAL$HAULJOIN,datAL$allLs,datAL$allAs))  #first do all age at lengths, unsexed
    names(TdatL.al) <- c("HAULJOIN","allLs","allAs","numU")
    TdatL.al <- TdatL.al[TdatL.al$numU>0,]
    datB <- merge(datB,TdatL.al,by="HAULJOIN",all=T)
    
    #Females and males
    TdatL.al <- as.data.frame(table(datAL$HAULJOIN,datAL$allLs,datAL$allAs,datAL$sex2))
    names(TdatL.al) <- c("HAULJOIN","allLs","allAs","SEX","num")
    TdatL.al <- TdatL.al[TdatL.al$num>0,]
    print(head(TdatL.al))
    TdatL.al <- split(TdatL.al,TdatL.al$SEX)
    temp <- TdatL.al[["f"]][,c("HAULJOIN","allLs","allAs","num")]
    names(temp) <- c("HAULJOIN","allLs","allAs","numF")
    datB <- merge(datB,temp,by=c("HAULJOIN","allLs","allAs"),all=T)
    datB[is.na(datB$numF),"numF"] <- 0
    temp <- TdatL.al[["m"]][,c("HAULJOIN","allLs","allAs","num")]
    names(temp) <- c("HAULJOIN","allLs","allAs","numM")
    datB <- merge(datB,temp,by=c("HAULJOIN","allLs","allAs"),all=T)
    datB[is.na(datB$numM),"numM"] <- 0
    print(head(datB))
    
    nSamps <- datB[!duplicated(paste(datB$HAULJOIN,datB$allLs)),]
    nSamps.f <- nSamps[nSamps$numF>0,]
    nSamps.m <- nSamps[nSamps$numM>0,]
    nSamps.f <- table(nSamps.f$year,nSamps.f$allLs)
    nSamps.m <- table(nSamps.m$year,nSamps.m$allLs)
    if(returnSamps) return(list(nSamps.f,nSamps.m))

    #now calculate the expanded lengths per tow
    datB$expU <- datB$numU*datB$TowExpFactorU
    datB$expF <- datB$numF*datB$TowExpFactorMF
    datB$expM <- datB$numM*datB$TowExpFactorMF
    
    #sum over strata within year
    datB.yrLstr <- split(datB,as.character(paste(datB$year,datB$allLs)))
    datB.yrLstr <- lapply(datB.yrLstr,function(x){split(x,as.character(x$stratum))})

    #lengthTotalRatio.fn <- function(x,strat) {
    #    #function to sum lengths within a stratum and a year
    #    #Uses the Total Ratio estimate and Mean ratio estimate
    #    theYear <- unique(x$year)
    #    theStratum <- unique(x$stratum)
    #    if(length(theYear)!=1) stop("there is a problem in length.fn. There should be exactly one year in each list item.\n")
    #    if(length(theStratum)!=1) stop("there is a problem in length.fn. There should be exactly one stratum in each list item.\n")
    #    a.hi <- unlist(lapply(split(x,x$OP_CODE),function(x){x$areaFished[1]}))  #area swept per tow in stratum
    #    a.h <- sum(a.hi)  #total area swept in stratum
    #    A.h <- strat[strat$name==theStratum,"area"]
    #    x$LENGTH_cm <- as.numeric(as.character(x$LENGTH_cm))
    #    noDups <- !duplicated(x$LENGTH_cm)
    #    xcols <- c("year","stratum") #must be two or more columns to keep the selection a dataframe
    #    lgths <- split(x,x$LENGTH_cm)
    #    LjhU <- unlist(lapply(lgths,function(x){sum(x$expU)}))
    #    x <- data.frame(x[rep(1,length(LjhU)),xcols],area=A.h,areaSwept=a.h,LENGTH_cm=as.numeric(names(LjhU)),LjhU=LjhU,TotalLjhU=A.h*LjhU/a.h)
    #    LjhF <- unlist(lapply(lgths,function(x){sum(x$expF)}))
    #    x <- data.frame(x,LjhF=LjhF,TotalLjhF=A.h*LjhF/a.h)
    #    LjhM <- unlist(lapply(lgths,function(x){sum(x$expM)}))
    #    x <- data.frame(x,LjhM=LjhM,TotalLjhM=A.h*LjhM/a.h)
    #    return(x)
    #}
    MeanRatio.fn <- function(x,strat,numTows,raw) {
        #function to sum lengths within a stratum and a year
        #Uses the Mean ratio estimate
        theYear <- unique(x$year)
        theStratum <- unique(x$stratum)
        if(length(theYear)!=1) stop("there is a problem in length.fn. There should be exactly one year in each list item.\n")
        if(length(theStratum)!=1) stop("there is a problem in length.fn. There should be exactly one stratum in each list item.\n")
        if(!(as.character(theYear)%in%row.names(numTows))) stop(paste("The year",theYear,"is in the lengths file but is not in the tow file"))
        ntows <- numTows[as.character(theYear),theStratum]
        A.h <- strat[strat$name==theStratum,"area"]
        xcols <- c("year","stratum","allLs") #must be two or more columns to keep the selection a dataframe
        ages <- split(x,x$allAs)
        if(raw) {
            AjhU <- unlist(lapply(ages,function(x){sum(x$numU)}))
            x <- data.frame(x[rep(1,length(AjhU)),xcols],area=A.h,AGE=as.numeric(names(AjhU)),AjhU=AjhU,TotalAjhU=AjhU)
            AjhF <- unlist(lapply(ages,function(x){sum(x$numF)}))
            x <- data.frame(x,AjhF=AjhF,TotalAjhF=AjhF)
            AjhM <- unlist(lapply(ages,function(x){sum(x$numM)})) 
            x <- data.frame(x,AjhM=AjhM,TotalAjhM=AjhM)
        } else {
            AjhU <- unlist(lapply(ages,function(x){sum(x$expU/x$areaFished)}))
            x <- data.frame(x[rep(1,length(AjhU)),xcols],area=A.h,AGE=as.numeric(names(AjhU)),AjhU=AjhU/ntows,TotalAjhU=A.h*AjhU/ntows)
            AjhF <- unlist(lapply(ages,function(x){sum(x$expF/x$areaFished)}))
            x <- data.frame(x,AjhF=AjhF/ntows,TotalAjhF=A.h*AjhF/ntows)
            AjhM <- unlist(lapply(ages,function(x){sum(x$expM/x$areaFished)})) 
            x <- data.frame(x,AjhM=AjhM/ntows,TotalAjhM=A.h*AjhM/ntows)
        }
        return(x)
    }

    if(meanRatioMethod) {
        if(raw) cat("\nUsing raw numbers of age-at-length\n\n")
        if(!raw) cat("\nUsing expanded numbers of age-at-length\n\n")
        A.year.L.str <- lapply(datB.yrLstr,function(x){lapply(x,MeanRatio.fn,strat=strat.df,numTows=numTows,raw=raw)})
    }  else{
      stop("Sorry. Only the mean Ratio Method is implemented")
      #  L.year.str <- lapply(datB.yrstr,function(x){lapply(x,lengthTotalRatio.fn,strat=strat.df)})
    }

     year.fn <- function(x,Ages) {   #calculate the age-at-length by year
        theAs <- unlist(lapply(x,function(x){x$AGE}))
        allAs <- Ages[findInterval(theAs,Ages,all.inside=T)]    #finds the interval that the age falls in (all.inside puts maximum age group into N-1 group, thus I padded with Inf.)
        Lengths <- rep(x[[1]]$allLs[1],length(Ages))
        TotalAjhU <- unlist(lapply(x,function(x){x$TotalAjhU}))   #over strata
        TotalAjhF <- unlist(lapply(x,function(x){x$TotalAjhF}))
        TotalAjhM <- unlist(lapply(x,function(x){x$TotalAjhM}))
        TotalAjU <- tapply(TotalAjhU,allAs,sum,na.rm=T)                                     #sum over strata for each age
        TotalAjF <- tapply(TotalAjhF,allAs,sum,na.rm=T)
        TotalAjM <- tapply(TotalAjhM,allAs,sum,na.rm=T)
        out <- data.frame(Age=Ages,Length=Lengths,propU=rep(NA,length(Ages)),propF=rep(NA,length(Ages)),propM=rep(NA,length(Ages)))
        row.names(out) <- out$Age
        out[names(TotalAjU),"propU"] <- 100*TotalAjU/sum(TotalAjU,na.rm=T)
        out[names(TotalAjF),"propF"] <- 100*TotalAjF/sum(TotalAjF,na.rm=T)
        out[names(TotalAjM),"propM"] <- 100*TotalAjM/sum(TotalAjM,na.rm=T)
        out <- out[-nrow(out),]   #remove last row because Inf and always NA due to inside.all=T
        return(out)
    }
    AL.year <- lapply(A.year.L.str,year.fn,Ages=Ages)
    if(!SS3out) {
        return(list(AL.year=AL.year,A.year.L.str=A.year.L.str))
    }

    #output SS3 format with gender on separate lines
    ages <- AL.year[[1]][,"Age"]
    
    #gender=1 (females only)
    #gender=2 (males only)
    #nsF <- unlist(lapply(nobs,function(x){x["nF"]})); length(nobs)
    #nsM <- unlist(lapply(nobs,function(x){x["nM"]})); length(nsM)
    AsF <- unlist(lapply(AL.year,function(x){x$propF}))
    AsF[is.na(AsF)] <- 0
    AsF <- matrix(AsF,nrow=length(AL.year),byrow=T,
          dimnames=list(NULL,paste(rep("F",length(ages)),ages,sep="")))
    AsF[,2] <- AsF[,1]+AsF[,2]     #add in all ages before the minimum age to the first age bin that we have to specify by ourselves
    numFzero <- sum(AsF[,"F0"])
    AsF <- AsF[,-match("F0",dimnames(AsF)[[2]])]        #remove F0 column

    AsM <- unlist(lapply(AL.year,function(x){x$propM}))
    AsM[is.na(AsM)] <- 0
    AsM <- matrix(AsM,nrow=length(AL.year),byrow=T,
          dimnames=list(NULL,paste(rep("M",length(ages)),ages,sep="")))
    AsM[,2] <- AsM[,1]+AsM[,2]     #add in all ages before the minimum age to the first age bin
    numMzero <- sum(AsM[,"M0"])
    AsM <- AsM[,-match("M0",dimnames(AsM)[[2]])]

    outF <- data.frame(year=as.numeric(substring(names(AL.year),1,4)),Season=season,Fleet=fleet,gender=1,partition=partition,ageErr=ageerr,
                          LbinLo=as.numeric(substring(names(AL.year),6)),LbinHi=as.numeric(substring(names(AL.year),6)),nSamps="ENTER",AsF,AsF)
    outM <- data.frame(year=as.numeric(substring(names(AL.year),1,4)),Season=season,Fleet=fleet,gender=2,partition=partition,ageErr=ageerr,
                          LbinLo=as.numeric(substring(names(AL.year),6)),LbinHi=as.numeric(substring(names(AL.year),6)),nSamps="ENTER",AsM,AsM)
    indZero <- apply(outF[,-c(1:9)],1,sum)==0
    outF <- outF[!indZero,]   #remove any rows that have no female observations (they may be there because of male obs)
    indZero <- apply(outM[,-c(1:9)],1,sum)==0
    outM <- outM[!indZero,]   #remove any rows that have no male observations (they may be there because of female obs)
    rownames(outF) <- paste("F",1:nrow(outF),sep="")
    rownames(outM) <- paste("M",1:nrow(outM),sep="")

    cat("There are",numFzero,"females age 0 to age",ages[2],"minus group that were added into the first age bin\n")
    cat("There are",numMzero,"males age 0 to age",ages[2],"minus group that were added into the first age bin\n")
    return(list(female=outF,male=outM))
}
