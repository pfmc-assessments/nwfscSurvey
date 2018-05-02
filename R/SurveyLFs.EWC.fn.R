#' Epands the lengths up to the total stratum area then sums over strata
#' Written by Allan Hicks 16 March 2009
#' modified to incorporate unsexed fish using sex ratios in May 2011
#' weighted by sample size and area
#' datL should have a column called "year" indicating year
#' femaleMale is a vector of codes for female then male (in that order)
#' lgthBin is the increment of each length bin or a vector of the actual bins
#' NOTE: The length bin called F0 or M0 is retained to show proportion of lengths smaller than smallest bin
#' You will want to likely add this to your first length bin and delete this before putting in SS3, or
#' start the lgthBins argument at the 2nd length bin and F0 will be all fish smaller (hence the first length bin)
#' SS3out: if True the output is in a format pastable into SS3 dat file
#' 
#' @param dir directory 
#' @param datL object
#' @param datTows
#' @param strat.vars
#' @param strat.df 
#' @param femaleMale
#' @param lgthBins length bins
#' @param SS3out if True the output is in a format pastable into SS3 dat file
#' @param meanRatioMethod
#' @param gender gender value for Stock Synthesis
#' @param NAs2zero change NAs to zeros
#' @param sexRatioUnsexed sex ratio to apply to any length bins of a certain size or smaller as defined by the maxSizeUnsexed
#' @param maxSizeUnsexed all sizes below this threshold will assign unsexed fish by sexRatio set equal to 0.50, fish larger than this size will have unsexed fish assigned by the calculated sex ratio in the data.
#' @param partition partition for Stock Synthesis
#' @param fleet fleet number
#' @param nSamps effective sample size for Stock Synthesis
#' @param season season
#' @param printfolder folder where the length comps will be saved
#' @param remove999 the output object by the function will have the 999 column combined with the first length bin
#'
#' @author Allan Hicks and Chantel Wetzel
#' @export 
#' @seealso \code{\link{StrataFactors.fn.fn}}

SurveyLFs.EWC.fn <- function(dir, datL, datTows, strat.vars=NULL, strat.df=NULL, femaleMale=c(2,1), lgthBins=1, SS3out=FALSE, meanRatioMethod=TRUE,
                             gender=3, NAs2zero=T, sexRatioUnsexed=NA, maxSizeUnsexed=NA, partition=0, fleet="Enter Fleet", 
                             nSamps="Enter Samps", season="Enter Season", printfolder = "forSS", remove999 = TRUE)  {

    row.names(strat.df) <- strat.df[,1]     #put in rownames to make easier to index later
    numStrata <- nrow(strat.df)
    ind <- !duplicated(datL$HAULJOIN)
    datB <- datL[ind,c("HAULJOIN","Weight","Number_fish",strat.vars,"DISTANCE_FISHED","NET_WIDTH","year")]    #individual tow data
    datB$areaFished <- datB$DISTANCE_FISHED*datB$NET_WIDTH/1000   #area swept for each tow in km2

    datL$sex2 <- rep(NA,nrow(datL))
    datL[datL$SEX==femaleMale[1],"sex2"] <- "f"
    datL[datL$SEX==femaleMale[2],"sex2"] <- "m"
    
    #set up length bins
    if(length(lgthBins)==1) {
        Lengths <- c(-999,seq(floor(min(datL$Length_cm)),ceiling(max(datL$Length_cm)),lgthBins),Inf)
    }
    else{
        Lengths <- c(-999,lgthBins,Inf)
    }
    #print(Lengths)
    datL$allLs <- Lengths[findInterval(datL$Length_cm,Lengths,all.inside=T)]
    #print(head(datL))

    
    #first create strata factors
    datB   <- data.frame(datB, stratum=StrataFactors.fn(datB,strat.vars,strat.df))        #create a new column for the stratum factor
    numTows <- table(datTows$year, StrataFactors.fn(datTows,strat.vars,strat.df))        #strata for each individual tow

    #calculate expansion factor per tow
    #for all sexes
    TdatL.tows <- as.data.frame(table(datL$HAULJOIN))
    datB <- data.frame(datB[match(as.character(TdatL.tows$Var1),as.character(datB$HAULJOIN)),],TowExpFactorU=TdatL.tows$Freq)
    datB$TowExpFactorU <- datB$Number_fish/datB$TowExpFactorU
    #for females and males only
    TdatL.tows <- as.data.frame(table(datL$HAULJOIN,datL$SEX%in%femaleMale))
    TdatL.tows <- TdatL.tows[TdatL.tows$Var2==TRUE,]
    datB <- data.frame(datB[match(as.character(TdatL.tows$Var1),as.character(datB$HAULJOIN)),],TowExpFactorMF=TdatL.tows$Freq)
    datB$TowExpFactorMF <- datB$Number_fish/datB$TowExpFactorMF
    datB$TowExpFactorMF[datB$TowExpFactorMF==Inf] <- NA
    
    #find frequency of number of lengths
    TdatL.lengths <- as.data.frame(table(datL$HAULJOIN,datL$Length_cm))  #first do all lengths, unsexed
    names(TdatL.lengths) <- c("HAULJOIN","Length_cm","numU")
    TdatL.lengths <- TdatL.lengths[TdatL.lengths$numU>0,]
    datB <- merge(datB,TdatL.lengths,by="HAULJOIN",all=T)
    #Females and males
    TdatL.lengths <- as.data.frame(table(datL$HAULJOIN,datL$Length_cm,datL$sex2))
    names(TdatL.lengths) <- c("HAULJOIN","Length_cm","SEX","num")
    TdatL.lengths <- TdatL.lengths[TdatL.lengths$num>0,]
    TdatL.lengths <- split(TdatL.lengths,TdatL.lengths$SEX)
    temp <- TdatL.lengths[["f"]][,c("HAULJOIN","Length_cm","num")]
    names(temp) <- c("HAULJOIN","Length_cm","numF")
    datB <- merge(datB,temp,by=c("HAULJOIN","Length_cm"),all=T)
    datB[is.na(datB$numF),"numF"] <- 0
    temp <- TdatL.lengths[["m"]][,c("HAULJOIN","Length_cm","num")]
    names(temp) <- c("HAULJOIN","Length_cm","numM")
    datB <- merge(datB,temp,by=c("HAULJOIN","Length_cm"),all=T)
    datB[is.na(datB$numM),"numM"] <- 0

    #now calculate the expanded lengths per tow
    datB$expU <- datB$numU*datB$TowExpFactorU
    datB$expF <- datB$numF*datB$TowExpFactorMF
    datB$expM <- datB$numM*datB$TowExpFactorMF
    
    datB$LENGTH <- as.numeric(as.character(datB$Length_cm))
    datB$allLs <- Lengths[findInterval(datB$Length_cm,Lengths,all.inside=T)]
    #print(head(datB))
   
    #incorporate unsexed fish using sex ratios
    if(length(sexRatioUnsexed)==1 & !is.na(sexRatioUnsexed)) {
        datB$sexRatio <- datB$expF/(datB$expF+datB$expM)
        datB$sexRatio[as.numeric(datB$Length_cm) <= maxSizeUnsexed] <- sexRatioUnsexed

        #in case there are any NA's, we can temporarily put in zeros for calcualtions below
        datB[is.na(datB$expF),"expF"] <- 0
        datB[is.na(datB$expM),"expM"] <- 0

        #now fill in any missing ratios with ratios of that bin from other years and strata (can probably be done more efficiently)
        noRatio <- which(is.na(datB$sexRatio))
        if(length(noRatio)>0) cat("\nThese are sex ratios that were filled in using observations from the same lengths from different strata and years\n")
        for(i in noRatio) {
            inds <- datB$allLs==datB$allLs[i]
            tmpF <- sum(datB$expF[inds])
            tmpM <- sum(datB$expM[inds])
            datB$sexRatio[i] <- tmpF/(tmpF+tmpM)
            print(datB[i,c("Length_cm","allLs","expF","expM","sexRatio")])
        }

        noRatio <- which(is.na(datB$sexRatio))
        if(length(noRatio)>0) cat("\nThese are sex ratios that were filled in using observations from nearby lengths\n")
        for(i in noRatio) {
            nearLens <- Lengths[c(which(Lengths==datB$allLs[i])-1,which(Lengths==datB$allLs[i])+1)]
            inds <- datB$allLs %in% nearLens
            tmpF <- sum(datB$expF[inds])
            tmpM <- sum(datB$expM[inds])
            datB$sexRatio[i] <- tmpF/(tmpF+tmpM)
            print(datB[i,c("Length_cm","allLs","expF","expM","sexRatio")])
        }
        noRatio <- which(is.na(datB$sexRatio))
        if(length(noRatio)>0) cat("Some sex ratios were left unknown and omitted\n\n")
        if(length(noRatio)==0) cat("Done filling in sex ratios\n\n")

        #expanded values does not need to be a whole number.
        #allow it to be decimal in case there is odd number of fish then they are nt always piled up in females
        datB$expF <- datB$expF + datB$sexRatio*datB$expU
        datB$expM <- datB$expM + (1-datB$sexRatio)*datB$expU

        # tmpFemUnsex <- round(datB$sexRatio*(datB$expU-datB$expF-datB$expM))
        # tmpMaleUnsex <- datB$expU - tmpFemUnsex
        # datB$expF <- datB$expF + tmpFemUnsex
        # datB$expM <- datB$expM + tmpMaleUnsex
        #print(unique(round(datB$sexRatio,1)))
    }
    
    #sum over strata within year
    datB.yrstr <- split(datB,as.character(datB$year))
    datB.yrstr <- lapply(datB.yrstr,function(x){split(x,as.character(x$stratum))})
    #names(datB.yrstr)

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
        noDups <- !duplicated(x$LENGTH_cm)  #not sure what this is doing
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
        x$LENGTH_cm <- as.numeric(as.character(x$Length_cm))
        noDups <- !duplicated(x$LENGTH) #not sure what this is doing
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
        L.year.str <- lapply(datB.yrstr, function(x){lapply(x,lengthMeanRatio.fn,strat=strat.df,numTows)})
    }
    else{
        L.year.str <- lapply(datB.yrstr, function(x){lapply(x,lengthTotalRatio.fn,strat=strat.df)})
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
        out <- data.frame(year=as.numeric(names(L.year)),season=season,fleet=fleet,gender=rep(0,length(L.year)),
            partition=partition,Nsamp=nSamps,Ls)    
    }
    if(gender==3) {
        #females then males
        lgths <- as.character(L.year[[1]]$Length)
        Ls <- unlist(lapply(L.year,function(x){c(x$TotalLjF,x$TotalLjM)}))
        if(NAs2zero){Ls[is.na(Ls)] <- 0}
        Ls <- matrix(Ls,nrow=length(L.year),byrow=T,
            dimnames=list(NULL,paste(c(rep("F",length(lgths)),rep("M",length(lgths))),lgths,sep="")))
        out <- data.frame(year=as.numeric(names(L.year)),season=season,fleet=fleet,gender=rep(3,length(L.year)),
            partition=partition,Nsamp=nSamps,Ls)
    }

    # save output as a csv
    plotdir <- file.path(dir, printfolder)
    plotdir.isdir <- file.info(plotdir)$isdir
    if(is.na(plotdir.isdir) | !plotdir.isdir){
      dir.create(plotdir)
    }
    write.csv(out, file = paste0(plotdir, "/Survey_Gender", gender, "_Bins_-999_", max(lgthBins),"_LengthComps.csv"), row.names = FALSE)

    usableOut = out
    if (gender == 3){
        usableOut[,paste0("F",min(lgthBins))] <- usableOut[,paste0("F",min(lgthBins))] + usableOut$F.999
        usableOut[,paste0("M",min(lgthBins))] <- usableOut[,paste0("M",min(lgthBins))] + usableOut$M.999
        usableOut <- usableOut[,-which(names(usableOut)%in%c("F.999","M.999"))]
        write.csv(usableOut, file = paste0(plotdir, "/Survey_Gender", gender, "_Bins_",min(lgthBins),"_", max(lgthBins),"_LengthComps.csv"), row.names = FALSE)
    }

    if (gender == 0){
        usableOut[,paste0("U",min(lgthBins))]  <- usableOut[,paste0("U",min(lgthBins))] + usableOut$U.999
        usableOut[,paste0("U",min(lgthBins)), ".1"]  <- usableOut[,paste0("U",min(lgthBins)), ".1"] + usableOut$U.999.1
        usableOut <- usableOut[,-which(names(usableOut)%in%c("U.999","U.999.1"))]
        write.csv(usableOut, file = paste0(plotdir, "/Survey_Gender", gender, "_Bins_",min(lgthBins),"_", max(lgthBins),"_LengthComps.csv"), row.names = FALSE)
    }

    #cat("\nNOTE: You may need to add the column called F.999 and/or M.999 to your first length bin\n\tand delete that column.\n\tThese are the percentage of lengths smaller than the first length bin\n\n")
    cat("\nNOTE: Two files have been saved the the printfolder directory.\n\tThe first has the 999 column showing fish smaller than the initial length bind. \n\tCheck to make sure there is not a large number of fish smaller than the initial length bin.\n\tThe second file has combined the 999 with the first length bin and is ready for use in SS.\n\n")
    if (!remove999) { return(out)}
    if (remove999)  { return(usableOut)}
}
