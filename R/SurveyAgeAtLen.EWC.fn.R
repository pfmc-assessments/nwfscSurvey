    #calculates proportion of age at length and reformats into SS3 format
    #Uses raw numbers at length, assuming that is a random sample conditioned on length and sex.
    #To use expanded numbers (up to strata areas), set raw=F
    #Only gender codes 1 and 2 and puts males and females on separate lines because the age@L is conditioned on sex (a sample of females of length 25cm, for example)
    #Gender=1: females only. Male values ignored
    #Gender=2: males only. Female values ignored.
    #lgthBins is either the interval between length bins or the actual length bins
    #note that 0 and Inf are tacked on the ends to account for lengths and ages outside the interval. You may want to add these in to first and last bin.
    #I assume all fish are sexed for age data, thus do not apply sex ratios for unsexed fish

SurveyAgeAtLen.EWC.fn <- function(datAL,datTows,strat.vars=NULL,strat.df=NULL,femaleMale=c(2,1),lgthBins=1,ageBins=1,SS3out=F,meanRatioMethod=T,raw=T,NAs2zero=T,season="ENTER",fleet="ENTER",partition="ETNER",ageerr="ENTER",returnSamps=F)  {
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
        Lengths <- c(-999,seq(floor(min(datAL$LENGTH)),ceiling(max(datAL$LENGTH)),lgthBins),Inf)
    }else{
        Lengths <- c(-999,lgthBins,Inf)
    }
    #print(Lengths)
    if(length(ageBins)==1) {
        Ages <- c(-999,seq(floor(min(datAL$AGE)),ceiling(max(datAL$AGE)),ageBins),Inf)
    } else{
        Ages <- c(-999,ageBins,Inf)        #put -999 and Inf on ends because all.inside=T in findInterval below. Treats these as minus and plus groups
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
    AL.year <- list()
    for(i in 1:length(A.year.L.str)) AL.year[[i]] = year.fn(A.year.L.str[[i]],Ages=Ages)
    names(AL.year) = names(A.year.L.str)
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
    numFzero <- sum(AsF[,"F-999"])
    AsF <- AsF[,-match("F-999",dimnames(AsF)[[2]])]        #remove F0 column

    AsM <- unlist(lapply(AL.year,function(x){x$propM}))
    AsM[is.na(AsM)] <- 0
    AsM <- matrix(AsM,nrow=length(AL.year),byrow=T,
          dimnames=list(NULL,paste(rep("M",length(ages)),ages,sep="")))
    AsM[,2] <- AsM[,1]+AsM[,2]     #add in all ages before the minimum age to the first age bin
    numMzero <- sum(AsM[,"M-999"])
    AsM <- AsM[,-match("M-999",dimnames(AsM)[[2]])]

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
