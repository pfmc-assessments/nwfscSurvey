#' Calculates proportion of age at length and reformats into SS3 format
#' Uses raw numbers at length, assuming that is a random sample conditioned on length and sex.
#' To use expanded numbers, set raw=F
#' Only gender codes 1 and 2 and puts males and females on separate lines because the age@L is conditioned on sex (a sample of females of length 25cm, for example)
#' Gender=1: females only. Male values ignored
#' Gender=2: males only. Female values ignored.
#' lgthBins is either the interval between length bins or the actual length bins
#' note that 0 and Inf are tacked on the ends to account for lengths and ages outside the interval. You may want to add these in to first and last bin.
#' 
#' @param age object
#' @param lgthBins Either the interval between length bins or the actual length bins (e.g., lgthBins = 11:47)
#' @param ageBins Either the interval between age bins or the actual age bins (e.g, ageBins = 1:40)
#' @param fleet Fleet number 
#' @param season Season number
#' @param partition partition as defined by Stock Synthesis
#' @param ageerr Age error value for Stock Synthesis
#' @param raw raw=T/F, input to define whether or not to expand numbers in the csv file (column header "NumF" and "NumM")
#' @param sexRatioUnsexed
#' @param maxSizeUnsexed
#'
#' @author Allan Hicks 
#' @export 

SS3AgeAtLen.fn <-function(ages,lgthBins=1,ageBins=1,fleet="EnterFleet",season=1,partition=0,
                          ageerr="EnterAgeErr",raw=T,sexRatioUnsexed=NA,maxSizeUnsexed=NA) {

    years <- sort(unique(ages$Year))
    if(length(lgthBins)==1) {
        Lengths <- c(-999,seq(floor(min(ages$Length)),ceiling(max(ages$Length)),lgthBins),Inf)
    }
    else{
        Lengths <- c(-999,lgthBins,Inf)        #put 0 and Inf on ends because all.inside=T in findInterval below. Treats these as minus and plus groups
    }
    if(length(ageBins)==1) {
        Ages <- c(-999,seq(floor(min(ages$Age)),ceiling(max(ages$Age)),ageBins),Inf)
    }
    else{
        Ages <- c(-999,ageBins,Inf)        #put 0 and Inf on ends because all.inside=T in findInterval below. Treats these as minus and plus groups
    }
    allLs <- Lengths[findInterval(ages$Length,Lengths,all.inside=T)]   #finds the interval that the length falls in and floors it (so 23.2 would be in 23 if 23 was a level in Lengths, all.inside puts maximum age group into N-1 group, thus I padded with Inf.)

    if(length(sexRatioUnsexed)==1 & !is.na(sexRatioUnsexed)) {
        ages$allLs <- allLs
        ages$sexRatio <- ages$NumF/(ages$NumF+ages$NumM)
        ages$sexRatio[ages$Length <= maxSizeUnsexed] <- sexRatioUnsexed
        #now fill in any missing ratios with ratios of that bin from other years and strata (can probably be done more efficiently)
        noRatio <- which(is.na(ages$sexRatio))
        if(length(noRatio)>0) cat("\nThese are sex ratios that were filled in using observations from the same lengths from different strata and years\n")
        for(i in noRatio) {
            inds <- allLs==allLs[i]
            tmpF <- sum(ages$NumF[inds])
            tmpM <- sum(ages$NumM[inds])
            ages$sexRatio[i] <- tmpF/(tmpF+tmpM)
            print(ages[i,c("Length","allLs","Age","NumF","NumM","sexRatio")])
        }

        noRatio <- which(is.na(ages$sexRatio))
        if(length(noRatio)>0) cat("\nThese are sex ratios that were filled in using observations from nearby lengths\n")
        for(i in noRatio) {
            nearLens <- Lengths[c(which(Lengths==allLs[i])-1,which(Lengths==allLs[i])+1)]
            inds <- ages$allLs %in% nearLens
            tmpF <- sum(ages$NumF[inds])
            tmpM <- sum(ages$NumM[inds])
            ages$sexRatio[i] <- tmpF/(tmpF+tmpM)
            print(ages[i,c("Length","allLs","Age","NumF","NumM","sexRatio")])
        }
        noRatio <- which(is.na(ages$sexRatio))
        if(length(noRatio)>0) cat("Some sex ratios were left unknown and omitted\n\n")
        if(length(noRatio)==0) cat("Done filling in sex ratios\n\n")

        tmpFemUnsex <- round(ages$sexRatio*ages$NumUnsexed)
        tmpMaleUnsex <- ages$NumUnsexed - tmpFemUnsex
        ages$NumF <- ages$NumF + tmpFemUnsex
        ages$NumM <- ages$NumM + tmpMaleUnsex

        tmpFemUnsexTally <- ages$sexRatio*ages$AgeTallyU #don't use round since these are whole numbers that are typically small (i.e., 1)
        tmpMaleUnsexTally <- ages$AgeTallyU - tmpFemUnsexTally
        ages$AgeTallyF <- ages$AgeTallyF + tmpFemUnsexTally
        ages$AgeTallyM <- ages$AgeTallyM + tmpMaleUnsexTally

        #print(unique(round(ages$sexRatio,1)))
    }


    if(raw){xx <- split(ages[,c("Year","Length","Age","AgeTallyF","AgeTallyM")],paste(ages$Year,allLs))}
    if(!raw){
        ages[,c("AgeTallyF","AgeTallyM")] <- ages[,c("NumF","NumM")]  #use the expanded numbers
        xx <- split(ages[,c("Year","Length","Age","AgeTallyF","AgeTallyM")],paste(ages$Year,allLs))
    }

    bin.fn <- function(x,Ages) {
        allAs <- Ages[findInterval(x$Age,Ages,all.inside=T)]
        totalF <- tapply(x$AgeTallyF,allAs,sum,na.rm=T)
        totalM <- tapply(x$AgeTallyM,allAs,sum,na.rm=T)
        out <- data.frame(Age=Ages,numF=rep(NA,length(Ages)),numM=rep(NA,length(Ages)))
        row.names(out) <- out$Age
        out[names(totalF),"numF"] <- totalF
        out[names(totalM),"numM"] <- totalM
        out[names(totalF),"propF"] <- 100*totalF/sum(totalF,na.rm=T)
        out[names(totalM),"propM"] <- 100*totalM/sum(totalM,na.rm=T)
        out <- out[-nrow(out),]   #remove last row because Inf and always NA due to inside.all=T (but needed in findInterval)
        return(out)
    }
    A.bin <- lapply(xx,bin.fn,Ages=Ages)
    
    Nobs.fn <- function(x) {
        nF <- sum(x$AgeTallyF,na.rm=T)
        nM <- sum(x$AgeTallyM,na.rm=T)
        out <- c(nF,nM)
        names(out) <- c("nF","nM")
        return(out)
    }
    nobs <- lapply(xx,Nobs.fn)

    #output SS3 format with gender on separate lines
    ages <- as.character(A.bin[[1]]$Age)
    
    #gender=1 (females only, males ignored)
    nsF <- unlist(lapply(nobs,function(x){x["nF"]}))
    nsM <- unlist(lapply(nobs,function(x){x["nM"]}))
    AsF <- unlist(lapply(A.bin,function(x){x$propF}))
    AsM <- unlist(lapply(A.bin,function(x){x$propM}))
    AsF[is.na(AsF)] <- 0
    AsM[is.na(AsM)] <- 0
    AsF <- matrix(AsF,nrow=length(A.bin),byrow=T,
          dimnames=list(NULL,paste(rep("F",length(ages)),ages,sep="")))
    AsF[,2] <- AsF[,1]+AsF[,2]     #add in all ages before the minimum age to the first age bin
    numFminus <- sum(AsF[,1])
    AsF <- AsF[,-1]        #remove minus group column
    AsM <- matrix(AsM,nrow=length(A.bin),byrow=T,
          dimnames=list(NULL,paste(rep("M",length(ages)),ages,sep="")))
    AsM[,2] <- AsM[,1]+AsM[,2]     #add in all ages before the minimum age to the first age bin
    numMminus <- sum(AsM[,1])
    AsM <- AsM[,-1]

    outF <- data.frame(year=as.numeric(substring(names(A.bin),1,4)),Season=season,Fleet=fleet,gender=1,partition=partition,ageErr=ageerr,
                          LbinLo=as.numeric(substring(names(A.bin),6)),LbinHi=as.numeric(substring(names(A.bin),6)),nSamps=nsF,AsF,AsF)
    outM <- data.frame(year=as.numeric(substring(names(A.bin),1,4)),Season=season,Fleet=fleet,gender=2,partition=partition,ageErr=ageerr,
                          LbinLo=as.numeric(substring(names(A.bin),6)),LbinHi=as.numeric(substring(names(A.bin),6)),nSamps=nsM,AsM,AsM)
    indZero <- apply(outF[,-c(1:9)],1,sum)==0
    outF <- outF[!indZero,]   #remove any rows that have no female observations (they may be there because of male obs)
    indZero <- apply(outM[,-c(1:9)],1,sum)==0
    outM <- outM[!indZero,]   #remove any rows that have no male observations (they may be there because of female obs)
    rownames(outF) <- paste("F",1:nrow(outF),sep="")
    rownames(outM) <- paste("M",1:nrow(outM),sep="")

    cat("There are",numFminus,"females less than age",ages[2],"that were added into the first age bin\n")
    cat("There are",numMminus,"males less than age",ages[2],"that were added into the first age bin\n")
    cat("The number of fish in each sample were input into the nSamps column\nUse Beth's Excel file for the number of tows\n")
    return(list(female=outF,male=outM))
}
