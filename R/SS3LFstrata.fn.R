SS3LFstrata.fn <-
function(len,Strata="Year",lgthBins=1,gender=3,nSamps="EnterNsamps",fleet="EnterFleet",season=1,partition=0,NAs2zero=T) {
    #calculates proportion at length and reformats into SS# format
    #Gender=0: sexes combined and entered in female placeholders. Male values ignored
    #Gender=1: females only. Male values ignored
    #Gender=2: males only. Female values ignored.
    #Gender=3: both sexes. Proportions over males and females sum to 1.
    #lgthBins is either the interval between length bins or the actual length bins
    #note that 0 and Inf are tacked on the ends to account for lengths outside the interval
    #The largest length bin includes all lengths greater
    #NOTE: The length bin called F0 or M0 is retained to show proportion of lengths smaller than smallest bin
    #      You will want to delete this before putting in SS3
    #The NAs2zero determines if NA values will be changed to 0.0.
    #     You will want this to be T when inputting into SS3, but may want it to be false when plotting
    #     Note that an NA means there were no males and females recorded. Therefore you may get a zero if only one sex was recorded
    # NOTICE: UNSEXED CORRECTION NOT IMPLEMENTED
    xx <- split(len[,c("Length","NumF","NumM","NumUnsexed")],len[,Strata])
    years <- names(xx)
    if(length(lgthBins)==1) {
        Lengths <- c(-999,seq(floor(min(len$Length)),ceiling(max(len$Length)),lgthBins),Inf)
    }
    else{
        Lengths <- c(-999,lgthBins,Inf)        #put 0 and Inf on ends because all.inside=T in findInterval below. Treats these as minus and plus groups
    }

    year.fn <- function(x,Lengths) {
        allLs <- Lengths[findInterval(x$Length,Lengths,all.inside=T)]    #finds the interval that the length falls in and floors it (so 23.2 would be in 23 if 23 was a level in Lengths, all.inside puts maximum age group into N-1 group, thus I padded with Inf.)
        totalU <- tapply(x$NumUnsexed,allLs,sum,na.rm=T)
        totalF <- tapply(x$NumF,allLs,sum,na.rm=T)
        totalM <- tapply(x$NumM,allLs,sum,na.rm=T)
        out <- data.frame(Length=Lengths,numF=rep(NA,length(Lengths)),numM=rep(NA,length(Lengths)),numU=rep(NA,length(Lengths)))
        row.names(out) <- out$Length
        out[names(totalF),"numF"] <- totalF
        out[names(totalM),"numM"] <- totalM
        out[names(totalU),"numU"] <- totalU
        out[names(totalF),"propF"] <- 100*totalF/(sum(totalF,na.rm=T)+sum(totalM,na.rm=T))
        out[names(totalM),"propM"] <- 100*totalM/(sum(totalF,na.rm=T)+sum(totalM,na.rm=T))
        out[names(totalU),"propU"] <- 100*(totalU+totalF+totalM)/sum(totalU+totalF+totalM,na.rm=T)
        out <- out[-nrow(out),]   #remove last row because Inf is always NA due to inside.all=T
        return(out)
    }
    L.year <- lapply(xx,year.fn,Lengths=Lengths)

    #output SS3 format specific to the gender choice
    lgths <- as.character(L.year[[1]]$Length)
    if(gender==0) {
        Ls <- unlist(lapply(L.year,function(x){x$propU}))
        if(NAs2zero){Ls[is.na(Ls)] <- 0}
        Ls <- matrix(Ls,nrow=length(L.year),byrow=T,
            dimnames=list(NULL,paste(rep("U",length(lgths)),lgths,sep="")))
        out <- data.frame(year=names(L.year),Season=season,Fleet=fleet,gender=gender,partition=partition,nSamps=nSamps,Ls,Ls)
    }
    if(gender==3) {
        Ls <- unlist(lapply(L.year,function(x){c(x$propF,x$propM)}))
        if(NAs2zero){Ls[is.na(Ls)] <- 0}
        Ls <- matrix(Ls,nrow=length(L.year),byrow=T,
            dimnames=list(NULL,paste(c(rep("F",length(lgths)),rep("M",length(lgths))),lgths,sep="")))
        out <- data.frame(year=names(L.year),Season=season,Fleet=fleet,gender=gender,partition=partition,nSamps=nSamps,Ls)
    }

    cat("\nNOTE: You may need to delete a column called F.999 and/or M.999. These are the percentage of lengths smaller than the first length bin\n\n")
    return(out)
}
#tmp <- SS3LFstrata.fn(PetLen,lgthBins=2,gender=3)  #same as SS3LF.fn
#tmpLen <- data.frame(PetLen,yearArea=paste(PetLen$Year,PetLen$AreaName))   #create year:Area strata
#tmp <- SS3LFstrata.fn(tmpLen,Strata="yearArea",lgthBins=2,gender=3)
