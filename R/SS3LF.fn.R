#' Calculates proportion at length and reformats into SS# format
#' Gender=0: sexes combined and entered in female placeholders. Male values ignored
#' Gender=1: females only. Male values ignored
#' Gender=2: males only. Female values ignored.
#' Gender=3: both sexes. Proportions over males and females sum to 1. See below for how unsexed are treated
#' lgthBins is either the interval between length bins or the actual length bins
#' note that 0 and Inf are tacked on the ends to account for lengths outside the interval
#' The largest length bin includes all lengths greater
#' NOTE: The length bin called F0 or M0 is retained to show proportion of lengths smaller than smallest bin
#' You will want to likely add this to your first length bin and delete this before putting in SS3, or
#' start the lgthBins argument at the 2nd length bin and F0 will be all fish smaller (hence the first length bin)
#' The NAs2zero determines if NA values will be changed to 0.0.
#' You will want this to be T when inputting into SS3, but may want it to be false when plotting
#' Note that an NA means there were no males and females recorded. Therefore you may get a zero if only one sex was recorded
#' Sex Ratio for unsexed
#' sexRatioUnsexed is the proportion used to assign unsexed fish to females
#'   -- if a single number, it is only used for sizes at or below maxSizeUnsexed, and sex ratio for size bins above that are calculated using the 
#'       number of males and females observed in the same size class (or one lower if none available in the same bin)
#'   -- if a vector (maybe to be implemented), it must be the same length as the lgthBins and indicates the proportion of unsexed assigned to females for each length bin
#'   -- if it is NA, unsexed fish are omitted WHEN GENDER=3 (THIS IS THE DEFAULT)
#' maxSizeUnsexed determines the maximum size at which the sexRatioUnsexed is applied. If sexRatioUnsexed is a vector, this is ignored
#' 
#' @param len object
#' @param lgthBins Either the interval between length bins or the actual length bins (e.g., lgthBins = 11:47)
#' @param gender gender code value for Stock Synthesis (0 = sexes combined, 1 = females only, 2 = males only, 3 = both sexes, females then males). Selecting gender 0 will give the user all fish combined males + females + unsexed.
#' @param nSamps effN
#' @param fleet Fleet number 
#' @param season Season number
#' @param partition partition as defined by Stock Synthesis (0 = whole catch in weight, 1 = discards in weight, 2 = retained catch in weight). Survey should have a value of 0.
#' @param NAs2zero change NA value to 0 in the ouput length comps to use in SS
#' @param sexRatioUnsexed sex ratio to apply to any length bins of a certain size or smaller as defined by the maxSizeUnsexed
#' @param maxSizeUnsexed all sizes below this threshold will assign unsexed fish by sexRatio set equal to 0.50, fish larger than this size will have unsexed fish assigned by the calculated sex ratio in the data.
#' @param printfolder folder where the length comps will be saved
#' @param remove999 the output object by the function will have the 999 column combined with the first length bin
#'
#' @author Allan Hicks and Chantel Wetzel
#' @export 

SS3LF.fn <-function(len, lgthBins=1, gender=3, nSamps="EnterNsamps", fleet="EnterFleet", season=1, partition=0,
                    NAs2zero=T, sexRatioUnsexed=NA, maxSizeUnsexed=NA,printfolder = "forSS", remove999 = TRUE) {
    
    if(length(lgthBins)==1) {
        Lengths <- c(-999,seq(floor(min(len$Length)),ceiling(max(len$Length)),lgthBins),Inf)
    }
    else{
        Lengths <- c(-999,lgthBins,Inf) #put 0 and Inf on ends because all.inside=T in findInterval below. Treats these as minus and plus groups
    }

    # Assign length bin to each observed length
    len$allLs <- Lengths[findInterval(x = len$Length,vec = Lengths, all.inside=T)]
    
    if(length(sexRatioUnsexed)==1 & !is.na(sexRatioUnsexed)) {
        len$sexRatio <- len$NumF/(len$NumF+len$NumM)
        len$sexRatio[len$Length <= maxSizeUnsexed] <- sexRatioUnsexed
        #now fill in any missing ratios with ratios of that bin from other years and strata (can probably be done more efficiently)
        noRatio <- which(is.na(len$sexRatio))
        if(length(noRatio)>0) cat("\nThese are sex ratios that were filled in using observations from the same lengths from different strata and years\n")
        for(i in noRatio) {
            inds <- len$allLs==len$allLs[i]
            tmpF <- sum(len$NumF[inds])
            tmpM <- sum(len$NumM[inds])
            len$sexRatio[i] <- tmpF/(tmpF+tmpM)
            print(len[i,c("Length","allLs","NumF","NumM","sexRatio")])
        }

        noRatio <- which(is.na(len$sexRatio))
        if(length(noRatio)>0) cat("\nThese are sex ratios that were filled in using observations from nearby lengths\n")
        for(i in noRatio) {
            nearLens <- Lengths[c(which(Lengths==len$allLs[i])-1,which(Lengths==len$allLs[i])+1)]
            inds <- len$allLs %in% nearLens
            tmpF <- sum(len$NumF[inds])
            tmpM <- sum(len$NumM[inds])
            len$sexRatio[i] <- tmpF/(tmpF+tmpM)
            print(len[i,c("Length","allLs","NumF","NumM","sexRatio")])
        }
        noRatio <- which(is.na(len$sexRatio))
        if(length(noRatio)>0) cat("Some sex ratios were left unknown and omitted\n\n")
        if(length(noRatio)==0) cat("Done filling in sex ratios\n\n")

        tmpFemUnsex <- round(len$sexRatio*len$NumUnsexed)
        tmpMaleUnsex <- len$NumUnsexed - tmpFemUnsex
        len$NumF <- len$NumF + tmpFemUnsex
        len$NumM <- len$NumM + tmpMaleUnsex
        #print(unique(round(len$sexRatio,1)))
    }
    #if(length(sexRatioUnsexed) > 1)
    #    if(length(sexRatioUnsexed)!=(length(Lengths)-2)) stop("sexRatioUnsexed must be a single number or the same length as the length bins")   
    #}

    xx <- split(len[,c("Length","NumF","NumM","NumUnsexed")],len$Year)
    years <- names(xx)
    
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
        out[names(totalU),"propU"] <- 100*(totalF+totalM)/sum(totalF+totalM,na.rm=T)            #unsexed have been added in above
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
        out <- data.frame(year=as.numeric(names(L.year)),Season=season,Fleet=fleet,gender=gender,partition=partition,nSamps=nSamps,Ls,Ls)
    }
    if(gender==3) {
        Ls <- unlist(lapply(L.year,function(x){c(x$propF,x$propM)}))
        if(NAs2zero){Ls[is.na(Ls)] <- 0}
        Ls <- matrix(Ls,nrow=length(L.year),byrow=T,
            dimnames=list(NULL,paste(c(rep("F",length(lgths)),rep("M",length(lgths))),lgths,sep="")))
        out <- data.frame(year=as.numeric(names(L.year)),Season=season,Fleet=fleet,gender=gender,partition=partition,nSamps=nSamps,Ls)
    }

    # save output as a csv
    plotdir <- file.path(dir, printfolder)
    plotdir.isdir <- file.info(plotdir)$isdir
    if(is.na(plotdir.isdir) | !plotdir.isdir){
      dir.create(plotdir)
    }
    write.csv(out, file = paste0(plotdir, "/NWFSCBT_Survey_Gender", gender, "_Bins_-999_", max(lgthBins),"_LengthComps.csv"), row.names = FALSE)

    usableOut = out
    if (gender == 3){
        usableOut$F11 <- usableOut$F11 + usableOut$F.999
        usableOut$M11 <- usableOut$M11 + usableOut$M.999
        usableOut <- usableOut[,-which(names(usableOut)%in%c("F.999","M.999"))]
        write.csv(usableOut, file = paste0(plotdir, "/NWFSCBT_Survey_Gender", gender, "_Bins_",min(lgthBins),"_", max(lgthBins),"_LengthComps.csv"), row.names = FALSE)
    }

    if (gender == 0){
        usableOut$U11   <- usableOut$U11 + usableOut$U.999
        usableOut$U11.1 <- usableOut$U11.1 + usableOut$U.999.1
        usableOut <- usableOut[,-which(names(usableOut)%in%c("U.999","U.999.1"))]
        write.csv(usableOut, file = paste0(plotdir, "/NWFSCBT_Survey_Gender", gender, "_Bins_",min(lgthBins),"_", max(lgthBins),"_LengthComps.csv"), row.names = FALSE)
    }

    #cat("\nNOTE: You may need to add the column called F.999 and/or M.999 to your first length bin\n\tand delete that column.\n\tThese are the percentage of lengths smaller than the first length bin\n\n")
    cat("\nNOTE: Two files have been saved the the printfolder directory.\n\tThe first has the 999 column showing fish smaller than the initial length bind. \n\tCheck to make sure there is not a large number of fish smaller than the initial length bin.\n\tThe second file has combined the 999 with the first length bin and is ready for use in SS.\n\n")
    if (!remove999) { return(out)}
    if (remove999)  { return(usableOut)}
}
