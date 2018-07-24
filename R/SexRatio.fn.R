#' Sex ratio function that assigns a sex to unsexed fish when creatin length or age comps
#' 

#' @param datB the read in length comps by the ReadInLengths.EWC.fn function
#' @param sexRatioUnsexed sex ratio to apply to any length bins of a certain size or smaller as defined by the maxSizeUnsexed
#' @param maxSizeUnsexed all sizes below this threshold will assign unsexed fish by sexRatio set equal to 0.50, fish larger than this size will have unsexed fish assigned by the calculated sex ratio in the data.
#'
#' @author Allan Hicks and Chantel Wetzel
#' @export 


SexRatio.fn <- function(datB, sexRatioUnsexed, maxSizeUnsexed){
    #incorporate unsexed fish using sex ratios
    if(length(sexRatioUnsexed)==1 & !is.na(sexRatioUnsexed)) {
        datB$sexRatio <- datB$expF/(datB$expF+datB$expM)
        # The below line was changed to as.character from as.numeric because it was not finding the correct lengths : CRW
        datB$sexRatio[datB$Length_cm <= maxSizeUnsexed] <- sexRatioUnsexed

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

        # These lines change to add the actual unsexed fish to the expansion factors -CRW
        datB$expF <- datB$expF + datB$sexRatio*datB$expU
        datB$expM <- datB$expM + (1-datB$sexRatio)*datB$expU
    }

    return(datB)
}