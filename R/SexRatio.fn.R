#' Sex ratio function that assigns a sex to unsexed fish when creatin length or age comps
#' 

#' @param x the data frame being passed to the function from either the SurveyLFs.fn or th SurveyAFs.fn
#' @param sexRatioStage the stage of the expansion to apply the sex ratio. Input either 1 or 2. 
#' @param sexRatioUnsexed sex ratio to apply to any length bins of a certain size or smaller as defined by the maxSizeUnsexed
#' @param maxSizeUnsexed all sizes below this threshold will assign unsexed fish by sexRatio set equal to 0.50, fish larger than this size will have unsexed fish assigned by the calculated sex ratio in the data.
#'
#' @author Allan Hicks and Chantel Wetzel
#' @export 


SexRatio.fn <- function(x, sexRatioStage, sexRatioUnsexed, maxSizeUnsexed){

    if (sexRatioStage == 1){
        cat("Sex ratio for unsexed fish being applied to the expanded numbers within a tow (stage 1) when possible. 
        If no data within a tow for bin then the sex ratio for the bin across all years applied to unsexed fish.
        If no data for that bin across all years then the sex ratio for nearby bins was applied to unsexed fish.\n")
        #incorporate unsexed fish using sex ratios
        if(length(sexRatioUnsexed)==1 & !is.na(sexRatioUnsexed)) {
            x$sexRatio <- x$expF/(x$expF+x$expM)
            # The below line was changed to as.character from as.numeric because it was not finding the correct lengths : CRW
            x$sexRatio[x$Length_cm <= maxSizeUnsexed] <- sexRatioUnsexed
    
            #in case there are any NA's, we can temporarily put in zeros for calcualtions below
            x[is.na(x$expF),"expF"] <- 0
            x[is.na(x$expM),"expM"] <- 0
    
            #now fill in any missing ratios with ratios of that bin from other years and strata (can probably be done more efficiently)
            noRatio <- which(is.na(x$sexRatio))
            if(length(noRatio)>0) cat("\nThese are sex ratios that were filled in using observations from the same lengths from different strata and years\n")
            for(i in noRatio) {
                inds <- x$allLs == x$allLs[i]
                tmpF <- sum(x$expF[inds])
                tmpM <- sum(x$expM[inds])
                x$sexRatio[i] <- tmpF/(tmpF+tmpM)
                print(x[i,c("Length_cm","allLs","expF","expM","sexRatio")])
            }
    
            noRatio <- which(is.na(x$sexRatio))
            if(length(noRatio)>0) cat("\nThese are sex ratios that were filled in using observations from nearby lengths\n")
            for(i in noRatio) {
                nearLens <- Lengths[c(which(Lengths==x$allLs[i])-1,which(Lengths==x$allLs[i])+1)]
                inds <- x$allLs %in% nearLens
                tmpF <- sum(x$expF[inds])
                tmpM <- sum(x$expM[inds])
                x$sexRatio[i] <- tmpF/(tmpF+tmpM)
                print(x[i,c("Length_cm","allLs","expF","expM","sexRatio")])
            }
            noRatio <- which(is.na(x$sexRatio))
            if(length(noRatio)>0) cat("Some sex ratios were left unknown and omitted\n\n")
            if(length(noRatio)==0) cat("Done filling in sex ratios\n\n")
    
            # These lines change to add the actual unsexed fish to the expansion factors -CRW
            x$expF <- x$expF + x$sexRatio*x$expU
            x$expM <- x$expM + (1-x$sexRatio)*x$expU
        }
    }

    if (sexRatioStage == 2){
        
        if(length(sexRatioUnsexed)==1 & !is.na(sexRatioUnsexed)) {
           sexRatio = x$TotalLjhF / (x$TotalLjhF + x$TotalLjhM)
           sexRatio[x$LENGTH <= maxSizeUnsexed] <- sexRatioUnsexed
        
           #now fill in any missing ratios with ratios of that bin from other years and strata (can probably be done more efficiently)
           noRatio <- which(is.na(sexRatio))
           if(length(noRatio)>0) {
               cat("\nThese are sex ratios that were filled in using observations from the similar lengths from the same strata and year.\n")
               for(i in noRatio) {
                   lower <- sexRatio[i-1]
                   upper <- sexRatio[i+1]
                   sexRatio[i] <- mean(lower + upper)
                   print(x$LENGTH[i], sexRatio[i])
               }
           }
           x$TotalLjhF    <- x$TotalLjhF + x$TotalLjhU * sexRatio
           x$TotalLjhM    <- x$TotalLjhM + x$TotalLjhU * (1 - sexRatio)
           x$TotalLjhU    <- round(x$TotalLjhU - x$TotalLjhU * sexRatio - x$TotalLjhU * (1 - sexRatio), 0)             
        }
    }

   return(x)   
}