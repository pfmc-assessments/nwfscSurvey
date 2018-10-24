#' Sex ratio function that assigns a sex to unsexed fish when creatin length or age comps
#' 
#' @param x the data frame being passed to the function from either the SurveyLFs.fn or th SurveyAFs.fn
#' @param sexRatioStage the stage of the expansion to apply the sex ratio. Input either 1 or 2. 
#' @param sexRatioUnsexed sex ratio to apply to any length bins of a certain size or smaller as defined by the maxSizeUnsexed
#' @param maxSizeUnsexed all sizes below this threshold will assign unsexed fish by sexRatio set equal to 0.50, fish larger than this size will have unsexed fish assigned by the calculated sex ratio in the data.
#' @param verbose opt to print out message statements
#'
#' @author Allan Hicks and Chantel Wetzel
#' @export 


SexRatio.fn <- function(x, sexRatioStage, sexRatioUnsexed, maxSizeUnsexed, verbose){

    if (sexRatioStage == 1){

        #incorporate unsexed fish using sex ratios
        if(length(sexRatioUnsexed)==1 & !is.na(sexRatioUnsexed)) {
            if (verbose) {
            message("Sex ratio for unsexed fish being applied to the expanded numbers within a tow (stage 1) when possible. 
            If no data within a tow for bin then the sex ratio for the bin across all years applied to unsexed fish.
            If no data for that bin across all years then the sex ratio for nearby bins was applied to unsexed fish.\n") }

            x$sexRatio <- x$expF/(x$expF+x$expM)
            # The below line was changed to as.character from as.numeric because it was not finding the correct lengths : CRW
            x$sexRatio[x$Length_cm <= maxSizeUnsexed] <- sexRatioUnsexed
    
            #in case there are any NA's, we can temporarily put in zeros for calcualtions below
            x[is.na(x$expF),"expF"] <- 0
            x[is.na(x$expM),"expM"] <- 0
    
            #now fill in any missing ratios with ratios of that bin from other years and strata (can probably be done more efficiently)
            noRatio <- which(is.na(x$sexRatio))
            check = round(length(noRatio)/length(x$sexRatio),3)
            if ( check > 0.10) {
                if (verbose) {
                message("\n There are", check, "percent of tows with observations that the sex ratio will be filled based on other tows.
                        Consider increasing the maxSizeUnsexed or create the comps as unsexed.\n")}
            if(length(noRatio)>0) cat("\nThese are sex ratios that were filled in using observations from the same lengths from different strata and years:\n")}
            for(i in noRatio) {
                inds <- x$allLs == x$allLs[i]
                tmpF <- sum(x$expF[inds])
                tmpM <- sum(x$expM[inds])
                x$sexRatio[i] <- tmpF/(tmpF+tmpM)
                #print(x[i,c("Length_cm","allLs","expF","expM","sexRatio")])
                if (verbose){
                message(cat("LengthAge:", x[i,c("Length_cm")], "Bin:", x[i,c("allLs")], "Sex Ratio:", x[i,c("sexRatio")])) }
            }
    
            noRatio <- which(is.na(x$sexRatio))
            if(length(noRatio)>0) {
                if (verbose){ 
                    message("\nThese are sex ratios that were filled in using observations from nearby lengths\n")}}

            for(i in noRatio) {
                nearLens <- Lengths[c(which(Lengths==x$allLs[i])-1, which(Lengths==x$allLs[i])+1)]
                inds <- x$allLs %in% nearLens
                tmpF <- sum(x$expF[inds])
                tmpM <- sum(x$expM[inds])
                x$sexRatio[i] <- tmpF/(tmpF+tmpM)
                if (verbose){ 
                message(cat("Length/Age:", x[i,c("Length_cm")], "Bin:", x[i,c("allLs")], "Sex Ratio:", x[i,c("sexRatio")])) }
            }
            noRatio <- which(is.na(x$sexRatio))
            if(length(noRatio)>0)  { if (verbose) { message("Some sex ratios were left unknown and omitted\n\n") } }
            if(length(noRatio)==0) { if (verbose) { message("Done filling in sex ratios\n\n") }}
    
            # These lines change to add the actual unsexed fish to the expansion factors -CRW
            x$expF <- x$expF + x$sexRatio*x$expU
            x$expM <- x$expM + (1-x$sexRatio)*x$expU
        }
    }

    if (sexRatioStage == 2){       
        # Take everything out of the list into a dataframe
        out = NULL
        for(a in 1:length(x)){
          tmp1 = x[[a]]
          for(b in 1:length(tmp1)){
            tmp = tmp1[[b]]
            init = data.frame(Year = tmp$Year, 
                      stratum = tmp$stratum,
                      area = tmp$area, 
                      LENGTH = tmp$LENGTH, 
                      TotalLjhAll = tmp$TotalLjhF,
                      TotalLjhF = tmp$TotalLjhF,  
                      TotalLjhM = tmp$TotalLjhM,  
                      TotalLjhU = tmp$TotalLjhU)
            out = rbind(out, init)
          }
        }
        # Calculate the sex ratio
        out$sexRatio = out$TotalLjhF / (out$TotalLjhF + out$TotalLjhM)
        # Fill in the input ratio for small fish
        out$sexRatio[out$LENGTH <= maxSizeUnsexed] <- sexRatioUnsexed
        
        # Calculate the ratio across years and strata for missing ratios
        noRatio <- which(is.na(out$sexRatio))
        check = round(length(noRatio)/length(out$sexRatio),3)
        if ( check > 0.10) {
            if (verbose) {
            message("\n There are", check, "percent of tows with observations that the sex ratio will be filled based on other tows.
                    Consider increasing the maxSizeUnsexed or create the comps as unsexed.\n")} }

        if(length(noRatio)>0) { if (verbose) { ("\nThese are sex ratios that were filled in using observations from the same lengths from different strata and years\n") } }
        for(i in noRatio) {
            inds <- out$LENGTH == out$LENGTH[i]
            tmpF <- sum(out$TotalLjhF[inds])
            tmpM <- sum(out$TotalLjhM[inds])
            out$sexRatio[i] <- tmpF/(tmpF+tmpM)
            if (verbose) {
            message(cat("Length/Age:", out[i,"LENGTH"], "Sex Ratio:", round(out[i,"sexRatio"],3))) }
        }
        
        # Calculate the ratio based upon near lengths 
        noRatio <- which(is.na(out$sexRatio))
        if(length(noRatio)>0) {
          if (verbose) { message("\nThese are sex ratios that were filled in using observations from nearby lengths\n") }
          for(i in noRatio) {
            unq.len = sort(unique(out$LENGTH))
            find = which(unq.len == out$LENGTH[i])
            if (out$LENGTH[i] == unq.len[length(unq.len)]) { nearLens <- which(out$LENGTH == unq.len[find-1]) }
            if (out$LENGTH[i] != unq.len[length(unq.len)]) { nearLens <-c(which(out$LENGTH == unq.len[find-1]), which(out$LENGTH == unq.len[find+1])) }
            tmpF <- sum(out$TotalLjhF[nearLens])
            tmpM <- sum(out$TotalLjhM[nearLens])
            out$sexRatio[i] <- tmpF/(tmpF+tmpM)
            if (verbose) { message(cat("Length/Age:", out[i,"LENGTH"], "Sex Ratio:", round(out[i,"sexRatio"],3))) }
          }
        }
        
        out$TotalLjhF    <- out$TotalLjhF + out$TotalLjhU * out$sexRatio
        out$TotalLjhM    <- out$TotalLjhM + out$TotalLjhU * (1 - out$sexRatio)
        out$TotalLjhU    <- round(out$TotalLjhU - out$TotalLjhU * out$sexRatio - out$TotalLjhU * (1 - out$sexRatio), 0)    
        
         #sum over strata within year
        list.yr <- split(out, as.character(out$Year))
        x <- lapply(list.yr,function(x){split(x, as.character(x$stratum))})
    }

   return(x)   
}