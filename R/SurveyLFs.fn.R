#' Epands the lengths up to the total stratum area then sums over strata
#' Written by Allan Hicks 16 March 2009
#' modified to incorporate unsexed fish using sex ratios in May 2011
#' weighted by sample size and area
#' datL should have a column called "year" indicating year
#' femaleMale is a vector of codes for female then male (in that order)
#' lgthBin is the increment of each length bin or a vector of the actual bins
#' NOTE: The length bin called F0 or M0 is retained to show proportion of lengths smaller than smallest bin
#' You will want to likely add this to your first length bin and delete this before putting in SS, or
#' start the lgthBins argument at the 2nd length bin and F0 will be all fish smaller (hence the first length bin)
#' SSout: if True the output is in a format pastable into SS dat file
#' 
#' @param dir directory this is where the output files will be saved
#' @param datL the read in length comps by the ReadInLengths.EWC.fn function
#' @param datTows the read in catch data by the DesignBasedEstBiomass.EWC.fn function
#' @param strat.vars the variables used define the stratas. Defaul is bottom depth and latitudes.
#' @param strat.df the created strata matrix with the calculated areas by the createStrataDF.fn function
#' @param lgthBins length bins
#' @param SSout if True the output is in a format pastable into SS dat file
#' @param meanRatioMethod TRUE/FALSE
#' @param gender gender value for Stock Synthesis
#' @param NAs2zero change NAs to zeros
#' @param sexRatioUnsexed sex ratio to apply to any length bins of a certain size or smaller as defined by the maxSizeUnsexed
#' @param maxSizeUnsexed all sizes below this threshold will assign unsexed fish by sexRatio set equal to 0.50, fish larger than this size will have unsexed fish assigned by the calculated sex ratio in the data.
#' @param partition partition for Stock Synthesis
#' @param fleet fleet number
#' @param nSamps effective sample size for Stock Synthesis
#' @param month month the samples were collected
#' @param printfolder folder where the length comps will be saved
#' @param remove999 the output object by the function will have the 999 column combined with the first length bin
#'
#' @author Allan Hicks and Chantel Wetzel
#' @export 
#' @seealso \code{\link{StrataFactors.fn}}

SurveyLFs.fn <- function(dir, datL, datTows, strat.vars=c("Depth_m","Latitude_dd"), strat.df=NULL, lgthBins=1, SSout=TRUE, meanRatioMethod=TRUE,
                         gender=3, NAs2zero=T, sexRatioUnsexed=NA, maxSizeUnsexed=NA, partition=0, fleet="Enter Fleet", 
                         nSamps="Enter Samps", month="Enter Month", printfolder = "forSS", remove999 = TRUE)  {

    row.names(strat.df) <- strat.df[,1]     #put in rownames to make easier to index later
    numStrata <- nrow(strat.df)
    ind <- !duplicated(datL$Trawl_id)
    datB <- datL[ind,c("Trawl_id", "Weight", strat.vars, "year")]    #individual tow data
    tows = unique(datL$Trawl_id)
    Area_Swept_km2 <- Area_Swept <- Total_fish_number <- Sub_fish_number <- Sexed_fish <- numeric(dim(datB)[1])
    for (i in 1:length(tows)){
        find = which(tows[i] == datTows$Trawl_id)
        area = datTows$Area_Swept_ha[find] / 0.01 #* 0.01
        tot.num = datTows$total_catch_numbers[find]
        sub.num = datTows$subsample_count[find]

        find = which(tows[i] == datL$Trawl_id)
        Sexed_fish[i] = sum(datL[find, "Sex"] %in% c("F", "M"))

        find = which(tows[i] == datB$Trawl_id)      
        #Area_Swept_km2[find]  = area 
        Area_Swept[find]  = area 
        Total_fish_number[find] = tot.num
        Sub_fish_number[find] = sub.num
    }
    #datB$areaFished <- Area_Swept_km2  #area swept for each tow in km2
    datB$areaFished <- Area_Swept
    datB$Number_fish<- Total_fish_number
    datB$Sub_fish_number <- Sub_fish_number
    datB$Sexed_fish <- Sexed_fish
    
    #set up length bins
    if(length(lgthBins)==1) {
        Lengths <- c(-999,seq(floor(min(datL$Length_cm)),ceiling(max(datL$Length_cm)),lgthBins),Inf)
    }
    else{
        Lengths <- c(-999,lgthBins,Inf)
    }

    # In case there a fish with decimal lengths round them down for processing
    datL$allLs <- Lengths[findInterval(datL$Length_cm, Lengths, all.inside=T)]
    
    #first create strata factors
    datB   <- data.frame(datB, stratum = StrataFactors.fn(datB, strat.vars, strat.df))        #create a new column for the stratum factor
    numTows <- table(datTows$Year, StrataFactors.fn(datTows, strat.vars, strat.df))        #strata for each individual tow

    #calculate expansion factor per tow
    #for all sexes
    TdatL.tows <- as.data.frame(table(datL$Trawl_id))
    #datB <- data.frame(datB[match(as.character(TdatL.tows$Var1), as.character(datB$Trawl_id)),], TowExpFactorU = TdatL.tows$Freq)
    datB <- data.frame(datB[match(as.character(TdatL.tows$Var1), as.character(datB$Trawl_id)),], true_sub_Allfish = TdatL.tows$Freq)
    datB$TowExpFactorAll <- datB$Number_fish / datB$true_sub_Allfish

    #for true unsexed fish
    TdatL.tows <- as.data.frame(table(datL$Trawl_id, datL$Sex%in%c("U")))
    TdatL.tows <- TdatL.tows[TdatL.tows$Var2=="TRUE",]
    datB <- data.frame(datB[match(as.character(TdatL.tows$Var1), as.character(datB$Trawl_id)),], true_sub_Ufish = TdatL.tows$Freq)
    datB$TowExpFactorU <- datB$Number_fish / (datB$Sexed_fish + datB$true_sub_Ufish)

    #for females and males only
    TdatL.tows <- as.data.frame(table(datL$Trawl_id, datL$Sex%in%c("F", "M")))
    TdatL.tows <- TdatL.tows[TdatL.tows$Var2=="TRUE",]
    datB <- data.frame(datB[match(as.character(TdatL.tows$Var1), as.character(datB$Trawl_id)),], TowExpFactorMF = TdatL.tows$Freq)
    # Find the numerator looking where the number of fish = sexed fish when all fish are sampled (e.g., sexed and unsexed in a fully sampled tow)
    # The previous approach expanded sexed fish relative to the full sample size resulting in expansions when there should not have been
    datB = data.frame(datB[match(as.character(TdatL.tows$Var1), as.character(datB$Trawl_id)),], true_sub_MFfish = TdatL.tows$Freq)
    datB$TowExpFactorMF <- datB$Number_fish / (datB$true_sub_MFfish + datB$true_sub_Ufish)
    datB$TowExpFactorMF[datB$TowExpFactorMF==Inf] <- NA
    
    #find frequency of number of lengths for all fish
    TdatL.lengths <- as.data.frame(table(datL$Trawl_id, datL$Length_cm)) 
    names(TdatL.lengths) <- c("Trawl_id","Length_cm","numAll")
    TdatL.lengths <- TdatL.lengths[TdatL.lengths$numAll>0,]
    datB <- merge(datB,TdatL.lengths,by="Trawl_id", all=T)

    #Females, males, and true unsexed
    TdatL.lengths <- as.data.frame(table(datL$Trawl_id,datL$Length_cm,datL$Sex))
    names(TdatL.lengths) <- c("Trawl_id","Length_cm","Sex","num")
    TdatL.lengths <- TdatL.lengths[TdatL.lengths$num>0,]
    TdatL.lengths <- split(TdatL.lengths,TdatL.lengths$Sex)
    temp <- TdatL.lengths[["F"]][,c("Trawl_id","Length_cm","num")]
    names(temp) <- c("Trawl_id","Length_cm","numF")
    datB <- merge(datB,temp,by=c("Trawl_id","Length_cm"),all=T)
    datB[is.na(datB$numF),"numF"] <- 0
    temp <- TdatL.lengths[["M"]][,c("Trawl_id","Length_cm","num")]
    names(temp) <- c("Trawl_id","Length_cm","numM")
    datB <- merge(datB,temp,by=c("Trawl_id","Length_cm"),all=T)
    datB[is.na(datB$numM),"numM"] <- 0
    temp <- TdatL.lengths[["U"]][,c("Trawl_id","Length_cm","num")]
    names(temp) <- c("Trawl_id","Length_cm","numU")
    datB <- merge(datB,temp,by=c("Trawl_id","Length_cm"),all=T)
    datB[is.na(datB$numU),"numU"] <- 0

    #now calculate the expanded lengths per tow
    datB$expAll <- datB$numAll*datB$TowExpFactorAll
    datB$expF   <- datB$numF*datB$TowExpFactorMF
    datB$expM   <- datB$numM*datB$TowExpFactorMF
    datB$expU   <- datB$numU*datB$TowExpFactorU
    
    datB$Length_cm <- as.numeric(as.character(datB$Length_cm))
    datB$allLs <- Lengths[findInterval(datB$Length_cm, Lengths, all.inside=T)]

   
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

    # If no sex ratio is used to fill in the unsexed fish, need to add replace NAs with 0 for female and male fish
    # in order to avoid the whole length bin resulting in an NA value when calculating the mean ratio
    if(is.na(sexRatioUnsexed)) {
        datB[is.na(datB$expF), "expF"] = 0
        datB[is.na(datB$expM), "expM"] = 0
    }
    
    #sum over strata within year
    datB.yrstr <- split(datB,as.character(datB$year))
    datB.yrstr <- lapply(datB.yrstr,function(x){split(x,as.character(x$stratum))})


    lengthTotalRatio.fn <- function(x,strat) {
        #function to sum lengths within a stratum and a year
        #Uses the Total Ratio estimate and Mean ratio estimate
        theYear <- unique(x$year)
        theStratum <- unique(x$stratum)
        if(length(theYear)!=1) stop("there is a problem in length.fn. There should be exactly one year in each list item.\n")
        if(length(theStratum)!=1) stop("there is a problem in length.fn. There should be exactly one stratum in each list item.\n")
        # Need to check the below line, not sure if the [1] should be there 
        a.hi <- unlist(x$areaFished[1])  #area swept per tow in stratum
        a.h <- sum(a.hi)  #total area swept in stratum
        A.h <- strat[strat$name==theStratum,"area"]
        x$LENGTH_cm <- as.numeric(as.character(x$Length_cm))
        xcols  <- c("year","stratum") #must be two or more columns to keep the selection a dataframe
        lgths  <- split(x,x$LENGTH_cm)
        LjhAll <- unlist(lapply(lgths, function(x){sum(x$expAll)}))
        out    <- data.frame(x[rep(1, length(LjhAll)), xcols], area = A.h, areaSwept = a.h, 
                             LENGTH_cm = as.numeric(names(LjhAll)), LjhAll = LjhAll, TotalLjhAll = round(A.h * LjhAll / a.h))
        LjhF   <- unlist(lapply(lgths, function(x){sum(x$expF)}))
        out    <- data.frame(out, LjhF = LjhF, TotalLjhF = round(A.h * LjhF / a.h))
        LjhM   <- unlist(lapply(lgths, function(x){sum(x$expM)}))
        out    <- data.frame(out, LjhM = LjhM, TotalLjhM = round(A.h * LjhM / a.h))
        LjhU   <- unlist(lapply(lgths, function(x){sum(x$expU)}))
        out    <- data.frame(out, LjhU = LjhU, TotalLjhU = round(A.h * LjhU / a.h))
        return(out)
    }

    lengthMeanRatio.fn <- function(x, strat, numTows) {
        #function to sum lengths within a stratum and a year
        #Uses the Mean ratio estimate
        theYear <- unique(x$year)
        theStratum <- unique(x$stratum)
        if(length(theYear)!=1) stop("there is a problem in length.fn. There should be exactly one year in each list item.\n")
        if(length(theStratum)!=1) stop("there is a problem in length.fn. There should be exactly one stratum in each list item.\n")
        if(!(as.character(theYear)%in%row.names(numTows))) stop(paste("The year",theYear,"is in the lengths file but is not in the tow file"))
        ntows <- numTows[as.character(theYear),theStratum]
        A.h   <- strat[strat$name == theStratum, "area"]
        x$LENGTH_cm <- as.numeric(as.character(x$Length_cm))
        xcols  <- c("year","stratum") #must be two or more columns to keep the selection a dataframe
        lgths  <- split(x, x$LENGTH) # splits by length bin for year and stratum
        LjhAll <- unlist(lapply(lgths, function(x){sum( x$expAll / x$areaFished)})) # calculated the expansion for unsexed by length bin
        out    <- data.frame(x[rep(1, length(LjhAll)), xcols], area = A.h, 
                             LENGTH=as.numeric(names(LjhAll)), LjhAll = LjhAll / ntows, TotalLjhAll = round(A.h * LjhAll / ntows))
        LjhF <- unlist(lapply(lgths, function(x){ sum( x$expF / x$areaFished ) }))
        out  <- data.frame(out, LjhF = LjhF / ntows, TotalLjhF = round(A.h * LjhF / ntows))
        LjhM <- unlist(lapply(lgths, function(x){ sum( x$expM / x$areaFished )})) 
        out  <- data.frame(out, LjhM = LjhM / ntows, TotalLjhM = round(A.h * LjhM / ntows))
        LjhU <- unlist(lapply(lgths, function(x){ sum( x$expU / x$areaFished )})) 
        out  <- data.frame(out, LjhU = LjhU / ntows, TotalLjhU = round(A.h * LjhU / ntows))
        return(out)
    }

    if(meanRatioMethod) {
        L.year.str <- lapply(datB.yrstr, function(x){lapply(x, lengthMeanRatio.fn, strat = strat.df, numTows)})
    }
    else{
        L.year.str <- lapply(datB.yrstr, function(x){lapply(x, lengthTotalRatio.fn, strat = strat.df)})
    }

    year.fn <- function(x,Lengths) {   #calculate the LFs by year
        theLs.yr    <- unlist(lapply(x, function(x){ as.numeric(as.character(x$LENGTH))}))
        TotalLjhAll <- unlist(lapply(x, function(x){ as.numeric(as.character(x$TotalLjhAll))}))
        TotalLjhF   <- unlist(lapply(x, function(x){ as.numeric(as.character(x$TotalLjhF))}))
        TotalLjhM   <- unlist(lapply(x, function(x){ as.numeric(as.character(x$TotalLjhM))}))
        TotalLjhU   <- unlist(lapply(x, function(x){ as.numeric(as.character(x$TotalLjhU))}))
        # Finds the interval that the length falls in and floors it (so 23.2 would be in 23 if 23 was a level in Lengths, 
        # all.inside puts maximum age group into N-1 group, thus I padded with Inf.)
        allLs       <- Lengths[findInterval(theLs.yr, Lengths, all.inside = T)] 
        TotalLjAll  <- tapply(TotalLjhAll, allLs, sum, na.rm = T)
        TotalLjF    <- tapply(TotalLjhF, allLs, sum, na.rm = T)
        TotalLjM    <- tapply(TotalLjhM, allLs, sum, na.rm = T)
        TotalLjU    <- tapply(TotalLjhU, allLs, sum, na.rm = T)
        out         <- data.frame(Length = Lengths, TotalLjAll = rep(NA, length(Lengths)), TotalLjF = rep(NA, length(Lengths)), 
                                  TotalLjM = rep(NA, length(Lengths)), TotalLjU = rep(NA, length(Lengths)))
        row.names(out) <- out$Length
        out[names(TotalLjAll),"TotalLjAll"] <- 100 * TotalLjAll / sum(TotalLjAll, na.rm = T)
        out[names(TotalLjF),"TotalLjF"]     <- 100 * TotalLjF / (sum(TotalLjF, na.rm = T) + sum(TotalLjM, na.rm = T)) 
        out[names(TotalLjM),"TotalLjM"]     <- 100 * TotalLjM / (sum(TotalLjF, na.rm = T) + sum(TotalLjM, na.rm = T)) 
        out[names(TotalLjU),"TotalLjU"]     <- 100 * TotalLjU / (sum(TotalLjF, na.rm = T))
        out <- out[-nrow(out),]   #remove last row because Inf and always NA due to inside.all=T
        return(out)
    }

    L.year <- lapply(L.year.str, year.fn, Lengths = Lengths)
    if(!SSout) {
        return(list(L.year = L.year, L.year.str = L.year.str))
    }
    
    #otherwise return SS output for gender type
    if(gender == 0) {
        lgths <- as.character(L.year[[1]]$Length)
        Ls    <- unlist(lapply(L.year, function(x){c(x$TotalLjAll, x$TotalLjAll)}))
        if(NAs2zero){Ls[is.na(Ls)] <- 0}
        Ls  <- matrix(Ls, nrow = length(L.year), byrow=T,
               dimnames = list(NULL, paste(c(rep("U", length(lgths)), rep("U", length(lgths))), lgths, sep="")))
        out <- data.frame(year = as.numeric(names(L.year)), month = month, fleet = fleet, gender = rep(0, length(L.year)),
               partition = partition, Nsamp = nSamps, Ls)    
    }

    if(gender == 3) {
        #females then males
        lgths <- as.character(L.year[[1]]$Length)
        Ls    <- unlist(lapply(L.year, function(x){c(x$TotalLjF, x$TotalLjM)}))
        if(NAs2zero){Ls[is.na(Ls)] <- 0}
        Ls  <- matrix(Ls, nrow = length(L.year), byrow = T,
               dimnames = list(NULL, paste(c(rep("F",length(lgths)), rep("M",length(lgths))), lgths, sep = "")))
        out <- data.frame(year = as.numeric(names(L.year)), month = month, fleet = fleet, gender=rep(3, length(L.year)),
               partition = partition, Nsamp = nSamps, Ls)

        if(is.na(sexRatioUnsexed)){
            #unsexed fish
            lgths <- as.character(L.year[[1]]$Length)
            Ls    <- unlist(lapply(L.year, function(x){c(x$TotalLjU, x$TotalLjU)}))
            if(NAs2zero){Ls[is.na(Ls)] <- 0}
            Ls  <- matrix(Ls, nrow = length(L.year), byrow=T,
                   dimnames = list(NULL, paste(c(rep("U", length(lgths)), rep("U", length(lgths))), lgths, sep="")))
            out2 <-data.frame(year = as.numeric(names(L.year)), month = month, fleet = fleet, gender = rep(0, length(L.year)),
                   partition = partition, Nsamp = nSamps, Ls)  

        }
    }

    # save output as a csv
    comp.type = ifelse(lgthBins[1] == 1, "Age", "Length")
    plotdir <- file.path(dir, printfolder)
    plotdir.isdir <- file.info(plotdir)$isdir
    if(is.na(plotdir.isdir) | !plotdir.isdir){
      dir.create(plotdir)
    }
    write.csv(out, file = paste0(plotdir, "/Survey_Gender", gender, "_Bins_-999_", max(lgthBins),"_", comp.type, "Comps.csv"), row.names = FALSE)
    if(gender == 3 && is.na(sexRatioUnsexed)){
        write.csv(out2, file = paste0(plotdir, "/Survey_Gender_Unsexed_Bins_-999_", max(lgthBins),"_", comp.type, "Comps.csv"), row.names = FALSE) }


    usableOut = out
    if(gender == 3 && is.na(sexRatioUnsexed)) { usableOut2 = out2 }

    if (gender == 3){
        usableOut[,paste0("F",min(lgthBins))] <- usableOut[,paste0("F",min(lgthBins))] + usableOut$F.999
        usableOut[,paste0("M",min(lgthBins))] <- usableOut[,paste0("M",min(lgthBins))] + usableOut$M.999
        usableOut <- usableOut[,-which(names(usableOut)%in%c("F.999","M.999"))]
        write.csv(usableOut, file = paste0(plotdir, "/Survey_Gender", gender, "_Bins_",min(lgthBins),"_", max(lgthBins),"_", comp.type, "Comps.csv"), row.names = FALSE)
        if(is.na(sexRatioUnsexed)){
            usableOut2[,paste0("U",min(lgthBins))]  <- usableOut2[,paste0("U",min(lgthBins))] + usableOut2$U.999
            usableOut2 <- usableOut2[,-which(names(usableOut2)%in%c("U.999","U.999.1"))]
            write.csv(usableOut2, file = paste0(plotdir, "/Survey_Gender_Unsexed_Bins_",min(lgthBins),"_", max(lgthBins),"_", comp.type, "Comps.csv"), row.names = FALSE)
        }
    }

    if (gender == 0){
        usableOut[,paste0("U",min(lgthBins))]  <- usableOut[,paste0("U",min(lgthBins))] + usableOut$U.999
        usableOut <- usableOut[,-which(names(usableOut)%in%c("U.999","U.999.1"))]
        write.csv(usableOut, file = paste0(plotdir, "/Survey_Gender", gender, "_Bins_",min(lgthBins),"_", max(lgthBins),"_", comp.type, "Comps.csv"), row.names = FALSE)
    }

    if (comp.type == "Length"){
        cat("\nNOTE: Two files have been saved the the printfolder directory.
            \n\tThe first has the 999 column showing fish smaller than the initial length bind. 
            \n\tCheck to make sure there is not a large number of fish smaller than the initial length bin.
            \n\tThe second file has combined the 999 with the first length bin and is ready for use in SS.\n\n")
    }
    if (!remove999) { return(out)}
    if (remove999)  { return(usableOut)}
}
