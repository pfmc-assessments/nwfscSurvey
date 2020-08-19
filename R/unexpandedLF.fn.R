#' Creates a matrix of length or age composition data WITHOUT expantion
#' Written by Chantel Wetzel to work with the data warehouse data formatting,
#' 
#' @param dir directory this is where the output files will be saved
#' @param datL the read in length comps by the PullBio.fn function
#' @param lgthBins length bins
#' @param ageErr Number of ageing error matrix for SS
#' @param agelow age bin for SS (default value of -1)
#' @param agehigh age bin for SS (default value of -1)
#' @param sex (0 = unsexed, 1 = females, 2 = males, 3 = females then males) sex value for Stock Synthesis
#' @param partition partition for Stock Synthesis
#' @param fleet fleet number
#' @param nSamps effective sample size for Stock Synthesis
#' @param month month the samples were collected
#' @param printfolder folder where the length comps will be saved
#' @param remove999 the output object by the function will have the 999 column combined with the first length bin
#' @param verbose opt to print out message statements
#'
#' @author Chantel Wetzel
#' @export 

UnexpandedLFs.fn <- function(dir = NULL, datL, lgthBins = 1, sex = 3,  partition = 0, fleet = "Enter Fleet",  
                             ageErr = "NA", agelow = -1, agehigh = -1, 
                             nSamps="Enter Samps", month="Enter Month", printfolder = "forSS", verbose = TRUE)  {

    if(is.null(dir) & verbose){ cat("\nDirectory not specified and csv will not be written.\n") }
    if(!is.null(dir)){
        plotdir <- file.path(dir, printfolder)
        plotdir.isdir <- file.info(plotdir)$isdir
        if(is.na(plotdir.isdir) | !plotdir.isdir){
          dir.create(plotdir)
        }
    }

    # Check to see if user is doing ages or lengths
    check = sum(datL$Length_cm, na.rm = TRUE) == sum(datL$Age, na.rm = TRUE)
    comp.type = ifelse(check, "Age", "Length")

    totRows  <- nrow(datL)
    datL     <- datL[!is.na(datL$Length_cm),]
    if (verbose){
    cat("There are ", nrow(datL)," records kept out of",totRows,"records after removing missing records.\n")}

   
    #set up length bins
    if(length(lgthBins)==1) {
        Lengths <- c(-999,seq(floor(min(datL$Length_cm)),ceiling(max(datL$Length_cm)),lgthBins),Inf)
    }else{
        Lengths <- c(-999,lgthBins,Inf)
    }

    # In case there a fish with decimal lengths round them down for processing
    datL$allLs <- Lengths[findInterval(datL$Length_cm, Lengths, all.inside=T)]

    # if there are NA sexes replace them with U
    if (sum(is.na(datL$Sex)) > 0) {    
        datL[is.na(datL$Sex),"Sex"] = "U"  
    } 

    # Create an assigned sex column
    datL$sex = datL$Sex  

    # Assign "U" to sex based on sex ratio
    # datL$sexRatio = NA
    # temp = table(datL$Length_cm, datL$Sex)
    # ratio = temp[,"F"] / (temp[,"F"] + temp[,"M"])
    # ratio[is.na(ratio)] <- 0
    # if(length(sexRatioUnsexed)==1 & !is.na(sexRatioUnsexed)) {
    #     datL$sexRatio[datL$Length_cm <= maxSizeUnsexed] <- sexRatioUnsexed
    # }
    
    sex_out = ifelse(sex == 3, "Both", 
              ifelse(sex == 2, "M", 
              ifelse(sex == 1, "F", "U")))
    if(sex_out == "Both") {sex_out = c("F", "M")}
    
    #Create the comps
    Results = NULL
    yrs = sort(unique(datL$Year))
    for(y in 1:length(yrs)) {
        # Identify relevant rows
        Which = which(datL[,'Year'] == yrs[y] )
        # Skip this year unless there are rows
        if(length(Which)>0) {
          ##### Deal first with "F" or "M" entries
          # Format reference stuff
          if (any(datL[Which,"sex"] %in% sex_out)) { 
            Row = c('Year' = yrs[y], 'Month' = month, 'Fleet'= fleet, 'Sex'= sex, 'Partition' = partition, 'Nsamp'= nSamps)
            # Loop across F then M
            for(s in 1:length(sex_out)) {
                # Loop across length bins
                for(l in 1:length(lgthBins))
                {
                    # Subset to relevant rows
                    Which2 = Which[which(datL[Which,'allLs'] == lgthBins[l] & datL[Which,'sex'] == sex_out[s])]
                    # Sum to effective sample size by length_bin x Sex x Fleet x Year
                    if(length(Which2) == 0) Row = c(Row, 0)
                    if(length(Which2) >= 1) Row = c(Row, dim(datL[Which2,])[1])
                }
            }
            # Add to results matrix
            Results = rbind(Results, Row)
          }
        } # end Which loop
    } # end year loop
    colnames(Results)[-c(1:6)] = paste(rep(sex_out, each = length(lgthBins)), lgthBins, sep="-")

    # Write the files including the -999 column
    if (comp.type == "Length") {
        out.comps = Results      
    }
    if (comp.type == "Age") {
        out.comps = cbind(Results[,1:5], ageErr, agelow, agehigh, Results[ , 6:dim(Results)[2]])
    }

    if(!is.null(dir)){ 
        write.csv(out.comps, file = file.path(plotdir, 
            paste0("Survey_notExpanded_", comp.type, "_comp_Sex_", sex,"_bin=", lgthBins[1], "-", max(lgthBins),".csv")))    
    }
    return(out.comps)
}