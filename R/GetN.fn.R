#' Calculate effN input sample sizes
#' based on Stewart & Hamel 2014
#' 
#' @param dir directory
#' @param dat excel file name for the NWFSC or data object for the Triennial or AKSFC survey data
#' @param type specify whether doing "length" or "age". Used to read associatied excel sheets
#' @param species species specific value to determine the number of unique samples per tow (flatfish, shelfrock, sloperock, thorny, others, all)
#' @param printfolder name of the folder to create and save files. Location will be paste0(dir, printfolder)
#' @param verbose opt to print out message statements
#'
#' @author Chantel Wetzel
#' @export 

GetN.fn <- function(dir, dat, type, species = NULL, printfolder = "forSS", verbose = TRUE){

    n.unq = NA
    if (species == "flatfish")  { n.unq = 3.09 }
    if (species == "shelfrock") { n.unq = 2.43 }
    if (species == "sloperock") { n.unq = 2.43 }
    if (species == "thorny")    { n.unq = 6.91 }
    if (species == "others")    { n.unq = 2.38 }
    if (species == "all")       { n.unq = 2.73 }
    if (is.na(n.unq)) {
        if (verbose){
        message("\nThe species input does not match one of the following options; flatfish, shelfrock, sloperock, thorny, others, or all\n")} }

    if (verbose){
    message("\nThe effN sample size is calculated as",n.unq,"multiplied by the number of tows in each year.\n") }


    if (type == "length"){
        temp = dat[!is.na(dat$Length_cm),]
        nSamp <- table(temp$Year,!duplicated(as.character(temp$Trawl_id)))[,"TRUE"]  
    }

    if (type == "age"){
        temp = dat[!is.na(dat$Age),]
        nSamp <- table(temp$Year,!duplicated(as.character(temp$Trawl_id)))[,"TRUE"]  
    }

    n = floor(n.unq * nSamp) 
    fish = table(temp$Year, temp$Sex)
    samples = data.frame( Year = as.numeric(names(nSamp)), Tows = nSamp, 
                         All_Fish = as.numeric(fish[,"F"]) + as.numeric(fish[,"M"]) + as.numeric(fish[,"U"]), 
                         Sexed_Fish = as.numeric(fish[,"F"]) + as.numeric(fish[,"M"]), 
                         Unsexed_Fish = as.numeric(fish[,"U"]), 
                         Sample_Size = n)       

    # save output as a csv
    plotdir <- file.path(dir,printfolder)
    plotdir.isdir <- file.info(plotdir)$isdir
    if(is.na(plotdir.isdir) | !plotdir.isdir){
      dir.create(plotdir)
    }
    write.csv(samples, file = paste0(plotdir, "/", type, "_SampleSize.csv"), row.names = FALSE)
    return(n)
}