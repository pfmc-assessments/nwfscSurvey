#' Calculate effN input sample sizes
#' based on Stewart & Hamel 2014
#' 
#' @param file excel file name
#' @param sheet sheet name in excel file
#' @param headerRow line of header row in excel file
#' @param species species specific value to determine the number of unique samples per tow (flatfish, shelfrock, sloperock, thorny, others, all)
#'
#' @author Allan Hicks 
#' @export 
#' \code{\link{readDataFromExcel.fn}}

getN <- function(dir, file, headerRow, species = "flatfish", printfolder = "forSS"){

	if (species == "flatfish")  {n.unq = 3.09 }
	if (species == "shelfrock") {n.unq = 2.43 }
	if (species == "sloperock") {n.unq = 2.43 }
	if (species == "thorny")    {n.unq = 6.91 }
	if (species == "others")    {n.unq = 2.38 }
	if (species == "all")       {n.unq = 2.73 }

	hauls = readInExcelBiomass.fn(file, sheet = "Lengths-StratumTowTallies", header = 7)
	len.num = readInExcelBiomass.fn(file, sheet = "Lengths", header = 9)

	find = names(hauls) == "N_Tow_Tally_(Stratum)"
	names(hauls[find]) = "N_Tow_Tally"

	# Currently not working below here
	xx = aggregate(N.Tow.Tally..Stratum. ~ Survey.Year, sum, data = hauls)
	nSamp = xx[,2]
	n = floor(n.unq * nSamp) 

	fish = aggregate(LENGTH_CM ~ PROJECT_CYCLE, length, data = len.num)

	samples = cbind(xx$Survey.Year, nSamp, fish$LENGTH_CM, n)
	colnames(samples) = c("Year", "Tows", "Fish", "Sample Size")

	# save output as a csv
    plotdir <- file.path(dir,printfolder)
    plotdir.isdir <- file.info(plotdir)$isdir
    if(is.na(plotdir.isdir) | !plotdir.isdir){
      dir.create(plotdir)
    }
	write.csv(samples, file = paste0(plotdir, "/NWFSC_TW_Survey_LengthSampleSize.csv")
}