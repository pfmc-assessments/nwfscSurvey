#' Calculate effN input sample sizes
#' based on Stewart & Hamel 2014
#' 
#' @param file excel file name
#' @param type specify whether doing "length" or "age". Used to read associatied excel sheets
#' @param sheet sheet name in excel file
#' @param headerRow line of header row in excel file
#' @param species species specific value to determine the number of unique samples per tow (flatfish, shelfrock, sloperock, thorny, others, all)
#'
#' @author Allan Hicks 
#' @export 
#' \code{\link{readDataFromExcel.fn}}

getN <- function(dir, file, type, headerRow = "default", species = "flatfish", printfolder = "forSS"){

	n.unq = NA
	if (species == "flatfish")  {n.unq = 3.09 }
	if (species == "shelfrock") {n.unq = 2.43 }
	if (species == "sloperock") {n.unq = 2.43 }
	if (species == "thorny")    {n.unq = 6.91 }
	if (species == "others")    {n.unq = 2.38 }
	if (species == "all")       {n.unq = 2.73 }
	if (is.na(n.unq)) { print("The species input does not match one of the following options; flatfish, shelfrock, sloperock, thorny, others, or all")}

	if (type == "length"){
		if (headerRow == "default") { headerRow = c(7,9) }
		hauls = readDataFromExcel.fn(file, sheet = "Lengths-StratumTowTallies", header = headerRow[1])
		new.names = c( "Species_Code", "Scientific_Name", "Species", "Project", "Survey_Year", "Area_Set_Identifier", "Area_Name", "Southern_Latitude",   
					  "Northern_Latitude", "Depth_Strata_Set", "Minimum_Stratum_Depth", "Maximum_Stratum_Depth",
					  "Length_Tally_Stratum", "N_Tow_Tally_Stratum" )
		names(hauls) = new.names
		len.num = readDataFromExcel.fn(file, sheet = "Lengths", header = headerRow[2])		
	}

	if (type == "age"){
		if (headerRow == "default") { headerRow = c(7,9) }
		hauls = readDataFromExcel.fn(file, sheet = "Ages-SexBasedTowTallies", header = headerRow[1])
		new.names = c("Species_Code", "Species", "Survey_Year", "Project", "INPFC_Area", "Min_Depth_m", "Max_Depth_m", "Number_of_Tows_Female", "Number_of_Tows_Male",              
					   "Number_of_Tows_Unsexed", "Number_of_Tows_wo_respect_to_sex")
		names(hauls) = new.names
		age.num = readDataFromExcel.fn(file, sheet = "SexedLgthWtAge", header = headerRow[2])		
	}

	if (type == "length"){
		xx = aggregate(N_Tow_Tally_Stratum ~ Survey_Year, sum, data = hauls)
		nSamp = xx[,2]
		n = floor(n.unq * nSamp) 
		fish = aggregate(LENGTH_CM ~ PROJECT_CYCLE, length, data = len.num)
		samples = cbind(xx$Survey_Year, nSamp, fish$LENGTH_CM, n)
	}

	if (type == "age"){
		xx = aggregate(Number_of_Tows_wo_respect_to_sex ~ Survey_Year, sum, data = hauls)
		nSamp = xx[,2]
		n = floor(n.unq * nSamp) 
		fish = aggregate(AGE_YRS ~ PROJECT_CYCLE, length, data = age.num)
		samples = cbind(xx$Survey_Year, nSamp, fish$AGE_YRS, n)
	}
	colnames(samples) = c("Year", "Tows", "Fish", "Sample Size")

	# save output as a csv
    plotdir <- file.path(dir,printfolder)
    plotdir.isdir <- file.info(plotdir)$isdir
    if(is.na(plotdir.isdir) | !plotdir.isdir){
      dir.create(plotdir)
    }
	write.csv(samples, file = paste0(plotdir, "/NWFSCBT_Survey_", type, "_SampleSize.csv"), row.names = FALSE)
	return(n)
}