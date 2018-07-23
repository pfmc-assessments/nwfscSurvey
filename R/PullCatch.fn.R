#' Pull catch data from the NWFSC data warehouse
#' The website is: https://www.nwfsc.noaa.gov/data
#' Curently, this function only pulls data for a single specified survey.
#' 
#' @param Species scientific name of species data to pull from the data warehouse
#' @param YearRange range of years to pull data
#' @param SurveyName survey to pull the data for
#' @param SaveFile option to save the file to the directory
#' @param Dir directory where the file should be saved
#' 
#' @author Chantel Wetzel based on code by John Wallac
#' @export
#'
#' @import jasonlite
#' @import chron


PullCatch.fn <- function (Species = "Sebastes pinniger", YearRange = c(1000, 5000), SurveyName = NULL, SaveFile = FALSE, Dir = NULL) 
{

	if(SaveFile){
        if(is.null(Dir)){
            stop("The Dir input needs to be specified in order to save output file.")
        }
    }

	#   match.f function for combining data sets
	#   AUTHOR:  John R. Wallace (John.Wallace@noaa.gov)  	
    match.f <- function (file, table, findex = 1, tindex = findex, tcol = NULL, round. = T, digits = 0) {

	    paste.col <- function(x) {
	        if (is.null(dim(x)))
	            return(paste(as.character(x)))
	        out <- paste(as.character(x[, 1]))
	        for (i in 2:ncol(x)) {
	            out <- paste(out, as.character(x[, i]))
	        }
	        out
	    }
	    if (is.null(dim(file))) {
	        dim(file) <- c(length(file), 1)
	    }
	    if (round.) {
	        for (i in findex) {
	            if (is.numeric(file[, i]))
	                file[, i] <- round(file[, i], digits)
	        }
	        for (i in tindex) {
	            if (is.numeric(table[, i]))
	                table[, i] <- round(table[, i], digits)
	        }
	    }
	    if (is.null(tcol))
	        tcol <- dimnames(table)[[2]][!(dimnames(table)[[2]] %in%
	            tindex)]
	    cbind(file, table[match(paste.col(file[, findex]), paste.col(table[,
	        tindex])), tcol, drop = F])
	}

	#   rename_columsn function for that renames columns
	#   AUTHOR:  John R. Wallace (John.Wallace@noaa.gov)  
    rename_columns = function(DF, origname = colnames(DF), newname) {
        " # 'age_years' has both age and years, first forcing a change to 'age' "
        colnames(DF)[grep("age_years", colnames(DF))] <- "age"
        DF_new = DF
        for (i in 1:length(newname)) {
            Match = grep(newname[i], origname, ignore.case = TRUE)
            if (length(Match) == 1) 
                colnames(DF_new)[Match] = newname[i]
        }
        return(DF_new)
    }

    # Survey options available in the data warehouse
    surveys = matrix(data = c("Triennial", 'Groundfish Triennial Shelf Survey',
                     "AFSC.Slope", "NA",
                     "NWFSC.Combo", 'Groundfish Slope and Shelf Combination Survey',
                     "NWFSC.Slope", 'Groundfish Slope Survey',
                     "NWFSC.Shelf", 'Groundfish Shelf Survey',
                     "NWFSC.Hypoxia", 'Hypoxia Study',
                     "NWFSC.Santa.Barb.Basin", 'Santa Barbara Basin Study',
                     "NWFSC.Shelf.Rockfish", 'Shelf Rockfish [2004-2015]',
                     "NWFSC.Video", 'Video Study'),
                     ncol = 2, 
                     byrow = TRUE)

    # Check the input survey name against available options
    if (!SurveyName %in% surveys[,1]) { 
         stop(paste("The SurveyName does not match one of the available options:", surveys[,1])) }

    # Find the long project name to extract data from the warehouse
    for(i in 1:dim(surveys)[1]){
        if(SurveyName == surveys[i,1]){ 
        	project = surveys[i,2]
        	projectShort = surveys[i,1]
        }
    }

    if (length(YearRange) == 1) {
        YearRange <- c(YearRange, YearRange)    }

    # Pull data for the specific species for the following variables
    Vars <- c("scientific_name", "year", "subsample_count", "subsample_wt_kg","project", "cpue_kg_per_ha_der",
        	  "total_catch_numbers", "total_catch_wt_kg", "vessel", "tow")


    UrlText <- paste0("https://www.nwfsc.noaa.gov/data/api/v1/source/trawl.catch_fact/selection.json?filters=project=", paste(strsplit(project, " ")[[1]], collapse = "%20"),",", 
               "actual_station_design_dim$stn_invalid_for_trawl_date_whid=0,", 
               "performance=Satisfactory,", "depth_ftm>=30,depth_ftm<=700,", 
               "field_identified_taxonomy_dim$scientific_name=", paste(strsplit(Species, " ")[[1]], collapse = "%20"), 
               ",date_dim$year>=", YearRange[1], ",date_dim$year<=", YearRange[2], "&variables=", 
               paste0(Vars, collapse = ","))

    print("Pulling catch data. This can take up to ~ 30 seconds.")
    # Pull data from the warehouse
    DataPull <- try(jsonlite::fromJSON(UrlText))
    if(!is.data.frame(DataPull)) {
         stop(cat("\nNo data returned by the warehouse for the filters given.\n Make sure the year range is correct for the project selected, \n otherwise there may be no data for this species from this project.\n"))
    }


    Data <- rename_columns(DataPull, newname = c("Year", "Vessel", "Tow", "Project", "Scientific_name", "CPUE_kg_per_ha", "Subsample_count", "Subsample_wt_kg", "Total_sp_numbers", "Total_sp_wt_kg"))

    Data <- Data[, c("Year", "Vessel", "Tow", "Project", "Scientific_name", "CPUE_kg_per_ha",  "Subsample_count", "Subsample_wt_kg", "total_catch_numbers", "total_catch_wt_kg")]


    # Pull all tow data (includes tows where the species was not observed)
    Vars <- c("project", "year", "vessel", "pass", "tow", "date_dim$full_date", "depth_m", "longitude_dd", "latitude_dd", "area_swept_ha_der", "trawl_id")

    UrlText <- paste0("https://www.nwfsc.noaa.gov/data/api/v1/source/trawl.operation_haul_fact/selection.json?filters=project=", paste(strsplit(project, " ")[[1]], collapse = "%20"),",", 
               "actual_station_design_dim$stn_invalid_for_trawl_date_whid=0,", 
               "performance=Satisfactory,", "depth_ftm>=30,depth_ftm<=700,", 
               "date_dim$year>=", YearRange[1], ",date_dim$year<=", YearRange[2], 
               "&variables=", paste0(Vars, collapse = ","))

    All.Tows <- jsonlite::fromJSON(UrlText)

    All.Tows <- rename_columns(All.Tows, newname = c("Project", "Trawl_id", "Year", "Pass", "Vessel", "Tow", "Date", "Depth_m", "Longitude_dd",  "Latitude_dd", "Area_Swept_ha"))

    All.Tows <- All.Tows[!duplicated(paste(All.Tows$Year, All.Tows$Pass, All.Tows$Vessel, All.Tows$Tow)), 
     			c("Project", "Trawl_id", "Year", "Pass", "Vessel", "Tow", "Date", "Depth_m", "Longitude_dd", "Latitude_dd", "Area_Swept_ha")]

    # Link each data set together based on trawl_id
    Out <- match.f(All.Tows, Data, c("Year", "Vessel", "Tow"), c("Year", "Vessel", "Tow"), c("Scientific_name", "CPUE_kg_per_ha", "Subsample_count", "Subsample_wt_kg", "total_catch_numbers", "total_catch_wt_kg"))
    
    # Fill in zeros where needed
    Out$total_catch_wt_kg[is.na(Out$total_catch_wt_kg)] <- 0

    Out$CPUE_kg_per_ha[is.na(Out$CPUE_kg_per_ha)] <- 0

    Out$Subsample_count[is.na(Out$Subsample_count)] <- 0

    Out$Subsample_wt_kg[is.na(Out$Subsample_wt_kg)] <- 0

    Out$total_catch_numbers[is.na(Out$total_catch_numbers)] <- 0

    # Need to check what this is doing
    noArea = which(is.na(Out$Area_Swept_ha))
    if (length(noArea) > 0) { 
    	print(cat("\nThere are", length(noArea), "records with no area swept calculation. These record will be filled with the mean swept area across all tows.\n"))
    	print(Out[noArea,c("Trawl_id", "Year", "Area_Swept_ha", "CPUE_kg_per_ha", "total_catch_numbers")])
    	Out$Area_Swept_ha[noArea,] <- mean(Out$Area_Swept_ha, trim = 0.05, na.rm = TRUE)
    }
    
    # Scientific Name is missing after the matching when Total_sp_wt_kg is zero  
    Out$Scientific_name <- Species

    Out$Date <- chron(format(as.POSIXlt(Out$Date, format = "%Y-%m-%dT%H:%M:%S"), "%Y-%m-%d"), format = "y-m-d", out.format = "YYYY-m-d")
    
    Out$Project <- projectShort

    Out$Trawl_id = as.character(Out$Trawl_id)

    # Convert the CPUE into km2 
    Out$cpue_kg_km2 = Out$CPUE_kg_per_ha / 100
    remove = "CPUE_kg_per_ha"
    Out = Out[,!(names(Out) %in% remove)]
    
    if(SaveFile){
        time = Sys.time()
        time = substring(time, 1, 10)
        save(Out, file = paste0(Dir, "/Catch_", time, "_", SurveyName, "_",  time, ".rda"))
        print(paste("Catch data file saved to following location:", Dir))
    }

    return(Out)
}

