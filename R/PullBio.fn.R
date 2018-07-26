#' Pull biological data from the NWFSC data warehouse
#' The website is: https://www.nwfsc.noaa.gov/data
#' Curently, this function only pulls data for a single specified survey.
#' 
#' @param Name  common name of species data to pull from the data warehouse
#' @param SciName scientific name of species data to pull from the data warehouse
#' @param YearRange range of years to pull data
#' @param SurveyName survey to pull the data for the options are: Triennial, AFSC.Slope, NWFSC.Combo, NWFSC.Slope, NWFSC.Shelf, NWFSC.Hypoxia, NWFSC.Santa.Barb.Basin, NWFSC.Shelf.Rockfish, NWFSC.Video
#' @param SaveFile option to save the file to the directory
#' @param Dir directory where the file should be saved
#' 
#' @author Chantel Wetzel based on code by John Wallace
#' @export
#'
#' @import jsonlite
#' @import chron


PullBio.fn <- function (Name = NULL, SciName = NULL, YearRange = c(1000, 5000), SurveyName = NULL, SaveFile = FALSE, Dir = NULL) 
{

    if(SaveFile){
        if(is.null(Dir)){
            stop("The Dir input needs to be specified in order to save output file.")
        }
    }

    if (is.null(Name)) { var.name = "scientific_name"; Species = SciName; new.name = "Scientific_name"; outName = Name}
    if (is.null(SciName)) { var.name = "common_name"; Species = Name; new.name = "Common_name"; outName = SciName}
    if (is.null(SciName) & is.null(Name)) { stop("Need to specifiy Name or SciName to pull data!")}


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

    if (!SurveyName %in% surveys[,1]) { 
         stop(paste("The SurveyName does not match one of the available options:", surveys[,1])) }

    for(i in 1:dim(surveys)[1]){
        if(SurveyName == surveys[i,1]){ 
            project = surveys[i,2]
            projectShort = surveys[i,1]
        }
    }

    if (length(YearRange) == 1) {
        YearRange <- c(YearRange, YearRange)    }

    Vars <- c("project", "trawl_id", var.name, "year", "vessel", "pass", 
              "tow", "date_dim$full_date", "depth_m", "weight_kg", 
              "length_cm", "sex", "age_years", "latitude_dd", "longitude_dd")

    UrlText  <- paste0(
                "https://www.nwfsc.noaa.gov/data/api/v1/source/trawl.individual_fact/selection.json?filters=project=", paste(strsplit(project, " ")[[1]], collapse = "%20"),",", 
                "actual_station_design_dim$stn_invalid_for_trawl_date_whid=0,", 
                "performance=Satisfactory,", "depth_ftm>=30,depth_ftm<=700,", 
                "field_identified_taxonomy_dim$", var.name, "=", paste(strsplit(Species, " ")[[1]], collapse = "%20"), 
                ",date_dim$year>=",  YearRange[1], ",date_dim$year<=", YearRange[2], "&variables=", 
                paste0(Vars, collapse = ","))

    print("Pulling biological data. This can take up to ~ 30 seconds.")
    DataPull <- try(jsonlite::fromJSON(UrlText))
    if(!is.data.frame(DataPull)) {       
         stop(cat("\nNo data returned by the warehouse for the filters given.\n Make sure the year range is correct for the project selected and the input name is correct, \n otherwise there may be no data for this species from this project.\n"))
    }

    Data <- rename_columns(DataPull, newname = c("Trawl_id", "Year", "Vessel", "Project", "Pass", new.name, "Tow", "Date", "Depth_m", "Weight", "Length_cm", "Sex", "Age", "Latitude_dd", "Longitude_dd"))

    Data <- Data[, c("Trawl_id", "Year", "Vessel", "Project", "Pass", "Tow", "Date", "Depth_m", new.name, "Weight", "Length_cm", "Sex", "Age", "Latitude_dd", "Longitude_dd")]

    Data$Date <- chron::chron(format(as.POSIXlt(Data$Date, format = "%Y-%m-%dT%H:%M:%S"), "%Y-%m-%d"), format = "y-m-d", out.format = "YYYY-m-d")

    Data$Trawl_id = as.character(Data$Trawl_id)

    Data$Project = projectShort


    Data$Depth_m   <- as.numeric(as.character(Data$Depth_m))
    Data$Length_cm <- as.numeric(as.character(Data$Length_cm))
    Data$Age       <- as.numeric(as.character(Data$Age))

    if(SaveFile){
        time = Sys.time()
        time = substring(time, 1, 10)
        save(Data, file = paste0(Dir, "/Bio_", time, "_", outName, "_", SurveyName, "_",  time, ".rda"))
        print(paste("Biological data file saved to following location:", Dir))
    }

    return(Data)
}




