#' Pull biological data from the NWFSC data warehouse
#' The website is: https://www.nwfsc.noaa.gov/data
#' This function can be used to pull a single species or all observed species
#' In order to pull all species leave Name = NULL and SciName = NULL
#'
#' @param Name  common name of species data to pull from the data warehouse
#' @param SciName scientific name of species data to pull from the data warehouse
#' @param YearRange range of years to pull data
#' @param SurveyName survey to pull the data for the options are: Triennial, AFSC.Slope, NWFSC.Combo, NWFSC.Slope, NWFSC.Shelf, NWFSC.Hypoxia, NWFSC.Santa.Barb.Basin, NWFSC.Shelf.Rockfish, NWFSC.Video
#' @param SaveFile option to save the file to the directory
#' @param Dir directory where the file should be saved
#' @param verbose opt to print out message statements
#'
#' @author Chantel Wetzel based on code by John Wallace
#' @export
#'
#' @import jsonlite
#' @import chron
#' @import dplyr
#'
#' @examples
#'\dontrun{
#' # SurveyName is only arg that has to be specified
#' bio_dat = PullBio.fn(SurveyName = "NWFSC.Combo")
#'}

PullBio.fn <- function (Name = NULL, SciName = NULL, YearRange = c(1000, 5000), SurveyName = NULL, SaveFile = FALSE, Dir = NULL, verbose = TRUE)
{
    # increase the timeout period to avoid errors when pulling data
    options(timeout= 4000000)

    if(SaveFile){
        if(is.null(Dir)){
            stop("The Dir input needs to be specified in order to save output file.")
        }
    }

    if (is.null(Name)) { var.name = "scientific_name"; Species = SciName; new.name = "Scientific_name"; outName = Name}
    if (is.null(SciName)) { var.name = "common_name"; Species = Name; new.name = "Common_name"; outName = SciName; outName = "All"}
    if (is.null(SciName) & is.null(Name)) { var.name = "scientific_name"; Species = "pull all"; new.name = "Scientific_name"}#stop("Need to specifiy Name or SciName to pull data!")}

    surveys = createMatrix()

    if (!SurveyName %in% surveys[,1]) {
        stop(cat("The SurveyName does not match one of the available options:", surveys[,1])) }

    for(i in 1:dim(surveys)[1]){
        if(SurveyName == surveys[i,1]){
            project = surveys[i,2]
            projectShort = surveys[i,1]
        }
    }

    if (length(YearRange) == 1) {
        YearRange <- c(YearRange, YearRange)    }

    Vars <- c("project", "trawl_id", var.name, "year", "vessel", "pass",
        "tow", "datetime_utc_iso","depth_m", "weight_kg",
        "length_cm", "width_cm", "sex", "age_years", "latitude_dd", "longitude_dd",
        "standard_survey_dim$standard_survey_age_indicator",
        "standard_survey_dim$standard_survey_length_or_width_indicator",
        "standard_survey_dim$standard_survey_weight_indicator",
        "operation_dim$legacy_performance_code")

    Vars.short = c("project", "trawl_id", var.name, "year", "vessel", "pass",
        "tow", "datetime_utc_iso","depth_m", "weight_kg",
        "length_cm", "width_cm", "sex", "age_years", "latitude_dd", "longitude_dd")

    UrlText  <- paste0(
        "https://www.nwfsc.noaa.gov/data/api/v1/source/trawl.individual_fact/selection.json?filters=project=", paste(strsplit(project, " ")[[1]], collapse = "%20"),",",
        "station_invalid=0,",
        "performance=Satisfactory,",
        "depth_ftm>=30,depth_ftm<=700,",
        "field_identified_taxonomy_dim$", var.name, "=", paste(strsplit(Species, " ")[[1]], collapse = "%20"),
        ",year>=",  YearRange[1], ",year<=", YearRange[2],
        "&variables=", paste0(Vars, collapse = ","))

    if (length(Species) == 1 && Species == "pull all"){
        UrlText  <- paste0(
            "https://www.nwfsc.noaa.gov/data/api/v1/source/trawl.individual_fact/selection.json?filters=project=", paste(strsplit(project, " ")[[1]], collapse = "%20"),",",
            "station_invalid=0,",
            "performance=Satisfactory,", "depth_ftm>=30,depth_ftm<=700,",
            "year>=",  YearRange[1], ",year<=", YearRange[2],
            "&variables=", paste0(Vars, collapse = ","))
    }

    DataPull = NULL
    if (verbose){
        message("Pulling biological data. This can take up to ~ 30 seconds.")}
    DataPull <- try(jsonlite::fromJSON(UrlText))

    if(is.data.frame(DataPull))
    {
        if(SurveyName == "NWFSC.Combo"){
            # Filter out non-standard samples
            keep = DataPull[, "standard_survey_dim$standard_survey_length_or_width_indicator"]  %in% c("NA", "Standard Survey Length or Width")
            DataPull = DataPull[keep,]
            keep = DataPull[, "standard_survey_dim$standard_survey_age_indicator"]  %in% c("NA", "Standard Survey Age")
            DataPull = DataPull[keep,]
            keep = DataPull[, "standard_survey_dim$standard_survey_weight_indicator"]  %in% c("NA","Standard Survey Weight")
            DataPull = DataPull[keep,]
        }

        if(SurveyName == "Triennial"){
            # Remove water hauls
            fix =  is.na(DataPull[,"operation_dim$legacy_performance_code"])
            if(sum(fix) > 0) { DataPull[fix,"operation_dim$legacy_performance_code"] = -999 }
            keep = DataPull[,"operation_dim$legacy_performance_code"] != 8
            DataPull = DataPull[keep,]
        }

        # Remove the extra columns now that they are not needed
        DataPull = DataPull[,Vars.short]
    }


    if (SurveyName == "Triennial"){

        UrlText <- paste0(
            "https://www.nwfsc.noaa.gov/data/api/v1/source/trawl.triennial_length_fact/selection.json?filters=project=",
            paste(strsplit(project, " ")[[1]], collapse = "%20"),",",
            "station_invalid=0,",
            "performance=Satisfactory,",
            "field_identified_taxonomy_dim$", var.name, "=", paste(strsplit(Species, " ")[[1]], collapse = "%20"),
            ",year>=",  YearRange[1], ",year<=", YearRange[2],
            "&variables=", paste0(Vars, collapse = ","))

        LenPull <- try(jsonlite::fromJSON(UrlText))

        #Remove water hauls
        fix =  is.na(LenPull[,"operation_dim$legacy_performance_code"])
        if(sum(fix) > 0) { LenPull[fix,"operation_dim$legacy_performance_code"] = -999}
        keep = LenPull[,"operation_dim$legacy_performance_code"] != 8
        LenPull = LenPull[keep,]

        colnames(LenPull)[2]  <- "Date"
        LenPull$Weight <- NA
        LenPull$Age <- NA
        Len = dplyr::rename(LenPull,
                            Trawl_id = trawl_id, Year = year, Vessel = vessel, Project = project,
                            Pass = pass, Tow = tow, Depth_m = depth_m, Length_cm = length_cm,
                            Width_cm = width_cm, Sex = sex, Latitude_dd = latitude_dd, Longitude_dd = longitude_dd)
        names(Len)[which(names(Len)=="scientific_name")] = "Scientific_name"
        names(Len)[which(names(Len)=="common_name")] = "Common_name"

        Len <- Len[, c("Trawl_id", "Year", "Vessel", "Project", "Pass", "Tow", "Date", "Depth_m", new.name, "Weight", "Length_cm", "Width_cm", "Sex", "Age", "Latitude_dd", "Longitude_dd")]
        Len$Date    <- chron::chron(format(as.POSIXlt(Len$Date, format = "%Y-%m-%dT%H:%M:%S"), "%Y-%m-%d"), format = "y-m-d", out.format = "YYYY-m-d")
        Len$Trawl_id  <- as.character(Len$Trawl_id)
        Len$Project   <- projectShort
        Len$Depth_m   <- as.numeric(as.character(Len$Depth_m))
        Len$Length_cm <- as.numeric(as.character(Len$Length_cm))
        Len$Age       <- as.numeric(as.character(Len$Age))
    }

    if (SurveyName == "AFSC.Slope"){ cat("Warning: The data warehouse may not have the AFSC slope data included yet.") }
    if(!is.data.frame(DataPull) & SurveyName != "Triennial") {
        stop(cat("\nNo data returned by the warehouse for the filters given.
            Make sure the year range is correct for the project selected and the input name is correct,
            otherwise there may be no data for this species from this project.\n"))
    }

    Data = NULL
    if (length(DataPull)>0){
        Data = dplyr::rename(DataPull,
                             Trawl_id = trawl_id, Year = year, Vessel = vessel, Project = project, Pass = pass,
                             Tow = tow, Date = datetime_utc_iso, Depth_m = depth_m, Weight = weight_kg,
                             Length_cm = length_cm, Width_cm = width_cm, Sex = sex, Age = age_years,
                             Latitude_dd = latitude_dd, Longitude_dd = longitude_dd)
        names(Data)[which(names(Data)=="scientific_name")] = "Scientific_name"
        names(Data)[which(names(Data)=="common_name")] = "Common_name"

        Data <- Data[, c("Trawl_id", "Year", "Vessel", "Project", "Pass", "Tow", "Date", "Depth_m", new.name, "Weight",
                         "Length_cm", "Width_cm", "Sex", "Age", "Latitude_dd", "Longitude_dd")]
        Data$Date <- chron::chron(format(as.POSIXlt(Data$Date, format = "%Y-%m-%dT%H:%M:%S"), "%Y-%m-%d"), format = "y-m-d", out.format = "YYYY-m-d")
        Data$Trawl_id  <- as.character(Data$Trawl_id)
        Data$Project   <- projectShort
        Data$Depth_m   <- as.numeric(as.character(Data$Depth_m))
        Data$Length_cm <- as.numeric(as.character(Data$Length_cm))
        Data$Age       <- as.numeric(as.character(Data$Age))
    }

    Ages = NULL
    if (SurveyName == "Triennial"){
        if (!is.null(Data)) { Ages <- Data }
        Data <- list()
        Data$Lengths <- Len
        if (!is.null(Ages)) { Data$Ages <- Ages }
        if (is.null(Ages))  { Data$Ages <- "no_ages_available"}
        if (verbose){
            message("Triennial data returned as a list: Data$Lengths and Data$Ages\n") }
    }

    if(SaveFile){
        time <- Sys.time()
        time <- substring(time, 1, 10)
        #save(Data, file = paste0(Dir, "/Bio_", outName, "_", SurveyName, "_",  time, ".rda"))
        save(Data, file = file.path(Dir, paste("Bio_", outName, "_", SurveyName, "_",  time, ".rda", sep="")))
        if (verbose){
            message(paste("Biological data file saved to following location:", Dir)) }
    }

    return(Data)
}
