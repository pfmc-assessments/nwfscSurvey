#' Pull haul data from the NWFSC data warehouse
#' The website is: https://www.nwfsc.noaa.gov/data
#' This function can be used to pull haul data and associated covariates
#'
#' @param YearRange range of years to pull data. Defaults to all years, 1977 - present.
#' @param SurveyName survey to pull the data for the options are: Triennial, AFSC.Slope, NWFSC.Combo, NWFSC.Slope, NWFSC.Shelf, NWFSC.Hypoxia, NWFSC.Santa.Barb.Basin, NWFSC.Shelf.Rockfish, NWFSC.Video. If NULL, all are used.
#' @param SaveFile option to save the file to the directory
#' @param Dir directory where the file should be saved
#' @param verbose opt to print out message statements
#'
#' @return Returns a data frame of haul characteristics for satisfactory hauls
#' @author Eric Ward
#' @export
#'
#' @import jsonlite
#' @import chron
#'
#' @examples
#'\dontrun{
#' haul_dat = PullHaul.fn(SurveyName = "NWFSC.Combo", YearRange=c(2003,2007))
#' haul_dat = PullHaul.fn()
#'}

PullHaul.fn <- function (YearRange = c(1000, 5000), SurveyName = NULL, SaveFile = FALSE, Dir = NULL, verbose = TRUE)
{
  # increase the timeout period to avoid errors when pulling data
  options(timeout= 4000000)

  if(SaveFile){
    if(is.null(Dir)){
      stop("The Dir input needs to be specified in order to save output file.")
    }
  }

  surveys = createMatrix()

  if(is.null(SurveyName)) {
    SurveyName = surveys[,1]
  }

  if(length(SurveyName)==1) {
  if (!SurveyName %in% surveys[,1]) {
    stop(cat("The SurveyName does not match one of the available options:", surveys[,1])) }
  } else {
    if (length(which(SurveyName %in% surveys[,1])) != length(SurveyName)) {
      stop(cat("One or more of the SurveyName fields does not match one of the available options:", surveys[,1])) }
  }

  project=""
  for(i in 1:dim(surveys)[1]){
    if(length(which(SurveyName %in% surveys[i,1]) > 0)){
      project = c(project, surveys[i,2])
    }
  }
  project = project[which(project%in%c("","NA")==FALSE)]

  if (length(YearRange) == 1) {
    YearRange <- c(YearRange, YearRange)    }

  Vars <- c("area_swept_ha_der", "date_dim.year", "date_yyyymmdd",
    "depth_hi_prec_m","door_width_m_der","fluorescence_at_surface_mg_per_m3_der",
    "gear_end_latitude_dd","gear_end_longitude_dd","gear_start_latitude_dd",
    "gear_start_longitude_dd","invertebrate_weight_kg","latitude_dd","leg",
    "longitude_dd","net_height_m_der","net_width_m_der","nonspecific_organics_weight_kg",
    "o2_at_gear_ml_per_l_der","pass","performance","project","salinity_at_gear_psu_der",
    "sampling_end_hhmmss","sampling_start_hhmmss",
    "target_station_design_dim.stn_invalid_for_trawl_date_whid",
    "temperature_at_gear_c_der","temperature_at_surface_c_der",
    "trawl_id","turbidity_ntu_der","vertebrate_weight_kg","vessel",
    "vessel_end_latitude_dd","vessel_end_longitude_dd",
    "vessel_start_latitude_dd","vessel_start_longitude_dd")

  project_str = NA
  for(i in 1:length(project)) {
    project_str[i] = paste(strsplit(project, " ")[[i]], collapse = "%20")
  }

  # Note: this string grabs data from all projects. Projects filtered below
  UrlText  <- paste0(
    "https://www.nwfsc.noaa.gov/data/api/v1/source/trawl.operation_haul_fact/selection.json?filters=",
    "station_invalid=0,",
    "performance=Satisfactory,",
    "year>=",  YearRange[1], ",year<=", YearRange[2],
    "&variables=", paste0(Vars, collapse = ","))

  DataPull = NULL
  if (verbose){
    message("Pulling haul data. This can take up to ~ 30 seconds.")}
  Data <- try(jsonlite::fromJSON(UrlText))

  # filter projects
  Data = Data[which(Data$project %in% project == TRUE),]

  if(SaveFile){
    time <- Sys.time()
    time <- substring(time, 1, 10)
    save(Data, file = file.path(Dir, paste("Haul_", outName, "_", SurveyName, "_",  time, ".rda", sep="")))
    if (verbose){
      message(paste("Haul data file saved to following location:", Dir)) }
  }

  return(Data)
}




