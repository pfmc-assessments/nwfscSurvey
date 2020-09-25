#' Rename columns in the AKFSC Slope data
#' 
#' 
#' 
#' 
#' @param dir directory
#' @param datTows input catch data frame
#' @param datL input biology list
#' @param start.year first year of data to include 
#' 
#' @author Chantel Wetzel 
#' @export


Format.AKSlope.fn <- function (dir = NULL, datTows, datL = NA, start.year = 1997) 
{

	# Filter for only the AKFSC Slope survey
	if ("SURVEY" %in% colnames(datTows)){
		datTows = datTows[datTows$SURVEY=="AFSC.Slope",]
	} else datTows$SURVEY = "AFSC.Slope"
	datTows = datTows[datTows$YEAR >= start.year, ]

	#Deal with the catch data file
	names(datTows)[names(datTows) == "HAULJOIN"] = "Trawl_id"
	names(datTows)[names(datTows) == "YEAR"] = "Year"
	names(datTows)[names(datTows) == "BOTTOM_DEPTH"] = "Depth_m"
	names(datTows)[names(datTows) == "WEIGHT"] = "Subsample_wt_kg" # Need to double check units
	names(datTows)[names(datTows) == "NUMBER_FISH"] = "Subsample_count" 
	names(datTows)[names(datTows) == "VESSEL"] = "Vessel" 

	datTows$Project = "AK.Slope"
	datTows$Pass = datTows$Tow = NA
	datTows$Date = paste0(datTows$Year, "-", datTows$MONTH, "-", datTows$DAY)

	datTows$Latitude_dd = (datTows$START_LATITUDE + datTows$END_LATITUDE) / 2
	datTows$Longitude_dd = (datTows$START_LONGITUDE + datTows$END_LONGITUDE) / 2


	datTows$total_catch_numbers = datTows$Subsample_count
	datTows$total_catch_wt_kg = datTows$Subsample_wt_kg
	datTows$Area_Swept_ha = (datTows$DISTANCE_FISHED*datTows$NET_WIDTH) / 10 #area swept for each tow in hectare
	datTows$cpue_kg_km2 = 100 * datTows$Subsample_wt_kg / datTows$Area_Swept_ha

	datTows <- datTows[, c("Project", "Trawl_id", "Year", "Pass", "Vessel", "Tow", "Date", "Depth_m", "Longitude_dd", "Latitude_dd", 
							"Area_Swept_ha", "cpue_kg_km2", "Subsample_count", "Subsample_wt_kg",
							"total_catch_numbers",  "total_catch_wt_kg")]
	
	#Deal with the biological length data file

	if ("SURVEY" %in% colnames(datL$Lengths)){
		tmp1 =  datL$Lengths[datL$Lengths$SURVEY=="AFSC.Slope",]
	} else {
		tmp1 = datL$Lengths
		tmp1$Lengths$SURVEY = "AFSC.Slope"
	}
	tmp1 = tmp1[tmp1$YEAR >= start.year, ]

	names(tmp1)[names(tmp1) == "HAULJOIN"] = "Trawl_id"
	names(tmp1)[names(tmp1) == "YEAR"] = "Year"
	names(tmp1)[names(tmp1) == "VESSEL"] = "Vessel" 
	names(tmp1)[names(tmp1) == "BOTTOM_DEPTH"] = "Depth_m"
	names(tmp1)[names(tmp1) == "START_LATITUDE"] = "Latitude_dd"
	names(tmp1)[names(tmp1) == "START_LONGITUDE"] = "Longitude_dd"
	names(tmp1)[names(tmp1) == "SP_TOW_WGHT_KG"] = "Weight" # Need to double check units
	names(tmp1)[names(tmp1) == "LENGTH"] = "Length_cm" 

	tmp1$Sex = tmp1$Age = tmp1$Weight = NA
	tmp1$Sex[tmp1$SEX == 1] = "M" #The akfsc slope sexes were specificied 1= males and 2 = females
	tmp1$Sex[tmp1$SEX == 2] = "F" 
	tmp1$Sex[tmp1$SEX == 3] = "U" 
	tmp1$Length_cm = tmp1$Length_cm / 10

	tmp1$Project = "AK.Slope"
	tmp1$Pass = tmp1$Tow = NA
	tmp1$Date = paste0(tmp1$Year, "-", tmp1$MONTH, "-", tmp1$DAY)


	tmp1 <- tmp1[, c("Project", "Trawl_id", "Year", "Pass", "Vessel", "Tow", "Date", "Depth_m", "Longitude_dd", "Latitude_dd",
					 "Weight", "Length_cm", "Sex", "Age")]

	#Deal with the biological age data file
	if ("SURVEY" %in% colnames(datL$Ages)){
		tmp2 =  datL$Ages[datL$Ages$SURVEY=="AFSC.Slope",]
	} else {
		tmp2 = datL$Ages
		tmp2$Ages$SURVEY = "AFSC.Slope"
	}

	tmp2 =  datL$Ages[datL$Ages$SURVEY=="AFSC.Slope",]

	if(dim(tmp2)[1] > 0) {
		tmp2 = tmp2[tmp2$YEAR >= start.year, ]
	
		names(tmp2)[names(tmp2) == "HAULJOIN"] = "Trawl_id"
		names(tmp2)[names(tmp2) == "YEAR"] = "Year"
		names(tmp2)[names(tmp2) == "VESSEL"] = "Vessel" 
		names(tmp2)[names(tmp2) == "BOTTOM_DEPTH"] = "Depth_m"
		names(tmp2)[names(tmp2) == "START_LATITUDE"] = "Latitude_dd"
		names(tmp2)[names(tmp2) == "START_LONGITUDE"] = "Longitude_dd"
		names(tmp2)[names(tmp2) == "SP_TOW_WGHT_KG"] = "Weight" # Need to double check units
		names(tmp2)[names(tmp2) == "LENGTH"] = "Length_cm" 
		names(tmp2)[names(tmp2) == "AGE"] = "Age" 
	
		tmp2$Sex = tmp2$Weight = NA
		tmp2$Sex[tmp2$SEX == 1] = "M" #The akfsc slope sexes were specificied 1= males and 2 = females
		tmp2$Sex[tmp2$SEX == 2] = "F" 
		tmp2$Sex[tmp2$SEX == 3] = "U" 
		tmp2$Length_cm = as.numeric(tmp2$Length_cm) / 10
	
		tmp2$Project = "AK.Slope"
		tmp2$Pass = tmp2$Tow = NA
		tmp2$Date = paste0(tmp2$Year, "-", tmp2$MONTH, "-", tmp2$DAY)
	
		tmp2 <- tmp2[, c("Project", "Trawl_id", "Year", "Pass", "Vessel", "Tow", "Date", "Depth_m", "Longitude_dd", "Latitude_dd",
					 "Weight", "Length_cm", "Sex", "Age")]
	}

	out = list()
	out$datTows = datTows
	out$length = tmp1
	out$age = NULL
	if( !is.null(tmp2) ) {
		if( dim(tmp2)[1] > 0) { out$age = tmp2 }
	}
	return(out)
}