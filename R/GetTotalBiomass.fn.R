GetTotalBiomass.fn <-
function(file,headerRow,fleet="EnterFleet",season=1,outputMedian=T) {
    #a wrapper for to output the biomass in SS3 format, reading in from a csv file
    bio <- readInBiomass.fn(file,headerRow)
    SS3Biomass.fn(bio,fleet,season)
}
