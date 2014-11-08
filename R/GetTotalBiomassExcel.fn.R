GetTotalBiomassExcel.fn <-
function(file,sheet="BiomassAbundance",headerRow,fleet="EnterFleet",season=1,outputMedian=T) {
    #a wrapper for to output the biomass in SS3 format, reading directly from Excel
    bio <- readInExcelBiomass.fn(file,sheet,headerRow)
    SS3Biomass.fn(bio,fleet,season)
}
