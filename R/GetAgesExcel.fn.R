GetAgesExcel.fn <-
function(file,sheet="AgeComps",headerRow,lgthBins=1,ageBins=1,nSamps="EnterNsamps",fleet="EnterFleet",season=1,partition=0,ageerr="EnterAgeErr",raw=T) {
    age <- readInExcelAgeComps.fn(file,sheet=sheet,headerRow=headerRow)
    SS3AgeAtLen.fn(age,lgthBins=lgthBins,ageBins=ageBins,fleet=fleet,season=season,partition=partition,ageerr=ageerr,raw=raw)
}
