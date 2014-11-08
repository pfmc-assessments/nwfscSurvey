GetAges.fn <-
function(file,headerRow,lgthBins=1,ageBins=1,nSamps="EnterNsamps",fleet="EnterFleet",season=1,partition=0,ageerr="EnterAgeErr",raw=T,sep=",") {
    age <- readInAgeComps.fn(file,headerRow=headerRow,sep=sep)
    SS3AgeAtLen.fn(age,lgthBins=lgthBins,ageBins=ageBins,fleet=fleet,season=season,partition=partition,ageerr=ageerr,raw=raw)
}
