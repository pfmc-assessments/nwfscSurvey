GetLFsFromExcel.fn <-
function(file,sheet="LengthComps",headerRow,lgthBins=1,gender=3,nSamps="EnterNsamps",fleet="EnterFleet",season=1,partition=0,NAs2zero=T,sexRatioUnsexed=NA,maxSizeUnsexed=NA) {
    len <- readInExcelLengthComps.fn(file,sheet=sheet,headerRow=headerRow)
    SS3LF.fn(len,lgthBins=lgthBins,gender=gender,nSamps=nSamps,fleet=fleet,season=season,partition=partition,NAs2zero,sexRatioUnsexed=NA,maxSizeUnsexed=NA)
}
