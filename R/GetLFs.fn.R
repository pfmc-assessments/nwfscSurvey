GetLFs.fn <-
function(file,headerRow,lgthBins=1,gender=3,nSamps="EnterNsamps",fleet="EnterFleet",season=1,partition=0,NAs2zero=T,sep=",",sexRatioUnsexed=NA,maxSizeUnsexed=NA) {
    len <- readInLengthComps.fn(file,headerRow=headerRow,sep=sep)
    SS3LF.fn(len,lgthBins=lgthBins,gender=gender,nSamps=nSamps,fleet=fleet,season=season,partition=partition,NAs2zero=NAs2zero,sexRatioUnsexed=NA,maxSizeUnsexed=NA)
}
