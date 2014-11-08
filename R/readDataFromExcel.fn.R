readDataFromExcel.fn <-
function(file,sheet,headerRow) {
    #reads in the data from an Excel sheet
    #the column names of the data are on row number headerRow
    #this function is meant to be used by more specific functions below
    #written in 2009 by Allan Hicks
    #note that in later, 64-bit versions of R, the RODBC package is not working
    require(RODBC)
    channel <- odbcConnectExcel(file)
    info <- sqlFetch(channel,sheet,as.is=T,max=headerRow-2)        #read in info at top of sheet. It always puts the first row as header
    xx <- sqlFetchMore(channel,as.is=T,colnames=T)
    close(channel)
    print(as.data.frame(info[,1]))
    return(xx)
}
