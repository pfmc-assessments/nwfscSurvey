#' Reads in the data from an Excel sheet
#' the column names of the data are on row number headerRow
#' this function is meant to be used by more specific functions below
#' originally written in 2009 by Allan Hicks
#' note that in later, 64-bit versions of R, the RODBC package is not working
#' updated in 2018 by Chantel Wetzel to use the readxl package
#'
#'
#' @param file excel file
#' @param sheet sheet name
#' @param headerRow line of header row in excel file
#'
#' @author Allan Hicks & Chantel Wetzel
#' @export 
#' 
#' @import readxl

readDataFromExcel.fn <-function(file, sheet, headerRow) {

    #require(RODBC)
    #channel <- odbcConnectExcel(file)
    #info <- sqlFetch(channel,sheet,as.is=T,max=headerRow-2)        #read in info at top of sheet. It always puts the first row as header
    #xx <- sqlFetchMore(channel,as.is=T,colnames=T)
    #close(channel)
    #print(as.data.frame(info[,1]))
    #return(xx)

    require(readxl)
    xx = read_excel(file, sheet = sheet, skip = headerRow-1)
    names(xx) <- gsub(" ", "_", names(xx))
    return(xx)
}
