#' A wrapper for the plotFreqData 
#' because I originally called it plotLFs.fn and want to keep it compatible with already written analyses
#'
#' @param dat object created by SS3LF.fn or SS3AF.fn
#' @param inch input to the symbols plot: TRUE, FALSE or a positive number.
#' @param ylab y-axis text label
#' @param xlab x-axis text label
#' @param zero2NAs T/F change 0 values to NA
#'
#' @author Allan Hicks 
#' @export
#' @seealso  \code{\link{plotFreqData.fn}}

plotLFs.fn <-function(dat, inch=0.15, ylab="Bins", xlab="Year", zero2NAs=T,...) {
    
    plotFreqData.fn(dat,inch,ylab,xlab,zero2NAs,...)
}
