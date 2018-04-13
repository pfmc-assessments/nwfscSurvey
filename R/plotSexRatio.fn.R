#' Function to plot sex ratio
#'
#' @param len data object
#' @param fn value to calculate from the data (e.g., median, mean)
#' @param circleSize circle size 
#'
#' @author Allan Hicks 
#' @export

plotSexRatio.fn <-function(len, fn=median, circleSize=0.1,...) {
    ratioF <- len$NumF/(len$NumF+len$NumM)
    yF <- lapply(split(ratioF,floor(len$Length)),fn,na.rm=TRUE)
    x <- names(split(ratioF,floor(len$Length)))
    nobs <- unlist(lapply(split(ratioF,floor(len$Length)),length))
    plot(x,yF,type="l",col="red",xlab="Length (cm)",ylab="Fraction female",...)
    symbols(x,yF,circles=nobs,inches=circleSize,fg="red",bg=rgb(1,0,0,alpha=0.5),add=T)
    return(invisible(data.frame(length=x,fraction.female=as.numeric(yF))))
}
