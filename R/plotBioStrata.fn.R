#' Not recommended for use yet
#' Name would imply that it would plot by strata but that is currently not happening
#' Duplicate to the plotBio.fn but not as polished
#' 
#' @param bio object created by the GetTotalBiomass.fn
#' @param CI confidence interval
#' @param scalar simply the divisor for the biomass
#' @param gap  a value that introduces a slight gap between the point estimate and the start of the line for the CI. A gap too large will invert the CI, making it look huge. You should know when this happens
#' @param ylab y-axis text label
#' @param xlab x-axis text label
#' @param ylim y-limits
#'
#' @author Allan Hicks 
#' @export

plotBioStrata.fn <-function(bio, CI=0.95, scalar=1e6, gap=0.03, ylab="Biomass ('000 mt)", xlab="Stratum", ylim=NULL,...) {

    y <- as.numeric(as.character(bio$Value/scalar))
    x <- 1:nrow(bio)
    se <- as.numeric(as.character(bio$seLogB))
    logB <- log(bio$Value)
    ci <- exp(rbind(c(logB+qnorm(1-(1-CI)/2)*se),c(logB-qnorm(1-(1-CI)/2)*se)))/scalar
    if(is.null(ylim)) {
        ylim <- c(0,1.05*max(ci))
    }
    
    gap <- gap*max(y)
    plot(x,y,ylab=ylab,xlab=xlab,ylim=ylim,xaxt="n",...)
    axis(1,at=x,label=as.character(bio$Stratum),padj=0,las=2,cex.axis=0.5)
    segments(x,y+gap,x,ci[1,])
    segments(x,y-gap,x,ci[2,])
}
