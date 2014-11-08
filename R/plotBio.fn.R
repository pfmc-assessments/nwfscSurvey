plotBio.fn <-
function(bio,CI=0.95,scalar=1e6,gap=0.03,ylab="Biomass ('000 mt)",xlab="Year",ylim=NULL,...) {
    #Plots the biomass with confidence intervals
    #uses data in the format of SS3, so you can use the GetTotalBiomass.fn or your own data.frame
    #scalar is simply the divisor for the biomass
    #gap is a value that introduces a slight gap between the point estimate and the start of the line for the CI
    # careful because a gap too large will invert the CI, making it look huge. You should know when this happens
    y <- as.numeric(as.character(bio$Value/scalar))
    x <- as.numeric(as.character(bio$Year))
    se <- as.numeric(as.character(bio$seLogB))
    logB <- log(bio$Value)
    ci <- exp(rbind(c(logB+qnorm(1-(1-CI)/2)*se),c(logB-qnorm(1-(1-CI)/2)*se)))/scalar
    if(is.null(ylim)) {
        ylim <- c(0,1.05*max(ci,na.rm=TRUE))
    }
    
    gap <- gap*max(y)
    plot(x,y,,ylab=ylab,xlab=xlab,ylim=ylim,...)
    segments(x,y+gap,x,ci[1,])
    segments(x,y-gap,x,ci[2,])
}
