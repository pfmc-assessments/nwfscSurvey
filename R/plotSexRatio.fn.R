#' Function to plot sex ratio
#'
#' @param dir directory location for saving the png
#' @param dat data object
#' @param fn value to calculate from the data (e.g., median, mean)
#' @param survey specify the survey data set for calculations. Options are "NWFSCBT", "Tri.Shelf", "AFSC.Slope"
#' @param circleSize circle size 
#' @param dopng TRUE/FALSE whether to save a png file
#'
#' @author Allan Hicks and Chantel Wetzel
#' @export

PlotSexRatio.fn <-function(dir, dat, fn=median, survey, circleSize=0.1, dopng = FALSE,...) {

    plotdir <- paste0(dir, "/plots")
    plotdir.isdir <- file.info(plotdir)$isdir
    if(is.na(plotdir.isdir) | !plotdir.isdir){
      dir.create(plotdir)
    }

    
	#if (survey == "NWFSCBT") {
        axis.name = ifelse(is.null(dat$Age), "Length (cm)", "Age")
        data.type = ifelse(is.null(dat$Age), "length", "age")
		if (dopng) { png(paste0(dir, "/plots/", survey, "_fraction_female.png"), height=7, width=7, units="in",res=300) }
        if (data.type == "length") { temp =  table(dat$Length_cm, dat$Sex) }
        if (data.type == "age")    { temp =  table(dat$Age, dat$Sex) }

        ratioF = temp[,"F"] / (temp[,"M"] + temp[,"F"])
        nobs = temp[,"F"] + temp[,"M"]
        plot(ratioF,type="l", col="red", xlab=axis.name, ylab="Fraction female",...)
        symbols(ratioF,circles=nobs,inches=circleSize,fg="red",bg=rgb(1,0,0,alpha=0.5),add=T)
    #}

    #if (survey%in%c("Tri.Shelf", "AFSC.Slope")){
    #	axis.name = ifelse(is.null(dat$AGE), "Length (cm)", "Age")
    #	data.type = ifelse(is.null(dat$AGE), "length", "age")
    #	if (dopng) { png(paste0(dir, "/plots/", survey, "_fraction_female_by_",data.type,".png"), height=7, width=7, units="in",res=300) }
    #	if (data.type == "length") { temp =  table(dat$Length_cm, dat$SEX) }
    #	if (data.type == "age")    { temp =  table(dat$AGE, dat$SEX) }
    #	ratioF = temp[,2] / (temp[,1] + temp[,2])
    #	nobs = temp[,1] + temp[,2]
    #	plot(ratioF,type="l", col="red", xlab=axis.name, ylab="Fraction female",...)
    #	symbols(ratioF,circles=nobs,inches=circleSize,fg="red",bg=rgb(1,0,0,alpha=0.5),add=T)
    #}
    if (dopng) { dev.off()}

}
