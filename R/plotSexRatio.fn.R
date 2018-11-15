#' Function to plot sex ratio
#'
#' @param dir directory location for saving the png
#' @param dat data object
#' @param data.type "length" or "age" 
#' @param survey specify the survey data set for calculations. Options are "NWFSCBT", "Tri.Shelf", "AFSC.Slope"
#' @param circleSize circle size 
#' @param dopng TRUE/FALSE whether to save a png file
#' @param ...      Additional arguments for the plots
#'
#' @author Allan Hicks and Chantel Wetzel
#' @export

PlotSexRatio.fn <-function(dir, dat, data.type = "length",  survey, circleSize=0.1, dopng = FALSE,...) {
    
	if (dopng) { 
        #plotdir <- paste0(dir, "/plots")
        plotdir <- file.path(dir, paste("plots", sep=""))
        plotdir.isdir <- file.info(plotdir)$isdir
        if(is.na(plotdir.isdir) | !plotdir.isdir){
            dir.create(plotdir) }
        png(paste0(dir, "/plots/", survey, "_", data.type, "_fraction_female.png"), height=7, width=7, units="in",res=300) }

    if (data.type == "length") { temp =  table(dat$Length_cm, dat$Sex); axis.name = "Length (cm)" }
    if (data.type == "age")    { temp =  table(dat$Age, dat$Sex); axis.name = "Age" }

    ratioF = temp[,"F"] / (temp[,"M"] + temp[,"F"])
    nobs = temp[,"F"] + temp[,"M"]
    plot(x = names(ratioF), y = ratioF, type="l", col="red", xlab=axis.name, ylab="Fraction female",...)
    symbols(x = names(ratioF), y = ratioF, circles=nobs, inches=circleSize, fg="red", bg=rgb(1,0,0,alpha=0.5), add=T)

    if (dopng) { dev.off()}

}
