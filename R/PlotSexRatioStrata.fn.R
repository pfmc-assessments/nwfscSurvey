#' Function to plot sex ratio by strata
#'
#' @param dir directory location for saving the png
#' @param dat data object
#' @param type length/age which data type to use
#' @param strat.vars the parameters to stratify the data
#' @param survey specify the survey data set for calculations. Options are "NWFSCBT", "Tri.Shelf", "AFSC.Slope"
#' @param circleSize circle size 
#' @param dopng TRUE/FALSE whether to save a png file
#' @param ...      Additional arguments for the plots
#'
#' @author Allan Hicks and Chantel Wetzel
#' @export
#' @seealso \code{\link{StrataFactors.fn}}

PlotSexRatioStrata.fn <-function(dir, dat, type = "length", strat.vars=c("Depth_m","Latitude_dd"), survey, circleSize=0.05, dopng = FALSE,...) {

    plotdir <- paste0(dir, "/plots")
    plotdir.isdir <- file.info(plotdir)$isdir
    if(is.na(plotdir.isdir) | !plotdir.isdir){
      dir.create(plotdir)
    }

    row.names(strat.df) <- strat.df[,1]     #put in rownames to make easier to index later
    numStrata <- nrow(strat.df)
    ind <- !duplicated(dat$Trawl_id)
    datB <- dat[ind,c("Trawl_id", "Weight", strat.vars, "Longitude_dd", "Year", "Length_cm", "Age", "Sex")]   

    datB   <- data.frame(datB, stratum = StrataFactors.fn(datB, strat.vars, strat.df))        #create a new column for the stratum factor
    
    if (dopng) { pdf(paste0(dir, "/plots/", survey,"_fraction_female.pdf") ) }
    par(mfrow = c(3,3))

    for(i in 1:length(row.names(strat.df))){
        #if (dopng) { png(paste0(dir, "/plots/", survey,"_", row.names(strat.df)[i], "_fraction_female.png"), height=7, width=7, units="in",res=300) }
        #par(mfrow = c(5,3))
        z = which(datB$stratum == row.names(strat.df)[i])
        subDF = datB[z,]

        if (type == "length") { temp =  table(subDF$Length_cm, subDF$Sex); axis.name = "Length (cm)" }
        if (type == "age")    { temp =  table(subDF$Age, subDF$Sex); axis.name = "Age" }

        ratioF = temp[,"F"] / (temp[,"M"] + temp[,"F"])
        nobs = temp[,"F"] + temp[,"M"]
        plot(ratioF,type="l", col="red", xlab=axis.name, ylab="Fraction female", main = row.names(strat.df)[i], ylim = c(0,1))# ,...)
        symbols(ratioF, circles=nobs, inches=circleSize, fg="red",bg=rgb(1,0,0,alpha=0.5),add=T)
        #if (dopng) {dev.off()}
    }
    if (dopng) {dev.off()}

}
