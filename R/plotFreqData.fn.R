#' This function plots frequency data as bubble plots
#' You may want to change all zeros to NA's so that those observations are not plotted.
#' If you don't then set zero2NAs=F
#' 
#' @param dir directory to save files to
#' @param dat object created by SS3LF.fn or SS3AF.fn
#' @param inch input to the symbols plot: TRUE, FALSE or a positive number.
#' @param ylab y-axis text label
#' @param xlab x-axis text label
#' @param zero2NAs T/F change 0 values to NA
#' @param main main plot text
#' @param xlim x-limit values
#' @param dopng save the plot as a png inside plots folder
#' @param ...      Additional arguments for the plots
#'
#' @author Allan Hicks and Chantel Wetzel
#' @export

PlotFreqData.fn <- function(dir = NULL, dat, inch=0.15, ylab="Bins", xlab="Year", zero2NAs=T, main=NULL, 
                            xlim=NULL, ymax = NULL, dopng = FALSE, w = 7, h = 7, ...) {

    dataType = sum(names(dat) == "ageErr")
    dataType = ifelse(dataType == 0,  "Length", "Age")

    if (dopng) { 
      if(is.null(dir)){ stop("Directory needs to be set.") }
      if (!file.exists(dir)) { stop("The dir argument leads to a location", ",\ni.e., ", dir, ", that doesn't exist.") }

      plotdir <- file.path(dir, paste("plots", sep=""))
      plotdir.isdir <- file.info(plotdir)$isdir
      if(is.na(plotdir.isdir) | !plotdir.isdir){
        dir.create(plotdir)}
      if ( is.null(main)) { png( file.path(dir, paste("plots/", dataType, "_Frequency.png", sep = "")), height=h, width=w, units="in",res=300) }
      if (!is.null(main)) { png( file.path(dir, paste("plots/", main, "_", dataType,"_Frequency.png", sep = "")), height=h, width=w, units="in",res=300) }
    }

    x <- as.numeric(as.character(dat$year))
    sex <- dat$sex[1]
    if(dataType == "Length"){ dat <- dat[,-c(1:6)] }
    if(dataType == "Age")   { dat <- dat[,-c(1:9)] }

    if(length(grep(".999",names(dat))>0)){
      # remove extra columns (if the user didn't remove them already)
      if(sex == 0) {
        dat <- dat[,-match("U.999",names(dat))]
      }
      if(sex == 3) {
        # exclude columns for fish below minimum bin
        dat <- dat[,-match("F.999",names(dat))]
        dat <- dat[,-match("M.999",names(dat))]
      }
    }
    
    numLens <- ncol(dat)/2 
    if (!is.null(ymax)) { 
      numLens <- ymax
    }

    y <- as.numeric(substring(names(dat),2))
    y <- y[1:numLens]

    if(zero2NAs) {dat[dat==0] <- NA}

    if(is.null(xlim)) {xlim <- range(x)}


    if (sex == 3) { par(mfrow=c(2,1)) } 
    if (sex == 0) { par(mfrow=c(1,1)) } 
    
    if(sex == 0) {
        if(is.null(main)) { main <- "Unsexed+Males+Females" }
        z <- c(unlist(dat[,1:numLens]),min(dat)) # Changed from max to min to better visualize subset data
        symbols(c(rep(x,length(y)),0),c(rep(y,each=length(x)),0),circles=z,
          main=main,inches=inch,xlab=xlab,ylab=ylab,xlim=xlim,...)
    }
    if(sex == 3) {
        name <- "Female"
        z <- c(unlist(dat[,1:numLens]), min(dat))
        symbols(c(rep(x,length(y)),0),c(rep(y,each=length(x)),0),circles=z,main=name,inches=inch,xlab=xlab,ylab=ylab,xlim=xlim,...)
        name <- "Male"
        z <- c(unlist(dat[,(numLens+1):ncol(dat)]), min(dat))
        symbols(c(rep(x,length(y)),0),c(rep(y,each=length(x)),0),circles=z,main=name,inches=inch,xlab=xlab,ylab=ylab,xlim=xlim,...)
    }
    if (dopng) { dev.off()}
}
