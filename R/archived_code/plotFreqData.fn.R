#' This function plots frequency data as bubble plots
#' You may want to change all zeros to NA's so that those observations are not plotted.
#' If you don't then set zero2NAs=F
#' 
#' @param dir directory to save files to
#' @param dat object created by SS3LF.fn or SS3AF.fn
#' @param survey survey name 
#' @param inch input to the symbols plot: TRUE, FALSE or a positive number.
#' @param ylab y-axis text label
#' @param xlab x-axis text label
#' @param zero2NAs T/F change 0 values to NA
#' @param main main plot text
#' @param xlim x-limit values
#' @param dopng save the plot as a png inside plots folder
#'
#' @author Allan Hicks and Chantel Wetzel
#' @export

plotFreqData.fn <- function(dir, dat, survey = "Survey", inch=0.15, ylab="Bins", xlab="Year", zero2NAs=T, main=NULL, xlim=NULL, dopng = FALSE, ...) {

    plotdir <- paste0(dir, "/plots")
    plotdir.isdir <- file.info(plotdir)$isdir
    if(is.na(plotdir.isdir) | !plotdir.isdir){
      dir.create(plotdir)
    }

    dataType = sum(names(dat) == "ageErr")
    dataType = ifelse(dataType == 0,  "Length", "Age")

    x <- as.numeric(as.character(dat$year))
    gender <- dat$gender[1]
    if(dataType == "Length"){ dat <- dat[,-c(1:6)] }
    if(dataType == "Age")   { dat <- dat[,-c(1:9)] }

    if(length(grep(".999",names(dat))>0)){
      # remove extra columns (if the user didn't remove them already)
      if(gender==0) {
        #dat <- dat[,1:(ncol(dat)/2)]
        #dat <- dat[,-match("U.999.1",names(dat))]
        #print(names(dat))
        dat <- dat[,-match("U.999",names(dat))]
        #print(names(dat))
      }
      if(gender==3) {
        # exclude columns for fish below minimum bin
        dat <- dat[,-match("F.999",names(dat))]
        dat <- dat[,-match("M.999",names(dat))]
      }
    }
    
    numLens <- ncol(dat)/2
    y <- as.numeric(substring(names(dat),2))
    y <- y[1:numLens]

    if(zero2NAs) {dat[dat==0] <- NA}

    if(is.null(xlim)) {xlim <- range(x)}

    if (dopng) { png(paste0(dir,"/plots/", survey, "_", dataType,"_Frequency.png"), height=7, width=7, units="in",res=300) }
    if (gender == 3) { par(mfrow=c(2,1)) } 
    if (gender == 0) { par(mfrow=c(1,1)) } 
    
    if(gender==0) {
        if(is.null(main)) {main <- "Unsexed+Males+Females"}
        z <- c(unlist(dat[,1:numLens]),max(dat))
        symbols(c(rep(x,length(y)),0),c(rep(y,each=length(x)),0),circles=z,main=main,inches=inch,xlab=xlab,ylab=ylab,xlim=xlim,...)
    }
    if(gender==3) {
        if(is.null(main[1])) {main <- "Female"}
        z <- c(unlist(dat[,1:numLens]),max(dat))
        symbols(c(rep(x,length(y)),0),c(rep(y,each=length(x)),0),circles=z,main=main[1],inches=inch,xlab=xlab,ylab=ylab,xlim=xlim,...)
        if(is.na(main[2])) {main <- "Male"}
        z <- c(unlist(dat[,(numLens+1):ncol(dat)]),max(dat))
        symbols(c(rep(x,length(y)),0),c(rep(y,each=length(x)),0),circles=z,main=main,inches=inch,xlab=xlab,ylab=ylab,xlim=xlim,...)
    }
    if (dopng) { dev.off()}
}
