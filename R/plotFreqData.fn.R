plotFreqData.fn <- function(dat,inch=0.15,ylab="Bins",xlab="Year",zero2NAs=T,main=NULL,xlim=NULL,...) {
    #This function plots frequency data as bubble plots
    #You may want to change all zeros to NA's so that those observations are not plotted.
    #   If you don't then set zero2NAs=F
    x <- as.numeric(as.character(dat$year))
    gender <- dat$gender[1]
    dat <- dat[,-c(1:6)]
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
    
    if(gender==0) {
        if(is.null(main)) {main <- "Unsexed+Males+Females"}
        z <- c(unlist(dat[,1:numLens]),max(dat))
        symbols(c(rep(x,length(y)),0),c(rep(y,each=length(x)),0),circles=z,main=main,inches=inch,xlab=xlab,ylab=ylab,xlim=xlim,...)
    }
    if(gender==3) {
        if(is.null(main)) {main <- "Female"}
        z <- c(unlist(dat[,1:numLens]),max(dat))
        symbols(c(rep(x,length(y)),0),c(rep(y,each=length(x)),0),circles=z,main=main,inches=inch,xlab=xlab,ylab=ylab,xlim=xlim,...)
        if(is.null(main)) {main <- "Male"}
        z <- c(unlist(dat[,(numLens+1):ncol(dat)]),max(dat))
        symbols(c(rep(x,length(y)),0),c(rep(y,each=length(x)),0),circles=z,main=main,inches=inch,xlab=xlab,ylab=ylab,xlim=xlim,...)
    }
}
