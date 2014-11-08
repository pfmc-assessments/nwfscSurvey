plotFreqDataStrata.fn <-
function(dat,inch=0.15,ylab="Bins",xlab="",zero2NAs=T,...) {
    #This function plots frequency data as bubble plots when strata instead of years are used
    #You may want to change all zeros to NA's so that those observations are not plotted.
    #   If you don't then set zero2NAs=F
    x <- 1:nrow(dat)
    xlabels <- as.character(dat$year)
    gender <- dat$gender[1]
    dat <- dat[,-c(1:6)]
    if(length(grep(".999",names(dat))>0)){
      # remove extra columns (if the user didn't remove them already)
      if(gender==0) {
        #dat <- dat[,1:(ncol(dat)/2)]
        dat <- dat[,-match("U.999.1",names(dat))]
        dat <- dat[,-match("U.999",names(dat))]
      }
      if(gender==3) {
        dat <- dat[,-match("F.999",names(dat))]
        dat <- dat[,-match("M.999",names(dat))]
      }
    }
    numLens <- ncol(dat)/2
    y <- as.numeric(substring(names(dat),2))
    y <- y[1:numLens]
    if(zero2NAs) {dat[dat==0] <- NA}
    
    if(gender==0) {
        print(dat$year)
        z <- c(unlist(dat[,1:numLens]),max(dat))
        symbols(c(rep(x,length(y)),0),c(rep(y,each=length(x)),0),circles=z,main="Unsexed+Males+Females",inches=inch,xlab=xlab,ylab=ylab,xlim=range(x),xaxt="n",...)
        axis(1,at=x,label=xlabels,padj=0,las=2,cex.axis=0.5)
    }
    if(gender==3) {
        z <- c(unlist(dat[,1:numLens]),max(dat))
        symbols(c(rep(x,length(y)),0),c(rep(y,each=length(x)),0),circles=z,main="Female",inches=inch,xlab=xlab,ylab=ylab,xlim=range(x),xaxt="n",...)
        axis(1,at=x,label=xlabels,padj=0,las=2,cex.axis=0.5)
        z <- c(unlist(dat[,(numLens+1):ncol(dat)]),max(dat))
        symbols(c(rep(x,length(y)),0),c(rep(y,each=length(x)),0),circles=z,main="Male",inches=inch,xlab=xlab,ylab=ylab,xlim=range(x),xaxt="n",...)
        axis(1,at=x,label=xlabels,padj=0,las=2,cex.axis=0.5)
    }
}
