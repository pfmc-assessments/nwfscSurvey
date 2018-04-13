#' Plot variability of length at age
#'
#' @details Plots the SD and CV of age at observed and predicted length
#'
#'
#' @param dat      The data loaded from the NWFSC database or the SexedLgthWtAge sheet
#' @param ageBin   Currently fixed at 1, so a moot parameter
#' @param bySex    Logical to indicate if plot by sex
#' @param parStart Vector of starting parameters for Linf, k, and t0 in VonB estimation
#' @param estVB    Logical. Estimate vonB growth to plot against predicted length. If F,
#'                 it uses the paramters in \code{parStart}.
#' @param bins     The bins to put ages into. If NULL then simply uses the ages as recorded.   
#' @param ...      Additional arguments for the plots
#'
#' @examples
#'   slwa <- read.csv(paste(directory,"SexedLgthWtAge.csv",sep="\\"),skip=8)
#'   ageBin currently is fixed at 1 no matter what number you enter
#'   res <- varLengthAtAge.fn(slwa,ageBin=1,bySex=T,parStart=c(52,0.09,1),estVB=T)
#' @author Allan Hicks
#'
#' @export

varLengthAtAge.fn <- function(dat,ageBin=1,bySex=T,parStart=c(52,0.09,1),estVB=T,bins=NULL,legX="bottomleft",legY=NULL,...) {
    #calculate and plot the sd and cv for length at age
    #if you enter estVB=F, then it uses the parStart as the VB parameters

    VB.fn <- function(age,Linf,k,t0) {
        out <- Linf*(1-exp(-k*(age-t0)))
        return(out)
    }
    VBopt.fn <- function(x,age,lengths) {sum((lengths-VB.fn(age,Linf=x[1],k=x[2],t0=x[3]))^2)}

    datL <- dat[!is.na(dat$AGE_YRS),]
    if(is.null(bins)) {datL$AGE_YRS_2 <- datL$AGE_YRS}
    if(!is.null(bins)) {datL$AGE_YRS_2 <- findInterval(datL$AGE_YRS,bins)}
#return(cbind(datL$AGE_YRS,datL$AGE_YRS_2))
    if(!bySex) {
        datL <- list(allSex=datL)
        nn <- 1
    }
    if(bySex) {
        datLf <- datL[datL$SEX=="f",]
        datLm <- datL[datL$SEX=="m",]
        datL <- list(female=datLf,male=datLm)
        nn <- 2
    }
    
    par(mfcol=c(2,nn))

    out <- vector(mode="list",length=nn)
    names(out) <- names(datL)
    for(i in 1:length(datL)) {
        if(estVB) {
            xpar <- optim(parStart,VBopt.fn,age=datL[[i]]$AGE_YRS,lengths=datL[[i]]$LENGTH_CM)$par
            cat("Estimated VB parameters for",names(datL)[i],xpar,"\n")
        }
        if(!estVB) {
            xpar <- parStart
        }
        predL <- VB.fn(1:max(datL[[i]]$AGE_YRS),xpar[1],xpar[2],xpar[3])
        names(predL) <- as.character(1:max(datL[[i]]$AGE_YRS))

        x <- split(datL[[i]]$LENGTH_CM,datL[[i]]$AGE_YRS_2)
        xsd <- unlist(lapply(x,sd))
        xcv <- xsd/predL[names(xsd)]
        if(is.null(bins)) {ages <- as.numeric(names(xsd))}
        if(!is.null(bins)) {ages <- bins[as.numeric(names(xsd))]}
        out[[i]] <- data.frame(ages=ages,sd=xsd,cv=xcv)

        plot(ages,xsd,xlab="Age",ylab="SD of L@A",type="b",pch=16,lty=1,main=names(datL)[i],...)
        par(new=T)
        plot(ages,xcv,xlab="",ylab="",yaxt="n",type="b",pch=3,lty=2,...)
        axis(4)
        mtext("CV",side=4,line=2.6)
        legend(x=legX,y=legY,c("SD","CV"),pch=c(16,3),lty=c(1,2))

        plot(VB.fn(ages,xpar[1],xpar[2],xpar[3]),xsd,xlab="Predicted Length at Age",ylab="SD of L@A",type="b",pch=16,lty=1,main=names(datL)[i],...)
        par(new=T)
        plot(VB.fn(ages,xpar[1],xpar[2],xpar[3]),xcv,xlab="",ylab="",yaxt="n",type="b",pch=3,lty=2,...)
        axis(4)
        mtext("CV",side=4,line=2.6)
        legend(x=legX,y=legY,c("SD","CV"),pch=c(16,3),lty=c(1,2))
    }

    return(out)
}


