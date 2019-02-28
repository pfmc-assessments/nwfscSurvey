#' Plot variability of length at age
#'
#' Plots the SD and CV of age at observed and predicted length
#'
#'
#' @param dir directory to save the file
#' @param dat      The data loaded from the NWFSC database or the SexedLgthWtAge sheet
#' @param main
#' @param ageBin   Currently fixed at 1, so a moot parameter
#' @param bySex    Logical to indicate if plot by sex
#' @param parStart Vector of starting parameters for Linf, k, and t0 in VonB estimation
#' @param estVB    Logical. Estimate vonB growth to plot against predicted length. If F, it uses the paramters in \code{parStart}.
#' @param bins     The bins to put ages into. If NULL then simply uses the ages as recorded.
#' @param legX legend location
#' @param dopng TRUE/FALSE whether to save a png file   
#' @param ...      Additional arguments for the plots
#'
#' @author Allan Hicks and Chantel Wetzel
#' @export

PlotVarLengthAtAge.fn <- function(dir = NULL, dat, main = NULL, ageBin=1, bySex=T, parStart=c(52, 0.09, 1), estVB=T, bins=NULL, legX="bottomleft", legY=NULL, dopng = FALSE,...) 
{
    #calculate and plot the sd and cv for length at age
    #if you enter estVB=F, then it uses the parStart as the VB parameters

    VB.fn <- function(age, Linf ,k, t0) {
        out <- Linf*(1-exp(-k*(age-t0)))
        return(out)
    }
    VBopt.fn <- function(x, age, lengths) {sum((lengths - VB.fn(age, Linf=x[1], k=x[2], t0=x[3]))^2)}

    dat <- dat[!is.na(dat$Length_cm), ]

    dat <- dat[!is.na(dat$Age),]
    dat <- dat[dat$Sex%in%c("F", "M"), ]
    

    datL <- dat[!is.na(dat$Age),]
    if(is.null(bins)) {datL$Age_2 <- datL$Age}
    if(!is.null(bins)) {datL$Age_2 <- findInterval(datL$Age,bins)}

    if(!bySex) {
        datL <- list(allSex=datL)
        nn <- 1
    }

    if(bySex) {
        datLf <- datL[datL$Sex=="F",]
        datLm <- datL[datL$Sex=="M",]
        datL <- list(female=datLf,male=datLm)            
        nn <- 2
    }
    
    if (dopng) {
        if(is.null(dir)){stop("Directory needs to be set.")} 
        #plotdir <- paste0(dir, "/plots")
        plotdir <- file.path(dir, paste("plots", sep=""))
        plotdir.isdir <- file.info(plotdir)$isdir
        if(is.na(plotdir.isdir) | !plotdir.isdir){
            dir.create(plotdir) }
        if ( is.null(main)) { png( file.path(dir, paste("plots/VarLengthAtAge.png", sep = "")), height=7, width=7, units="in",res=300) }
        if (!is.null(main)) { png(file.path(dir, paste("plots/", main, "_VarLengthAtAge.png", sep = "")), height=7, width=7, units="in",res=300) }
    }

    par(mfcol=c(2,nn), mar =c(3,5,3,5))

    out <- vector(mode="list", length=nn)
    names(out) <- names(datL)
    for(i in 1:length(datL)) {
        if(estVB) {
            xpar <- optim(parStart, VBopt.fn, age = datL[[i]]$Age , lengths=datL[[i]]$Length_cm)$par
            cat("Estimated VB parameters for",names(datL)[i],xpar,"\n")
        }
        if(!estVB) {
            xpar <- parStart
        }
        predL <- VB.fn(1:max(datL[[i]]$Age),xpar[1],xpar[2],xpar[3])
        names(predL) <- as.character(1:max(datL[[i]]$Age))

        x <- split(datL[[i]]$Length_cm,datL[[i]]$Age_2)
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
    if (dopng) { dev.off()}
    return(out)
}