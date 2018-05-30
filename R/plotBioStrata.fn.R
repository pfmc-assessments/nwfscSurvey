#' Plot the design based estimated biomass by strata and year
#' Currently only works with the triennial or afsc survey data
#' This is a new function and the results should be scrutinized
#' 
#' @param dir directory to save the file
#' @param dat object created by the GetTotalBiomass.fn
#' @param CI confidence interval
#' @param scalar simply the divisor for the biomass
#' @param legloc location to put legend in the single panel plot
#' @param single TRUE/FALSE true plots each stratum on the same figure
#' @param gap  a value that introduces a slight gap between the point estimate and the start of the line for the CI. A gap too large will invert the CI, making it look huge. You should know when this happens
#' @param ylab y-axis text label
#' @param xlab x-axis text label
#' @param yliminput user defined y-limits
#' @param dopng TRUE/FALSE whether to save a png file   
#'
#' @author Allan Hicks and Chantel Wetzel
#' @export

plotBioStrata.fn <-function(dir, dat, CI=0.95, scalar=1e6, gap=0.01, legloc = "topright", single = TRUE,
    ylab="Biomass ('000 mt)", xlab="Year", yliminput=NULL, dopng = FALSE, ...)
{

    plotdir <- paste0(dir, "/plots")
    plotdir.isdir <- file.info(plotdir)$isdir
    if(is.na(plotdir.isdir) | !plotdir.isdir){
      dir.create(plotdir)
    }
    
    #Some ugly code to manipulte the biomass list - CRW
    temp <- dat[[1]]$Strat
    df   <- do.call("rbind", temp)
    year <- as.numeric(toupper(substr(row.names(df),5,8)))
    df   <- cbind(year, df)
    byStrat <- split(df,df$name)

    #Calculate values
    yrTotal.fn <- function(x) {
        data.frame(year = x$year, strata = x$name, Bhat=x$Bhat,seBhat=sqrt(x$varBhat),cv=sqrt(x$varBhat)/x$Bhat)
    }
    ests   <- lapply(byStrat, yrTotal.fn)
    ests   <- do.call("rbind", ests)
    logVar <- log(ests$cv^2+1)
    bio.df <- data.frame(strata = ests$strata, year=ests$year, Value=ests$Bhat, seLogB=sqrt(logVar))
    
    nStrat <- length(unique(bio.df$strata))
    namesStrat <- unique(bio.df$strata)

    if (dopng) { png(paste0(dir, "/plots/design_based_biomass_by_strata.png"), height=7, width=7, units="in",res=300) }
    if(!single){       
        par(mfrow = c(nStrat, 1))
        for(i in 1:nStrat){
            bio <- bio.df[bio.df$strata == namesStrat[i], ]
            y <- as.numeric(as.character(bio$Value/scalar))
            x <- 1:nrow(bio)
            se <- as.numeric(as.character(bio$seLogB))
            logB <- log(bio$Value)
            ci <- exp(rbind(c(logB+qnorm(1-(1-CI)/2)*se),c(logB-qnorm(1-(1-CI)/2)*se)))/scalar
            if(is.null(yliminput)) { ylim <- c(0,1.05*max(ci)) }
            if(!is.null(yliminput)) { ylim <- yliminput }

            gap <- gap*max(y)
            plot(x,y,ylab=ylab,xlab=xlab,ylim=ylim,xaxt="n",main = namesStrat[i],...)
            points(x, y, pch=16, bg='white', cex=1)            
            arrows(x0=x,y0=ci[1,], x1=x, y1=ci[2,], angle=90, code=3, length=0.01)
            axis(1,at=x,label=as.character(bio$year),padj=0,las=2,cex.axis=1)
            #segments(x,y+gap,x,ci[1,])
            #segments(x,y-gap,x,ci[2,])
        }
    }

    if(single){
        par(mfrow = c(1, 1))
        colvec = c(1, rainbow(nStrat))
        i = 1
        bio <- bio.df[bio.df$strata == namesStrat[i], ]
        y <- as.numeric(as.character(bio$Value/scalar))
        x <- 1:nrow(bio)
        se <- as.numeric(as.character(bio$seLogB))
        logB <- log(bio$Value)
        ci <- exp(rbind(c(logB+qnorm(1-(1-CI)/2)*se),c(logB-qnorm(1-(1-CI)/2)*se)))/scalar
        if(is.null(yliminput)) { ylim <- c(0,1.05*max(ci)) }
        if(!is.null(yliminput)) { ylim <- yliminput }
        
        gap <- gap*max(y)
        plot(x,y,ylab=ylab,xlab=xlab,ylim=ylim,xaxt="n",...)
        points(x, y, pch=16, bg='white', cex=1, col = colvec[i])   
        arrows(x0=x,y0=ci[1,], x1=x, y1=ci[2,], angle=90, code=3, length=0.01, colvec[i])
        axis(1,at=x,label=as.character(bio$year),padj=0,las=2,cex.axis=1)

        add = 0
        for(i in 2:nStrat){
            add = add + 0.20
            bio <- bio.df[bio.df$strata == namesStrat[i], ]
            y <- as.numeric(as.character(bio$Value/scalar))
            x <- 1:nrow(bio)
            se <- as.numeric(as.character(bio$seLogB))
            logB <- log(bio$Value)
            ci <- exp(rbind(c(logB+qnorm(1-(1-CI)/2)*se),c(logB-qnorm(1-(1-CI)/2)*se)))/scalar
            
            points(x+add, y, pch=16, bg='white', cex=1, col = colvec[i])
            arrows(x0=x+add, y0=ci[1,], x1=x+add, y1=ci[2,], angle=90, code=3, length=0.01, col = colvec[i])
        }
        legend(legloc, legend = namesStrat, col = colvec[1:nStrat], bty = 'n', pch = 16)
    }
    if(dopng){ dev.off()}
}
