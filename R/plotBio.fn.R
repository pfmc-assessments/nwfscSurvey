#' Plots the biomass with confidence intervals
#' 
#' @param dir directory to save the file
#' @param dat object created by the GetTotalBiomass.fn
#' @param CI confidence interval
#' @param scalar simply the divisor for the biomass
#' @param gap  a value that introduces a slight gap between the point estimate and the start of the line for the CI. A gap too large will invert the CI, making it look huge. You should know when this happens
#' @param ylab y-axis text label
#' @param xlab x-axis text label
#' @param ylim y-limits
#' @param add add additional line to plot
#' @param col color
#' @param main plot label
#' @param dopng save the plot as a png inside plots folder
#' @param ...      Additional arguments for the plots
#'
#' @author Allan Hicks and John Wallace
#' @export

PlotBio.fn <-function(dir = NULL, dat, CI=0.95, scalar=1e6, gap=0.03, ylab="Biomass ('000 mt)", xlab="Year", 
                      main = NULL, ylim=NULL, add = FALSE, col = 'black', dopng = FALSE, ...) {

    bio = dat[[2]]

    if (dopng) {
      #plotdir <- paste0(dir, "/plots")
      if(!is.null(dir)){stop("Directory needs to be set.")}
      plotdir <- file.path(dir, paste("plots", sep=""))
      plotdir.isdir <- file.info(plotdir)$isdir
      if(is.na(plotdir.isdir) | !plotdir.isdir){
        dir.create(plotdir)
      }
      if ( is.null(main)) { png(paste0(dir, "/plots/designed_based_index.png"), height=7, width=7, units="in",res=300) }
      if (!is.null(main)) { png(paste0(dir, "/plots/", main, "_designed_based_index.png"), height=7, width=7, units="in",res=300) } 
    }
    
    y <- as.numeric(as.character(bio$Value))/scalar
    x <- as.numeric(as.character(bio$Year))
    se <- as.numeric(as.character(bio$seLogB))
    logB <- log(as.numeric(as.character(bio$Value)))
    ci <- exp(rbind(c(logB + qnorm(1 - (1 - CI)/2) * se), 
                    c(logB - qnorm(1 - (1 - CI)/2) * se)))/scalar
    if (is.null(ylim)) {
        ylim <- c(0, 1.05 * max(ci, na.rm = TRUE))
    }
    gap <- gap * max(y)
    if(add) {
      points(x, y, col = col)
    }
    else {
       plot(x, y, ylab = ylab, xlab = xlab, ylim = ylim, main = main, col = col, ...)
    }
    segments(x, y + gap, x, ci[1, ], col = col)
    segments(x, y - gap, x, ci[2, ], col = col)
    if (dopng) { dev.off()}
    
}
