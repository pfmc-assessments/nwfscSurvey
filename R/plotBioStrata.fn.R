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
#' @param mfrow.in option to specify the mfrow for plotting
#' @param col color
#' @param main plot label
#' @param dopng save the plot as a png inside plots folder
#' @param ...      Additional arguments for the plots
#'
#' @author Allan Hicks and John Wallace
#' @export

PlotBioStrata.fn <-function(dir = NULL, dat, CI=0.95, scalar=1e6, gap=0.03, ylab="Biomass ('000 mt)", xlab="Year", 
                            main = NULL, ylim=NULL, sameylim = FALSE, add = FALSE, mfrow.in = NULL, col = 'black', 
                            pch.col = 'black', pch.type = 16, dopng = FALSE, ...) {

    bio = dat$StrataEsts
    strata.names = names(bio)

    if (dopng) {
      if(is.null(dir)){stop("Directory needs to be set.")}
      if (!file.exists(dir)) { stop("The dir argument leads to a location", ",\ni.e., ", dir, ", that doesn't exist.") }
      plotdir <- file.path(dir, paste("plots", sep=""))
      plotdir.isdir <- file.info(plotdir)$isdir
      if(is.na(plotdir.isdir) | !plotdir.isdir){
        dir.create(plotdir)
      }
      if ( is.null(main)) { png(paste0(dir, "/plots/designed_based_by_strata_index.png"), height=7, width=7, units="in",res=300) }
      if (!is.null(main)) { png(paste0(dir, "/plots/", main, "_designed_based_by_strata_index.png"), height=7, width=7, units="in",res=300) } 
    }
    if(is.null(main)){main = strata.names}
    
    if(!is.null(mfrow.in)) { 
      if(length(mfrow.in) == 1) { par(mfrow = c(mfrow.in[1], mfrow.in[1])) }
      if(length(mfrow.in) != 1) { par(mfrow = c(mfrow.in[1], mfrow.in[2])) } }
    if(is.null(mfrow.in)) { par(mfrow = c(length(bio) / 2, 2)) }

    for (a in 1:length(bio)){
      df <- bio[[a]]
      se = logB = ci = NULL
      y    <- as.numeric(as.character(df$Bhat))/scalar
      x    <- as.numeric(as.character(df$name))
      se   <- as.numeric(as.character(log(df$cv^2 + 1)))
      logB <- log(as.numeric(as.character(df$Bhat)) + 0.000001)
      ci <- exp(rbind(c(logB + qnorm(1 - (1 - CI)/2) * se), 
                      c(logB - qnorm(1 - (1 - CI)/2) * se)))/scalar

      if (sameylim & is.null(ylim)) {
          tmp   <- do.call('rbind', bio)
          tmpse <- as.numeric(as.character(log(tmp$cv^2 + 1)))
          tmplogB <- log(as.numeric(as.character(tmp$Bhat)) + 0.000001)
          tmpci   <- exp(rbind(c(tmplogB + qnorm(1 - (1 - CI)/2) * tmpse),  c(tmplogB - qnorm(1 - (1 - CI)/2) * tmpse)))/scalar
          ylim <- c(0, 1.05 * max(tmpci, na.rm = TRUE))
      }
      if (!sameylim) {
        ylim <- c(0, 1.05 * max(ci, na.rm = TRUE))
      }

      if (is.null(ylim)) {
        ylim <- c(0, 1.05 * max(ci, na.rm = TRUE))
      }
      #gap <- gap * max(y)
      if(add) {
        points(x, y, col = pch.col, pch = pch.type)
      }
      else {
         plot(x, y, ylab = ylab, xlab = xlab, ylim = ylim, main = main[a], col = pch.col, pch = pch.type, ...)
      }
      segments(x = x, y = y + gap, x1 = x, y1 = ci[1, ], col = col)
      segments(x = x, y = y - gap, x1 = x, y1 = ci[2, ], col = col)  
    }

    if (dopng) { dev.off()}
    
}
