#' #This function will create two plots
#' 1. CPUE from survey data across all years, and
#' 2. CPUE plot of the survey data by year.
#' 
#' @param dir directory
#' @param dat object created by the PullCatch.fn function
#' @param main name that will be used to name the saved png (i.e., "NWFSC" results in a file called "NWFSC_CPUE_Map.png")
#' @param dopng save the plot as a png inside plots folder
#' 
#' @author Chantel Wetzel
#' @export
#'
#' @import ggplot2

PlotMap.fn <- function(dir = NULL, dat, main = NULL, dopng = FALSE){

   # Create specialized plots
   pngfun <- function(dir, file,w=7,h=7,pt=12){
      file <- file.path(dir, file)
      cat('writing PNG to',file,'\n')
      png(filename=file,
            width=w,height=h,
            units='in',res=300,pointsize=pt)
   }


  if (dopng) { 
    if(is.null(dir)){ stop("Directory needs to be set.") }
    if (!file.exists(dir)) { stop("The dir argument leads to a location", ",\ni.e., ", dir, ", that doesn't exist.") }

    plotdir <- file.path(dir, paste("plots", sep=""))
    plotdir.isdir <- file.info(plotdir)$isdir
    if(is.na(plotdir.isdir) | !plotdir.isdir){
      dir.create(plotdir)}
    if (is.null(main))  { pngfun(dir = plotdir, file = 'CPUE_Map.png', h = 7, w = 5) }
    if (!is.null(main)) { pngfun(dir = plotdir, file  = paste0(main, '_CPUE_Map.png'), h = 7, w = 5) }
   }
   
   if(!dopng) { windows(width = 5, height = 7, record = TRUE)}

   states <- ggplot2::map_data("state")
   west_coast <- subset(states, region %in% c("washington", "oregon", "california"))
   wa_coast <- subset(west_coast, long <= -116 )
   map.df <- ggplot2::fortify(wa_coast)
   
   ind <- dat$cpue_kg_km2 > 0
   pos.cat = dat[ind,]
   ind <- pos.cat$Latitude_dd >= min(map.df$lat)
   pos.cat <- pos.cat[ind,]
   neg <- dat[dat$cpue_kg_km2 == 0 & dat$Latitude_dd >= min(map.df$lat),]
   mid <- as.numeric(quantile(pos.cat$cpue_kg_km2, 0.50))
   max.size <- 12 
   
   plot_format = theme(
             panel.grid = element_blank(), 
             panel.background = element_rect(fill = 'white')
             )

   color = c("#fffa00", "#ffcc00", "#ff7700", "#B60000")
   
   g <- ggplot(data = wa_coast) + 
       geom_polygon(aes(x = long, y = lat, group = group), fill = "lemonchiffon", color = "black") + 
       geom_point(data = neg, aes(x = Longitude_dd, y = Latitude_dd, color = cpue_kg_km2, size = cpue_kg_km2), pch = 1, col = "lightgrey", alpha = 0.15) +
       geom_point(data = pos.cat, aes(x = Longitude_dd, y = Latitude_dd, color = cpue_kg_km2, size = cpue_kg_km2), pch = 16, alpha = 0.7) +
       scale_size_area(max_size = max.size, name = "CPUE kg/km2") +  
        scale_color_gradient2(midpoint = mid, low=color[2], mid=color[3], high=color[4], space ="Lab", name = "CPUE kg/km2") +
       plot_format + 
        xlab("Longitude") + ylab("Latitude") +
        labs(title = "                     US West Coast") +
        theme(legend.position = "right") +
       coord_fixed(1.3) 
   print(g)
   if(dopng) { dev.off() }
   
   # By year
   plot_format = theme(
             panel.grid = element_blank(), 
             panel.background = element_rect(fill = 'white'),
             axis.title.x = element_blank(),
             axis.text.x=element_blank(),
             axis.ticks.x = element_blank())

   if (dopng) { 
    if (is.null(main))  { pngfun(dir = plotdir, file = 'CPUE_Map_Year.png', h = 7, w = 5) }
    if (!is.null(main)) { pngfun(dir = plotdir, file  = paste0(main, '_CPUE_Map_Year.png'), h = 7, w = 5) }
   }

   

   g <- ggplot(data = wa_coast) + 
        geom_polygon(aes(x = long, y = lat, group = group), fill = "lemonchiffon", color = "black") + 
        geom_point(data = neg, aes(x = Longitude_dd, y = Latitude_dd, color = cpue_kg_km2, size = cpue_kg_km2), pch = 1, col = "lightgrey", alpha = 0.15) +
        geom_point(data = pos.cat, aes(x = Longitude_dd, y = Latitude_dd, color = cpue_kg_km2, size = cpue_kg_km2), pch = 16, alpha = 0.7) +
        scale_size_area(max_size = 12, name = "CPUE kg/km2") +  
        scale_color_gradient2(midpoint = mid, low=color[2], mid=color[3], high=color[4], space ="Lab", name = "CPUE kg/km2") +
        plot_format + 
        xlab("Longitude") + ylab("Latitude") +
        labs(title = "                   US West Coast") +
        theme(legend.position = "right") +
        facet_wrap(~Year)
        coord_fixed(1.3) 
   print(g)
   if(dopng) { dev.off() }
}