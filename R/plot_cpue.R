#' This function plots cpue and length by latitude and depth
#'
#' @param dir Directory to save files to
#' @param catch Data catch file
#' @param plot A vector of integers specifying the figures you want.
#' @param width Numeric figure width in inches, defaults to 7
#' @param height Numeric figure height in inches, defaults to 7
#'
#' @import ggplot2
#' @import cowplot
#' 
#' @author Chantel Wetzel
#' @export
#' 
plot_cpue <- function(
  dir = NULL, 
  catch, 
  plot = 1:3, 
  width = 7,
  height = 7){

  # plot 1 = cpue by latitude and depth 
  # plot 2 = cpue by lat and year
  # plot 3 = cpue by depth and year

  if (!is.null(dir)){
    if (!file.exists(dir)) {
      stop("The dir argument leads to a location", ",\ni.e., ", dir, ", that doesn't exist.")
    }  
    plotdir <- file.path(dir, "plots")
    plotdir.isdir <- file.info(plotdir)$isdir
    if (is.na(plotdir.isdir) | !plotdir.isdir) {
      dir.create(plotdir)
    }
  }

  catch$log_cpue <- log(catch$cpue_kg_km2)
  pos <- catch$cpue_kg_km2 != 0
  size_adj <- 100 / floor(sum(pos))

  # log(cpue) by depth in meters
  cd <- ggplot2::ggplot(catch[pos,], aes(x = Depth_m, y = log_cpue)) +
        geom_point(aes(size = log_cpue / size_adj), fill = 'darkorange', colour = 'darkorange', alpha = 0.75, shape = 21) +
        labs(x = "Depth (m)", y = "ln(CPUE)", size = "ln(CPUE)", fill = 'darkorange') +
        geom_smooth(method = 'loess', color = 'darkgrey', lwd = 2) + 
        scale_x_continuous(n.breaks = 7) +
        scale_y_continuous(n.breaks = 7) +
        theme(legend.key = element_blank(), 
          axis.text.x = element_text(colour = "black", size = 12), 
          axis.text.y = element_text(colour = "black", size = 11), 
          legend.text = element_text(size = 10, colour ="black"), 
          legend.title = element_text(size = 12), 
          panel.background = element_blank(), 
          panel.border = element_rect(fill = NA), 
          legend.position = "right") +
        guides(size = "legend", color = "none", fill = "none")

  # log(cpue) by latitude
  cl <- ggplot2::ggplot(catch[pos,], aes(x = Latitude_dd, y = log_cpue)) +
        geom_point(aes(size = log_cpue / size_adj), fill = 'darkorange', colour = 'darkorange', alpha = 0.75, shape = 21) +
        geom_smooth(method = 'loess', color = 'darkgrey', lwd = 2) +  
        labs(x = "Latitude", y = "ln(CPUE)", size = "ln(CPUE)", fill = 'darkorange', colour = 'darkorange',) +
        scale_x_continuous(n.breaks = 7) +
        scale_y_continuous(n.breaks = 7) +
        theme(legend.key = element_blank(), 
          axis.text.x = element_text(colour = "black", size = 12), 
          axis.text.y = element_text(colour = "black", size = 11), 
          legend.text = element_text(size = 10, colour ="black"), 
          legend.title = element_text(size = 12), 
          panel.background = element_blank(), 
          panel.border = element_rect(fill = NA), 
          legend.position = "right") 

  cly <- ggplot2::ggplot(catch[pos,], aes(x = Latitude_dd, y = log_cpue)) +
    geom_point(aes(size = log_cpue / (100*size_adj)), fill = 'darkorange', colour = 'darkorange', alpha = 0.75, shape = 21) +
    facet_wrap(facets = "Year") + 
    geom_smooth(method = 'loess', color = 'darkgrey', lwd = 2) + 
    labs(x = "Latitude", y = "ln(CPUE)", size = "ln(CPUE)", fill = 'darkorange', colour = 'darkorange',) +
    guides(size = "legend", color = "none", fill = "none") + 
    theme(panel.background = element_blank(), 
          panel.border = element_rect(fill = NA))

  # Length by depth and sex by year
  cdy <- ggplot2::ggplot(catch[pos, ], aes(x = Depth_m, y = log_cpue)) +
    geom_point(aes(size = log_cpue / (100*size_adj)), fill = 'darkorange', colour = 'darkorange', alpha = 0.75, shape = 21) +
    facet_wrap(facets = "Year") + 
    geom_smooth(method = 'loess', color = 'darkgrey', lwd = 2) + 
    labs(x = "Depth (m)", y = "ln(CPUE)", size = "ln(CPUE)", fill = 'darkorange', colour = 'darkorange') +
    guides(size = "legend", color = "none", fill = "none") + 
    theme(panel.background = element_blank(), 
          panel.border = element_rect(fill = NA))

  # plot 1
  cowplot::plot_grid(cl, cd, nrow = 2)
  if(!is.null(dir)){    
    ggsave(filename = file.path(dir, "plots", "cpue_by_lat_depth.png"), 
      width = width, height = height, units = 'in')     
  }  

  # plot 2
  print(cly)
  if(!is.null(dir)){ 
      ggsave(filename = file.path(dir, "plots", "cpue_by_year_lat.png"), 
          width = width + 3, height = height + 3, units = 'in') 
  } 

  # plot 3  
  print(cdy)
  if(!is.null(dir)){ 
      ggsave(filename = file.path(dir, "plots", "cpue_by_year_depth.png"), 
          width = width + 3, height = height + 3, units = 'in') 
  } 
 
}