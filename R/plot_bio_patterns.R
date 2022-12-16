#' This function plots length by latitude and depth
#'
#' @param dir Directory to save files to
#' @param bio Data biological sample file
#' @param col_name Option to switch between plotting lengths or ages. 
#' Options are "Length_cm", "Width_cm", or "Age".
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
#' 
plot_bio_patterns <- function(
  dir = NULL, 
  bio, 
  col_name = "Length_cm",
  plot = 1:3, 
  width = 7,
  height = 7){
 
  # plot 1 = length/age by latitude and depth
  # plot 2 = length/age by depth and year
  # plot 3 = length/age by lat and year

  round_any <- function(x, accuracy, f = round) {
      f(x / accuracy) * accuracy
  }

  plotdir <- file.path(dir, "plots")
  check_dir(dir = plotdir)

  lab_name = col_name
  if (col_name == "Length_cm"){
    lab_name = "Length (cm)"
    bio$x <- bio$Length_cm
  }
  if (col_name == "Width_cm"){
    lab_name = "Width (cm)"
    bio$x = bio$Width_cm
  }
  if (col_name == "Age"){
    lab_name = "Age (yrs)"
    bio$x <- bio$Age
  }

  alpha_set <- 0.30
  alpha_by_year <- 0.40
  if (dim(bio)[1] > 10000){
    alpha_set <- 0.05
    alpha_by_year <- 0.20
    if (dim(bio)[1] > 50000){
      alpha_set <- 0.01
      alpha_by_year <- 0.10
    }
  }
  bin_size <- ifelse(max(bio$Depth_m) - min(bio$Depth_m) > 500, 100, 
    ifelse(max(bio$Depth_m) - min(bio$Depth_m) > 250, 50, 25))
  bio$depth_bin <- round_any(bio$Depth_m, bin_size, f = floor)
  bio$lat <- round_any(bio$Latitude_dd, 0.25)

  # Length by depth for each sex
  ld <- ggplot2::ggplot(bio, aes(x = Depth_m, y = x, color = Sex)) +
      geom_point(aes(fill = Sex, colour = Sex), alpha = alpha_set, shape = 21, size = 3) +
      stat_summary(aes(y = x, x = depth_bin), fun = mean, geom = 'line', 
        lwd = 2, alpha = 1) + 
      scale_fill_manual(values = c('F' = 'red', 'M' = 'blue', 'U' = "darkseagreen")) +
      scale_color_manual(values = c('F' = 'darkred', 'M' = 'darkblue', 'U' = "darkgreen")) +
      labs(x = "Depth (m)", y = lab_name) +
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

  # Length by latitude    
  ll <- ggplot2::ggplot(bio, aes(x = Latitude_dd, y = x)) +
    geom_point(aes(fill = Sex, colour = Sex), alpha = alpha_set, shape = 21, size = 3) +
    stat_summary(aes(y = x, x = lat, colour = Sex), fun = mean, geom = 'line', 
      lwd = 2, alpha = 1) + 
    scale_fill_manual(values = c('F' = 'red', 'M' = 'blue', 'U' = "darkseagreen")) +
    scale_color_manual(values = c('F' = 'darkred', 'M' = 'darkblue', 'U' = "darkgreen")) +
    labs(x = "Latitude", y = lab_name) +
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

  # Length by latitude and sex by year
  lly <- ggplot2::ggplot(bio, aes(x = Latitude_dd, y = x)) +
    geom_point(aes(fill = Sex, colour = Sex), 
      alpha = alpha_by_year, shape = 21, size = 3) +
    facet_wrap(facets = "Year") + 
    scale_fill_manual(values = c('F' = 'red', 'M' = 'blue', 'U' = "darkseagreen")) +
    scale_color_manual(values = c('F' = 'darkred', 'M' = 'darkblue', 'U' = "darkgreen")) +
    labs(x = "Latitude", y = lab_name) + 
    theme(panel.background = element_blank(), 
          panel.border = element_rect(fill = NA))

  # Length by depth and sex by year
  ldy <- ggplot2::ggplot(bio, aes(x = Depth_m, y = x)) +
    geom_point(aes(fill = Sex, colour = Sex), 
      alpha = alpha_by_year, shape = 21, size = 3) +
    facet_wrap(facets = "Year") + 
    scale_fill_manual(values = c('F' = 'red', 'M' = 'blue', 'U' = "darkseagreen")) +
    scale_color_manual(values = c('F' = 'darkred', 'M' = 'darkblue', 'U' = "darkgreen")) +
    labs(x = "Depth (m)", y = lab_name) + 
    theme(panel.background = element_blank(), 
          panel.border = element_rect(fill = NA))

  # plot 1
  cowplot::plot_grid(ll, ld, nrow = 2) 
  if(!is.null(dir)){ 
      ggsave(filename = file.path(dir, "plots", paste0(col_name, "_by_lat_depth.png")), 
          width = width, height = height, units = 'in') 
  }

  # plot 2
  print(ldy)
  if(!is.null(dir)){ 
      ggsave(filename = file.path(dir, "plots", paste0(col_name, "_by_year_depth.png")), 
          width = width + 3, height = height + 3, units = 'in') 
  }   

  # plot 3
  print(lly)
  if(!is.null(dir)){ 
      ggsave(filename = file.path(dir, "plots", paste0(col_name, "_by_year_lat.png")), 
          width = width + 3, height = height + 3, units = 'in') 
  }  
 
}