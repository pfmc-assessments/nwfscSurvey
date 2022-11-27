#' This function plots frequency data as bubble plots
#'
#' @param dir Directory to save files to
#' @param data Data file object created by SS3LF.fn or SS3AF.fn
#' @param add_save_name Option to add text to a saved figure name. This option can
#' be useful if creating plots across multiple species and saving them into a single
#' folder. Default NULL.
#' @param add_0_ylim TRUE/FALSE, option to specificy if the y-axis should start at 0 
#' or the minimum bin in the composition data file. Default TRUE.
#' @param width Numeric figure width in inches, defaults to 10
#' @param height Numeric figure height in inches, defaults to 7
#'
#' @import ggplot2
#' 
#' @author Chantel Wetzel
#' @export
#' 
#' @examples
#' \dontrun{ plot_comps(data = LFs)}
#' 
plot_comps <- function(
  data, 
  dir = NULL, 
  add_save_name = NULL,
  add_0_ylim = TRUE,
  width = 10, 
  height = 7) {

  data_type <- ifelse(sum(names(data) == "ageErr") == 0, "length", "age")
  sex_type <- paste(unique(substr(names(data)[10:ncol(data)], 1, 1)), collapse = "_")
  if(is.numeric(data[, "Nsamp"])) {
    N <- data[, "Nsamp"]
  } else {
    N <- rep(1,nrow(data))
  }

  plotdir <- file.path(dir, "plots")
  check_dir(dir = plotdir)
  name <- file.path(
    plotdir,
    paste0(
      add_save_name,
      ifelse(test = is.null(add_save_name), yes = "", no = "_"),
      data_type,
      "_frequency_sex_", sex_type,
      ".png"
    )
  )

  name2 <- file.path(
    plotdir,
    paste0(
      add_save_name,
      ifelse(test = is.null(add_save_name), yes = "", no = "_"),
      data_type,
      "_r4ss_frequency_sex_", sex_type,
      ".png"
    )
  )

  year <- as.numeric(as.character(data$year))
  sex <- data$sex[1]
  if (data_type == "length") {
    comps <- data[, -c(1:6)]
  }
  if (data_type == "age") {
    comps <- data[, -c(1:9)]
  }

  if (length(grep(".999", names(comps)) > 0)) {
    # remove extra columns (if the user didn't remove them already)
    if (sex %in% 0:2) {
      sex_lab <- substr(names(comps)[1], 1, 1)
      comps <- comps[, -match(paste0(sex_lab, "999"), names(comps))]
    }
    if (sex == 3) {
      # exclude columns for fish below minimum bin
      comps <- comps[, -match("F.999", names(comps))]
      comps <- comps[, -match("M.999", names(comps))]
    }
  }

  # Check to see if the unsexed or single sexed comps are double printed
  if (sum(grepl(".", colnames(comps), fixed = TRUE)) > 0 ) {
      comps <- comps[, !grepl(".", colnames(comps), fixed = TRUE)]
  }
  
  num <- ncol(comps) / ifelse(sex == 3, 2, 1)

  # add a 0 first column to the comps for plotting
  if (add_0_ylim) {
    if (sex %in% 0:2){
      sex_lab <- substr(names(comps)[1], 1, 1)
      comps <- cbind(0, comps[, 1:num])
      colnames(comps)[1] <- paste0(sex_lab, "0")
    } 
    if (sex == 3) {
      comps <- cbind(0, comps[, 1:num], 0, comps[, (num + 1):ncol(comps)])
      num <- num + 1
      colnames(comps)[1] <- "F0"
      colnames(comps)[num + 1] <- "M0"
    }
  }

  # Determine if entries are proportions (e.g., sum to 1 or 100)
  # and convert if needed
  if (!(sum(comps[1, ]) %in% c(1, 100))){
    comps <- 100 * comps / apply(comps, 1, sum)
  }

  mod_comps <- cbind(year, comps)
  df <- melt(mod_comps, id = "year")
  df$year <- factor(df$year, levels = unique(df$year))
  df$sex  <- substr(df$variable, 1, 1)
  df$sex  <- replace(df$sex, df$sex == "F", "FEMALE")
  df$sex  <- replace(df$sex, df$sex == "M", "MALE")
  df$sex  <- replace(df$sex, df$sex == "U", "UNSEXED")
  df$sex  <- factor(df$sex, levels = unique(df$sex))
  df$variable <- type.convert(gsub('[FMU]', '', df$variable), as.is = TRUE)
  df$n <- 0; a <- 1
  for(y in year){
    df$n[df$year == y] <- N[a]
    a <- a + 1
  }

  ylabel <- ifelse(data_type == "length", "Length (cm)", "Age (yr)")
  bub_step <- ifelse(max(df$value) < 50, 5, 10)
  bub_range <- c(1, seq(bub_step, floor(max(df$value)), bub_step))
  max_range <- 15
  if(max(df$variable) - min(df$variable) > 40 ){
    y_axis <- seq(min(df$variable), max(df$variable), by = 10)
  } else {
    y_axis <- seq(min(df$variable), max(df$variable), by = 5)
  }

  p <- ggplot2::ggplot(df, aes(x = year, y = variable)) +
      geom_point(aes(size = value, fill = sex, colour = sex), # add alpha = n inside the aes to shade by annual sample size
        alpha = 0.75, shape = 21) +
      scale_fill_manual(values = c('FEMALE' = 'red', 'MALE' = 'blue', 'UNSEXED' = "darkseagreen")) +
      scale_color_manual(values = c('FEMALE' = 'darkred', 'MALE' = 'darkblue', 'UNSEXED' = "darkgreen")) +
      scale_size_continuous(limits = c(0.1, 50), range = c(1, max_range), breaks = bub_range) + 
      facet_grid(sex~.) + 
      scale_y_continuous(breaks = y_axis) +
      labs(x = "Year", y = ylabel, size = "Relative\nAbundance (%)", fill = "") +
      theme(legend.key = element_blank(), 
          axis.title.x = element_text (size = 12),
          axis.title.y = element_text (size = 1),
          axis.text.x = element_text("Year", colour = "black", size = 12, angle = 90, vjust = 0.3, hjust = 1), 
          axis.text.y = element_text(ylabel, colour = "black", size = 12), 
          legend.text = element_text(size = 10, colour ="black"), 
          legend.title = element_text(size = 12), 
          panel.background = element_blank(), 
          panel.border = element_rect(colour = "black", fill = NA, size = 1), 
          legend.position = "right") +
      guides(size = "legend", color = "none", fill = "none")  

  if (!is.null(dir)){
      ggsave(filename = name, width = width, height = height, units = 'in')     
  }

  df2 <- df
  df2$value <- df2$value / 100
  df2[df2$sex == "MALE", 'value'] <- -1 * df2[df2$sex == "MALE", 'value']

  p2 <- ggplot2::ggplot(df2, aes(x = variable, y = value)) +
      geom_line(aes(colour = sex), # add alpha = n inside the aes to shade by annual sample size
        lwd = 1.1) +
      facet_wrap(facets = "year") +
      scale_fill_manual(values = c('FEMALE' = 'red', 'MALE' = 'blue', 'UNSEXED' = "darkseagreen")) +
      scale_color_manual(values = c('FEMALE' = 'darkred', 'MALE' = 'darkblue', 'UNSEXED' = "darkgreen")) +
      labs(x = ylabel, y = "Proportion") +
      geom_hline(yintercept = 0) +
      theme(legend.key = element_blank(), 
          axis.title.x = element_text (size = 12),
          axis.title.y = element_text (size = 1),
          axis.text.x = element_text(ylabel, colour = "black", size = 12, angle = 90, vjust = 0.3, hjust = 1), 
          axis.text.y = element_text("Proportion", colour = "black", size = 12), 
          legend.text = element_text(size = 10, colour ="black"), 
          legend.title = element_text(size = 12), 
          panel.background = element_blank(), 
          panel.border = element_rect(colour = "black", fill = NA, size = 1), 
          legend.position = "right")

  if (!is.null(dir)){
      ggsave(filename = name2, width = width, height = height, units = 'in')     
  }

  print(p)
  print(p2) 
         
}
