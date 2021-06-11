#' Calculate input sample sizes based on Stewart, I.J. and O.S. Hamel 2014.
#' Bootstrapping of sample size for legth- or age-composition bds used in stock assessment
#' Canadian Journal of Fishery and Aquatic Science, 71: 581-588. The function outputs a list
#' with samples sizes for lengths and ages (if present in the data ) for each sex combination 
#' (males and female combined, all fish, and/or unsexed only) and a table with number of 
#' tows and fish sampled by year. This list can be passed directly to the SurveyLFs.fn or the 
#' SurveyAFs.fn via the nSamps function input. 
#' 
#' @template dir 
#' @template bds 
#' @param dat *Deprecated* bds are passed to the function now via the bds function input
#' @param type *Deprecated* Now automatically calculates samples for lengths and ages in one call. Specify whether doing "length" or "age" that will allow the function to calculate the appropriate sample size by bds type.
#' @param species species specific value to determine the number of unique samples per tow (flatfish, shelfrock, sloperock, thorny, others, all)
#' @template printfolder 
#' @param output *Deprecated* Default = NULL will return only a vector of samples sizes, summary will return a table of observations
#' by year and sex
#' @param verbose Default TRUE. Option to print out message statements to the R terminal screen.
#'
#' @author Chantel Wetzel
#' @export

GetN.fn <- function(dir = NULL, bds, dat = lifecycle::deprecated(), type = lifecycle::deprecated(), species = NULL, printfolder = "forSS", 
  output = lifecycle::deprecated(), verbose = TRUE) {

  if (lifecycle::is_present(dat)) {
    lifecycle::deprecate_stop(
      when = "3.0",
      what = paste0("GetN.fn(dat = )"),
      details = paste0(
        "Please use the bds function input to pass the bds object"
        )
      )
  }

  if (lifecycle::is_present(output)) {
    lifecycle::deprecate_stop(
      when = "3.0",
      what = paste0("GetN.fn(output = )"),
      details = paste0(
        "This input is no longer used. Both a vector of samples and sample table are now returned."
        )
      )
  }

  if (lifecycle::is_present(type)) {
    lifecycle::deprecate_stop(
      when = "3.0",
      what = paste0("GetN.fn(type = )"),
      details = paste0(
        "Both length and age sample sizes and sample tables are automatically calculated if the bds are present."
        )
      )
  }

  if (!species %in% c("flatfish", "shelfrock", "sloperock", "thorny", "all", "others")) {
      stop("\nThe species input does not match one of the following options; flatfish, shelfrock, sloperock, thorny, others, or all\n")
  }

  # Could use GetSpp.fn to pre-define the species grouping for common
  # species but would need time to define correctly
  if (species == "flatfish")  { n.unq <- 3.09 }
  if (species == "shelfrock") { n.unq <- 2.43 }
  if (species == "sloperock") { n.unq <- 2.43 }
  if (species == "thorny")    { n.unq <- 6.91 }
  if (species == "others")    { n.unq <- 2.38 }
  if (species == "all")       { n.unq <- 2.73 }

  if (verbose) {
    message("\nThe effN sample size is calculated using the ", species, " multiplier of ", n.unq, ". This number is multiplied by the number of tows in each year.\n")
  }

  # Make year a factor to prevent dropping
  bds$Year = factor(bds$Year)

  calc_n <- function(dir, type, n.unq, verbose){
    options(dplyr.summarise.inform = FALSE)
    if(type == "length"){
      tmp = bds[!is.na(bds$Length_cm), ]
    } else {
      tmp = bds[!is.na(bds$Age), ]
    }
    # Calculate unique tows across all sexes
    samps_all =  tmp %>%
      dplyr::group_by(Year, .drop = FALSE) %>%
      dplyr::summarise(count = n_distinct(Trawl_id), fish = length(Year))
    samps_all = data.frame( Sex = "All", 
                            Year  = samps_all$Year, 
                            count = samps_all$count,
                            fish  = samps_all$fish)
    # Calculate unique tows by sex
    samps_sex = data.frame(tmp %>%
      dplyr::group_by(Sex,  Year, .drop = FALSE) %>%
      dplyr::summarise(count = n_distinct(Trawl_id), fish = length(Year)))
    samps = rbind(samps_all, samps_sex) 
  
    n = n_out = floor(n.unq * samps$count)
    samps <- cbind(samps, n, n_out)
    fish_table  <- table(tmp[, "Year"], tmp[, "Sex"])
    # Check to see if the input N based on tows is > # of fish sampled
    ind   <- samps$n > samps$fish
    samps$n_out[ind] <- samps$fish[ind]

    sample_table <- data.frame(
                          Year = samps[samps$Sex == "All", "Year"],
                          Tows = samps[samps$Sex == "All", "count"],
                          All_Fish = samps[samps$Sex == "All", "fish"],
                          Females  = samps[samps$Sex == "F", "fish"],
                          Males    = samps[samps$Sex == "M", "fish"],
                          Unsexed  = samps[samps$Sex == "U", "fish"])
    # save output as a csv
    if (!is.null(dir)) {
      plotdir <- file.path(dir, printfolder)
      plotdir.isdir <- file.info(plotdir)$isdir
      if (is.na(plotdir.isdir) | !plotdir.isdir) {
        dir.create(plotdir)
      }
      if ("Project" %in% colnames(tmp)){
        write.csv(sample_table, 
          file = file.path(plotdir, paste0(tmp$Project[1], "_", type, "_tows_samples.csv", sep = "")), row.names = FALSE)
      } else {
        write.csv(sample_table, file = file.path(plotdir, paste0(type, "_tows_samples.csv", sep = "")), row.names = FALSE)        
      }    
    }

    samp_list <- list()
    samp_list$samps <- samps
    samp_list$sample_table <- sample_table
    names(samp_list) <- c(paste0(type, "_samps"), paste0(type, "_sample_table"))
    return(samp_list)
  } # end function

  n <- calc_n(dir = dir, type = "length", n.unq = n.unq, verbose = verbose)
  if (sum(!is.na(bds$Age)) > 0 ) {
    n_age <- calc_n(dir = dir, type = "age", n.unq = n.unq, verbose = verbose)
    n <- c(n, n_age)
  } 

  return(n)
}
