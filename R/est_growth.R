#' Estimate length-at-age using the von Bertanlaffy
#' parametization from Stock Synthesis. 
#'
#'
#' @param dir Directory to save output.
#' @param dat The data loaded from the NWFSC database
#' @param return_df TRUE/FALSE If set to TRUE the dat data frame is 
#' returned with added columns for the estimated Lhat_low, Lhat_pred, and Lhat_high.
#' @param bySex Logical to indicate if plot by sex
#' @param Par  Dataframe of starting parameters for K, Linf, L0, CV0, and CV2 based on the 
#' Stock Synthesis parameterization of von Bertanlaffy growth.
#' @param estVB Logical. Estimate vonB growth to plot against predicted length. If F, it uses the paramters in \code{parStart}.
#' @param bins The bins to put ages into. If NULL then simply uses the ages as recorded.
#' @param sdFactor The number of standard deviations to include in the
#' low and high calculations. The default is 1.0.
#' @param dopng Deprecated with {nwfscSurvey} 2.1 because providing a non-NULL
#'   value to `dir` can serve the same purpose as `dopng = TRUE` without the
#'   potential for errors when `dopng = TRUE` and `dir = NULL`. Thus, users
#'   no longer have to specify `dopng` to save the plot as a png.
#'
#' @seealso [fit_vbgrowth()]
#'
#' @author Chantel Wetzel
#' @export

est_growth <- function(dir = NULL, dat, return_df = TRUE, 
  Par = data.frame(K = 0.13, Linf = 55, L0 = 15, CV0 = 0.10, CV1 = 0.10),
  bySex = TRUE, estVB = TRUE, bins = NULL, sdFactor = 1,
  dopng = lifecycle::deprecated()) {

  if (lifecycle::is_present(dopng)) {
    lifecycle::deprecate_warn(
      when = "2.1",
      what = "nwfscSurvey::PlotMap.fn(dopng =)"
    )
  }

  #### Initialize the three new columns
  dat$Lhat_pred <- NA
  dat$Lhat_low <- NA
  dat$Lhat_high <- NA

  if (is.null(bins)) {
    dat$Age_2 <- dat$Age
  } else {
    dat$Age_2 <- findInterval(dat$Age, bins)
  }

  if (bySex){
    use_data <- which(
      !is.na(dat$Length_cm) & 
      !is.na(dat$Age) 
    )
    la_data <- dat[use_data, ]

    sex_list <- list(which(
        !is.na(dat$Length_cm) & 
        !is.na(dat$Age) &
        dat$Sex %in% c("F") 
      ), which(
        !is.na(dat$Length_cm) & 
        !is.na(dat$Age) &
        dat$Sex %in% c("M") 
      ), which(
        !is.na(dat$Length_cm) & 
        !is.na(dat$Age) &
        dat$Sex %in% c("U") 
      ))

    la_data_list <- list(
      female = la_data[la_data$Sex == "F", ], 
      male = la_data[la_data$Sex == "M", ],
      unsexed = la_data[la_data$Sex == "U", ])
    sex_vec = c("F", "M", "U")
    nn <- 3

  } else {
    use_data <- which(
      !is.na(dat$Length_cm) & 
      !is.na(dat$Age) 
    )
    la_data <- dat[use_data, ]
    sex_list <- list(use_data)

    la_data_list <- list(allSex = la_data)
    nn <- 1
  }

  check_dir(dir = dir)

  out <- vector(mode = "list", length = nn)
  xpar <- vector(mode = "list", length = nn)
  names(out) <- paste0(names(la_data_list), "_sd_cv")
  names(xpar) <- paste0(names(la_data_list), "_growth")
  
  # Loop by sex
  for (i in 1:length(la_data_list)) {
    if (estVB) {
      ests_log <- stats::optim(fn = fit_vbgrowth,
                  par = log(Par),
                  hessian = FALSE,
                  par_logspace = TRUE, 
                  Ages = la_data_list[[i]]$Age,
                  Lengths = la_data_list[[i]]$Length_cm)$par
      xpar[[i]] <- exp(ests_log)
      #cat("Estimated VB parameters for", names(la_data_list)[i], xpar, "\n")
    } else {
      xpar[[i]] <- Par
    }

    # Predicts from 1 to the maximum observed age by sex
    #predL <- VB.fn(1:max(la_data_list[[i]]$Age), xpar[1], xpar[2], xpar[3])
    predL <- fit_vbgrowth(
      Par = xpar[[i]], 
      par_logspace = FALSE, 
      Ages = la_data_list[[i]]$Age, 
      Lengths = la_data_list[[i]]$Length_cm, 
      ReturnType = "Pred",
      sdFactor = sdFactor
    )
    rownames(predL) <- as.character(la_data_list[[i]]$Age)
    dat[sex_list[[i]], c("Lhat_low","Lhat_pred", "Lhat_high")] <- predL

    x <- split(la_data_list[[i]]$Length_cm, la_data_list[[i]]$Age_2)
    xsd <- unlist(lapply(x, sd))
    xcv <- xsd / predL[names(xsd), "Lhat_pred"]
    if (is.null(bins)) {
      ages <- as.numeric(names(xsd))
    } else {
      ages <- bins[as.numeric(names(xsd))]
    }
    out[[i]] <- data.frame(ages = ages, sd = xsd, cv = xcv)
  } # end sex loop

  if (!is.null(dir)){
    save(xpar, file = file.path(dir, "growth_vonB_estimates.Rdata"))   
  }

  ests <- c(xpar, out)

  if(return_df){
    return(dat)
  } else {
    return(ests)  
  }
  
}
