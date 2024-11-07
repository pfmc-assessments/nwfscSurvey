#' Estimate length-at-age using the von Bertanlaffy
#' parametization
#'
#'
#' @template dir
#' @param data Data frame of biological data from [pull_bio()]
#' @param col_length A numeric or character value specifying the column
#'   to use in `data` for length information. These lengths are assumed to
#'   be in centimeters. The default value is `length_cm`.
#' @param col_weight A numeric or character value specifying the column
#'   to use in `data` for weight information. These weights are assumed to
#'   be in kilograms The default value is `weight_kg`.
#' @param init_params  Data frame of starting parameters for K, Linf, L0, CV0, and CV2 based on the
#' Stock Synthesis parameterization of von Bertanlaffy growth.
#' @param bins The bins to put ages into. If NULL then simply uses the ages as recorded.
#'
#' @seealso [fit_vbgrowth()]
#'
#' @author Chantel Wetzel
#' @export
#'
est_vbgrowth <- function(
    data,
    dir = NULL,
    col_length = "length_cm",
    col_age = "age",
    init_params = data.frame(K = 0.13, Linf = 55, L0 = 15, CV0 = 0.10, CV1 = 0.10),
    bins = NULL) {
  check_dir(dir = dir)
  col_length <- tolower(col_length)
  col_age <- tolower(col_age)
  colnames(data) <- tolower(colnames(data))
  stopifnotcolumn(data = data, string = col_length)
  stopifnotcolumn(data = data, string = col_age)
  stopifnotcolumn(data = data, string = "sex")
  data[, "age_column"] <- data[, col_age]
  data[, "length_column"] <- data[, col_length]

  if (is.null(bins)) {
    data[, "age_bin"] <- data[, "age_column"]
  } else {
    data[, "age_bin"] <- findInterval(data[, "age_column"], bins)
  }

  la_data <- data |>
    dplyr::filter(!is.na(length_column), !is.na(age_column))

  la_data_list <- list(
    female = la_data[la_data[, "sex"] == "F", ],
    male = la_data[la_data[, "sex"] == "M", ],
    all = la_data
  )

  out <- vector(mode = "list", length = length(la_data_list))
  xpar <- vector(mode = "list", length = length(la_data_list))
  names(out) <- paste0(names(la_data_list), "_sd_cv")
  names(xpar) <- paste0(names(la_data_list), "_growth")

  # Loop by sex-group
  for (i in 1:length(la_data_list)) {
    ests_log <- stats::optim(
      fn = fit_vbgrowth,
      par = log(init_params),
      hessian = FALSE,
      par_logspace = TRUE,
      Ages = la_data_list[[i]]$age_column,
      Lengths = la_data_list[[i]]$length_column
    )$par
    xpar[[i]] <- exp(ests_log)

    predL <- fit_vbgrowth(
      Par = xpar[[i]],
      par_logspace = FALSE,
      Ages = la_data_list[[i]]$age_column,
      Lengths = la_data_list[[i]]$length_column,
      ReturnType = "Pred",
      sdFactor = 1
    )
    rownames(predL) <- as.character(la_data_list[[i]]$age_column)
    x <- split(la_data_list[[i]]$length_column, la_data_list[[i]]$age_bin)
    xsd <- unlist(lapply(x, sd))
    xcv <- xsd / predL[names(xsd), "Lhat_pred"]
    if (is.null(bins)) {
      ages <- as.numeric(names(xsd))
    } else {
      ages <- bins[as.numeric(names(xsd))]
    }
    out[[i]] <- data.frame(ages = ages, sd = xsd, cv = xcv)
  } # end sex loop

  vbgrowth_ests <- c(xpar, out)
  if (!is.null(dir)) {
    save(vbgrowth_ests, file = file.path(dir, "von_bert_growth_estimates.Rdata"))
  }
  return(vbgrowth_ests)
}
