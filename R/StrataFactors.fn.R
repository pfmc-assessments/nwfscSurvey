#' Creates a vector of strata factors by name
#'
#' @param dat object
#' @param strat.vars strata
#' @param strat.df strata data frame
#'
#' @author Allan Hicks
#' @export

StrataFactors.fn <- function(dat, strat.vars, strat.df) {
  lifecycle::deprecate_warn(
    when = "2.8",
    what = "StrataFactors.fn()",
    details = "This function is no longer needed and will be removed in a future version. Please use strata_factors() instead."
  )
  numStrata <- nrow(strat.df)
  # create strata factors
  stratum <- rep(NA, nrow(dat)) # the stratum factor
  for (strat in 1:numStrata) {
    ind <- rep(T, nrow(dat))
    for (i in 1:length(strat.vars)) {
      ind <- ind &
        dat[, strat.vars[i]] >=
          strat.df[strat, paste(strat.vars[i], ".1", sep = "")] &
        dat[, strat.vars[i]] <
          strat.df[strat, paste(strat.vars[i], ".2", sep = "")]
    }
    stratum[ind] <- as.character(strat.df[strat, 1])
  }
  stratum <- factor(stratum, levels = as.character(strat.df[, 1]))
  return(stratum)
}
