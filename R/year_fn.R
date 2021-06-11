#  Calculate the LFs by year. This function is called
#' interanlly in the SurveyLFs.fn.
#'
#' @param x The L.year.str object created by the SurveyLF.fn that contains the stage two
#' expansion information by length, strata, and year.
#' @param Lengths Object that contains the length bins including the -999 and 
#' the Inf for lengths below or above the length bin range.
#'
#'
year.fn <- function(x, Lengths) { 

    theLs.yr <- unlist(lapply(x, function(x) {
      as.numeric(as.character(x$LENGTH))
    }))
    TotalLjhAll <- unlist(lapply(x, function(x) {
      as.numeric(as.character(x$TotalLjhAll))
    }))
    TotalLjhF <- unlist(lapply(x, function(x) {
      as.numeric(as.character(x$TotalLjhF))
    }))
    TotalLjhM <- unlist(lapply(x, function(x) {
      as.numeric(as.character(x$TotalLjhM))
    }))
    TotalLjhU <- unlist(lapply(x, function(x) {
      as.numeric(as.character(x$TotalLjhU))
    }))

    # Finds the interval that the length falls in and floors it (so 23.2 would be in 23 if 23 was a level in Lengths,
    # all.inside puts maximum age group into N-1 group, thus I padded with Inf.)
    allLs <- Lengths[findInterval(theLs.yr, Lengths, all.inside = T)]
    TotalLjAll <- tapply(TotalLjhAll, allLs, sum, na.rm = T)
    TotalLjF <- tapply(TotalLjhF, allLs, sum, na.rm = T)
    TotalLjM <- tapply(TotalLjhM, allLs, sum, na.rm = T)
    TotalLjU <- tapply(TotalLjhU, allLs, sum, na.rm = T)

    out <- data.frame(
      Length = Lengths, TotalLjAll = rep(NA, length(Lengths)), TotalLjF = rep(NA, length(Lengths)),
      TotalLjM = rep(NA, length(Lengths)), TotalLjU = rep(NA, length(Lengths))
    )
    row.names(out) <- out$Length
    out[names(TotalLjAll), "TotalLjAll"] <- 100 * TotalLjAll / sum(TotalLjAll, na.rm = T)
    out[names(TotalLjF), "TotalLjF"] <- 100 * TotalLjF / (sum(TotalLjF, na.rm = T) + sum(TotalLjM, na.rm = T))
    out[names(TotalLjM), "TotalLjM"] <- 100 * TotalLjM / (sum(TotalLjF, na.rm = T) + sum(TotalLjM, na.rm = T))
    out[names(TotalLjU), "TotalLjU"] <- 100 * TotalLjU / (sum(TotalLjU, na.rm = T))
    out <- out[-nrow(out), ] # remove last row because Inf and always NA due to inside.all=T
    return(out)
}