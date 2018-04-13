#' Wrapper function to create age frequency data in format for Stock Synthesis
#' 
#' @param Age.df
#' @param ageBins
#' @param ... additional inputs
#'
#' @author Allan Hicks 
#' @export 
#' \code{\link{SS3.LF.fn}}

SS3AF.fn <- function(Age.df, ageBins, ...) {

       names(Age.df)[names(Age.df %in% 'Age')] <- "Length"
       SS3LF.fn(Age.df, lgthBins = ageBins, ...)
}


