#' Wrapper function to create age frequency data in format for Stock Synthesis
#' 
#' @param Age.df age data frame 
#' @param ageBins defined age bins
#' @param ... additional inputs
#'
#' @author Allan Hicks 
#' @export 
#' @seealso  \code{\link{SS3.LF.fn}}

SS3AF.fn <- function(Age.df, ageBins, ...) {

	   # this does not work because it only renames the column, but does not replace the values
       #names(Age.df)[names(Age.df) %in% 'Age'] <- "Length"

	   # this approach replaces the column name AND values 
	   Age.df$Length <- Age.df$Age
       SS3LF.fn(Age.df, lgthBins = ageBins, ...)
}


