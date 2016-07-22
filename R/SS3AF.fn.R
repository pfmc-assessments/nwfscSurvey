SS3AF.fn <- function(Age.df, ageBins, ...) {

       names(Age.df)[names(Age.df %in% 'Age')] <- "Length"
       SS3LF.fn(Age.df, lgthBins = ageBins, ...)
}


