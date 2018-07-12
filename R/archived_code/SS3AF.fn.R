#' Wrapper function to create age frequency data in format for Stock Synthesis
#' 
#' @param dir directory to save the file
#' @param Age.df age data frame 
#' @param ageBins defined age bins
#' @param agelow SS value
#' @param agehigh SS value
#' @param ageErr age error vector value
#' @param printfolder folder where the length comps will be saved
#' @param ... additional inputs
#'
#' @author Allan Hicks 
#' @export 
#' @seealso  \code{\link{SS3.LF.fn}}

SS3AF.fn <- function(dir, Age.df, ageBins, agelow = -1, agehigh = -1, ageErr = "NA", printfolder = "forSS", ...) {

	# this does not work because it only renames the column, but does not replace the values
    #names(Age.df)[names(Age.df) %in% 'Age'] <- "Length"

	# this approach replaces the column name AND values 
	Age.df$Length <- Age.df$Age
    out = SS3LF.fn(dir, len = Age.df, lgthBins = ageBins,...)

    Ages.out = cbind(out[,1:5],
                 agelow,
                 agehigh,
                 ageErr,
                 out[,6:dim(out)[2]])

    # save output as a csv
    comp.type ="Age"
    plotdir <- file.path(dir, printfolder)
    plotdir.isdir <- file.info(plotdir)$isdir
    if(is.na(plotdir.isdir) | !plotdir.isdir){
      dir.create(plotdir)
    }

    fn = paste0(plotdir, "/NWFSCBT_Survey_Gender", gender, "_Bins_-999_", max(ageBins),"_", comp.type, "Comps.csv")
    if (file.exists(fn)) {file.remove(fn)}

    write.csv(Ages.out, file = paste0(plotdir, "/NWFSCBT_Survey_Gender", gender, "_Bins_",min(ageBins),"_", max(ageBins),"_", comp.type, "Comps.csv"), row.names = FALSE)

    return(Ages.out)
}


