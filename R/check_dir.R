#' Directory check
#'
#' 
#' @template dir
#' @template verbose
#'
#' @author Chantel Wetzel
#' @export
#'
check_dir <- function(dir, verbose){

  if (is.null(dir)) {
    if (verbose) {
      message("Output will not be saved to since dir is not specified.")  
    }   
  }
  if (!is.null(dir)) {
    if (!file.exists(dir)) {
      stop(
        "The dir argument leads to a location",
        ",\ni.e., ", dir, ", that doesn't exist."
      )
    }
  }  

}