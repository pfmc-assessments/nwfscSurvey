#' 
#'
#' @param L.year
#' @template sex
#' @temlate month
#' @template fleet
#' @template partition
#' @template nSamps
#' @param lgthBins
#' @param verbose
#'
#' @author Chantel Wetzel
#' @export
#' @seealso \code{\link{SurveyLFs.fn}}
#'
#'
getLs <- function(L.year, sex, month, fleet, partition, nSamps, lgthBins, verbose){
  
  if (sex == 0){
  	sex.name = c("U","U")
  	get = c("TotalLjU", "TotalLjU") 
    if (sum(c("n_out", "Sex") %in% colnames(nSamps)) == 2) {
  	 input_n <- nSamps[nSamps$Sex == "U", "n_out"]
    }
  }

  if (sex == 1){
  	sex.name = c("F","F")
  	get = c("TotalLjF","TotalLjF")
    if (sum(c("n_out", "Sex") %in% colnames(nSamps)) == 2) { 
      input_n <- nSamps[nSamps$Sex == "F", "n_out"]
    }
  } 

  if (sex == 2){
  	sex.name = c("M", "M")
  	get = c("TotalLjM","TotalLjM") 
    if (sum(c("n_out", "Sex") %in% colnames(nSamps)) == 2) {
  	 input_n <- nSamps[nSamps$Sex == "M", "n_out"]
    }
  }

  if (sex == 3){
  	sex.name = c("F", "M")
  	get = c("TotalLjF", "TotalLjM") 
    if (sum(c("n_out", "Sex") %in% colnames(nSamps)) == 2) {
  	 input_n <- nSamps[nSamps$Sex == "All", "n_out"]
    }
  }

  if (sex == -1){
  	sex = 0
  	sex.name = c("U","U")
  	get = c("TotalLjAll", "TotalLjAll") 
    if (sum(c("n_out", "Sex") %in% colnames(nSamps)) == 2) {
  	 input_n <- nSamps[nSamps$Sex == "All", "n_out"]
    }
  }

  if(!exists("input_n")) { 
    if(length(nSamps) == length(L.year)) { 
      input_n <- nSamps
    } else {
      input_n <- NA
    }
  }
    
  lgths <- as.character(L.year[[1]]$Length)
  Ls <- unlist(lapply(L.year, function(x) {
        c(x[get[1]], x[get[2]]) }))
  Ls[is.na(Ls)] <- 0

  Ls <- matrix(Ls,
               nrow = length(L.year), byrow = T,
               dimnames = list(NULL, paste(c(rep(sex.name[1], length(lgths)), 
               rep(sex.name[2], length(lgths))), lgths, sep = "")) )


  out <- data.frame(year = as.numeric(names(L.year)),
    				month = month,
    				fleet = fleet,
    				sex = sex, 
    				partition = partition,
    				Nsamp = input_n,
    				Ls)

  find <- grep("999", colnames(out))
  out[, paste0(sex.name[1], min(lgthBins))] <- out[, paste0(sex.name[1], min(lgthBins))] + out[,find[1]]
  out[, paste0(sex.name[2], min(lgthBins))] <- out[, paste0(sex.name[2], min(lgthBins))] + out[,find[2]]
  out <- out[, -find]

  return(out)
}