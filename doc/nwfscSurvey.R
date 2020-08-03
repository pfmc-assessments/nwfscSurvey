## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- results = 'asis', echo = FALSE-------------------------------------
  library(xtable)
  library(kableExtra)

  tab = rbind( c(2003, 1, "EnterFleet", 21055083, 0.357),
               c("...", "", "", "", ""),
               c(2015, 1, "EnterFleet", 9766200, 0.563))
  
  colnames(tab) = c("Year", "Season", "Fleet", "Value", "seLogB")
  
  table = tab 
  kable(table, "html")

