#' Function that is able to create strata data frame and will calculate strata areas using the StrataAreas.fn
#'
#' Example function call
#' strata = CreateStrataDF.fn(names          = c("deep_s", "shallow_s", "deep_mn", "shallow_m", "shallow_n"),
#'                            depths.shallow = c(183,  55, 183, 55, 55),
#'                            depths.deep    = c(549, 183, 549, 183, 183),
#'                            lats.south     = c( 32,  32,  40,  40, 44),
#'                            lats.north     = c( 40,  40,  49,  44, 49))
#'
#' @param names  vector of names for strata, if not specified the default with be A, B, ...
#' @param depths.shallow  vector of the shallow depths splits for each strata
#' @param depths.deep  vector of the deep depths splits for each strata
#' @param lats.south vector of the southern latitude splits for each strata
#' @param lats.north vector of the northern latitude splits for each strata
#'
#' @return Returns the dataframe input with a column called area.
#' @author Allan Hicks and Chantel Wetzel
#' @export
#' @seealso \code{\link{StrataAreas.fn}}

CreateStrataDF.fn <- function(names = NA, depths.shallow, depths.deep, lats.south, lats.north) {

  SA3 <- NULL
  utils::data("SA3", envir = environment(), overwrite = TRUE)

  out <- data.frame(
    name = NA,
    area = NA,
    Depth_m.1 = depths.shallow,
    Depth_m.2 = depths.deep,
    Latitude_dd.1 = lats.south,
    Latitude_dd.2 = lats.north
  )

  if (is.na(names[1])) {
    out$name <- LETTERS[1:nrow(out)]
  }
  if (!is.na(names[1])) {
    out$name <- names[1:nrow(out)]
  }
  out <- StrataAreas.fn(strat.df = out, df = SA3)
}
