#' Create strata data frame using \code{\link{StrataAreas.fn}}
#'
#' Create a data frame of strata formatted for use by models that need
#' estimates of the area for each strata to expand estimates of abundance.
#' Strata limits are provided by the user in terms of
#' and east and west ranges using depth (shallow and deep, respectively) and
#' north and south ranges using latitudes.
#'
#' @examples
#' strata <- CreateStrataDF.fn(
#'   names          = c("deep_s", "shallow_s", "deep_mn", "shallow_m", "shallow_n"),
#'   depths.shallow = c(183,  55, 183, 55, 55),
#'   depths.deep    = c(549, 183, 549, 183, 183),
#'   lats.south     = c( 32,  32,  40,  40, 44),
#'   lats.north     = c( 40,  40,  49,  44, 49))
#' # Which returns a data frame that looks like the following:
#' #      name      area Depth_m.1 Depth_m.2 Latitude_dd.1 Latitude_dd.2
#' #    deep_s 16468.110       183       549            32            40
#' # shallow_s 16109.711        55       183            32            40
#' #   deep_mn 12478.575       183       549            40            49
#' # shallow_m  7042.491        55       183            40            44
#' # shallow_n 16390.383        55       183            44            49
#'
#' @param names A vector of character values providing a name for each strata;
#' if left at the default value of \code{NA}, then names will be created
#' using capital letters starting with \code{A} for each \code{NA} value.
#' @param depths.shallow  vector of the shallow depths splits for each strata
#' @param depths.deep  vector of the deep depths splits for each strata
#' @param lats.south vector of the southern latitude splits for each strata
#' @param lats.north vector of the northern latitude splits for each strata
#'
#' @return Returns the data frame formatted for use by \code{\link{Biomass.fn}}
#' and additional prediction functions in other downstream packages.
#' The data frame will have six columns,
#' (1) name, (2) area, (3) Depth_m.1, (4) Depth_m.2,
#' (5) Latitude_dd.1, and (6) Latitude_dd.2.
#' @author Allan Hicks and Chantel Wetzel
#' @export
#' @seealso
#' See \code{\link{StrataAreas.fn}} for how areas are calculated.
#' See \code{\link{Biomass.fn}} for how the areas are used to predict biomass.

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

  # Check that users supplied either
  # (1) one NA (default) and repeat it the appropriate # of times
  # (2) correct # of names
  stopifnot(length(names) == 1 | length(names) == NROW(out))
  if (length(names) == 1) {
    if (is.na(names)) {
      names <- rep(NA, NROW(out))
    } else {
      stop("The length of names needs to be the same as other input arguments, i.e., ",
        NROW(out), ",\ninstead the single value of ", names, " that you supplied.",
        "\nA single value will only be repeated if it is 'NA', which leads to LETTERS for names.")
    }
  }
  nanames <- which(is.na(names))
  names[nanames] <- LETTERS[seq_along(nanames)]
  out[["name"]] <- names

  out <- StrataAreas.fn(strat.df = out, df = SA3)
}
