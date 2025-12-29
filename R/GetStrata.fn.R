#' Get Default Strata
#'
#' @details
#' Get a data frame of strata for design-based estimates and survey
#' composition data. Several strata are available for different default areas,
#' as well as combinations using \code{"_"} in the argument \code{area}.
#'
#' @param area A single character value of the area or areas you want to
#' predict to. Multiple areas must be separated with an underscore, e.g.,
#' \code{"wa_or"} or \code{"north_south"}.
#' The default is \code{"coast"}, which provides shallow and deep
#' strata for each state; thereby, covering the entire US West Coast.
#' @importFrom rlang current_env
#' @export
#' @return
#' A data frame with six columns,
#' \itemize{
#' \item name,
#' \item area,
#' \item Depth_m.1 (m; the shallow depth border),
#' \item Depth_m.2 (m; the deep depth border),
#' \item Latitude_dd.1 (decimal degrees; southern border), and
#' \item Latitude_dd.2 (decimal degrees; northern border).}
#' @seealso This function uses \code{\link{CreateStrataDF.fn}}
#' to generate the strata returned by this function. Also, see
#' convert_strata4vast function in the VASTWestCoast package that converts results
#' from this function to strata that can be used within the VAST package.
#'
#' @author Chantel Wetzel and Kelli Faye Johnson
#'
GetStrata.fn <- function(
  area = "coast"
) {
  # development instructions ... find the numbers for each comment
  # (1) Add your area here in lower-case text
  # (2) make a new strata and name the object the same lower-case text

  # internal function to order data frame by name column
  localorder <- function(data) {
    # order both sides of "_" descending
    orderhere <- as.data.frame(do.call("rbind", strsplit(data[, "name"], "_")))
    colnames(orderhere) <- c("a", "b")
    out <- data[do.call(order, c(lapply(
      orderhere[c("a", "b")],
      function(x) -xtfrm(x)
    ))), ]
    return(out)
  }
  # internal values
  deepm <- c(183, 549)
  shalm <- c(55, deepm[1])
  deepestm <- 1280
  CA <- c(32.0, 42.0)
  OR <- c(CA[2], 46.0)
  WA <- c(OR[2], 49.0)

  area <- tolower(area)
  if (length(area) > 1) {
    stop("The argument area must be a single character value.", call. = FALSE)
  }

  #### (1)
  defaults <- c(
    # coastwide strata
    "coast", "deep", "medium",
    # states
    "wa", "or", "ca",
    # areas
    "north", "south", "north_south",
    # spp specific
    "sablefish"
  )

  #### (2)
  # standard state-based strata
  ca <- CreateStrataDF.fn(
    names = c("shallow_ca", "deep_ca"),
    depths.shallow = shalm,
    depths.deep = deepm,
    lats.south = rep(CA[1], 2),
    lats.north = rep(CA[2], 2)
  )
  or <- CreateStrataDF.fn(
    names = c("shallow_or", "deep_or"),
    depths.shallow = shalm,
    depths.deep = deepm,
    lats.south = rep(OR[1], 2),
    lats.north = rep(OR[2], 2)
  )
  wa <- CreateStrataDF.fn(
    names = c("shallow_wa", "deep_wa"),
    depths.shallow = shalm,
    depths.deep = deepm,
    lats.south = rep(WA[1], 2),
    lats.north = rep(WA[2], 2)
  )
  if (area == "coast") {
    area <- "wa_or_ca"
  }

  # standard species-based strata
  sablefish <- CreateStrataDF.fn(
    names = c(outer(c("shallow", "deep"), c("coast", "north", "south"),
      paste,
      sep = "_"
    )),
    depths.shallow = rep(shalm, 3),
    depths.deep = rep(c(shalm[2], deepestm), 3),
    lats.south = rep(c(CA[1], 36, CA[1]), each = 2),
    lats.north = rep(c(WA[2], WA[2], 36), each = 2)
  )

  north_south <- CreateStrataDF.fn(
    names = c(outer(c("shallow", "deep"), c("coast", "north", "south"),
      paste,
      sep = "_"
    )),
    depths.shallow = rep(shalm, 3),
    depths.deep = rep(c(shalm[2], deepm[2]), 3),
    lats.south = rep(c(CA[1], 40.166667, CA[1]), each = 2),
    lats.north = rep(c(WA[2], WA[2], 40.166667), each = 2)
  )

  # other standard strata
  medium <- CreateStrataDF.fn(
    names = c("shallow_coast", "deep_coast"),
    depths.shallow = shalm,
    depths.deep = deepm,
    lats.south = rep(CA[1], 2),
    lats.north = rep(WA[2], 2)
  )
  deep <- CreateStrataDF.fn(
    names = c("shallow_coast", "med_coast", "deep_coast"),
    depths.shallow = c(shalm, deepm[2]),
    depths.deep = c(deepm, deepestm),
    lats.south = rep(CA[1], 3),
    lats.north = rep(WA[2], 3)
  )
  north <- CreateStrataDF.fn(
    names = c("shallow_north", "deep_north"),
    depths.shallow = shalm,
    depths.deep = deepm,
    lats.south = c(40.166667, 40.166667),
    lats.north = c(49.0, 49.0)
  )
  south <- CreateStrataDF.fn(
    names = c("shallow_south", "deep_south"),
    depths.shallow = shalm,
    depths.deep = deepm,
    lats.south = c(32.0, 32.0),
    lats.north = c(40.166667, 40.166667)
  )

  # get and return the needed strata
  areasp <- unlist(strsplit(area, "_"))
  if (!any(areasp %in% defaults)) {
    stop("Unrecognized area provided, please use '_' to separate these defaults",
      "\n", knitr::combine_words(defaults, and = " or "),
      call. = FALSE
    )
  }
  thisenv <- current_env()
  out <- localorder(do.call("rbind", lapply(areasp, get, envir = thisenv)))
  return(out)
}
