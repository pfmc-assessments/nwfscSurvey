#' @param verbose A logical value specifying if output should be written
#'   to the screen or not. Good for testing and exploring your data but
#'   can be turned off when output indicates errors.
#' The printing does not affect any of the returned objects, instead,
#' printing makes it easier to see if the data has errors and what was
#' done to rectify them in the given function.
#' Sorry, but the default is to always print to the screen,
#' i.e., `verbose = TRUE`, so you do not miss out on anything.
#' This is standard practice for packages in the nwfsc-assess organization.