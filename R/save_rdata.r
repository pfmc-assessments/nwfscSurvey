#' Save an object in an `.rdata` file
#'
#' @inheritParams pull_catch
#' @param x An object.
#' @param name_base A string that will be appended to with the system time and
#'   the file extension (i.e., `".rdata"`).
#'
#' @return Nothing is returned, instead a file is saved in `dir`.
save_rdata <- function(x, name_base, dir = NULL, verbose = TRUE) {
  if (is.null(dir)) {
    return()
  }
  check_dir(dir = dir, verbose = verbose)

  time <- substring(Sys.time(), 1, 10)
  file_name <- paste0(name_base, "_", time, ".rdata")
  type_message <- gsub(
    pattern = "(^[a-z]{1})",
    replacement = "\\U\\1",
    x = strsplit(file_name, "_")[[1]][1],
    perl = TRUE
  )
  save(
    x,
    file = file.path(dir, file_name)
  )

  if (verbose) {
    cli::cli_alert_info(
      "{type_message} data file saved to following location: {dir}"
    )
  }
}
