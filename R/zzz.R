op.dlw <- list(
  dlw.verbose         = TRUE,
  dlw.default_api_version = "v1",
  dlw.output_method   = cli::ansi_has_hyperlink_support()

)


.onLoad <- function(libname, pkgname) {
  op <- options()
  toset <- !(names(op.dlw) %in% names(op))

  if(any(toset)) {
    options(op.dlw[toset])
  }

  #get_dlw_options()

  invisible()
}
