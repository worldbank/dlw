op.dlw <- list(
  dlw.verbose         = TRUE,
  dlw.default_api_version = "v1",
  dlw.output_method   = cli::ansi_has_hyperlink_support(),
  cli.ignore_unknown_rstudio_theme = TRUE,
  dlw.local_dir = Sys.getenv("DLW_local_dir"),
  dlw.board_type = c("folder", "local"),
  dlw.format     = c("qs", "parquet")
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
