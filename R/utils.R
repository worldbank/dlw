#' Select base URL
#'
#' Helper function to switch base URLs depending on DLW server being used
#'
#' @param server character: c("prod", "qa", "dev"). Defaults to NULL (ie. prod).
#' @return character
#' @noRd
select_base_url <- function(server) {
  if (!is.null(server)) {
    match.arg(server, c("prod", "qa", "dev"))
    if (server %in% c("qa", "dev")) {
      cli::cli_abort("servers {.code qa} and {.code dev} have not been setup yet")
    }
  }

  if (is.null(server) || server == "prod") {
    base_url <- prod_url
  }

  return(base_url)
}
