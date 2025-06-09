#' Select base URL
#'
#' Helper function to switch base URLs depending on DLW server being used
#'
#' @param dlw_url character: c("prod", "qa", "dev"). Defaults to NULL (ie. prod).
#' @return character
#' @noRd
select_base_url <- function(dlw_url) {
  if (!is.null(dlw_url)) {
    match.arg(dlw_url, c("prod", "qa", "dev"))
    if (dlw_url %in% c("qa", "dev")) {
      cli::cli_abort("URLS {.code qa} and {.code dev} have not been setup yet")
    }
  }

  if (is.null(dlw_url) || dlw_url == "prod") {
    base_url <- prod_url
  }

  return(base_url)
}

#' Select base server
#'
#' Helper function to switch base URLs depending on DLW server being used
#'
#' @param server character: c("GMD"). Defaults to NULL (ie. GMD).
#' @return character
#' @noRd
select_base_server <- function(server) {
  if (!is.null(server)) {
    match.arg(server, c("GMD", "x"))
    if (server %in% c("x")) {
      cli::cli_abort("server {.code {server}} has not been setup yet")
    }
  }

  if (is.null(server) || server == "GMD") {
    base_server <- "GMD"
  }

  return(base_server)
}
