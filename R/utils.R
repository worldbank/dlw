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
#' Helper function to select server
#'
#' @param server character: c("GMD"). Defaults to NULL (ie. GMD).
#' @return character
#' @keywords internal
select_server <- function(server = NULL) {
  if (is.null(server)) {
    return("GMD")
  }

  # we should add some conditions to notify the user when server is not
  # available.
  server
}


#' Select base collection
#'
#' Helper function to select collection argument for endpoint based on server
#'
#' @rdname select_server
select_collection <- function(server = NULL) {
  if (is.null(server)) {
    return("GMD")
  }

  # we should add some conditions to notify the user when server is not
  # available.
  server
}







#' capitalize first letter in each word
#'
#' @param s character
#'
#' @returns first letter of each work in s capitalized
#' @keywords internal
simpleCap <- function(s) {
  s <- strsplit(s, " ")[[1]]
  paste(toupper(substring(s, 1, 1)), substring(s, 2),
        sep = "", collapse = " ")
}
