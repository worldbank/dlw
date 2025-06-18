#' Get country catalog
#'
#' @param country_code character: ISO3 code
#' @param internal logical: for internal use ONLY.
#' @inheritParams dlw_server_catalog
#'
#' @returns data.table with country catalog
#' @export
#'
#' @examples
#' \dontrun{
#' dlw_country_catalog("COL")
#' }
dlw_country_catalog <- function(country_code,
                                dlw_url = NULL,
                                api_version = getOption("dlw.default_api_version"),
                                force = FALSE,
                                internal = FALSE,
                                verbose = getOption("dlw.verbose"),
                                store_request = FALSE) {
  if (internal) {
    endpoint <- "CountryCatalogInternal"
  } else {
    endpoint <- "CountryCatalog"
  }

  key <- paste(endpoint, country_code , sep = "_")

  ctl <- get_from_dlwenv(key, verbose)

  # Early return
  if (!is.null(ctl) && force == FALSE) return(ctl)

  endpoint <- c(endpoint, country_code)
  req <- build_request(dlw_url = dlw_url,
                       api_version = api_version,
                       endpoint = endpoint,
                       store_request = store_request)

  ctl <- handle_resp(req)

  set_in_dlwenv(key, ctl, verbose)
}
