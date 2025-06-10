#' Server catalog
#'
#' @param server character: in case we have more than one server. default is  GMD
#' @param verbose logical: whether to display info
#' @param force logical: If TRUE, it will query the API regardless of whether
#'   there is version available in .dlwenv
#' @inheritParams build_request
#'
#' @returns dataframe with
#' @export
#'
#' @examples
#' \dontrun{
#' dlw_server_catalog(sever = "GMD")
#' }
dlw_server_catalog <- function(server = NULL,
                              dlw_url = NULL,
                              api_version = getOption("dlw.default_api_version"),
                              force = FALSE,
                              verbose = getOption("dlw.verbose")
                              ) {
  endpoint <- "ServerCatalog"
  base_server <- select_server(server = server)

  key <- paste(endpoint, base_server, sep = "_")

  ctl <- get_from_dlwenv(key, verbose)

  # Early return
  if (!is.null(ctl) && force == FALSE) return(ctl)

  req <- build_request(dlw_url = dlw_url,
                       api_version = api_version,
                       endpoint,
                       Server = base_server)
  ctl <- req |>
    httr2::req_perform() |>
    httr2::resp_body_string() |>
    fread(data.table = TRUE)

  set_in_dlwenv(key, ctl, verbose)

  ctl
}
