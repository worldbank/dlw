#' Server catalog
#'
#' @param server character: in case we have more than one server. default GMD
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
                              api_version = getOption("dlw.default_api_version")
                              ) {
  endpoint <- "ServerCatalog"
  base_server <- select_base_server(server = server)

  req <- build_request(dlw_url = dlw_url,
                       api_version = api_version,
                       endpoint,
                       Server = base_server)
  req |>
    httr2::req_perform() |>
    httr2::resp_body_string() |>
    fread()
}
