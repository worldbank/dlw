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
dlw_server_catalog <- function(server = NULL) {
  endpoint <- "ServerCatalog"
  base_server <- select_base_server(server = server)

}
