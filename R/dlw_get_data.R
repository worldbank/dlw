
#' get data from datalibweb
#'
#' @param server
#' @param country_code
#' @param year
#' @param collection
#' @param module
#'
#' @returns
#' @export
#'
#' @examples
dlw_get_data <- function(server = NULL,
                         country_code,
                         year,
                         collection,
                         module) {

  ctl <- dlw_server_catalog(server = server,
                            verbose = FALSE)

  endpoint <- "FileInformation/GetFileInfo"

  req <- build_request(endpoint = endpoint,
                       method = "POST",
                       Server = server,
                       Country = country_code,
                       Collection = collection,
                       Folder = module,
                       Year = year)

  resp <- req |>
    req_perform()   |>
    resp_body_string() |>
    gsub("\r\n|\r", "\n", x = _) |>
    fread(text = _, data.table = TRUE)

  resp
}



#
# latest_file <- function(server, country, year, collection, folder) {
#   endpoint <- "FileInformation/GetFileInfo"
#   form <- c(Server = server, Country = country, Collection = collection,
#             Folder = folder, Year = year)
#
#   req <- build_request(endpoint = endpoint,
#                        Server = server, Country = country, Collection = collection,
#                        Folder = folder, Year = year)
#
#   resp <- req |>
#     req_perform()   |>
#     resp_body_string() |>
#     gsub("\r\n|\r", "\n", x = _) |>
#     fread(text = _, data.table = TRUE)
#   resp
# }
