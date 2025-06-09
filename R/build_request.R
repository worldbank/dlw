#' Build request version 2
#'
#' @param dlw_url character: in case we have more than one url.
#' @param server character: in case we have more than one server. default GMD
#' @param api_version character: API version
#' @param endpoint character: dlw  API endpoint
#' @param ... other parameters
#'
#' @return httr2 request
build_request <- function(dlw_url = NULL,
                          server = NULL,
                          api_version = getOption("dlw.default_api_version"),
                          endpoint,
                          ...) {

  # endpoint <- "FileInformation/GetFileInfo"

  base_url    <- select_base_url(dlw_url = dlw_url)
  base_server <- select_base_server(server = server)
  params <- list(...) |>
    append(Server = base_server)
  # params <- list(Server = "GMD", Country = "VNM",
  #           Collection = "GMD", Year = 2018)

  req <- httr2::request(base_url) |>
    httr2::req_url_path_append(api_version) |>
    httr2::req_url_path_append(endpoint) |>
    httr2::req_auth_bearer_token(dlw_get_token()) |>
    # .multi = "comma" works fine without applying fix_params
    httr2::req_url_query(!!!params, .multi = "comma") |>
    httr2::req_cache(tools::R_user_dir("dlw", which = "cache"),
                     use_on_error = TRUE,
                     debug = TRUE) # |>

  # To add later
    # httr2::req_user_agent(dlw_user_agent) |>
    # httr2::req_error(body = parse_error_body) |>
    # httr2::req_retry(
    #   is_transient = dlw_is_transient,
    #   after = retry_after,
    #   max_seconds = 60
    # )


  return(req)

}
