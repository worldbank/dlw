#' Build request version 2
#'
#' @param dlw_url character: in case we have more than one url.
#' @param api_version character: API version
#' @param endpoint character: dlw  API endpoint
#' @param ... other parameters
#'
#' @return httr2 request
build_request <- function(dlw_url = NULL,
                          api_version = getOption("dlw.default_api_version"),
                          endpoint,
                          ...) {

  base_url    <- select_base_url(dlw_url = dlw_url)
  params <- list(...)

  req <- httr2::request(base_url) |>
    httr2::req_url_path_append(api_version) |>
    httr2::req_url_path_append(endpoint) |>
    httr2::req_auth_bearer_token(dlw_get_token())

  if (length(params) > 0) {
    req <- req |>
    httr2::req_url_query(!!!params, .multi = "comma") |>
    httr2::req_cache(tools::R_user_dir("dlw", which = "cache"),
                     use_on_error = TRUE,
                     debug = TRUE) # |>
  }

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


#' perform httr2::req_perform and handle errors properly
#'
#' @param req A httr2 request object.
#'
#' @returns an HTTP response.
#' @export
handle_resp <- function(req) {
  out <- tryCatch(
    expr = {
        httr2::req_perform(req)
    }, # end of expr section
    httr2_failure = \(e) {
      cli::cli_abort(" the connection is dropped or the server doesn't exist")

    },
    error = \(e) {
      resp <- httr2::last_response()
      status <- resp$status_code
      if (status == 401) {
        cli::cli_abort("DatalibWeb Token invalid.
                    Go to {.href [Datalibweb](http://datalibweb/)} website, generate a new token,
                    and then use {.code dlw::dlw_set_token()}")
      } else {
        cli::cli_abort("Error with the DLW request", conditionMessage(e))
      }

    },
    # httr2_http_401 = \(e) {
    #   cli::cli_abort("DatalibWeb Token invalid.
    #                 Go to {.href [Datalibweb](http://datalibweb/)} website, generate a new token,
    #                 and then use {.code dlw::dlw_set_token()}")
    # }, # End of invalit Token
    warning = \(w) {
      w
    }, # end of warning section

    finally = {
      # Do this at the end before quitting the tryCatch structure...
    } # end of finally section

  ) # End of trycatch

  out
}


