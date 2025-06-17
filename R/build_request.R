#' Build request version 2
#'
#' @param dlw_url character: in case we have more than one url.
#' @param api_version character: API version
#' @param endpoint character: dlw  API endpoint
#' @param ... other parameters
#' @param method character: method of http request. Either "GET" or "POST".
#'   Default is "GET".
#'
#' @return httr2 request
build_request <- function(dlw_url = NULL,
                          api_version = getOption("dlw.default_api_version"),
                          endpoint,
                          method = "GET",
                          ...) {
  base_url <- select_base_url(dlw_url = dlw_url)
  params   <- list(...)

  req <- httr2::request(base_url) |>
    httr2::req_url_path_append(api_version) |>
    httr2::req_url_path_append(endpoint) |>
    httr2::req_user_agent(dlw_user_agent) |>
    httr2::req_auth_bearer_token(dlw_get_token())

  if (length(params) > 0) {

    get_or_post <- query_or_form(method = method)

    req <- req |>
    # httr2::req_url_query(!!!params, .multi = "comma") |>
    get_or_post(!!!params, .multi = "comma") |>
    httr2::req_cache(tools::R_user_dir("dlw", which = "cache"),
                     use_on_error = TRUE,
                     debug = TRUE) # |>
  }

  rlang::env_poke(env = .dlwenv,
                  nm = "last_req",
                  value = req)

  # To add later
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
      # I had to do it this way because, for some reason, the datalibweb API is
      # not returning the right code in the response, so I could not use the
      # handle httr2_http_401 of httr2
      resp <- httr2::last_response()
      status <- httr2::resp_status(resp)
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


#' select between query or form according to method
#'
#' @inheritParams build_request
#'
#' @returns correspongin httr2 function
#' @keywords internal
query_or_form <- function(method = c("GET", "POST")) {
  method <- toupper(method)
  if (method == "GET") {
    return(httr2::req_url_query)
  } else if (method == "POST") {
    return(httr2::req_body_form)
  } else {
    cli::cli_abort("Only 'GET' and 'POST' methods are supported for parameter placement.")
  }
}
