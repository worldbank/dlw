#' Build request version 2
#'
#' @param dlw_url character: in case we have more than one url.
#' @param api_version character: API version
#' @param endpoint character: dlw  API endpoint
#' @param ... other parameters
#' @param method character: method of http request. Either "GET" or "POST".
#'   Default is "GET".
#' @param store_request logical: store request in .dlwenv as `last_req`
#'
#' @return httr2 request
#' @keywords internal
build_request <- function(dlw_url = NULL,
                          api_version = getOption("dlw.default_api_version"),
                          endpoint,
                          method = "GET",
                          store_request = TRUE,
                          ...) {
  base_url <- select_base_url(dlw_url = dlw_url)
  params   <- list(...)

  req <- base_url |>
    httr2::request() |>
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
                     debug = TRUE)
  }

  if (store_request) set_in_dlwenv(key = "last_req", value = req)

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
#' @returns a data.table if the response is CSV. A raw vector if the response is
#'   dta. Error otherwise
#'
#' @keywords internal
handle_resp <- function(req) {
  out <- tryCatch(
    expr = {
        resp <- handle_req_perform(req)

        file_type <-
          httr2::resp_content_type(resp) |>
          fs::path_file()

        if (file_type == "csv") {
          rs <- resp |>
            httr2::resp_body_string() |>
            strsplit("\r\n") |>
            unlist() |>
            paste(collapse = "\n") |>
            fread(data.table = TRUE)

          # It here a variables with error
          error_found <- names(rs) |>
            tolower() |>
            grepl("error", x = _) |>
            any()
          if (error_found) {
            abort_dlw_error(rs)
          }
          rs
        } else if (file_type == "dta") {
            httr2::resp_body_raw(resp)
        } else {
          cli::cli_abort("For now, only CSV and dta formats are supported")
        }

    }, # end of expr section
    dlw_api_error = \(e) {
      # placeholder
      rlang::cnd_signal(e)
    },
    error = \(e) {
      # placeholder
      rlang::cnd_signal(e)
    },
    warning = \(w) {
      # placeholder
      rlang::cnd_signal(w)
    }, # end of warning section

    finally = {
      # placeholder
    } # end of finally section

  ) # End of trycatch

  out
}


#' perform httr2::req_perform and handle errors properly
#'
#' @param req A httr2 request object.
#'
#' @returns an HTTP response.
#' @rdname handle_resp
#' @export
handle_req_perform <- \(req) {
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
      # Do this at the end before quitting the tryCatch
    } # end of finally section

  ) # End of trycatch

  out
}


#' return information of error
#'
#' @param info csv file with at least one variable with "error" in its name.
#'
#' @returns error
#' @keywords internal
#'
abort_dlw_error <- function(info) {
  # Filter out NULL or NA values
  select_cols <- vapply(info, \(x) {
    !is.null(x) && !is.na(x)
  },
  logical(1))

  show_fields <- names(info)[select_cols]
  # Build a named list for cli::format_error
  details <-
    vapply(show_fields, \(nm) {
      paste0("\n{.field ", nm, "}: {.val ", info[[nm]], "}\n")
    },
    character(1)) |>
    stats::setNames(rep("*", length(show_fields)))
  # Add a main error message
  msg <- c("!" = "API request failed. Error details:", details)
  cli::cli_abort(msg, .subclass = c("dlw_api_error", "api_error"))
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
