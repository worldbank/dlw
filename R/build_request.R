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
          resp_csv(resp)
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


#' parse a CSV response of the API
#'
#' @param resp httr2 response with a CSV body.
#'
#' @returns data.table. Error if the CSV describes an API error.
#' @keywords internal
resp_csv <- function(resp) {
  rs <- resp |>
    httr2::resp_body_string() |>
    strsplit("\r\n|\r|\n") |>
    unlist() |>
    paste(collapse = "\n") |>
    fread(text = _, data.table = TRUE)

  # It here a variables with error. Only single-row responses are
  # errors; multi-row responses are lists of file versions where
  # ErrorCode is a status.
  error_found <- names(rs) |>
    tolower() |>
    grepl("error", x = _) |>
    any()
  if (error_found && nrow(rs) == 1) {
    abort_dlw_error(rs)
  }
  rs
}


#' perform request and keep the file as returned by the API
#'
#' Unlike [handle_resp], the body of any file type is returned as raw bytes,
#' with no parsing. The CSV that the API uses to report errors and lists of
#' file versions (`ECAFileinfo.csv`, or an unnamed CSV with `ErrorCode`) is
#' parsed as in [handle_resp].
#'
#' @param req A httr2 request object.
#'
#' @returns a data.table if the response is a CSV report of the API. Otherwise
#'   a list with the raw `body`, `filename` (from the Content-Disposition
#'   header, NULL if missing), `content_type` and HTTP `status`.
#' @keywords internal
handle_file_resp <- function(req) {
  resp <- handle_req_perform(req)

  content_type <- httr2::resp_header(resp, "Content-Type")
  file_type    <- httr2::resp_content_type(resp) |>
    fs::path_file()
  filename     <- resp_filename(resp)

  body <- if (httr2::resp_has_body(resp)) httr2::resp_body_raw(resp) else raw(0)
  if (length(body) == 0) {
    cli::cli_abort("The API returned an empty file.",
                   .subclass = c("dlw_api_error", "api_error"))
  }

  if (identical(file_type, "csv")) {
    report <- if (is.null(filename)) {
      cols <- fread(text = httr2::resp_body_string(resp), nrows = 0)
      "errorcode" %in% tolower(names(cols))
    } else {
      tolower(filename) == "ecafileinfo.csv"
    }
    if (report) return(resp_csv(resp))
  }

  list(body         = body,
       filename     = filename,
       content_type = content_type,
       status       = httr2::resp_status(resp))
}


#' file name in the Content-Disposition header of a response
#'
#' @param resp httr2 response.
#'
#' @returns character or NULL if the header has no file name.
#' @keywords internal
resp_filename <- function(resp) {
  disp <- httr2::resp_header(resp, "Content-Disposition")
  if (is.null(disp)) {
    return(NULL)
  }
  # first group of the first match of `pattern` in `disp`, or NULL
  header_value <- \(pattern) {
    m <- regmatches(disp, regexec(pattern, disp, perl = TRUE))[[1]]
    if (length(m) == 2) m[2]
  }

  # only `filename*=` (RFC 5987) is percent-encoded; plain names are literal
  nm <- header_value("(?i)filename\\*\\s*=\\s*[^']*'[^']*'([^;]+)")
  if (!is.null(nm)) {
    nm <- tryCatch(utils::URLdecode(nm), error = \(e) NULL)
    if (!is.null(nm)) Encoding(nm) <- "UTF-8"
  } else {
    nm <- c(header_value("(?i)filename\\s*=\\s*\"([^\"]*)\""),
            header_value("(?i)filename\\s*=\\s*([^;\"]+)"))[1]
  }
  if (is.null(nm) || is.na(nm)) {
    return(NULL)
  }
  # only the last component: the name comes from the server and may be used
  # to build a local path
  nm <- trimws(nm) |>
    gsub("\\\\", "/", x = _) |>
    fs::path_file()
  if (nm %in% c("", ".", "..")) NULL else nm
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
      cli::cli_abort("The connection is dropped or the server doesn't exist.",
                     # the underlying curl error, not `e`, which holds the request
                     parent = e$parent,
                     .subclass = c("dlw_transport_error", "dlw_api_error",
                                   "api_error"))

    },
    error = \(e) {
      # not an HTTP error: nothing to translate
      if (!inherits(e, "httr2_http")) rlang::cnd_signal(e)
      abort_http_error(e, req)
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


#' abort with the details of a failed HTTP request
#'
#' The condition keeps the status, reason phrase, endpoint, form parameters,
#' response body, `Retry-After` header and the request (without its
#' Authorization header).
#'
#' @param e httr2 HTTP error (class `httr2_http`) from [httr2::req_perform].
#' @param req The httr2 request that failed.
#'
#' @returns error of class `dlw_http_<status>`, `dlw_http_error`,
#'   `dlw_api_error` and `api_error`.
#' @keywords internal
abort_http_error <- function(e, req) {
  resp        <- e$resp
  status      <- httr2::resp_status(resp)
  reason      <- httr2::resp_status_desc(resp)
  endpoint    <- sub("^.*/v[0-9]+/", "", req$url)
  params      <- req$body$data
  retry_after <- httr2::resp_header(resp, "Retry-After")
  # the API can echo request headers (i.e., the token) in its error body
  body        <- tryCatch(httr2::resp_body_string(resp), error = \(e) "") |>
    redact_token(req)
  api_msg     <- http_error_message(resp, body, req)

  hint <- if (status == 401) {
    "DatalibWeb Token invalid.
     Go to {.href [Datalibweb](http://datalibweb/)} website, generate a new token,
     and then use {.code dlw::dlw_set_token()}"
  } else if (status == 403) {
    "Your account does not have access to this resource."
  } else if (status == 404) {
    "The requested resource was not found."
  } else if (status == 429) {
    paste0("Too many requests.",
           if (!is.null(retry_after)) " Retry after {retry_after} seconds.")
  } else if (status >= 500) {
    "Server-side failure."
  }

  params_txt <- vapply(params, \(x) paste(x, collapse = ","), character(1)) |>
    (\(x) paste(names(x), x, sep = "=", collapse = ", "))()

  msg <- c("DatalibWeb request failed [{status} {reason}]",
           "i" = hint,
           "i" = "Endpoint: {.val {endpoint}}",
           "i" = if (length(params) > 0) "Parameters: {params_txt}",
           "x" = if (nzchar(api_msg)) "API message: {api_msg}")

  cli::cli_abort(msg,
                 .subclass = c(paste0("dlw_http_", status), "dlw_http_error",
                               "dlw_api_error", "api_error"),
                 status = status,
                 reason = reason,
                 endpoint = endpoint,
                 params = params,
                 response_body = body,
                 retry_after = retry_after,
                 request = httr2::req_headers(req, Authorization = NULL))
}


#' remove the token of a request from text
#'
#' @param x character
#' @param req httr2 request
#'
#' @returns `x` without the bearer token or any Authorization header.
#' @keywords internal
redact_token <- function(x, req) {
  auth  <- httr2::req_get_headers(req, redacted = "reveal")$Authorization
  token <- sub("^Bearer ", "", auth)
  if (length(token) == 1 && nzchar(token)) {
    x <- gsub(token, "[REDACTED]", x, fixed = TRUE)
  }
  gsub("(?i)(authorization:\\s*(bearer\\s+)?)\\S+", "\\1[REDACTED]", x,
       perl = TRUE)
}


#' short message from the body of an error response
#'
#' @param resp httr2 response.
#' @param body character: body of `resp` as text.
#' @param req httr2 request.
#'
#' @returns character: `detail` or `title` of a JSON body, or the first line of
#'   a text body. Empty string if there is none.
#' @keywords internal
http_error_message <- function(resp, body, req) {
  if (grepl("json", httr2::resp_content_type(resp))) {
    parsed <- tryCatch(httr2::resp_body_json(resp), error = \(e) NULL)
    body   <- c(parsed[["detail"]], parsed[["title"]], "")[[1]]
  }
  # redact after JSON decoding, before a token can be cut short
  body <- redact_token(body, req)
  strsplit(body, "\r?\n")[[1]][1] |>
    trimws() |>
    substr(1, 200) |>
    (\(x) if (is.na(x)) "" else x)()
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
