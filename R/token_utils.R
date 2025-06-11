# Constants for keyring and service
.dlw_keyring <- "datalibweb"
.dlw_service <- "datalibweb"

#' Store Datalibweb API token securely
#' @param token The API token as a string
#' @export
dlw_set_token <- function(token) {
  # If using the "file" backend (not the default on Windows/Mac/Linux)
  if (keyring::default_backend()$name == "file") {
    # Create the keyring if it doesn't exist
    if (!.dlw_keyring %in% keyring::keyring_list()$keyring) {
      keyring::keyring_create(keyring = .dlw_keyring)
    }
    # Unlock the keyring if it's locked
    if (keyring::keyring_is_locked(keyring = .dlw_keyring)) {
     cli::cli_inform("Unlocking keyring...")
      keyring::keyring_unlock(keyring = .dlw_keyring)
    }
    # Store the token in the named keyring
    keyring::key_set_with_value(
      service = .dlw_service,
      password = token,
      keyring = .dlw_keyring
    )
  } else {
    # For Windows/Mac/Linux default: just store with service name
    keyring::key_set_with_value(
      service = .dlw_service,
      password = token
    )
  }
  cli::cli_inform("Token stored successfully.")
  invisible(TRUE)
}

#' Retrieve Datalibweb API token, prompt if missing/expired
#'
#' @param prompt_if_missing logical: whether to be prompted to store token
#'
#' @return invisible token
#' @export
dlw_get_token <- function(prompt_if_missing = TRUE) {
  # Try to get the token
  tryCatch({
    if (keyring::default_backend()$name == "file") {
      if (keyring::keyring_is_locked(.dlw_keyring)) {
        keyring::keyring_unlock(.dlw_keyring)
      }
      token <- keyring::key_get(
        service = .dlw_service,
        keyring = .dlw_keyring
      )
    } else {
      token <- keyring::key_get(service = .dlw_service)
    }

  }, error = function(e) {
    if (prompt_if_missing) {
     cli::cli_inform("No token found. Please enter your Datalibweb API token:")
      token <- readline("Please, provide DLW Token: ")
      dlw_set_token(token)
      return(invisible(token))
    } else {
      stop("No token found. Please set it with dlw_set_token().")
    }
  })
}

#' Remove/reset the Datalibweb API token
#' @export
dlw_remove_token <- function() {
  if (keyring::default_backend()$name == "file") {
    keyring::key_delete(
      service = .dlw_service,
      keyring = .dlw_keyring
    )
  } else {
    keyring::key_delete(service = .dlw_service)
  }
  cli::cli_inform("Token removed.")
  invisible(TRUE)
}

#' confirm wither the token is working correctly
#'
#' @inheritParams build_request
#'
#' @returns TRUE if all goes well
#' @export
#'
#' @examples
#' \dontrun {
#' dlw_test_token()
#' }
dlw_test_token <- function(dlw_url = NULL,
                           api_version = getOption("dlw.default_api_version"),
                           verbose =  getOption("dlw.verbose")) {

  endpoint <- "Token/profile"
  req <- build_request(dlw_url = dlw_url,
                       api_version = api_version,
                       endpoint = endpoint) |>
    handle_resp()
  if (verbose) {
    cli::cli_alert_success("Token valid")
  }
  invisible(TRUE)
}

