# Constants for keyring and service
.dlw_keyring <- "datalibweb"
.dlw_service <- "datalibweb"

#' Store Datalibweb API token securely
#' @param token The API token as a string
#' @export
dlw_set_token <- function(token) {
  # Ensure keyring exists and is unlocked
  if (keyring::default_backend()$name == "file") {
    if (!.dlw_keyring %in% keyring::keyring_list()$keyring) {
      keyring::keyring_create(.dlw_keyring)
    }
    if (keyring::keyring_is_locked(.dlw_keyring)) {
      message("Unlocking keyring...")
      keyring::keyring_unlock(.dlw_keyring)
    }
    keyring::key_set_with_value(
      service = .dlw_service,
      password = token,
      keyring = .dlw_keyring
    )
  } else {
    keyring::key_set_with_value(
      service = .dlw_service,
      password = token
    )
  }
  message("Token stored successfully.")
  invisible(TRUE)
}

#' Retrieve Datalibweb API token, prompt if missing/expired
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
    token
  }, error = function(e) {
    if (prompt_if_missing) {
      message("No token found. Please enter your Datalibweb API token:")
      token <- readline("Token: ")
      dlw_set_token(token)
      return(token)
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
  message("Token removed.")
  invisible(TRUE)
}
