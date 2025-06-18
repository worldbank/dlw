#' Get function: Returns the entire .dlwenv environment
#'
#' @return The .dlwenv environment
#' @export
#'
#' @examples
#' env <- get_dlwenv()
get_dlwenv <- function() {
  .dlwenv
}

#' Get for a specific key from .dlwenv
#'
#' @inheritParams set_in_dlwenv
#' @return The value associated with the key in .dlwenv
#' @export
#'
#' @examples
#' set_in_dlwenv("example_key", 42)
#' get_from_dlwenv("example_key") # returns 42
get_from_dlwenv <- function(key, verbose =  FALSE) {
  x <- rlang::env_get(.dlwenv, key, default = NULL) |>  # Returns NULL if key doesn't exist
    copy() # make sure it does not get modified in the dlwenv
  if (verbose && !is.null(x)) {
    cli::cli_alert_info("Returning {key} from .dlwevn")
  }
  x
}

#' Setter function: Assign a value in .dlwenv
#'
#' @param key A character string representing the key
#' @param value The value to store in .dlwenv
#' @inheritParams dlw_country_catalog
#'
#' @return The assigned value (invisibly)
#' @export
#'
#' @examples
#' set_in_dlwenv("example_key", 42)
set_in_dlwenv <- function(key,
                          value,
                          verbose =  FALSE) {
  rlang::env_poke(.dlwenv, key, value)
  if (verbose) {
    cli::cli_alert_info("saving {key} in .dlwevn")
  }
  invisible(value)  # Return value invisibly to avoid clutter in console
}
