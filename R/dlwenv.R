#' .dlwenv environment
#' @name dlwenv
NULL
#> NULL


#' Get function: Returns the entire .dlwenv environment
#'
#' @return The .dlwenv environment
#' @rdname dlwenv
#' @family dlwenv utilities
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
#' @rdname dlwenv
#' @family dlwenv utilities
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
#' @rdname dlwenv
#' @family dlwenv utilities
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


#' Get the list of objects in a specified environment
#'
#' @param env A Environment name, default to `.dlwenv`
#'
#' @returns List of objects
#' @export
#'
#' @examples
#' dlw_list_env()
dlw_list_env <- function(env = .dlwenv) {

  # Check if the provided argument is an environment
  if (!is.environment(env)) {

    cli::cli_abort("The provided argument is not an environment.")
  }

  # get the list of objects in the environment
  obj_list <- ls(env)

  return(invisible(obj_list))
}
