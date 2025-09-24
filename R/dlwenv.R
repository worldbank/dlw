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


##' List objects in an environment with their classes
#'
#' Lists the names and classes of all objects in a specified environment, using the rlang package for robust environment handling.
#'
#' @param env An environment object. Defaults to `.dlwenv`.
#' @param invisible Logical; if TRUE (default), returns the info invisibly. If FALSE, prints a formatted summary to the console.
#'
#' @return A data frame with columns `name` (object name) and `class` (object class), invisibly by default.
#' @export
#'
#' @examples
#' dlw_list_env() # List objects in .dlwenv
#' dlw_list_env(globalenv(), invisible = FALSE) # List objects in the global environment and print
#' tmpenv <- new.env(); assign("x", 1, envir = tmpenv); dlw_list_env(tmpenv, invisible = FALSE)
dlw_list_env <- function(env = .dlwenv, invisible = TRUE) {

  # Check if the provided argument is an environment
  if (!rlang::is_environment(env)) {
    cli::cli_abort("The provided argument is not an environment.")
  }

  obj_names <- rlang::env_names(env)
  if (length(obj_names) == 0) {
    cli::cli_alert_info("The environment is empty.")
    return(invisible(character(0)))
  }

  obj_classes <- vapply(obj_names, function(nm) {
    obj <- rlang::env_get(env, nm)
    paste(class(obj), collapse = ", ")
  }, character(1))

  info <- data.frame(
    name = obj_names,
    class = obj_classes,
    stringsAsFactors = FALSE
  )

  if (invisible) {
    return(invisible(info))
  }

  cli::cli_h1("Objects in environment:")
  for (i in seq_len(nrow(info))) {
    cli::cli_text("{info$name[i]}: {info$class[i]}")
  }
  invisible(info)
}
