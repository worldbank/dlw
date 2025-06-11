#' Select base URL
#'
#' Helper function to switch base URLs depending on DLW server being used
#'
#' @param dlw_url character: c("prod", "qa", "dev"). Defaults to NULL (ie. prod).
#' @return character
#' @noRd
select_base_url <- function(dlw_url) {
  if (!is.null(dlw_url)) {
    match.arg(dlw_url, c("prod", "qa", "dev"))
    if (dlw_url %in% c("qa", "dev")) {
      cli::cli_abort("URLS {.code qa} and {.code dev} have not been setup yet")
    }
  }

  if (is.null(dlw_url) || dlw_url == "prod") {
    base_url <- prod_url
  }

  return(base_url)
}

#' Select base server
#'
#' Helper function to select server
#'
#' @param server character: c("GMD"). Defaults to NULL (ie. GMD).
#' @return character
#' @keywords internal
select_server <- function(server = NULL) {
  if (is.null(server)) {
    return("GMD")
  }

  # we should add some conditions to notify the user when server is not
  # available.
  server
}


#' Select base collection
#'
#' Helper function to select collection argument for endpoint based on server
#'
#' @rdname select_server
select_collection <- function(server = NULL) {
  if (is.null(server)) {
    return("GMD")
  }

  # we should add some conditions to notify the user when server is not
  # available.
  server
}

#' capitalize first letter in each word
#'
#' @param s character
#'
#' @returns first letter of each work in s capitalized
#' @keywords internal
simpleCap <- function(s) {
  s <- strsplit(s, " ")[[1]]
  paste(toupper(substring(s, 1, 1)), substring(s, 2),
        sep = "", collapse = " ")
}



# Define a constructor for your call list
as.dlw_call_list <- function(x) {
  class(x) <- c("dlw_call_list", class(x))
  x
}

# # S3 print method using cli
# print.dlw_call_list <- function(x, ...) {
#   cli::cli_h1("dlw_get_data Calls")
#   for (i in seq_along(x)) {
#     call_str <- deparse(x[[i]])
#     cli::cli_div(theme = list(span.emph = list(color = "cyan")))
#     cli::cli_text("{.emph Call {i}}:")
#     cli::cli_code(call_str)
#     cli::cli_end()
#   }
#   invisible(x)
# }

#
# print.dlw_call_list <- function(x, ...) {
#   cli::cli_h1("dlw_get_data Calls")
#   for (i in seq_along(x)) {
#     call_str <-paste(rlang::expr_deparse(x[[i]], width =  60), collapse = "\n")
#     cli::cli_div(theme = list(span.emph = list(color = "cyan")))
#     cli::cli_text("{.emph Call {i}}:")
#     cli::cli_code(call_str)
#     cli::cli_end()
#   }
#   invisible(x)
# }

# print.dlw_call_list <- function(x, ...) {
#   cli::cli_h1("dlw_get_data Calls")
#   for (i in seq_along(x)) {
#     call_obj <- x[[i]]
#     fn_name <- as.character(call_obj[[1]])
#     args <- as.list(call_obj)[-1]
#     cli::cli_div(theme = list(span.emph = list(color = "cyan")))
#     cli::cli_text("{.emph Call {i}}:")
#     cli::cli_code(paste0(fn_name, "("))
#     for (j in seq_along(args)) {
#       arg_name <- names(args)[j]
#       arg_val <- rlang::expr_deparse(args[[j]])
#       comma <- if (j < length(args)) "," else ""
#       cli::cli_code(paste0("  ", arg_name, " = ", arg_val, comma, "\n"))
#     }
#     cli::cli_code(")")
#     cli::cli_end()
#   }
#   invisible(x)
# }


# print.dlw_call_list <- function(x, ...) {
#   cli::cli_h1("dlw_get_data Calls")
#   for (i in seq_along(x)) {
#     call_obj <- x[[i]]
#     fn_name <- as.character(call_obj[[1]])
#     args <- as.list(call_obj)[-1]
#     arg_names <- names(args)
#     cli::cli_div(theme = list(span.emph = list(color = "cyan")))
#     cli::cli_text("{.emph Call {i}}:")
#     cli::cli_text("{.code {fn_name}(}")
#     for (j in seq_along(args)) {
#       arg_name <- arg_names[j]
#       arg_val <- rlang::expr_deparse(args[[j]])
#       comma <- if (j < length(args)) "," else ""
#       cli::cli_text("  {.code {arg_name} = {arg_val}{comma}}")
#     }
#     cli::cli_text("{.code )}")
#     cli::cli_end()
#   }
#   invisible(x)
# }


print.dlw_call_list <- function(x, ...) {
  cli::cli_h1("dlw_get_data Calls")
  for (i in seq_along(x)) {
    call_obj <- x[[i]]
    fn_name <- as.character(call_obj[[1]])
    args <- as.list(call_obj)[-1]
    arg_names <- names(args)
    # Build code lines as a character vector
    code_lines <- c(
      paste0(fn_name, "("),
      vapply(
        seq_along(args),
        function(j) {
          arg_name <- arg_names[j]
          arg_val <- rlang::expr_deparse(args[[j]])
          comma <- if (j < length(args)) "," else ""
          paste0("  ", arg_name, " = ", arg_val, comma)
        },
        character(1)
      ),
      ")"
    )
    cli::cli_div(theme = list(span.emph = list(color = "cyan")))
    cli::cli_text("{.emph Call {i}}:")
    cli::cli_code(code_lines)
    cli::cli_end()
  }
  invisible(x)
}
