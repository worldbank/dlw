#' Download data from datalibweb and save as a pin
#'
#' @inheritParams dlw_get_data
#' @param filename character: Name of the file to save/read (required)
#' @param board_type character: Which pins board to use: 'folder' (default,
#'   shared) or 'local' (user-specific)
#' @param format character: File format to use for pinning data ('parquet'
#'   [default] or 'qs')
#' @returns A list with the board and pin_name used
#' @keywords internal
#' @importFrom pins board_local board_temp pin_write
#' @importFrom httr2 req_perform resp_body_raw
#' @importFrom haven read_dta
#' @importFrom data.table setDT
#'
dlw_download <- function(country_code,
                         filename,
                         server = NULL,
                         local_dir = getOption("dlw.local_dir"),
                         local = fs::is_dir(local_dir),
                         board_type = c("folder", "local"),
                         format = c("parquet", "qs"),
                         local_overwrite = FALSE,
                         ...,
                         verbose = getOption("dlw.verbose")) {

  board_type <- match.arg(board_type)
  format     <- match.arg(format)

  if (missing(filename) || is.null(filename)) {
    cli::cli_abort("{.arg filename} is a required argument.")
  }

  # prepare the args for request
  dots <- list(...)
  endpoint <- "FileInformation/GetFileInfo"
  args <- c(list(Country = country_code,
                 method = "POST",
                 Server = server,
                 endpoint = endpoint,
                 filename = filename),
            dots)

  # Choose board
  board <- if (local) {
    if (board_type == "local") {
      pins::board_local(local_dir)
    } else {
      pins::board_folder(local_dir)
    }
  } else {
    if (!rlang::env_has(.dlwenv, "temp_board")) {
      brd <- pins::board_temp()
      rlang::env_poke(env = .dlwenv,
                      nm = "temp_board",
                      value = brd)
      brd
    } else {
      rlang::env_get(.dlwenv, "temp_board")
    }
  }
  pin_name <- paste0(fs::path_ext_remove(filename), ".", format)

  # If not overwriting and pin exists, skip download (early return)
  if (!local_overwrite && pin_name %in% pins::pin_list(board)) {
    return(list(board = board, pin_name = pin_name))
  }


  req <- do.call("build_request", args)
  raw_data <- req |>
    httr2::req_perform() |>
    httr2::resp_body_raw()

  # store in

  # Save raw data to a temp file for reading
  tmpfile <- fs::file_temp(ext =  "dta")
  writeBin(raw_data, tmpfile)

  # Read .dta and pin as parquet or qs
  dt <- haven::read_dta(tmpfile, encoding = "latin1") |>
    setDT()
  unlink(tmpfile)
  pins::pin_write(board = board,
                  x     = dt,
                  name  = pin_name,
                  type  = format,
                  versioned = TRUE)
  list(board = board, pin_name = pin_name)
}

#' Read data from a pin (local or temp)
#'
#' @param board A pins board object (as returned by dlw_download)
#' @param pin_name The name of the pin (as returned by dlw_download)
#' @returns data.table
#' @keywords internal
#' @importFrom pins pin_read
#' @importFrom data.table setDT
#'
dlw_read <- function(board, pin_name) {
  if (!(pin_name %in% pins::pin_list(board))) {
    cli::cli_abort("File {.file {pin_name}} not found in the provided board.")
  }
  pins::pin_read(board, pin_name) |>
    setDT()
}

#' get data from datalibweb (refactored)
#' @param filename character: Name of the file to save/read (required)
#' @inheritParams dlw_country_catalog
#' @inheritParams dlw_server_catalog
#' @param ... additional filtering arguments (e.g.,survey_year, survey_acronym,
#'   vermast, veralt, collection, module)
#' @param local_dir character: Local directory to save data. Default available
#'   in option dlw.local_dir which is set initially as "".
#' @param local logical: whether or not to save and read data locally. default
#'   is TRUE if `local_dir` exists.
#' @param board_type character: Which pins board to use: 'folder' (default,
#'   shared) or 'local' (user-specific)
#' @param format character: File format to use for pinning data ('parquet'
#'   [default] or 'qs')
#' @param local_overwrite logical. Whether to overwrite any saved data. Default
#'   is FALSE
#' @returns data base request as data.table
#' @export
dlw_get_data <- function(country_code,
                         server = NULL,
                         filename,
                         verbose =  getOption("dlw.verbose"),
                         local_dir = getOption("dlw.local_dir"),
                         local = fs::is_dir(local_dir),
                         board_type = c("folder", "local"),
                         format = c("parquet", "qs"),
                         local_overwrite = FALSE,
                         ...) {

  board_type <- match.arg(board_type)
  format     <- match.arg(format)

  if (missing(filename) || is.null(filename)) {
    cli::cli_abort("{.arg filename} is a required argument.")
  }

  dlw_info <- dlw_download(country_code    = country_code,
                          server          = server,
                          filename        = filename,
                          local_dir       = local_dir,
                          local           = local,
                          board_type      = board_type,
                          format          = format,
                          local_overwrite = local_overwrite,
                          ...,
                          verbose = verbose)

  dlw_read(board = dlw_info$board,
           pin_name = dlw_info$pin_name)
}
