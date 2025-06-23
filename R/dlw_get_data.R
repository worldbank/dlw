#' Download data from datalibweb and save as a pin
#'
#' @inheritParams dlw_get_data
#' @inheritParams dlw_read
#' @param filename character: Name of the file to save/read (required)
#' @param format character: File format to use for pinning data ('qs'
#'   [default] or 'parquet')
#' @returns A list with the board and pin_name used
#' @keywords internal
#' @importFrom pins board_local board_temp pin_write
#' @importFrom httr2 req_perform resp_body_raw
#' @importFrom haven read_dta
#' @importFrom data.table setDT
#'
dlw_download <- function(country_code,
                         filename,
                         board,
                         pin_name,
                         format,
                         server = NULL,
                         ...,
                         verbose = getOption("dlw.verbose")) {


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

  raw_data <- do.call("build_request", args) |>
    get_raw_data()

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
  dt
}

#' Read data from a pin (local or temp)
#'
#' @param board A pins board object (as returned by dlw_download)
#' @param pin_name The name of the pin (as returned by dlw_download)
#' @param version numeric: Version of the pin to read (for pinning data
#'   retrieval only)
#' @returns data.table
#' @keywords internal
#' @importFrom pins pin_read
#' @importFrom data.table setDT
#'
dlw_read <- function(board, pin_name, version = NULL) {

  if (!(pin_name %in% pins::pin_list(board))) {
    cli::cli_abort("File {.file {pin_name}} not found in the provided board.")
  }
  pins::pin_read(board, pin_name, version = version) |>
    setDT()

}


#' Get data from datalibweb (refactored)
#'
#' @param filename character: Name of the file to save/read (required)
#' @inheritParams dlw_country_catalog
#' @inheritParams dlw_server_catalog
#' @param ... additional filtering arguments (e.g.,survey_year, survey_acronym,
#'   vermast, veralt, collection, module)
#' @param local_dir character: Local directory to save data. Default available
#'   in option dlw.local_dir which is set initially as "".
#' @param local logical: whether or not to save and read data locally. default
#'   is TRUE if `local_dir` exists.
#' @param format character: File format to use for pinning data ('parquet'
#'   [default] or 'qs')
#' @param local_overwrite logical. Whether to overwrite any saved data. Default
#'   is FALSE
#' @param version numeric: Version of the pin to read (for pinning data
#'   retrieval only)
#' @returns data base request as data.table
#' @export
dlw_get_data <- function(country_code,
                         filename,
                         server          = NULL,
                         local_dir       = getOption("dlw.local_dir"),
                         local           = fs::is_dir(local_dir),
                         format          = getOption("dlw.format"),
                         local_overwrite = FALSE,
                         version         = NULL,
                         verbose = getOption("dlw.verbose"),
                         ...) {
  format     <- match.arg(format)

  if (missing(filename) || is.null(filename)) {
    cli::cli_abort("{.arg filename} is a required argument.")
  }

  # Construct board and pin_name for reading
  board <- get_wrk_board(local = local,
                         local_dir = local_dir)

  pin_name <- filename |>
    fs::path_ext_remove() |>
    fs::path(ext = format)

  # set in dlwenv
  set_in_dlwenv("current_board", board)
  set_in_dlwenv("current_pin", pin_name)

  if (!local_overwrite && pin_name %in% pins::pin_list(board)) {
    # Only read the requested version, do not download
    out <- dlw_read(board = board,
                    pin_name = pin_name,
                    version = version)
    return(out)
  }

  dlw_download(country_code    = country_code,
               server          = server,
               filename        = filename,
               format          = format,
               board           = board,
               pin_name        = pin_name,
               ...,
               verbose = verbose)
}


#' perform request and get raw data
#'
#' @param req request from [build_request]
#'
#' @returns raw data from [resp_body_raw]
#' @keywords internal
get_raw_data <- \(req) {

  raw_data <- handle_resp(req)

  set_in_dlwenv(key = "last_raw_data", value = raw_data)

}





#' Get workign pips board
#'
#' @inheritParams dlw_get_data
#' @returns board from (pins) package
#' @keywords internal
get_wrk_board <- function(local, local_dir) {
  if (local) {
    if (fs::is_dir(local_dir)) {
      pins::board_folder(local_dir)
    } else {
      pins::board_local(local_dir)
    }
  } else {
    brd <- get_from_dlwenv("temp_board")
    if (is.null(brd)) {
      brd <- pins::board_temp()
      set_in_dlwenv(key = "temp_board", value = brd)
    }
    brd
  }
}
