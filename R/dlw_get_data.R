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
#' @param format character: File format to use for saving data ('parquet'
#'   [default] or 'qs2')
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

  # Construct directory and id_name for reading
  dlw_dir <- get_wrk_board(local = local,
                           local_dir = local_dir)

  # Get file name without extension
  file_name <- filename |>
    fs::path_ext_remove()

  dlw_dir <- fs::path(dlw_dir, file_name)

  if (!fs::is_dir(dlw_dir)) {
    fs::dir_create(dlw_dir)
  }

  id_name <- filename |>
    fs::path_ext_remove() |>
    fs::path(ext = format)

  # set in dlwenv
  set_in_dlwenv("current_dir", dlw_dir)
  set_in_dlwenv("current_id", id_name)

  files_in_dir <- basename(list.files(dlw_dir))

  if (!local_overwrite && id_name %in% files_in_dir) {
    # Only read the requested version, do not download
    out <- dlw_read(dlw_dir = dlw_dir,
                    id_name = id_name,
                    version = version)
    return(out)
  }


  dlw_download(country_code    = country_code,
               server          = server,
               filename        = filename,
               format          = format,
               dlw_dir         = dlw_dir,
               id_name         = id_name,
               ...,
               verbose = verbose)
}


#' Download data from datalibweb and save as a pin
#'
#' @inheritParams dlw_get_data
#' @inheritParams dlw_read
#' @param filename character: Name of the file to save/read (required)
#' @param format character: File format to use for pinning data ('qs'
#'   [default] or 'parquet')
#' @returns A list with the board and pin_name used
#' @keywords internal
dlw_download <- function(country_code,
                         filename,
                         dlw_dir,
                         id_name,
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

  id_name <- id_name |>
    fs::path_ext_remove()

  stamp::st_init(dlw_dir)

  pipload::pip_write(x = dt,
    id = id_name,
    dir = dlw_dir,
    format  = format)
  dt
}

#' Read data from (local or temp)
#'
#' @param dlw_dir A folder object (as returned by dlw_download)
#' @param id_name The name of the a dataset (as returned by dlw_download)
#' @param version numeric: Version of the data to read (for versioning data
#'   retrieval only)
#' @returns data.table
#' @keywords internal
dlw_read <- function(dlw_dir, id_name, version = NULL) {

  files_in_dir <- basename(list.files(dlw_dir))

  if (!(id_name %in% files_in_dir)) {
    cli::cli_abort("File {.file {id_name}} not found in the provided directory.")
  }

  id_name <- id_name |>
    fs::path_ext_remove()

  pipload::pip_read(id_name, dir = dlw_dir, version = version)

  # pipload::pip_read(id_name, dlw_dir, version = version) |>
  #   setDT()

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

  # return raw data so callers receive the bytes
  raw_data

}

#' Get workign pips board
#'
#' @inheritParams dlw_get_data
#' @returns Folder path
#' @keywords internal
get_wrk_board <- function(local, local_dir) {
  if (local) {
    if (!fs::is_dir(local_dir)) {
      wrk_dir <- fs::dir_create(local_dir)
    } else {
      wrk_dir <- fs::dir_create(local_dir)
    }
  } else {
    wrk_dir <- get_from_dlwenv("temp_dir")

    if (is.null(wrk_dir)) {
      wrk_dir <- fs::path_temp("wrk_dir_dlw")
      fs::dir_create(wrk_dir)

      set_in_dlwenv(key = "temp_dir", value = wrk_dir)

    }
    wrk_dir
  }
}
