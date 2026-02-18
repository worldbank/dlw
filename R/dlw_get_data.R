#' Get data from datalibweb (refactored)
#'
#' @description
#' `dlw_get_data()` function is the main user-facing function for 
#' retrieving datasets from the Datalibweb (DLW) API. It handles 
#' downloading, caching, and reading datasets, supporting both local 
#' and temporary storage.
#'
#' **How it works:**
#' 1. Checks if the requested file exists locally (unless `local_overwrite = TRUE`).
#' 2. If it exists, reads it using `dlw_read()`.
#' 3. If not, downloads the data from the DLW API using `dlw_download()`, saves 
#'    it in the specified format, and returns it as a `data.table`.
#' 4. Handles directory management and caching via `get_wrk_board()`.
#'
#' This function streamlines access to DLW datasets, automatically 
#' managing download, storage, and retrieval.
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
  dlw_dir <- get_wrk_board(local = local, local_dir = local_dir)

  id_name <- filename |>
    fs::path_ext_remove() |>
    fs::path(ext = format)

  # set in dlwenv
  set_in_dlwenv("current_board", dlw_dir)
  set_in_dlwenv("current_pin", id_name)

  files_in_dir <- basename(list.files(dlw_dir))

  if (!local_overwrite && id_name %in% files_in_dir) {
    # Only read the requested version, do not download
    out <- dlw_read(dlw_dir = dlw_dir,
      id_name = id_name, 
      version = version)
    return(out)
  }

  dlw_download(
    country_code = country_code,
    server = server,
    filename = filename,
    format = format,
    dlw_dir = dlw_dir,
    id_name = id_name,
    ...,
    verbose = verbose
  )
}


#' Download data from datalibweb and save using {stamp} framework
#'
#' @inheritParams dlw_get_data
#' @inheritParams dlw_read
#' @param filename character: Name of the file to save/read (required)
#' @param format character: File format to use for saving data ('qs2'
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

  # Read .dta and save it as qs2 using {stamp}
  dt <- haven::read_dta(tmpfile, encoding = "latin1") |>
    setDT()
  unlink(tmpfile)

  stamp::st_save(dt, fs::path(dlw_dir, id_name), alias = "dlw")
  dt
}

#' Read data from a pin (local or temp)
#'
#' Reads a dataset from a specified directory and file name, returning it as a `data.table`.
#'
#' - Lists files in `dlw_dir` and checks if `id_name` exists.
#' - If not found, aborts with an error.
#' - Removes any extension from `id_name`.
#' - Loads the data using `stamp::st_load()` from a `.qs2` file in the directory.
#'
#' This function is used internally to retrieve previously saved or downloaded datasets in a fast, versioned format.
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

  stamp::st_load(fs::path(dlw_dir, id_name), alias = "dlw")

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



#' Get working folder for saving or reading data
#'
#' Determines the directory to use for saving or reading data, depending on whether you want to use a local directory or a temporary one.
#'
#' - If `local` is `TRUE`, it checks if `local_dir` exists. If not, it creates it (using `fs::dir_create()`). It then returns this directory path.
#' - If `local` is `FALSE`, it tries to get a temporary directory path from the package environment (`get_from_dlwenv("temp_dir")`). If this does not exist, it creates a new temporary directory, stores its path in the environment, and returns it.
#'
#' This function ensures that data is always saved to a valid directory, either user-specified or temporary.
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
      #wrk_dir <- fs::path_temp("wrk_dir")
      #fs::dir_create(wrk_dir)
      wrk_dir <- fs::path(tempdir())
      stamp::st_init(wrk_dir, alias = "dlw")

      set_in_dlwenv(key = "temp_dir", value = wrk_dir)
    }
    wrk_dir
  }
}
