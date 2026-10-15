#' Get data from datalibweb (refactored)
#'
#' Downloads a file and pins it, so later calls read it from the board. Files
#' with extension dta, sav, csv, tsv, rds or parquet are read as tables and
#' pinned as `format`. Any other file (e.g., .do, .R, .pdf, .zip) is pinned as
#' is and its path is returned. To save a file without pinning it, see
#' [dlw_download_file].
#'
#' @param filename character: Name of the file to save/read (required)
#' @inheritParams dlw_country_catalog
#' @inheritParams dlw_server_catalog
#' @param ... additional filtering arguments (e.g.,survey_year, survey_acronym,
#'   vermast, veralt, collection, module). When `filename` exists in several
#'   versions on the server, `vermast` and `veralt` (e.g., "v02") select one
#'   (sent to the API as `para1`); if neither is given, the latest version is
#'   used. `year` is required. If `collection` is not given, it is taken from
#'   harmonized file names (e.g., "GMD" in "COL_2010_GEIH_V04_M_V01_A_GMD_ALL.dta").
#' @param local_dir character: Local directory to save data. Default available
#'   in option dlw.local_dir which is set initially as "".
#' @param local logical: whether or not to save and read data locally. default
#'   is TRUE if `local_dir` exists.
#' @param format character: File format to use for pinning data ('qs2'
#'   [default] or 'parquet')
#' @param local_overwrite logical. Whether to overwrite any saved data. Default
#'   is FALSE
#' @param version numeric: Version of the pin to read (for pinning data
#'   retrieval only)
#' @returns data base request as data.table; for a file that is not a table,
#'   the path to its pinned copy. If the server holds several versions of
#'   `filename` and none could be chosen, a data.table with the versions.
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

  # checked before reading the cache so a cached file cannot hide it
  if (is.null(list(...)[["year"]])) {
    cli::cli_abort("{.arg year} is required to request a file from the API.")
  }

  # Construct board and pin_name for reading
  board <- get_wrk_board(local = local,
                         local_dir = local_dir)

  # an explicit file version is part of the pin name so versions do not collide
  pin_name <- get_pin_name(filename, get_version_arg(list(...)), format,
                           country_code, list(...)[["year"]])

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


#' Download data from datalibweb and save as a pin
#'
#' @inheritParams dlw_get_data
#' @inheritParams dlw_read
#' @param filename character: Name of the file to save/read (required)
#' @param format character: File format to use for pinning data ('qs2'
#'   [default] or 'parquet')
#' @returns The table as data.table, or the path to the pinned copy of a file
#'   that is not a table, or the data.table of file versions (see
#'   [dlw_get_data]).
#' @keywords internal
dlw_download <- function(country_code,
                         filename,
                         board,
                         pin_name,
                         format,
                         server = NULL,
                         ...,
                         verbose = getOption("dlw.verbose")) {

  file <- fetch_file(country_code = country_code,
                     filename     = filename,
                     server       = server,
                     ...)

  # Could not choose a version: let the user decide.
  if (is.data.frame(file)) {
    cli::cli_alert_warning(paste0("{.file {filename}} has {nrow(file)} ",
                                  "versions on the server. Returning the ",
                                  "version list instead of data."))
    return(file)
  }

  # Save raw data to a temp file for reading, under the name of the API
  tmpdir <- fs::file_temp()
  fs::dir_create(tmpdir)
  on.exit(unlink(tmpdir, recursive = TRUE))
  tmpfile <- fs::path(tmpdir, c(file$filename, filename)[[1]])
  writeBin(file$body, tmpfile)

  # Anything but tabular data is pinned as is
  ext <- tolower(fs::path_ext(filename))
  if (!ext %in% tabular_formats) {
    # pins fails if an identical file is pinned twice within a second
    pinned <- if (pin_name %in% pins::pin_list(board)) {
      dlw_read(board = board, pin_name = pin_name)
    }
    if (is.null(pinned) ||
        tools::md5sum(pinned) != tools::md5sum(tmpfile)) {
      pins::pin_upload(board = board,
                       paths = tmpfile,
                       name  = pin_name,
                       versioned = TRUE)
    }
    return(dlw_read(board = board, pin_name = pin_name))
  }

  # Read the table and pin as parquet or qs
  dt <- read_table(tmpfile, ext = ext, filename = filename) |>
    setDT()
  pins::pin_write(board = board,
                  x     = dt,
                  name  = pin_name,
                  type  = format,
                  versioned = TRUE)
  dt
}


#' File extensions that are read as tables
#' @keywords internal
tabular_formats <- c("dta", "sav", "csv", "tsv", "rds", "parquet")


#' Read a table by its file extension
#'
#' @param path character: path to the file
#' @param ext character: extension, one of `tabular_formats`
#' @param filename character: name of the file, for messages
#' @returns data.frame
#' @keywords internal
read_table <- function(path, ext, filename) {
  out <- switch(ext,
                dta     = haven::read_dta(path, encoding = "latin1"),
                sav     = haven::read_sav(path),
                csv     = ,
                tsv     = fread(path, data.table = TRUE),
                rds     = readRDS(path),
                parquet = {
                  rlang::check_installed("arrow", "to read parquet files.")
                  arrow::read_parquet(path)
                })
  if (!is.data.frame(out)) {
    cli::cli_abort(c("{.file {filename}} does not contain a table.",
                     "i" = "Use {.fn dlw_download_file} to save it."))
  }
  out
}


#' Name of the pin of a file
#'
#' The pin of a dta file is named after the file, with the pin format as
#' extension. Other tables also keep their extension in the name, so that
#' files like "a.csv" and "a.dta" do not collide. Files that are not tables are
#' pinned as is, with their own name. Unless the name of such a file is that of
#' a harmonized file (which holds country and year), the country and year are
#' part of the pin name, as the same name can be used in several of them.
#'
#' @param filename character: name of the file
#' @param version_arg character: explicit file version (see
#'   [get_version_arg]) or NULL.
#' @param year numeric: year of the request
#' @inheritParams dlw_get_data
#' @returns character
#' @keywords internal
get_pin_name <- function(filename, version_arg, format, country_code = NULL,
                         year = NULL) {
  ext <- fs::path_ext(filename)
  id  <- if (is.null(filename_collection(filename))) {
    c(toupper(country_code), year)
  }
  stem <- \(id = NULL) c(fs::path_ext_remove(filename), id, version_arg) |>
    paste(collapse = "_")

  if (tolower(ext) == "dta") {
    fs::path(stem(), ext = format)
  } else if (tolower(ext) %in% tabular_formats) {
    fs::path(paste(stem(id), tolower(ext), sep = "_"), ext = format)
  } else if (nzchar(ext)) {
    fs::path(stem(id), ext = ext)
  } else {
    fs::path(stem(id))
  }
}


#' Request a file from datalibweb
#'
#' Sends the request, selects the version of an ambiguous file and checks the
#' response, but does not read the file.
#'
#' @inheritParams dlw_get_data
#' @returns A list with the raw `body`, `filename`, `content_type` and `status`
#'   of the file (see [handle_file_resp]); or, if the server holds several
#'   versions of `filename` and none could be chosen, a data.table with the
#'   versions.
#' @keywords internal
fetch_file <- function(country_code,
                       filename,
                       server = NULL,
                       ...) {

  if (missing(filename) || is.null(filename)) {
    cli::cli_abort("{.arg filename} is a required argument.")
  }

  # prepare the args for request
  server <- select_server(server)
  dots <- list(...)

  if (is.null(dots[["year"]])) {
    cli::cli_abort("{.arg year} is required to request a file from the API.")
  }
  endpoint <- "FileInformationInternal/GetFileInfo"

  # the API checks the subscription by collection
  if (is.null(dots[["collection"]])) {
    dots[["collection"]] <- filename_collection(filename)
  }

  # the API selects a version of an ambiguous file through `para1`
  version_arg <- get_version_arg(dots)
  if (!is.null(version_arg)) dots[["para1"]] <- version_arg

  request_file <- \(dots) {
    args <- c(list(Country = country_code,
                   method = "POST",
                   Server = server,
                   endpoint = endpoint,
                   filename = filename),
              dots)
    do.call("build_request", args) |>
      get_raw_data()
  }

  raw_data <- request_file(dots)

  # A CSV response (one row per version) means the file name is ambiguous.
  # Without a requested version, repeat the request with the latest one.
  if (is.data.frame(raw_data) && nrow(raw_data) > 0 && is.null(version_arg)) {
    latest <- latest_version(raw_data)
    if (!is.null(latest)) {
      cli::cli_alert_info(paste0("{.file {filename}} has {nrow(raw_data)} ",
                                 "versions on the server. Using the latest: ",
                                 "{.val {latest}}."))
      dots[["para1"]] <- latest
      raw_data <- request_file(dots)
    }
  }

  # A CSV response without rows means no file matched the request.
  if (is.data.frame(raw_data) && nrow(raw_data) == 0) {
    cli::cli_abort("No file {.file {filename}} found for the requested
                   arguments.")
  }

  raw_data
}


#' Download a file from datalibweb as is
#'
#' Downloads any file (data, programs, documents) without reading or pinning
#' it. Use [dlw_get_data] to read data files.
#'
#' @inheritParams dlw_get_data
#' @param path character: where to save the file. If a directory, the file is
#'   saved in it with the name given by the API. If `NULL` (default), the
#'   file is not saved but returned as raw bytes.
#' @param overwrite logical: whether to replace an existing file at `path`.
#'   Default is FALSE.
#' @param ... additional request arguments (e.g., `year`, which is required,
#'   `vermast`, `veralt`, `collection`), as in [dlw_get_data].
#'
#' @returns If `path` is `NULL`, the raw bytes of the file, with attributes
#'   `filename`, `content_type` and `status`. Otherwise, invisibly, a list with
#'   the `path` of the saved file, `filename`, `size` in bytes,
#'   `content_type` and `status`. If the server holds several versions of
#'   `filename` and none could be chosen, a data.table with the versions.
#' @export
#'
#' @examples
#' \dontrun{
#' dlw_download_file("WLD", "WLD_2026_ALL_V02_M_V01_A_GMI_INE.do",
#'                   year = 2026, path = tempdir())
#' }
dlw_download_file <- function(country_code,
                              filename,
                              server    = NULL,
                              path      = NULL,
                              overwrite = FALSE,
                              ...) {

  # fail before the request if the target is known
  if (!is.null(path) && !is_dir_path(path) && fs::file_exists(path) &&
      !overwrite) {
    cli::cli_abort(c("File {.file {path}} already exists.",
                     "i" = "Use {.code overwrite = TRUE} to replace it."))
  }

  file <- fetch_file(country_code = country_code,
                     filename     = filename,
                     server       = server,
                     ...)

  if (is.data.frame(file)) {
    cli::cli_alert_warning(paste0("{.file {filename}} has {nrow(file)} ",
                                  "versions on the server. Returning the ",
                                  "version list instead of the file."))
    return(file)
  }

  # the name given by the API; if it is missing, the requested one
  file$filename <- c(file$filename, filename)[[1]]

  if (is.null(path)) {
    return(structure(file$body,
                     filename     = file$filename,
                     content_type = file$content_type,
                     status       = file$status))
  }

  if (is_dir_path(path)) {
    path <- fs::path(path, file$filename)
  }
  if (fs::file_exists(path) && !overwrite) {
    cli::cli_abort(c("File {.file {path}} already exists.",
                     "i" = "Use {.code overwrite = TRUE} to replace it."))
  }

  # write beside the target, then move it: no partial file if interrupted
  fs::dir_create(fs::path_dir(path))
  tmpfile <- fs::file_temp(tmp_dir = fs::path_dir(path), pattern = ".dlw-")
  on.exit(unlink(tmpfile))
  writeBin(file$body, tmpfile)
  fs::file_move(tmpfile, path)

  invisible(list(path         = as.character(fs::path_abs(path)),
                 filename     = file$filename,
                 size         = length(file$body),
                 content_type = file$content_type,
                 status       = file$status))
}


#' Is `path` a directory, or meant to be one?
#'
#' @param path character
#' @returns logical
#' @keywords internal
is_dir_path <- function(path) {
  fs::is_dir(path) || grepl("[/\\\\]$", path)
}


#' Collection of a harmonized file
#'
#' @param filename character: file name
#' @returns character in upper case, e.g. "GMD", taken from a file name like
#'   "COL_2010_GEIH_V04_M_V01_A_GMD_ALL.dta"; NULL for any other file name.
#' @keywords internal
filename_collection <- function(filename) {
  # same pattern as in add_gmd_vars()
  ptt <- "^[A-Za-z]+_[0-9]{4}_[^_]+_[Vv][0-9]{2}_M_[Vv][0-9]{2}_A_[^_]+_[^_]+\\.[A-Za-z]+$"
  if (!grepl(ptt, filename)) {
    return(NULL)
  }
  toupper(strsplit(filename, "_|[.]")[[1]][8])
}


#' Version argument of the API request
#'
#' Builds the value of the `para1` request parameter from `vermast` and
#' `veralt` (e.g., "V12_M", "V12_M_V01_A"), unless `para1` is already given.
#'
#' @param dots list of additional arguments of [dlw_get_data]
#' @returns character or NULL
#' @keywords internal
get_version_arg <- function(dots) {
  if (!is.null(dots[["para1"]])) {
    return(dots[["para1"]])
  }
  version <- c(if (!is.null(dots[["vermast"]])) paste0(dots[["vermast"]], "_M"),
               if (!is.null(dots[["veralt"]])) paste0(dots[["veralt"]], "_A"))
  if (length(version) == 0) {
    return(NULL)
  }
  toupper(paste(version, collapse = "_"))
}


#' Latest master version in a list of file versions
#'
#' @param versions data.table of file versions returned by the API, with the
#'   version folder (e.g., "Survey_2005_CPI_V15_M") in `FileSharePath`.
#' @returns character like "V15_M", or NULL if no version could be identified.
#' @keywords internal
latest_version <- function(versions) {
  if (!"FileSharePath" %in% names(versions)) {
    return(NULL)
  }
  path <- versions$FileSharePath
  ver  <- regmatches(path, regexpr("_[Vv][0-9]+_M/", path))
  if (length(ver) == 0) {
    return(NULL)
  }
  ver <- toupper(sub("^_(.*)/$", "\\1", ver))
  ver[which.max(as.integer(gsub("\\D", "", ver)))]
}


#' Read data from a pin (local or temp)
#'
#' @param board A pins board object (as returned by dlw_download)
#' @param pin_name The name of the pin (as returned by dlw_download)
#' @param version numeric: Version of the pin to read (for pinning data
#'   retrieval only)
#' @returns data.table; or the path to the file if the pin is a file that is
#'   not a table.
#' @keywords internal
dlw_read <- function(board, pin_name, version = NULL) {

  if (!(pin_name %in% pins::pin_list(board))) {
    cli::cli_abort("File {.file {pin_name}} not found in the provided board.")
  }
  if (pins::pin_meta(board, pin_name, version = version)$type == "file") {
    return(pins::pin_download(board, pin_name, version = version)[[1]])
  }
  pins::pin_read(board, pin_name, version = version) |>
    setDT()

}



#' perform request and get raw data
#'
#' @param req request from [build_request]
#'
#' @returns raw data from [resp_body_raw]
#' @keywords internal
get_raw_data <- \(req) {

  raw_data <- handle_file_resp(req)

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
