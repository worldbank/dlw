#' Download data from datalibweb and save as a pin
#'
#' @inheritParams dlw_get_data
#' @param filename character: Name of the file to save/read (required)
#' @param board_type character: Which pins board to use: 'folder' (default, shared) or 'local' (user-specific)
#' @param format character: File format to use for pinning data ('parquet' [default] or 'qs')
#' @returns Invisibly returns TRUE if download and pinning succeed
#' @keywords internal
#' @importFrom pins board_local board_temp pin_write
#' @importFrom httr2 req_perform resp_body_raw
#' @importFrom haven read_dta
#' @importFrom data.table setDT
#' @export
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
  format <- match.arg(format)
  if (missing(filename) || is.null(filename)) {
    cli::cli_abort("{.arg filename} is a required argument.")
  }
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
    pins::board_temp()
  }
  pin_name <- paste0(tools::file_path_sans_ext(filename), ".", format)
  # If not overwriting and pin exists, skip download
  if (!local_overwrite && pin_name %in% pins::pin_list(board)) {
    return(invisible(TRUE))
  }
  req <- do.call("build_request", args)
  raw_data <- req |>
    httr2::req_perform() |>
    httr2::resp_body_raw()
  # Save raw data to a temp file for reading
  tmpfile <- tempfile(fileext = ".dta")
  writeBin(raw_data, tmpfile)
  # Read .dta and save as parquet or qs
  dt <- haven::read_dta(tmpfile) |> setDT()
  unlink(tmpfile)
  tmpout <- tempfile(fileext = paste0(".", format))
  if (format == "parquet") {
    arrow::write_parquet(dt, tmpout)
  } else if (format == "qs") {
    qs::qsave(dt, tmpout)
  }
  pins::pin_write(board, tmpout, name = pin_name, type = "file", overwrite = TRUE)
  unlink(tmpout)
  invisible(TRUE)
}

#' Read data from a pin (local or temp)
#'
#' @inheritParams dlw_get_data
#' @returns data.table
#' @keywords internal
#' @importFrom pins board_local board_temp pin_read
#' @importFrom haven read_dta
#' @importFrom data.table setDT
#' @export
dlw_read <- function(filename,
                    local_dir = getOption("dlw.local_dir"),
                    local = fs::is_dir(local_dir),
                    board_type = c("folder", "local"),
                    format = c("parquet", "qs")) {
  board_type <- match.arg(board_type)
  format <- match.arg(format)
  board <- if (local) {
    if (board_type == "local") {
      pins::board_local(local_dir)
    } else {
      pins::board_folder(local_dir)
    }
  } else {
    pins::board_temp()
  }
  pin_name <- paste0(tools::file_path_sans_ext(filename), ".", format)
  if (!(pin_name %in% pins::pin_list(board))) {
    board_type_str <- if (local) board_type else "temp"
    cli::cli_abort("File {.file {pin_name}} not found in the {.file {board_type_str}} board.")
  }
  pin_path <- pins::pin_read(board, pin_name)
  # pin_read returns a file path for type = 'file'
  if (format == "parquet") {
    data <- arrow::read_parquet(pin_path)
  } else if (format == "qs") {
    data <- qs::qread(pin_path)
  }
  setDT(data)
  data
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
#' @param board_type character: Which pins board to use: 'folder' (default, shared) or 'local' (user-specific)
#' @param format character: File format to use for pinning data ('parquet' [default] or 'qs')
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
  format <- match.arg(format)
  if (missing(filename) || is.null(filename)) {
    cli::cli_abort("{.arg filename} is a required argument.")
  }
  dlw_download(country_code    = country_code,
               server          = server,
               filename        = filename,
               local_dir       = local_dir,
               local           = local,
               board_type      = board_type,
               format          = format,
               local_overwrite = local_overwrite,
               ...,
               verbose = verbose)
  dlw_read(filename = filename,
           local_dir = local_dir,
           local = local,
           board_type = board_type,
           format = format)
}
