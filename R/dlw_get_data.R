#' Download data from datalibweb and save as a pin
#'
#' @inheritParams dlw_get_data
#' @returns Invisibly returns TRUE if download and pinning succeed
#' @keywords internal
#' @importFrom pins board_local board_temp pin_write
#' @importFrom httr2 req_perform resp_body_raw
#' @importFrom haven read_dta
#' @importFrom data.table setDT
#' @export
#' 
dlw_download <- function(country_code,
                         server = NULL,
                         local_dir = getOption("dlw.local_dir"),
                         local = fs::is_dir(local_dir),
                         local_overwrite = FALSE,
                         ...,
                         verbose = getOption("dlw.verbose")) {
  dots <- list(...)
  endpoint <- "FileInformation/GetFileInfo"
  args <- c(list(Country = country_code,
                 method = "POST",
                 Server = server,
                 endpoint = endpoint),
            dots)
  filename <- dots$filename
  if (is.null(filename)) stop("filename must be provided in ... arguments")
  # Choose board
  board <- if (local) pins::board_local(local_dir) else pins::board_temp()
  # If not overwriting and pin exists, skip download
  if (!local_overwrite && filename %in% pins::pin_list(board)) return(invisible(TRUE))
  req <- do.call("build_request", args)
  raw_data <- req |>
    httr2::req_perform() |>
    httr2::resp_body_raw()
  # Save raw data to a temp file for reading
  tmpfile <- tempfile(fileext = ".dta")
  writeBin(raw_data, tmpfile)
  # Pin the file
  pins::pin_write(board, tmpfile, name = filename, type = "file", overwrite = TRUE)
  unlink(tmpfile)
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
#' 
dlw_read <- function(filename,
                    local_dir = getOption("dlw.local_dir"),
                    local = fs::is_dir(local_dir)) {
  board <- if (local) pins::board_local(local_dir) else pins::board_temp()
  if (!(filename %in% pins::pin_list(board))) {
    stop(sprintf("File '%s' not found in the %s board.", filename, if (local) "local" else "temp"))
  }
  pin_path <- pins::pin_read(board, filename)
  # pin_read returns a file path for type = 'file'
  data <- haven::read_dta(pin_path) |> setDT()
  data
}

#' get data from datalibweb (refactored)
#' @inheritParams dlw_country_catalog
#' @inheritParams dlw_server_catalog
#' @param ... additional filtering arguments (e.g.,survey_year, survey_acronym,
#'   vermast, veralt, collection, module)
#' @param local_dir character: Local directory to save data. Default available
#'   in option dlw.local_dir which is set initially as "".
#' @param local logical: whether or not to save and read data locally. default
#'   is TRUE if `local_dir` exists.
#' @param local_overwrite logical. Whether to overwrite any saved data. Default
#'   is FALSE
#' @returns data base request as data.table
#' @export
#' 
dlw_get_data <- function(country_code,
                         server = NULL,
                         verbose =  getOption("dlw.verbose"),
                         local_dir = getOption("dlw.local_dir"),
                         local = fs::is_dir(local_dir),
                         local_overwrite = FALSE,
                         ...) {
  dots <- list(...)
  filename <- dots$filename
  if (is.null(filename)) stop("filename must be provided in ... arguments")
  dlw_download(country_code = country_code,
               server = server,
               local_dir = local_dir,
               local = local,
               local_overwrite = local_overwrite,
               ...,
               verbose = verbose)
  dlw_read(filename = filename, local_dir = local_dir, local = local)
}
