
#' get data from datalibweb
#'
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
#'
#' @returns data base request as data.table
#' @export
#'
#' @examples
#' \dontrun{
#'   dlw_get_data(
#'     country_code = "COL",
#'     year = 2010L,
#'     server = "GMD",
#'     survey = "GEIH",
#'     module = "GPWG",
#'     filename = "COL_2010_GEIH_V02_M_V09_A_GMD_GPWG.dta",
#'     collection = "GMD")
#' }
dlw_get_data <- function(country_code,
                         server = NULL,
                         verbose =  getOption("dlw.verbose"),
                         local_dir = getOption("dlw.local_dir"),
                         local = fs::is_dir(local_dir),
                         local_overwrite = FALSE,
                         ...
                         ) {
  # Capture ... arguments as a list
  dots <- list(...)
  # Combine country and ... into a single list of arguments
  endpoint <- "FileInformation/GetFileInfo"
  args <- c(list(Country = country_code,
                 method = "POST",
                 Server = server,
                 endpoint = endpoint),
            dots)

  filename <- dots$filename
  if (local == TRUE && local_overwrite == FALSE) {
    resp <- load_from_local(local_dir = local_dir,
                            filename = filename)
    if (!is.null(resp)) return(resp)
  }

  req <- do.call("build_request", args)

  resp <- req |>
    httr2::req_perform()   |>
    httr2::resp_body_raw() |>
    # this part should be changed to read generic data from other formats
    haven::read_dta() |>
    setDT()


  if (local == TRUE) {
    save_to_local(x = resp, local_dir = local_dir, filename = filename)
  }

  resp
}





#' Load from local drive
#'
#' @param local_dir local directory
#' @param filename  File name. "dta" format for now
#'
#' @returns data in `fs::path(local_dir, filename)` or NULL if file does not
#'   exist
#'   @keywords internal
load_from_local <- function(local_dir, filename) {
  if (fs::is_dir(local_dir)) {
    fn <- fs::path(local_dir, filename)
    if (fs::file_exists(fn)) {
      resp <- haven::read_dta(fn) |>
        setDT()
      return(resp)
    } else {
      return(NULL)
    }
  } else {
    cli::cli_abort("directory {.file  {local_dir}} does nto exist")
  }
}


#' save to local drive
#'
#' @param x data in "dta" formar for now
#' @param local_dir  local directory
#' @param filename  filename. dta for now
#'
#' @returns  invisible(TRUE)
#' @keywords internal
save_to_local <- function(x, local_dir, filename){
  if (fs::is_dir(local_dir)) {
    fn <- fs::path(local_dir, filename)
    haven::write_dta(data = x, path = fn)
  } else {
    cli::cli_abort("directory {.file  {local_dir}} does nto exist")
  }
  invisible(TRUE)
}
