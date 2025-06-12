
#' get data from datalibweb
#'
#' @param server
#' @param country_code
#' @param verbose
#' @param ... additional filtering arguments (e.g.,survey_year, survey_acronym,
#'   vermast, veralt, collection, module)
#' @param local_dir
#' @param local
#' @param local_overwrite
#'
#' @returns
#' @export
#'
#' @examples
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


#' Get GMD data.
#'
#' This is a wrapper of dlw_get_data. It just build the calls to dlw_get_data,
#' for ease of use.
#'
#' @param country_code character: ISO3
#' @param year numeric: four digit year
#' @param module character: module of GMD collection (e.g., ALL, GPWG, L)
#' @param survey character: survey acronyn
#' @param filename character: File name
#' @param vermast  character: Version of the master data in the form "vXX" where
#'   X is a number of two digits like "01" or "02".
#' @param veralt character: Version of the alternative  data in the form "vXX"
#'   where X is a number of two digits like "01" or "02".
#' @param latest logical. If TRUE and  `vermast` and `veralt` are NULL and then,
#'   it will use the most recent data.
#' @param verbose logical. to display info.
#'
#' @returns If the call is unique, it will return the data. If not, it will
#'   return the posibilities for the user to choose.
#' @export
#'
#' @examples
#' \dontrun{
#'   dlw_get_gmd("COL", 2010, "GPWG")
#' }
dlw_get_gmd <- function(country_code,
                        year = NULL,
                        module = NULL,
                        survey  = NULL,
                        filename = NULL,
                        vermast = NULL,
                        veralt = NULL,
                        latest = TRUE,
                        local_dir = getOption("dlw.local_dir"),
                        local = fs::is_dir(local_dir),
                        local_overwrite = FALSE,
                        verbose =  getOption("dlw.verbose")) {


  ctl <- dlw_server_inventory(country = country_code,
                              server = "GMD",
                              year = year,
                              module = module,
                              survey = survey,
                              fileName = filename,
                              vermast  = vermast,
                              veralt   = veralt)

  if (is.null(vermast) & is.null(veralt) & latest == TRUE) {
    ctl <- ctl[Vermast == max(Vermast, na.rm = TRUE)
               ][Veralt == max(Veralt, na.rm = TRUE)]
  }
  calls <- gmd_calls(ctl = ctl,
                     country_code = country_code)
  if (length(calls) > 1) {
    cli::cli_alert("your arguments do not uniquely identify a dataset. So you need execute one of the following:")
    print(calls)
    return(calls)
  }
  print(calls)
  rlang::eval_tidy(calls[[1]])
}


#' filter server catolog to show inventory by country and other variables
#'
#' @inheritParams dlw_get_data
#' @param country character. same as `country_code` in [dlw_get_data] but with
#'   the purpose of easy programming. Not meant to be used by final user.
#' @param ... additional filtering arguments (e.g., year, module, survey, fileName)
#'
#' @returns filter server catalog from [dlw_server_catalog]
#' @export
#'
#' @examples
#' \dontrun{
#' dlw_server_inventory("COL", 2010)
#' }
dlw_server_inventory <- function(country,
                                 server = NULL,
                                 ...) {

  # Capture ... arguments as a list
  dots <- list(...)
  # Combine country and ... into a single list of arguments
  args <- c(list(country = country), dots) |>
    lapply(\(.) {
      if (is.character(.)) {
        toupper(.)
      } else {
        .
      }
    })

  # get names of arguments that are not null
  args_info <- Filter(Negate(is.null), args) |>
    names()

  ctl <- dlw_server_catalog(server = server,
                            verbose = FALSE)

  # create condition for arguments not NULL
  cnds <- lapply(args_info, \(.) {
    paste(simpleCap(.), ., sep = " %in% ")
  }) |>
    # append them together
    paste(collapse = " & ") |>
    # convert to expression
    rlang::parse_expr()

  ctl[rlang::eval_tidy(cnds, data = args)] |>
    unique()
}




#' get data calls when more than one option.
#'
#' @param ctl data.table from [dlw_server_inventory]
#' @param country_code ISO3
#'
#' @returns list of possible calls
#' @keywords internal
gmd_calls <- function(ctl,
                      country_code,
                      ...) {
  # For each row in ctl, build a call to dlw_get_data with the right parameters
  calls <- vector("list", nrow(ctl))
  dots <- list(...)
  for (i in seq_len(nrow(ctl))) {

    l <- c(list(country_code = country_code,
                year = ctl$Year[i],
                server = ctl$ServerAlias[i],
                survey = ctl$Survey[i],
                module = ctl$Module[i],
                filename = ctl$FileName[i],
                collection = ctl$Collection[i]),
           dots)

    calls[[i]] <- c(quote(dlw_get_data), l) |>
      as.call()

  }
  as.dlw_call_list(calls)
}


#
# latest_file <- function(server, country, year, collection, folder) {
#   endpoint <- "FileInformation/GetFileInfo"
#   form <- c(Server = server, Country = country, Collection = collection,
#             Folder = folder, Year = year)
#
#   req <- build_request(endpoint = endpoint,
#                        Server = server, Country = country, Collection = collection,
#                        Folder = folder, Year = year)
#
#   resp <- req |>
#     req_perform()   |>
#     resp_body_string() |>
#     gsub("\r\n|\r", "\n", x = _) |>
#     fread(text = _, data.table = TRUE)
#   resp
# }



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
