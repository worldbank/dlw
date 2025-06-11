
#' get data from datalibweb
#'
#' @param server
#' @param country_code
#' @param year
#' @param module
#' @param survey
#' @param fileName
#' @param verbose
#'
#' @returns
#' @export
#'
#' @examples
dlw_get_data <- function(country_code,
                         server = NULL,
                         verbose =  getOption("dlw.verbose"),
                         skip_filter = FALSE,
                         ...
                         ) {

  ctl <- dlw_server_inventory(country = country_code,
                             server = server,
                             ...)

  calls <- data_calls(ctl = ctl,
                      country_code = country_code)



  endpoint <- "FileInformation/GetFileInfo"

  collection <- select_collection(server = server)
  req <- build_request(endpoint = endpoint,
                       method = "POST",
                       Server = server,
                       Country = country_code,
                       Collection = collection,
                       Folder = module,
                       Year = year)

  resp <- req |>
    req_perform()   |>
    resp_body_string() |>
    gsub("\r\n|\r", "\n", x = _) |>
    fread(text = _, data.table = TRUE)

  resp
}


dlw_get_gmd <- function(country_code,
                        year = NULL,
                        survey  = NULL,
                        module = NULL,
                        fileName = NULL,
                        vermast = NULL,
                        veralt = NULL,
                        verbose =  getOption("dlw.verbose")) {


  ctl <- dlw_server_inventory(country = country_code,
                              server = "GMD",
                              year = year,
                              module = module,
                              survey = survey,
                              fileName = fileName,
                              vermast  = vermast,
                              veralt   = veralt)

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
  args <- c(list(country = country), dots)
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

  ctl[rlang::eval_tidy(cnds)] |>
    funique()
}




#' get data calls when more than one option.
#'
#' @param ctl data.table from [dlw_server_inventory]
#' @param country_code ISO3
#'
#' @returns list of possible calls
#' @keywords internal
data_calls <- function(ctl,
                       country_code) {
  # For each row in ctl, build a call to dlw_get_data with the right parameters
  calls <- vector("list", nrow(ctl))

  for (i in seq_len(nrow(ctl))) {
    calls[[i]] <- call(
      "dlw_get_data",
      country_code = country_code,
      year = ctl$Year[i],
      server = ctl$ServerAlias[i],
      survey = ctl$Survey[i],
      module = ctl$Module[i]
    )
  }
  calls
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
