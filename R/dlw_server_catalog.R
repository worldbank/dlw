#' Server catalog
#'
#' @param server character: in case we have more than one server. default is  GMD
#' @param verbose logical: whether to display info
#' @param force logical: If TRUE, it will query the API regardless of whether
#'   there is version available in .dlwenv
#' @inheritParams build_request
#'
#' @returns dataframe with
#' @family catalogs
#' @export
#'
#' @examples
#' \dontrun{
#' dlw_server_catalog(sever = "GMD")
#' }
dlw_server_catalog <- function(server = NULL,
                              dlw_url = NULL,
                              api_version = getOption("dlw.default_api_version"),
                              force = FALSE,
                              verbose = getOption("dlw.verbose"),
                              store_request = FALSE
                              ) {
  endpoint <- "ServerCatalog"
  base_server <- select_server(server = server)

  key <- paste(endpoint, base_server, sep = "_")

  ctl <- get_from_dlwenv(key, verbose)

  # Early return
  if (!is.null(ctl) && force == FALSE) return(ctl)

  req <- build_request(dlw_url = dlw_url,
                       api_version = api_version,
                       endpoint = endpoint,
                       Server = base_server,
                       store_request = store_request)

  ctl <-  handle_resp(req)

  ctl[, FileName := basename(FilePath)]

  ctl <- ctl[(Ext == "dta"), ]

  add_gmd_vars(ctl)

  set_in_dlwenv(key, ctl, verbose)

  ctl
}

#' filter server catolog to show inventory by country and other variables
#'
#' @inheritParams dlw_get_data
#' @param country character. same as `country_code` in [dlw_get_data] but with
#'   the purpose of easy programming. Not meant to be used by final user.
#' @param ... additional filtering arguments (e.g., year, module, survey, fileName)
#'
#' @returns filter server catalog from [dlw_server_catalog]
#' @family catalogs
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




#' Add GMD vars to GMD server catalog
#'
#' @param ctl data.table of server catalog raw
#'
#' @returns data.table with more variables
#' @keywords internal
add_gmd_vars <- function(ctl) {
  vars <- c(
    "Country_code",
    "Survey_year",
    "Survey_acronym",
    "Vermast",
    "M",
    "Veralt",
    "A",
    "Collection",
    "Module",
    "ext"
  )

  ptt <- "^[A-Za-z]+_[0-9]{4}_[^_]+_[Vv][0-9]{2}_M_[Vv][0-9]{2}_A_[^_]+_[^_]+\\.[A-Za-z]+$"
  ctl[grepl(ptt, FileName),
      (vars) := tstrsplit(FileName, split = "_|[.]", fill = NA)
  ][,
    c("M", "A") := NULL]

}
