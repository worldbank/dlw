
#' Get GMD data.
#'
#' This is a wrapper of dlw_get_data. It just build the calls to dlw_get_data,
#' for ease of use.
#'
#' @inheritParams dlw_country_catalog
#' @param year numeric: four digit year
#' @param module character: module of GMD collection (e.g., ALL, GPWG, L)
#' @param survey character: survey acronyn
#' @param filename character: File name
#' @param vermast  character: Version of the master data in the form "vXX" where
#'   X is a number of two digits like "01" or "02".
#' @param veralt character: Version of the alternative  data in the form "vXX"
#'   where X is a number of two digits like "01" or "02".
#' @param latest logical: If TRUE and  `vermast` and `veralt` are NULL and then,
#'   it will use the most recent data.
#' @inheritDotParams dlw_get_data local local_dir local_overwrite
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
                        verbose =  getOption("dlw.verbose"),
                        ...) {


  ctl <- dlw_server_inventory(country = country_code,
                              server = "GMD",
                              year = year,
                              module = module,
                              survey = survey,
                              fileName = filename,
                              vermast  = vermast,
                              veralt   = veralt)

  if (latest == TRUE & (is.null(year) | is.null(vermast) | is.null(veralt))) {
    ctl <- ctl[Year == max(Year, na.rm = TRUE)
    ][Vermast == max(Vermast, na.rm = TRUE)
    ][Veralt == max(Veralt, na.rm = TRUE)]
  }
  calls <- gmd_calls(ctl = ctl,
                     country_code = country_code,
                     ...)

  if (length(calls) > 1) {
    if (verbose) {
      cli::cli_alert("your arguments do not uniquely identify a dataset.
                     So you need execute one of the following:")
      print(calls)

    }
    return(invisible(calls))
  }
  eval_gmd_call(calls, verbose)
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

#' get Support files for GMD
#'
#' @inheritParams dlw_get_gmd
#' @param module character: As of now, either CPIICP (the default) or CPI.
#'
#' @returns data.table
#' @export
#'
#' @examples
#' \dontrun{
#' dlw_get_gmd_support()
#' dlw_get_gmd_support(vermast = "v10")
#' }
dlw_get_gmd_support <- function(module = c("CPIICP", "CPI"),
                                vermast = NULL,
                                verbose =  getOption("dlw.verbose")) {

  module       <- match.arg(module)
  country_code <- "Support"
  ctl <- dlw_server_catalog(server = "GMD")
  ctl <- ctl[Country == country_code &
               Country_code == country_code &
               Module == module]

  # get latest by default.

  if (is.null(vermast)) {
    ctl <- ctl[Vermast == max(Vermast, na.rm = TRUE)]
  } else {
    ctl <- ctl[Vermast == Vermast]
  }

  calls <- gmd_calls(ctl, country_code = country_code)

  eval_gmd_call(calls, verbose)

}


#' evaluate GMD call
#'
#' @param calls calls from [gmd_calls]
#' @inheritParams dlw_get_gmd
#'
#' @returns evaluation
#' @keywords internal
eval_gmd_call <- function(calls, verbose = getOption("dlw.verbose"))  {
  if (verbose) print(calls)
  rlang::eval_tidy(calls[[1]])
}
