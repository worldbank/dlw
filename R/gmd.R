
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
#' @param latest_version logical: If TRUE and  `vermast` and `veralt` are NULL,
#'   it will use the most recent version of the data for a particular year.
#' @param  latest_year logical: If `TRUE` and  `year` is NULL, it retrieves the
#'   most recent year. Otherwise, it will return the calls for all the years
#'   available. This is the default.
#' @inheritDotParams dlw_get_data local local_dir local_overwrite
#' @family GMD utilities
#'
#' @returns If the call is unique, it will return the data. If not, it will
#'   return the posibilities for the user to choose.
#' @export
#'
#' @examples
#' \dontrun{
#'   dlw_get_gmd("PRY", 2010, "GPWG") # latest version of 2010
#'   dlw_get_gmd("PRY", module = "GPWG") # latest year and latest version
#' }
dlw_get_gmd <- function(country_code,
                        year           = NULL,
                        module         = NULL,
                        survey         = NULL,
                        filename       = NULL,
                        vermast        = NULL,
                        veralt         = NULL,
                        latest_version = TRUE,
                        latest_year    = FALSE,
                        verbose        =  getOption("dlw.verbose"),
                        ...) {


  ctl <- dlw_server_inventory(country = country_code,
                              server = "GMD",
                              year = year,
                              module = module,
                              survey = survey,
                              fileName = filename,
                              vermast  = vermast,
                              veralt   = veralt)

  if (latest_year == TRUE && is.null(year)) {
    ctl <- ctl[Year == max(Year, na.rm = TRUE)]
  }

  if (is.null(vermast) & is.null(veralt) & latest_version == TRUE) {
    ctl <- ctl[ ,
                #  for each year, the row(s) with the maximum Vermast.
                .SD[toupper(Vermast) == max(toupper(Vermast))],
                by = Year
                ][,
                  #It should return only one row per year (even if there are ties)
                  .SD[toupper(Veralt) == max(toupper(Veralt), na.rm = TRUE)],
                  by = Year]
  }

  calls <- gmd_calls(ctl = ctl,
                     country_code = country_code,
                     ...)

  if (length(calls) > 1) {
    if (verbose) {
      cli::cli_alert("your arguments do not uniquely identify a dataset.
                     You need execute one of the following:")
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
#' @family GMD utilities
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
#' @family GMD utilities
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
    ctl <- ctl[toupper(Vermast) == max(toupper(Vermast), na.rm = TRUE)]
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
#' @family GMD utilities
#'
#' @returns evaluation
#' @keywords internal
eval_gmd_call <- function(calls, verbose = getOption("dlw.verbose"))  {
  if (verbose) print(calls)
  rlang::eval_tidy(calls[[1]])
}
