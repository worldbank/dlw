#' Retrieve versions of a GMD pin from a local board
#'
#' Attempts to retrieve the version history of a pin from a board using
#' `pin_versions()`. If the board doesn't support versioning or an error occurs,
#' it falls back to using `pin_meta()` to retrieve the latest version only.
#'
#' The result is normalized into a `data.table` with consistent columns:
#' `pin_name`, `version`, `hash`, and `created`.
#'
#' @inheritParams dlw_get_pins_versions
#' @param pin_name The name of the pin to retrieve versions for.
#'
#' @return A `data.table` with columns:
#' \describe{
#'   \item{pin_name}{The name of the pin.}
#'   \item{version}{The version identifier (if available).}
#'   \item{hash}{The content hash (if available).}
#'   \item{created}{The creation timestamp (POSIXct if parsable).}
#' }
#'
#' @examples
#' \dontrun{
#'   dlw_get_pin_versions("pin_name")
#' }
#'
#' @importFrom data.table data.table as.data.table :=
#' @importFrom pins pin_versions pin_meta
#' @export
dlw_pin_versions <- function(local_dir =  fs::path(Sys.getenv("PIP_ROOT_DIR"),
                                                   "PIP_ingestion_pipeline_v2/dlw_repository/dlw_data"),
                             pin_name) {

  #  set-up release and dlw data boards
  # pipfun::get_wrk_release(verbose = FALSE)
  # dlw_data_board <- pipfun::get_pins_boards(board = "dlw_data")
  dlw_data_board <- pins::board_folder(local_dir)

  # get a pin version details
  ver_dt <- tryCatch({

    v <- pins::pin_versions(dlw_data_board, pin_name)

    # normalize to expected columns
    if (!"version" %in% names(v)) v$version <- NA_character_
    if (!"created" %in% names(v)) v$created <- NA_character_
    if (!"hash" %in% names(v))    v$hash    <- NA_character_
    out <- as.data.table(v)
    out[, pin_name := pin_name]

    # ensure created is POSIXct if possible
    suppressWarnings({
      if (is.character(out$created)) {
        # parsing common formats; if fails, leave as character
        parsed <- as.POSIXct(out$created, tz = "UTC", tryFormats = c(
          "%Y-%m-%d %H:%M:%OS", "%Y-%m-%dT%H:%M:%OSZ",
          "%Y-%m-%d", "%m/%d/%Y %H:%M", "%m/%d/%Y"
        ))
        if (any(!is.na(parsed))) out[, created := parsed]
      }
    })
    out[]
  }, error = function(e) NULL)

  # Fallback: latest only via pin_meta()
  if (is.null(ver_dt) || nrow(ver_dt) == 0 || all(is.na(ver_dt$version))) {
    meta <- tryCatch(pins::pin_meta(dlw_data_board, pin_name), error = function(e) NULL)
    v <- if (!is.null(meta)) meta$version else list()
    return(data.table(
      pin_name = pin_name,
      version = if (!is.null(v$version)) v$version else NA_character_,
      hash    = if (!is.null(v$hash))    v$hash    else NA_character_,
      created = as.POSIXct(NA)
    ))
  }

  return(invisible(ver_dt))
}

#' Retrieve and Save Version Information for GMD Pins
#'
#' This function retrieves metadata (including versions, creation dates, and hashes)
#' for all pins stored in the `dlw_data` board, processes and orders the metadata,
#' and writes the resulting summary to the `dlw_inventory` board as a versioned pin.
#'
#' @param local_dir character: Local directory where the pins are saved.
#' @param local_info character: Local directory where the pins information is saved.
#'
#' @return Invisibly returns `NULL`. The function is called for its side effects:
#' saving the pin metadata to the inventory board.
#' @export
#'
#' @examples
#' \dontrun{
#' dlw_get_pins_versions()
#' }
dlw_get_pins_versions <- function(local_dir =  fs::path(Sys.getenv("PIP_ROOT_DIR"),
                                                        "PIP_ingestion_pipeline_v2/dlw_repository/dlw_data"),
                                  local_info = fs::path(Sys.getenv("PIP_ROOT_DIR"),
                                                        "PIP_ingestion_pipeline_v2/dlw_repository/dlw_inventory")){

  #  set-up release and dlw data and inventory boards
  # pipfun::get_wrk_release(verbose = FALSE)
  # dlw_data_board <- pipfun::get_pins_boards(board = "dlw_data")
  # dlw_inv_board <- pipfun::get_pins_boards(board = "dlw_inventory")
  dlw_data_board <- pins::board_folder(local_dir)
  dlw_inv_board  <- pins::board_folder(local_info)

  # get a list of all pins on dlw gmd data board
  pin_names <- pins::pin_list(dlw_data_board)

  # collect pins details for all pins (versions, created date, hash)
  all_pins_info <- rbindlist(lapply(pin_names, function(x) dlw_pin_versions(x)),
                             fill = TRUE)

  # drop duplicate and order
  all_pins_info <- unique(all_pins_info, by = c("pin_name", "version", "hash", "created"))

  # order by pin then created desc (when available) then version desc
  setorderv(all_pins_info,
            cols = c("pin_name", "created", "version"),
            order = c(1, -1, -1),
            na.last = TRUE)

  dlw_inv_board |>
    pins::pin_write(all_pins_info, "dlw_gmd_pins_info", versioned = TRUE, type = "qs")

  invisible(NULL)

}

#' Retrieve the DLW GMD Pins Version Info
#'
#' This function reads the pin containing metadata about DLW GMD pins
#' (written by `dlw_get_pins_versions()`) from the `dlw_inventory` board.
#'
#' @inheritParams dlw_get_pins_versions
#' @return A `data.table` containing pin metadata including pin name, version,
#' creation date, and hash.
#' @export
#'
#' @examples
#' \dontrun{
#' pin_info <- dlw_read_pins_versions()
#' head(pin_info)
#' }
dlw_read_pins_versions <- function(local_info = fs::path(Sys.getenv("PIP_ROOT_DIR"),
                                                         "PIP_ingestion_pipeline_v2/dlw_repository/dlw_inventory")) {

  # get the inventory board
  dlw_inv_board  <- pins::board_folder(local_info)

  # read the stored pin
  pin_info <- pins::pin_read(dlw_inv_board, name = "dlw_gmd_pins_info")

  # ensure it's a data.table
  data.table::setDT(pin_info)

  return(pin_info)
}
