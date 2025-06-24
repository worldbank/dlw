#' dlw repository
#' @param name character: name of the repo (do NOT use spaces)
#' @param local_dir directory path
#' @name repo
NULL
#> NULL



#' @rdname repo
#' @param catalog data.table from [dlw_server_catalog]
#' @returns invisible data.table. It saves a dlw_repo file in `local_dir`
#' @export
repo_create <- function(name,
                        local_dir = getwd(),
                        catalog = NULL) {

}

#' @rdname repo
#' @returns Loads dlw_repo file in `local_dir`
#' @export
repo_load <- function(name,
                      local_dir = getwd()) {

}

