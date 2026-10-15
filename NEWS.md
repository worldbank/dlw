# dlw 0.2.0

## Breaking changes

* `dlw_get_data()` requires `year`. Server and collection are now sent by default (collection is read from harmonized file names) (#24).
* `dlw_server_catalog()` lists all file types. Set `options(dlw.download_formats = "dta")` for the old behavior. `dlw_get_gmd()` and `dlw_get_gmd_support()` still use dta files only.
* Pin names change when `vermast` or `veralt` is used (e.g., `a_V12_M.qs2`), and for files that are not dta. Existing pins of these files are downloaded again.

## New features

* `dlw_download_file()` downloads any file as is, as raw bytes or to a path (#22).
* `dlw_get_data()` reads csv, tsv, sav, rds, parquet, xls and xlsx files. Other files (e.g., .do, .pdf) are pinned unchanged and their path is returned (#4).
* HTTP errors keep their status, endpoint and API message, with the token redacted (#23).

## Fixes

* Files with several versions on the server (e.g., Support files) download. The latest is used unless `vermast` or `veralt` is given (#25).
* `dlw_get_gmd_support()` matches `SUPPORT` and honors `vermast` (#17).

# dlw 0.1.2

# dlw 0.1.1

* fixes issue in #14
* display elements of environment
* informative message when the requests is not available in catalog.
