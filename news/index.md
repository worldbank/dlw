# Changelog

## dlw 0.2.0

### Breaking changes

- [`dlw_get_data()`](https://worldbank.github.io/dlw/reference/dlw_get_data.md)
  requires `year`. Server and collection are now sent by default
  (collection is read from harmonized file names)
  ([\#24](https://github.com/worldbank/dlw/issues/24)).
- [`dlw_server_catalog()`](https://worldbank.github.io/dlw/reference/dlw_server_catalog.md)
  lists all file types. Set `options(dlw.download_formats = "dta")` for
  the old behavior.
  [`dlw_get_gmd()`](https://worldbank.github.io/dlw/reference/dlw_get_gmd.md)
  and
  [`dlw_get_gmd_support()`](https://worldbank.github.io/dlw/reference/dlw_get_gmd_support.md)
  still use dta files only.
- Pin names change when `vermast` or `veralt` is used (e.g.,
  `a_V12_M.qs2`), and for files that are not dta. Existing pins of these
  files are downloaded again.

### New features

- [`dlw_download_file()`](https://worldbank.github.io/dlw/reference/dlw_download_file.md)
  downloads any file as is, as raw bytes or to a path
  ([\#22](https://github.com/worldbank/dlw/issues/22)).
- [`dlw_get_data()`](https://worldbank.github.io/dlw/reference/dlw_get_data.md)
  reads csv, tsv, sav, rds, parquet, xls and xlsx files. Other files
  (e.g., .do, .pdf) are pinned unchanged and their path is returned
  ([\#4](https://github.com/worldbank/dlw/issues/4)).
- [`dlw_get_gmd_support()`](https://worldbank.github.io/dlw/reference/dlw_get_gmd_support.md)
  gets any other Support file with `filename` (e.g.,
  `"Monthly_CPI.dta"`, `vermast = "v15"`).
- HTTP errors keep their status, endpoint and API message, with the
  token redacted ([\#23](https://github.com/worldbank/dlw/issues/23)).

### Fixes

- Files with several versions on the server (e.g., Support files)
  download. The latest is used unless `vermast` or `veralt` is given
  ([\#25](https://github.com/worldbank/dlw/issues/25)).
- [`dlw_get_gmd_support()`](https://worldbank.github.io/dlw/reference/dlw_get_gmd_support.md)
  matches `SUPPORT` and honors `vermast`
  ([\#17](https://github.com/worldbank/dlw/issues/17)).

## dlw 0.1.2

## dlw 0.1.1

- fixes issue in [\#14](https://github.com/worldbank/dlw/issues/14)
- display elements of environment
- informative message when the requests is not available in catalog.
