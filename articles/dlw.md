# dlw

[dlw](https://github.com/worldbank/dlw) is an R client for the World
Bank’s internal [datalibweb](https://datalibweb2.worldbank.org/) API. It
provides access to microdata and documentation, including current and
historical versions of harmonized collections and raw, non-harmonized
data.

## Authentication

Set your datalibweb API token before making requests. Obtain or renew a
token from the [datalibweb page](https://datalibweb2.worldbank.org/) and
set it for the current R session:

``` r

library(dlw)
dlw_set_token("your_token_here")
```

## Server catalog

[`dlw_server_catalog()`](https://worldbank.github.io/dlw/reference/dlw_server_catalog.md)
retrieves the catalog for a server (GMD by default). It includes all
file types, such as data, programs, and documents. Use
`dlw.download_formats` to restrict the extensions returned by the
catalog.

``` r

ctl <- dlw_server_catalog()
ctl[Country_code == "COL" & Module == "ALL" & Survey_year == 2010,
    .(FileName, Vermast, Veralt, Ext)]

# Limit catalog results to Stata data files
options(dlw.download_formats = "dta")
ctl_dta <- dlw_server_catalog(force = TRUE)
```

Catalog results are `data.table` objects and can be filtered using
data.table syntax.

## Read data

[`dlw_get_data()`](https://worldbank.github.io/dlw/reference/dlw_get_data.md)
downloads a named file and pins it for reuse. The `year` argument is
required. Common tabular formats (`dta`, `sav`, `csv`, `tsv`, `rds`,
`parquet`, `xls`, and `xlsx`) are read as tables; Parquet requires the
suggested `arrow` package, and Excel requires `readxl`. Other file types
are pinned unchanged and the path to the pinned file is returned.

``` r

pry <- dlw_get_data(
  country_code = "PRY",
  year = 2011,
  server = "GMD",
  survey = "EPH",
  module = "GPWG",
  filename = "PRY_2011_EPH_V01_M_V03_A_GMD_GPWG.dta"
)
```

The collection is inferred from harmonized filenames when it is not
supplied. When a filename has multiple server versions, the latest is
selected by default. Supply `vermast` and/or `veralt` to select a
specific version.

For GMD data,
[`dlw_get_gmd()`](https://worldbank.github.io/dlw/reference/dlw_get_gmd.md)
is a convenience wrapper that resolves the catalog entry from country,
year, and module:

``` r

pry <- dlw_get_gmd(country_code = "PRY", year = 2011, module = "GPWG")
```

## Download files unchanged

Use
[`dlw_download_file()`](https://worldbank.github.io/dlw/reference/dlw_download_file.md)
when a file should be returned as raw bytes or saved directly, without
reading or pinning it. The `year` argument is also required.

``` r

# Return raw bytes
program <- dlw_download_file(
  "WLD", "WLD_2026_ALL_V02_M_V01_A_GMI_INE.do", year = 2026
)

# Save the downloaded file in a directory
dlw_download_file(
  "WLD", "WLD_2026_ALL_V02_M_V01_A_GMI_INE.do",
  year = 2026, path = tempdir()
)
```
