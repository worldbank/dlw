# dlw

[![Ask
DeepWiki](https://deepwiki.com/badge.svg)](https://deepwiki.com/worldbank/dlw)

[dlw](https://github.com/worldbank/dlw) is an R client of the internal
[datalibweb](https://datalibweb2.worldbank.org/) API of the World Bank.
Datalibweb is a data system designed to enable users to seamlessly
access microdata and documentation in the World Bank. Users can access
the most up-to-date and historical versions of harmonized data
collections and raw/non-harmonized data for subsequent analysis

The package can list all files available on a server, read common
tabular formats, and download other files such as programs and documents
without changing their contents.

## Installation

Since [dlw](https://github.com/worldbank/dlw) is hosted in the World
Bank’s GitHub organization, you need to authorize your Personal Access
Token (PAT) for SAML Single Sign-On (SSO) before you can install it.
Follow these steps to authorize your PAT:

1.  Go to your GitHub account: Visit <https://github.com> and log in.

2.  Check your PAT: Go to Settings → Developer settings → Personal
    access tokens. Find the token you are using (or create a new one if
    needed).

3.  Authorize your PAT for SAML SSO: Go to
    <https://github.com/settings/tokens>. Next to your token, You’ll see
    a button that says “Configure SSO.” Click it to authorize your token
    to the worldbank.

4.  Try installing

You can install the development version of dlw from
[GitHub](https://github.com/) with:

``` r

# install.packages("pak")
pak::pak("worldbank/dlw")
# or
remotes::github_install("worldbank/dlw")
```

## Usage

``` r

library(dlw)
```

### Token

To begin working with [dlw](https://github.com/worldbank/dlw), you must
provide your datalibweb API token using the
[`dlw_set_token()`](https://worldbank.github.io/dlw/reference/token.md)
function.

You can obtain or renew your token by visiting the [datalibweb
page](https://datalibweb2.worldbank.org/) and following the instructions
there. Once you have your token, set it in your R session as follows:

``` r

dlw_set_token("your_token_here")
```

### Server catalog

After setting your token, you can download the catalog for the
corresponding server using
[`dlw_server_catalog()`](https://worldbank.github.io/dlw/reference/dlw_server_catalog.md).
By default, this downloads the “GMD” catalog and lists all file types,
including data, programs, and documents. To restrict the catalog to
specific extensions, set `options(dlw.download_formats = "dta")` (or a
vector of extensions) before requesting it. The downloaded catalog is
saved for the current session in a hidden environment within the
[dlw](https://github.com/worldbank/dlw) package, allowing for easy and
efficient access in subsequent operations.

``` r

ctl <- dlw_server_catalog()
#> Pruning cache
#> ℹ saving ServerCatalog_GMD in .dlwevn
```

The [dlw](https://github.com/worldbank/dlw) package returns results as
`data.table` objects, enabling lightning-fast data manipulation and
filtering using concise syntax. For example, you can instantly list all
available files for Colombia in 2010 in module “ALL” of the GMD
collection with a single line of code:

``` r

ctl[Country_code == "COL" & Module == "ALL" & Survey_year == 2010, 
  .(FileName, Vermast, Veralt )]
#>                                 FileName Vermast Veralt
#>                                   <char>  <char> <char>
#> 1: COL_2010_GEIH_V02_M_V09_A_GMD_ALL.dta     V02    V09
#> 2: COL_2010_GEIH_V02_M_V08_A_GMD_ALL.dta     V02    V08
#> 3: COL_2010_GEIH_v02_M_v07_A_GMD_ALL.dta     v02    v07
```

### Downloading files

The workhorse function to download data is
[`dlw_get_data()`](https://worldbank.github.io/dlw/reference/dlw_get_data.md).
However, it requires several pieces of information that you may not have
at hand, such as:

``` r

dlw_get_data(
  country_code = "PRY",
  year = 2011L,
  server = "GMD",
  survey = "EPH",
  module = "GPWG",
  filename = "PRY_2011_EPH_V01_M_V03_A_GMD_GPWG.dta",
  collection = "GMD"
)
```

To simplify this process, we have developed a wrapper function that
works only for the GMD server:
[`dlw_get_gmd()`](https://worldbank.github.io/dlw/reference/dlw_get_gmd.md).
It chooses the latest master and alternative versions by default and is
easier to use:

``` r

pry <- dlw_get_gmd(country_code = "PRY", year = 2011, module = "GPWG", vermast = "v01", veralt = "v03")
#> 
#> ── dlw_get_data Calls ──────────────────────────────────────────────────────────
#> Call 1:
#> dlw_get_data(
#>   country_code = "PRY",
#>   year = 2011L,
#>   server = "GMD",
#>   survey = "EPH",
#>   module = "GPWG",
#>   filename = "PRY_2011_EPH_V01_M_V03_A_GMD_GPWG.dta",
#>   collection = "GMD"
#> )
#> ℹ saving last_req in .dlwevn
#> ℹ saving last_raw_data in .dlwevn
#> Creating new version '20250617T202722Z-662d2'
#> Writing to pin 'PRY_2011_EPH_V01_M_V03_A_GMD_GPWG.parquet'
pry[, weighted.mean(welfare, weight, na.rm = TRUE)]
#> [1] 1099527
```

If you are interested in downloading the most recent version of a file,
you can simply omit the version arguments:

``` r

pry <- dlw_get_gmd(country_code = "PRY", year = 2011, module = "GPWG")
#> 
#> ── dlw_get_data Calls ──────────────────────────────────────────────────────────
#> Call 1:
#> dlw_get_data(
#>   country_code = "PRY",
#>   year = 2011L,
#>   server = "GMD",
#>   survey = "EPH",
#>   module = "GPWG",
#>   filename = "PRY_2011_EPH_V02_M_V01_A_GMD_GPWG.dta",
#>   collection = "GMD"
#> )
#> ℹ saving last_req in .dlwevn
#> ℹ saving last_raw_data in .dlwevn
#> Creating new version '20250617T202725Z-6bd54'
#> Writing to pin 'PRY_2011_EPH_V02_M_V01_A_GMD_GPWG.parquet'
pry[, weighted.mean(welfare, weight, na.rm = TRUE)]
#> [1] 12675293
```

In this case, the most recent version available will be used (for
example, master version v02 and alternative version v01). To inspect
calls for all matching versions instead, use `latest_version = FALSE`.

[`dlw_get_data()`](https://worldbank.github.io/dlw/reference/dlw_get_data.md)
reads Stata, SPSS, CSV, TSV, RDS, Parquet, XLS, and XLSX files as
tables. Reading Parquet files requires the suggested `arrow` package;
reading Excel files requires `readxl`. Other file types are pinned
unchanged and the path to the pinned file is returned. To download any
file without pinning it, use
[`dlw_download_file()`](https://worldbank.github.io/dlw/reference/dlw_download_file.md):
it returns raw bytes by default or saves the file when `path` is
supplied. Both functions require `year` for the API request.

``` r

# Return the file contents as raw bytes
code <- dlw_download_file("WLD", "WLD_2026_ALL_V02_M_V01_A_GMI_INE.do",
                          year = 2026)

# Save a file to a directory
dlw_download_file("WLD", "WLD_2026_ALL_V02_M_V01_A_GMI_INE.do",
                  year = 2026, path = tempdir())
```
