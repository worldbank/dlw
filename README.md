
<!-- README.md is generated from README.Rmd. Please edit that file -->

# dlw

<!-- badges: start -->
<!-- badges: end -->

`{dlw}` is an R client of the internal
[datalibweb](https://datalibweb2.worldbank.org/) API of the World Bank.
Datalibweb is a data system designed to enable users to seamlessly
access microdata and documentation in the World Bank. Users can access
the most up-to-date and historical versions of harmonized data
collections and raw/non-harmonized data for subsequent analysis

## Installation

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

To begin working with `{dlw}`, you must provide your datalibweb API
token using the `dlw_set_token()` function.

You can obtain or renew your token by visiting the [datalibweb
page](https://datalibweb2.worldbank.org/) and following the instructions
there. Once you have your token, set it in your R session as follows:

``` r
dlw_set_token("your_token_here")
```

### server catalob

After setting your token, you can download the catalog for the
corresponding server using `dlw_server_catalog()`. By default, this
downloads the “GMD” catalog. The downloaded catalog is saved for the
current session in a hidden environment within the `{dlw}` package,
allowing for easy and efficient access in subsequent operations.

``` r
ctl <- dlw_server_catalog()
#> Pruning cache
#> ℹ saving ServerCatalog_GMD in .dlwevn
```

The `{dlw}` package returns results as `data.table` objects, enabling
lightning-fast data manipulation and filtering using concise syntax. For
example, you can instantly list all available files for Colombia in 2010
in module “ALL” of the GMD collection with a single line of code:

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

The workhorse function to download data is `dlw_get_data()`. However, it
requires several pieces of information that you may not have at hand,
such as:

``` r
dlw_get_data(
  country_code = "PRY",
  year = 2011L,
  server = "GMD",
  survey = "EPH",
  module = "GPWG",
  filename = "PRY_2011_EPH_V01_M_V08_A_GMD_GPWG.dta",
  collection = "GMD"
)
```

To simplify this process, we have developed a wrapper function that
works only for the GMD server: `dlw_get_gmd()`. This function is much
easier to use:

``` r
pry <- dlw_get_gmd(country_code = "PRY", year = 2011, module = "GPWG", vermast = "v01", veralt = "v08")
#> 
#> ── dlw_get_data Calls ──────────────────────────────────────────────────────────
#> Call 1:
#> dlw_get_data(
#>   country_code = "PRY",
#>   year = 2011L,
#>   server = "GMD",
#>   survey = "EPH",
#>   module = "GPWG",
#>   filename = "PRY_2011_EPH_V01_M_V08_A_GMD_GPWG.dta",
#>   collection = "GMD"
#> )
pry[, weighted.mean(welfare, weight, na.rm = TRUE)]
#> [1] 12675293
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
pry[, weighted.mean(welfare, weight, na.rm = TRUE)]
#> [1] 12675293
```

In this case, the most recent version available will be used (for
example, master version v02 and alternative version v01).
