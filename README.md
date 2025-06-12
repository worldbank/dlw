
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
downloads the “GMD” catalog:

``` r
dlw_server_catalog()
```

The downloaded catalog is saved for the current session in a hidden
environment within the `{dlw}` package, allowing for easy and efficient
access in subsequent operations.

``` r
# Set your token first
# dlw_set_token("your_token_here")
# Download the default catalog (GMD)
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
