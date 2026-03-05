# Get country catalog

Get country catalog

## Usage

``` r
dlw_country_catalog(
  country_code,
  dlw_url = NULL,
  api_version = getOption("dlw.default_api_version"),
  force = FALSE,
  internal = FALSE,
  verbose = getOption("dlw.verbose"),
  store_request = FALSE
)
```

## Arguments

- country_code:

  character: ISO3 code

- dlw_url:

  character: in case we have more than one url.

- api_version:

  character: API version

- force:

  logical: If TRUE, it will query the API regardless of whether there is
  version available in .dlwenv

- internal:

  logical: for internal use ONLY.

- verbose:

  logical: whether to display info

- store_request:

  logical: store request in .dlwenv as \`last_req\`

## Value

data.table with country catalog

## See also

Other catalogs:
[`dlw_server_catalog()`](https://worldbank.github.io/dlw/reference/dlw_server_catalog.md),
[`dlw_server_inventory()`](https://worldbank.github.io/dlw/reference/dlw_server_inventory.md)

## Examples

``` r
if (FALSE) { # \dontrun{
dlw_country_catalog("COL")
} # }
```
