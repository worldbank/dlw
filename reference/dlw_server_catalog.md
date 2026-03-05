# Server catalog

Server catalog

## Usage

``` r
dlw_server_catalog(
  server = NULL,
  dlw_url = NULL,
  api_version = getOption("dlw.default_api_version"),
  force = FALSE,
  verbose = getOption("dlw.verbose"),
  store_request = FALSE
)
```

## Arguments

- server:

  character: in case we have more than one server. default is GMD

- dlw_url:

  character: in case we have more than one url.

- api_version:

  character: API version

- force:

  logical: If TRUE, it will query the API regardless of whether there is
  version available in .dlwenv

- verbose:

  logical: whether to display info

- store_request:

  logical: store request in .dlwenv as \`last_req\`

## Value

dataframe with

## See also

Other catalogs:
[`dlw_country_catalog()`](https://worldbank.github.io/dlw/reference/dlw_country_catalog.md),
[`dlw_server_inventory()`](https://worldbank.github.io/dlw/reference/dlw_server_inventory.md)

## Examples

``` r
if (FALSE) { # \dontrun{
dlw_server_catalog(sever = "GMD")
} # }
```
