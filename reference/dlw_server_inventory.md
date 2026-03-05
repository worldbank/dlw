# filter server catolog to show inventory by country and other variables

filter server catolog to show inventory by country and other variables

## Usage

``` r
dlw_server_inventory(country, server = NULL, ...)
```

## Arguments

- country:

  character. same as \`country_code\` in \[dlw_get_data\] but with the
  purpose of easy programming. Not meant to be used by final user.

- server:

  character: in case we have more than one server. default is GMD

- ...:

  additional filtering arguments (e.g., year, module, survey, fileName)

## Value

filter server catalog from \[dlw_server_catalog\]

## See also

Other catalogs:
[`dlw_country_catalog()`](https://worldbank.github.io/dlw/reference/dlw_country_catalog.md),
[`dlw_server_catalog()`](https://worldbank.github.io/dlw/reference/dlw_server_catalog.md)

## Examples

``` r
if (FALSE) { # \dontrun{
dlw_server_inventory("COL", 2010)
} # }
```
