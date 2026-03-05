# get Support files for GMD

get Support files for GMD

## Usage

``` r
dlw_get_gmd_support(
  module = c("CPIICP", "CPI"),
  vermast = NULL,
  verbose = getOption("dlw.verbose")
)
```

## Arguments

- module:

  character: As of now, either CPIICP (the default) or CPI.

- vermast:

  character: Version of the master data in the form "vXX" where X is a
  number of two digits like "01" or "02".

- verbose:

  logical: whether to display info

## Value

data.table

## See also

Other GMD utilities:
[`dlw_get_gmd()`](https://worldbank.github.io/dlw/reference/dlw_get_gmd.md),
[`eval_gmd_call()`](https://worldbank.github.io/dlw/reference/eval_gmd_call.md),
[`gmd_calls()`](https://worldbank.github.io/dlw/reference/gmd_calls.md)

## Examples

``` r
if (FALSE) { # \dontrun{
dlw_get_gmd_support()
dlw_get_gmd_support(vermast = "v10")
} # }
```
