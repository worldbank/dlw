# get Support files for GMD

get Support files for GMD

## Usage

``` r
dlw_get_gmd_support(
  module = c("CPIICP", "CPI"),
  vermast = NULL,
  verbose = getOption("dlw.verbose"),
  filename = NULL
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

- filename:

  character: Name of any other file of the Support collection in the
  catalog (e.g., "Monthly_CPI.dta"). If given, \`module\` is ignored and
  \`vermast\` selects the version of the file ("v15" for the folder
  "Support_2005_CPI_V15_M"); the latest by default. See
  \`dlw_server_inventory("Support")\` for the files.

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
dlw_get_gmd_support(filename = "Monthly_CPI.dta", vermast = "v15")
} # }
```
