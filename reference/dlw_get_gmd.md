# Get GMD data.

This is a wrapper of dlw_get_data. It just build the calls to
dlw_get_data, for ease of use.

## Usage

``` r
dlw_get_gmd(
  country_code,
  year = NULL,
  module = NULL,
  survey = NULL,
  filename = NULL,
  vermast = NULL,
  veralt = NULL,
  latest_version = TRUE,
  latest_year = FALSE,
  verbose = getOption("dlw.verbose"),
  ...
)
```

## Arguments

- country_code:

  character: ISO3 code

- year:

  numeric: four digit year

- module:

  character: module of GMD collection (e.g., ALL, GPWG, L)

- survey:

  character: survey acronyn

- filename:

  character: File name

- vermast:

  character: Version of the master data in the form "vXX" where X is a
  number of two digits like "01" or "02".

- veralt:

  character: Version of the alternative data in the form "vXX" where X
  is a number of two digits like "01" or "02".

- latest_version:

  logical: If TRUE and \`vermast\` and \`veralt\` are NULL, it will use
  the most recent version of the data for a particular year.

- latest_year:

  logical: If \`TRUE\` and \`year\` is NULL, it retrieves the most
  recent year. Otherwise, it will return the calls for all the years
  available. This is the default.

- verbose:

  logical: whether to display info

- ...:

  Arguments passed on to
  [`dlw_get_data`](https://worldbank.github.io/dlw/reference/dlw_get_data.md)

  `local_dir`

  :   character: Local directory to save data. Default available in
      option dlw.local_dir which is set initially as "".

  `local`

  :   logical: whether or not to save and read data locally. default is
      TRUE if \`local_dir\` exists.

  `local_overwrite`

  :   logical. Whether to overwrite any saved data. Default is FALSE

## Value

If the call is unique, it will return the data. If not, it will return
the posibilities for the user to choose.

## See also

Other GMD utilities:
[`dlw_get_gmd_support()`](https://worldbank.github.io/dlw/reference/dlw_get_gmd_support.md),
[`eval_gmd_call()`](https://worldbank.github.io/dlw/reference/eval_gmd_call.md),
[`gmd_calls()`](https://worldbank.github.io/dlw/reference/gmd_calls.md)

## Examples

``` r
if (FALSE) { # \dontrun{
  dlw_get_gmd("PRY", 2010, "GPWG") # latest version of 2010
  dlw_get_gmd("PRY", module = "GPWG") # latest year and latest version
} # }
```
