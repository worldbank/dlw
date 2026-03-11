# Get data from datalibweb (refactored)

Get data from datalibweb (refactored)

## Usage

``` r
dlw_get_data(
  country_code,
  filename,
  server = NULL,
  local_dir = getOption("dlw.local_dir"),
  local = fs::is_dir(local_dir),
  format = getOption("dlw.format"),
  local_overwrite = FALSE,
  version = NULL,
  verbose = getOption("dlw.verbose"),
  ...
)
```

## Arguments

- country_code:

  character: ISO3 code

- filename:

  character: Name of the file to save/read (required)

- server:

  character: in case we have more than one server. default is GMD

- local_dir:

  character: Local directory to save data. Default available in option
  dlw.local_dir which is set initially as "".

- local:

  logical: whether or not to save and read data locally. default is TRUE
  if \`local_dir\` exists.

- format:

  character: File format to use for pinning data ('parquet' \[default\]
  or 'qs')

- local_overwrite:

  logical. Whether to overwrite any saved data. Default is FALSE

- version:

  numeric: Version of the pin to read (for pinning data retrieval only)

- verbose:

  logical: whether to display info

- ...:

  additional filtering arguments (e.g.,survey_year, survey_acronym,
  vermast, veralt, collection, module)

## Value

data base request as data.table
