# Download data from datalibweb and save as a pin

Download data from datalibweb and save as a pin

## Usage

``` r
dlw_download(
  country_code,
  filename,
  board,
  pin_name,
  format,
  server = NULL,
  ...,
  verbose = getOption("dlw.verbose")
)
```

## Arguments

- country_code:

  character: ISO3 code

- filename:

  character: Name of the file to save/read (required)

- board:

  A pins board object (as returned by dlw_download)

- pin_name:

  The name of the pin (as returned by dlw_download)

- format:

  character: File format to use for pinning data ('qs2' \[default\] or
  'parquet')

- server:

  character: in case we have more than one server. default is GMD

- ...:

  additional filtering arguments (e.g.,survey_year, survey_acronym,
  vermast, veralt, collection, module)

- verbose:

  logical: whether to display info

## Value

A list with the board and pin_name used
