# Download data from datalibweb and save using stamp framework

Download data from datalibweb and save using stamp framework

## Usage

``` r
.dlw_download(
  country_code,
  filename,
  dlw_dir,
  id_name,
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

- dlw_dir:

  A folder object (as returned by .dlw_download)

- id_name:

  The name of the a dataset (as returned by .dlw_download)

- format:

  character: File format to use for saving data ('qs2' \[default\] or
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
