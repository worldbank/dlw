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
  vermast, veralt, collection, module). When \`filename\` exists in
  several versions on the server, \`vermast\` and \`veralt\` (e.g.,
  "v02") select one (sent to the API as \`para1\`); if neither is given,
  the latest version is used. \`year\` is required. If \`collection\` is
  not given, it is taken from harmonized file names (e.g., "GMD" in
  "COL_2010_GEIH_V04_M_V01_A_GMD_ALL.dta").

- verbose:

  logical: whether to display info

## Value

The table as data.table, or the path to the pinned copy of a file that
is not a table, or the data.table of file versions (see
\[dlw_get_data\]).
