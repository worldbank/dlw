# Get data from datalibweb (refactored)

Downloads a file and pins it, so later calls read it from the board.
Files with extension dta, sav, csv, tsv, rds, parquet, xls or xlsx are
read as tables (the first sheet of an Excel file) and pinned as
\`format\`. Any other file (e.g., .do, .R, .pdf, .zip) is pinned as is
and its path is returned. To save a file without pinning it, see
\[dlw_download_file\].

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

  character: File format to use for pinning data ('qs2' \[default\] or
  'parquet')

- local_overwrite:

  logical. Whether to overwrite any saved data. Default is FALSE

- version:

  numeric: Version of the pin to read (for pinning data retrieval only)

- verbose:

  logical: whether to display info

- ...:

  additional filtering arguments (e.g.,survey_year, survey_acronym,
  vermast, veralt, collection, module). When \`filename\` exists in
  several versions on the server, \`vermast\` and \`veralt\` (e.g.,
  "v02") select one (sent to the API as \`para1\`); if neither is given,
  the latest version is used. \`year\` is required. If \`collection\` is
  not given, it is taken from harmonized file names (e.g., "GMD" in
  "COL_2010_GEIH_V04_M_V01_A_GMD_ALL.dta").

## Value

data base request as data.table; for a file that is not a table, the
path to its pinned copy. If the server holds several versions of
\`filename\` and none could be chosen, a data.table with the versions.
