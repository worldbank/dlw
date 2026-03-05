# Get data from datalibweb (refactored)

\`dlw_get_data()\` function is the main user-facing function for
retrieving datasets from the Datalibweb (DLW) API. It handles
downloading, caching, and reading datasets, supporting both local and
temporary storage.

\*\*How it works:\*\* 1. Checks if the requested file exists locally
(unless \`local_overwrite = TRUE\`). 2. If it exists, reads it using
\`.dlw_read()\`. 3. If not, downloads the data from the DLW API using
\`.dlw_download()\`, saves it in the specified format, and returns it as
a \`data.table\`. 4. Handles directory management and caching via
\`.get_wrk_board()\`.

This function streamlines access to DLW datasets, automatically managing
download, storage, and retrieval.

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
  or 'qs2')

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
