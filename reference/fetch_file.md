# Request a file from datalibweb

Sends the request, selects the version of an ambiguous file and checks
the response, but does not read the file.

## Usage

``` r
fetch_file(country_code, filename, server = NULL, ...)
```

## Arguments

- country_code:

  character: ISO3 code

- filename:

  character: Name of the file to save/read (required)

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

## Value

A list with the raw \`body\`, \`filename\`, \`content_type\` and
\`status\` of the file (see \[handle_file_resp\]); or, if the server
holds several versions of \`filename\` and none could be chosen, a
data.table with the versions.
