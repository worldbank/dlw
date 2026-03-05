# Read data from a pin (local or temp)

Reads a dataset from a specified directory and file name, returning it
as a \`data.table\`.

## Usage

``` r
.dlw_read(dlw_dir, id_name, version = NULL)
```

## Arguments

- dlw_dir:

  A folder object (as returned by .dlw_download)

- id_name:

  The name of the a dataset (as returned by .dlw_download)

- version:

  numeric: Version of the data to read (for versioning data retrieval
  only)

## Value

data.table

## Details

\- Lists files in \`dlw_dir\` and checks if \`id_name\` exists. - If not
found, aborts with an error. - Removes any extension from \`id_name\`. -
Loads the data using \`stamp::st_load()\` from a \`.qs2\` file in the
directory.

This function is used internally to retrieve previously saved or
downloaded datasets in a fast, versioned format.
