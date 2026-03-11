# Read data from a pin (local or temp)

Read data from a pin (local or temp)

## Usage

``` r
dlw_read(board, pin_name, version = NULL)
```

## Arguments

- board:

  A pins board object (as returned by dlw_download)

- pin_name:

  The name of the pin (as returned by dlw_download)

- version:

  numeric: Version of the pin to read (for pinning data retrieval only)

## Value

data.table
