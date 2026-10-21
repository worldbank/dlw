# Latest master version in a list of file versions

Latest master version in a list of file versions

## Usage

``` r
latest_version(versions)
```

## Arguments

- versions:

  data.table of file versions returned by the API, with the version
  folder (e.g., "Survey_2005_CPI_V15_M") in \`FileSharePath\`.

## Value

character like "V15_M", or NULL if no version could be identified.
