# Server Catalog

## Introduction

The **dlw** package provides access to datasets from the Datalibweb
(DLW) API. Before downloading data, you need to explore what datasets
are available. The
[`dlw_server_catalog()`](https://worldbank.github.io/dlw/reference/dlw_server_catalog.md)
function retrieves a comprehensive catalog of all available files on the
server, making it easy to discover and filter datasets.

This vignette walks you through:

- Retrieving the server catalog
- Understanding the catalog structure
- Filtering and exploring datasets
- Leveraging caching for performance

## Prerequisites

Before using
[`dlw_server_catalog()`](https://worldbank.github.io/dlw/reference/dlw_server_catalog.md),
ensure you have:

1.  Installed the **dlw** package
2.  Set your API token using
    [`dlw_set_token()`](https://worldbank.github.io/dlw/reference/token.md)

``` r
# Install dlw (if not already installed)
# devtools::install_github("worldbank/dlw")

# devtools::load_all(".")
library(dlw)
library(data.table)

# Set your API token
# dlw_set_token("your_api_token_here")
```

## Basic Usage: Retrieve the Catalog

The simplest way to get the server catalog is:

``` r
# Retrieve the default (GMD) server catalog list of datasets
catalog <- dlw_server_catalog()

# View the first few rows
head(catalog)
```

This returns a `data.table` with metadata about all `.dta` files
available on the server.

### What’s in the Catalog?

The catalog contains the following key columns:

``` r
# View column names
colnames(catalog)

# Check the structure
str(catalog)
```

Key columns include:

| Column             | Description                                                         |
|--------------------|---------------------------------------------------------------------|
| **FileName**       | The file name (e.g., `COL_2022_GEIH_v01_M_v02_A_GMD_GPWG.dta`)      |
| **Country_code**   | ISO3 country code (e.g., `COL`, `IND`, `BRA`)                       |
| **Survey_year**    | Year of the survey (integer)                                        |
| **Survey_acronym** | Survey acronym (e.g., `ECH`, `ENH`, `GEIH`, `HBS`)                  |
| **Vermast**        | Master version (e.g., `V01`, `V02`)                                 |
| **Veralt**         | Alternative version (e.g., `V01`, `V02`)                            |
| **Collection**     | Collection name (e.g., `GMD`)                                       |
| **Module**         | Module name (e.g., `ALL`, `GPWG`, `ASPIRE`, `HIST`, `GROUP`, `BIN`) |

## Exploring the Catalog

### Count Datasets by Country

``` r
# How many datasets per country?
catalog[, .(count = .N), by = Country_code][order(-count)]
```

### Find All Surveys for a Specific Country

``` r
# Get all surveys for Colombia
colombia <- catalog[Country_code == "COL"]

# View unique surveys and years
colombia[, .(count = .N), by = .(Survey_acronym, Survey_year)][order(Survey_year)]
```

### Find the Latest Version of a Dataset

``` r
# Find the most recent survey for Colombia
latest_col <- catalog[Country_code == "COL"][order(-Survey_year)][1]

# View details
latest_col[, .(Country_code, Survey_year, Survey_acronym, Vermast, Veralt, FileName)]
```

### Filter by Multiple Criteria

``` r
# Find all GEIH surveys from 2010 onwards
geih_recent <- catalog[
  Survey_acronym == "GEIH" & Survey_year >= 2010
][order(Country_code, -Survey_year)]

head(geih_recent)
```

### Search by Module or Collection

``` r
# Find all datasets containing GPWG Module
gpwg_data <- catalog[grepl("GPWG", Module, ignore.case = TRUE)]

nrow(gpwg_data)  # How many?

# View unique modules
catalog[, .(count = .N), by = Module][order(-count)]
```

## Using `dlw_server_inventory()` for Easier Filtering

While
[`dlw_server_catalog()`](https://worldbank.github.io/dlw/reference/dlw_server_catalog.md)
returns the full catalog,
[`dlw_server_inventory()`](https://worldbank.github.io/dlw/reference/dlw_server_inventory.md)
provides a convenient wrapper for filtering:

``` r
# Filter by country and year
inv <- dlw_server_inventory("COL", year = 2010)

# This is equivalent to:
inv_manual <- catalog[Country_code == "COL" & Year == 2010]

# They should be identical
identical(inv, inv_manual)
```

## Understanding Caching

By default,
[`dlw_server_catalog()`](https://worldbank.github.io/dlw/reference/dlw_server_catalog.md)
caches the catalog in memory during your R session. This improves
performance for repeated calls:

``` r
# First call: queries the API (slower)
system.time({
  cat1 <- dlw_server_catalog()
})

# Second call: uses cache (much faster)
system.time({
  cat2 <- dlw_server_catalog()
})

# Force a fresh API call
system.time({
  cat_fresh <- dlw_server_catalog(force = TRUE)
})
```

### When to Use `force = TRUE`

- **Development/testing**: When you expect the server catalog to have
  changed
- **Production workflows**: Occasionally refresh to catch new datasets
- **Debugging**: When troubleshooting connectivity or data issues

``` r
# Refresh the catalog from the server
catalog_latest <- dlw_server_catalog(force = TRUE, verbose = TRUE)
```

## Advanced: Understanding Filename Structure

The dlw package follows a strict naming convention for GMD files.
Understanding this helps you work with datasets programmatically.

### Filename Pattern

The general pattern is:

    <Country>_<Year>_<Acronym>_V<nn>_M_V<nn>_A_<Collection>_<Module>.dta

**Example:** `COL_2022_GEIH_V01_M_V02_A_GMD_GPWG.dta`

**Breakdown:**

- **COL**: Country code
- **2022**: Survey year
- **GEIH**: Survey acronym
- **V01**: Master version
- **M**: Marker (always present)
- **V02**: Alternative version
- **A**: Marker (always present)
- **GMD**: Collection
- **GPWG**: Module
- **.dta**: File format (Stata)

### Accessing Parsed Components

The catalog automatically parses this structure into columns:

``` r
# Extract key components for a file
file_info <- catalog[FileName == "COL_2022_GEIH_V01_M_V02_A_GMD_GPWG.dta"]

data.frame(
  Country = file_info$Country_code,
  Year = file_info$Survey_year,
  Acronym = file_info$Survey_acronym,
  MasterVersion = file_info$Vermast,
  AltVersion = file_info$Veralt,
  Collection = file_info$Collection,
  Module = file_info$Module)
```

### Downloading Data

See `?dlw_get_data()` for more download options.

## Best Practices

1.  **Cache wisely**: Use the default caching behavior for interactive
    work; use `force = TRUE` in automated scripts.
2.  **Validate filenames**: Always check that your filtered dataset
    contains the expected files before downloading.
3.  **Use descriptive filters**: Prefer filtering by country, year, and
    survey acronym rather than searching by module names.
4.  **Handle errors gracefully**: Wrap
    [`dlw_get_data()`](https://worldbank.github.io/dlw/reference/dlw_get_data.md)
    calls in [`tryCatch()`](https://rdrr.io/r/base/conditions.html) for
    production workflows.

## Troubleshooting

### No Datasets Returned

If your filter returns zero rows, verify:

- Country code is correct (use 3-letter ISO code)
- Survey year exists in the catalog
- Survey acronym is spelled correctly

### API Authentication Issues

If you get authentication errors:

1.  Verify your token with
    [`dlw_get_token()`](https://worldbank.github.io/dlw/reference/token.md)
2.  Re-set your token with
    [`dlw_set_token()`](https://worldbank.github.io/dlw/reference/token.md)
3.  Try `dlw_server_catalog(force = TRUE)` to refresh

## Summary

The
[`dlw_server_catalog()`](https://worldbank.github.io/dlw/reference/dlw_server_catalog.md)
function provides a powerful way to:

- Discover available datasets
- Filter by country, year, survey, and topic
- Understand dataset structure and versions
- Prepare for data downloads

### Related Functions

- [`dlw_server_catalog()`](https://worldbank.github.io/dlw/reference/dlw_server_catalog.md)
  — Retrieve full server catalog
- [`dlw_server_inventory()`](https://worldbank.github.io/dlw/reference/dlw_server_inventory.md)
  — Convenient filtering wrapper
- [`dlw_get_data()`](https://worldbank.github.io/dlw/reference/dlw_get_data.md)
  — Download data using catalog filenames
- [`dlw_set_token()`](https://worldbank.github.io/dlw/reference/token.md)
  — Set API authentication token
- [`dlw_get_token()`](https://worldbank.github.io/dlw/reference/token.md)
  — Retrieve current API token

For more information, use:

``` r
?dlw_server_catalog
?dlw_server_inventory
?dlw_get_data
```
