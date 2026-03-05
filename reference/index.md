# Package index

## Configuration and Utility

Functions for setting up authentication, managing environment variables,
and providing general utilities required for interacting with the
Datalibweb API.

- [`dlw_set_token()`](https://worldbank.github.io/dlw/reference/token.md)
  [`dlw_get_token()`](https://worldbank.github.io/dlw/reference/token.md)
  [`dlw_remove_token()`](https://worldbank.github.io/dlw/reference/token.md)
  [`dlw_test_token()`](https://worldbank.github.io/dlw/reference/token.md)
  : Datalibweb Token utilities
- [`dlw_list_env()`](https://worldbank.github.io/dlw/reference/dlw_list_env.md)
  : List objects in an environment with their classes
- [`get_dlwenv()`](https://worldbank.github.io/dlw/reference/dlwenv.md)
  [`get_from_dlwenv()`](https://worldbank.github.io/dlw/reference/dlwenv.md)
  [`set_in_dlwenv()`](https://worldbank.github.io/dlw/reference/dlwenv.md)
  : .dlwenv environment
- [`repo_create()`](https://worldbank.github.io/dlw/reference/repo.md)
  [`repo_load()`](https://worldbank.github.io/dlw/reference/repo.md) :
  dlw repository

## Preparing Input Arguments for Downloading DLW Data

Functions for constructing API requests and retrieving metadata
catalogs. These help users filter, select, and specify the datasets they
want to download.

- [`build_request()`](https://worldbank.github.io/dlw/reference/build_request.md)
  : Build request version 2
- [`dlw_country_catalog()`](https://worldbank.github.io/dlw/reference/dlw_country_catalog.md)
  : Get country catalog
- [`dlw_server_catalog()`](https://worldbank.github.io/dlw/reference/dlw_server_catalog.md)
  : Server catalog
- [`dlw_server_inventory()`](https://worldbank.github.io/dlw/reference/dlw_server_inventory.md)
  : filter server catolog to show inventory by country and other
  variables

## Downloading Data

Core functions for downloading, caching, and reading datasets from
Datalibweb. These handle the full data retrieval workflow, including
checking for local copies, downloading from the API, and reading data
into R.

- [`dlw_get_data()`](https://worldbank.github.io/dlw/reference/dlw_get_data.md)
  : Get data from datalibweb (refactored)
- [`dlw_get_gmd()`](https://worldbank.github.io/dlw/reference/dlw_get_gmd.md)
  : Get GMD data.
- [`dlw_get_gmd_support()`](https://worldbank.github.io/dlw/reference/dlw_get_gmd_support.md)
  : get Support files for GMD

## Miscellaneous

Miscellaneous functions, including print methods and other helpers.

- [`print(`*`<dlw_call_list>`*`)`](https://worldbank.github.io/dlw/reference/print.dlw_call_list.md)
  : print gmd calls
