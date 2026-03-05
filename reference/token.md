# Datalibweb Token utilities

Datalibweb Token utilities

Store Datalibweb API token securely

Retrieve Datalibweb API token, prompt if missing/expired

Remove/reset the Datalibweb API token

confirm wither the token is working correctly

## Usage

``` r
dlw_set_token(token)

dlw_get_token(prompt_if_missing = TRUE)

dlw_remove_token()

dlw_test_token(
  dlw_url = NULL,
  api_version = getOption("dlw.default_api_version"),
  verbose = getOption("dlw.verbose")
)
```

## Arguments

- token:

  The API token as a string

- prompt_if_missing:

  logical: whether to be prompted to store token

- dlw_url:

  character: in case we have more than one url.

- api_version:

  character: API version

- verbose:

  logical: whether to display info

## Value

invisible token

TRUE if all goes well

## Examples

``` r
if (FALSE) { # \dontrun{
dlw_test_token()
} # }
```
