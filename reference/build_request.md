# Build request version 2

Build request version 2

## Usage

``` r
build_request(
  dlw_url = NULL,
  api_version = getOption("dlw.default_api_version"),
  endpoint,
  method = "GET",
  store_request = TRUE,
  ...
)
```

## Arguments

- dlw_url:

  character: in case we have more than one url.

- api_version:

  character: API version

- endpoint:

  character: dlw API endpoint

- method:

  character: method of http request. Either "GET" or "POST". Default is
  "GET".

- store_request:

  logical: store request in .dlwenv as \`last_req\`

- ...:

  other parameters

## Value

httr2 request
