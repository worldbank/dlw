# abort with the details of a failed HTTP request

The condition keeps the status, reason phrase, endpoint, form
parameters, response body, \`Retry-After\` header and the request
(without its Authorization header).

## Usage

``` r
abort_http_error(e, req)
```

## Arguments

- e:

  httr2 HTTP error (class \`httr2_http\`) from \[httr2::req_perform\].

- req:

  The httr2 request that failed.

## Value

error of class \`dlw_http\_\<status\>\`, \`dlw_http_error\`,
\`dlw_api_error\` and \`api_error\`.
