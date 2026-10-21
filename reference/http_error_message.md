# short message from the body of an error response

short message from the body of an error response

## Usage

``` r
http_error_message(resp, body, req)
```

## Arguments

- resp:

  httr2 response.

- body:

  character: body of \`resp\` as text.

- req:

  httr2 request.

## Value

character: \`detail\` or \`title\` of a JSON body, or the first line of
a text body. Empty string if there is none.
