# perform httr2::req_perform and handle errors properly

perform httr2::req_perform and handle errors properly

perform httr2::req_perform and handle errors properly

## Usage

``` r
handle_resp(req)

handle_req_perform(req)
```

## Arguments

- req:

  A httr2 request object.

## Value

a data.table if the response is CSV. A raw vector if the response is
dta. Error otherwise

an HTTP response.
