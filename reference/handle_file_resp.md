# perform request and keep the file as returned by the API

Unlike \[handle_resp\], the body of any file type is returned as raw
bytes, with no parsing. The CSV that the API uses to report errors and
lists of file versions (\`ECAFileinfo.csv\`, or an unnamed CSV with
\`ErrorCode\`) is parsed as in \[handle_resp\].

## Usage

``` r
handle_file_resp(req)
```

## Arguments

- req:

  A httr2 request object.

## Value

a data.table if the response is a CSV report of the API. Otherwise a
list with the raw \`body\`, \`filename\` (from the Content-Disposition
header, NULL if missing), \`content_type\` and HTTP \`status\`.
