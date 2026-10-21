# Name of the pin of a file

The pin of a dta file is named after the file, with the pin format as
extension. Other tables also keep their extension in the name, so that
files like "a.csv" and "a.dta" do not collide. Files that are not tables
are pinned as is, with their own name. Unless the name of such a file is
that of a harmonized file (which holds country and year), the country
and year are part of the pin name, as the same name can be used in
several of them.

## Usage

``` r
get_pin_name(filename, version_arg, format, country_code = NULL, year = NULL)
```

## Arguments

- filename:

  character: name of the file

- version_arg:

  character: explicit file version (see \[get_version_arg\]) or NULL.

- format:

  character: File format to use for pinning data ('qs2' \[default\] or
  'parquet')

- country_code:

  character: ISO3 code

- year:

  numeric: year of the request

## Value

character
