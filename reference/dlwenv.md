# .dlwenv environment

.dlwenv environment

Get function: Returns the entire .dlwenv environment

Get for a specific key from .dlwenv

Setter function: Assign a value in .dlwenv

## Usage

``` r
get_dlwenv()

get_from_dlwenv(key, verbose = FALSE)

set_in_dlwenv(key, value, verbose = FALSE)
```

## Arguments

- key:

  A character string representing the key

- verbose:

  logical: whether to display info

- value:

  The value to store in .dlwenv

## Value

The .dlwenv environment

The value associated with the key in .dlwenv

The assigned value (invisibly)

## See also

Other dlwenv utilities:
[`dlw_list_env()`](https://worldbank.github.io/dlw/reference/dlw_list_env.md)

Other dlwenv utilities:
[`dlw_list_env()`](https://worldbank.github.io/dlw/reference/dlw_list_env.md)

Other dlwenv utilities:
[`dlw_list_env()`](https://worldbank.github.io/dlw/reference/dlw_list_env.md)

## Examples

``` r
env <- get_dlwenv()
set_in_dlwenv("example_key", 42)
get_from_dlwenv("example_key") # returns 42
#> [1] 42
set_in_dlwenv("example_key", 42)
```
