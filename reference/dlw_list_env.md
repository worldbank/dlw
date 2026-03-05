# List objects in an environment with their classes

Lists the names and classes of all objects in a specified environment,
using the rlang package for robust environment handling.

## Usage

``` r
dlw_list_env(env = .dlwenv, invisible = TRUE)
```

## Arguments

- env:

  An environment object. Defaults to \`.dlwenv\`.

- invisible:

  Logical; if TRUE (default), returns the info invisibly. If FALSE,
  prints a formatted summary to the console.

## Value

A data frame with columns \`name\` (object name) and \`class\` (object
class), invisibly by default.

## See also

Other dlwenv utilities:
[`dlwenv`](https://worldbank.github.io/dlw/reference/dlwenv.md)

## Examples

``` r
dlw_list_env() # List objects in .dlwenv
#> ℹ The environment is empty.
dlw_list_env(globalenv(), invisible = FALSE) # List objects in the global environment and print
#> 
#> ── Objects in environment: ─────────────────────────────────────────────────────
#> .Random.seed--> class: integer
tmpenv <- new.env(); assign("x", 1, envir = tmpenv); dlw_list_env(tmpenv, invisible = FALSE)
#> 
#> ── Objects in environment: ─────────────────────────────────────────────────────
#> x--> class: numeric
```
