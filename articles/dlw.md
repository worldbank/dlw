# dlw

``` r

library(dlw)
```

[dlw](https://github.com/worldbank/dlw) is an R client of the internal
[datalibweb](https://datalibweb2.worldbank.org/) API of the World Bank.
Datalibweb is a data system designed to enable users to seamlessly
access microdata and documentation in the World Bank. Users can access
the most up-to-date and historical versions of harmonized data
collections and raw/non-harmonized data for subsequent analysis

## Token

To begin working with [dlw](https://github.com/worldbank/dlw), you must
provide your datalibweb API token using the
[`dlw_set_token()`](https://worldbank.github.io/dlw/reference/token.md)
function.

You can obtain or renew your token by visiting the [datalibweb
page](https://datalibweb2.worldbank.org/) and following the instructions
there. Once you have your token, set it in your R session as follows:

``` r

dlw_set_token("your_token_here")
```

\[TO COMPLETE\]
