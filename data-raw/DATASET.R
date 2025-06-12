prod_url <- "https://datalibwebapiprod.ase.worldbank.org/dlw/api"
# Constants for keyring and service
.dlw_keyring <- "datalibweb"
.dlw_service <- "datalibweb"

usethis::use_data(
  prod_url,
  .dlw_keyring,
  .dlw_service,
  internal = TRUE,
  overwrite = TRUE
)

