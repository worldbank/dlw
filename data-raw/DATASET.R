prod_url <- "https://datalibwebapiprod.ase.worldbank.org/dlw/api"
# Constants for keyring and service
.dlw_keyring <- "datalibweb"
.dlw_service <- "datalibweb"

dlw_user_agent <- "dlw (https://github.com/worldbank/dlw)"

usethis::use_data(
  prod_url,
  .dlw_keyring,
  .dlw_service,
  dlw_user_agent,
  internal = TRUE,
  overwrite = TRUE
)

