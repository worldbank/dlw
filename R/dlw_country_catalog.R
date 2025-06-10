dlw_country_catalog <- function(country_code) {
  endpoint <- "CountryCatalog"

  req <- build_request(dlw_url = dlw_url,
                       api_version = api_version,
                       endpoint = endpoint,
                       country = country_code)
  req <- request(url) |>
    req_url_path_append(endpoint) |>
    req_url_path_append(country_code) |>
    req_auth_bearer_token(key_get("datalibweb"))

  req |> req_perform() |> resp_body_string() |> read_csv(show_col_types = FALSE)
}
