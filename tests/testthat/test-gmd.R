support_ctl <- function() {
  data.table::data.table(
    Country      = "SUPPORT",
    Country_code = "SUPPORT",
    Year         = 2005L,
    ServerAlias  = "GMD",
    Survey       = "CPI",
    Module       = "CPIICP",
    Collection   = "GMD",
    Vermast      = c("V11", "V12"),
    FileName     = paste0("Support_2005_CPI_", c("v11", "v12"),
                          "_M_v01_A_GMD_CPIICP.dta")
  )
}

test_that("dlw_get_gmd_support matches SUPPORT and picks latest version", {
  local_mocked_bindings(
    dlw_server_catalog = \(...) support_ctl(),
    eval_gmd_call = \(calls, verbose) calls
  )
  out <- dlw_get_gmd_support(verbose = FALSE)
  expect_length(out, 1)
  expect_match(out[[1]]$filename, "_v12_")
})

test_that("dlw_get_gmd_support honors vermast", {
  local_mocked_bindings(
    dlw_server_catalog = \(...) support_ctl(),
    eval_gmd_call = \(calls, verbose) calls
  )
  out <- dlw_get_gmd_support(vermast = "v11", verbose = FALSE)
  expect_match(out[[1]]$filename, "_v11_")
  expect_error(dlw_get_gmd_support(vermast = "v99", verbose = FALSE))
})
