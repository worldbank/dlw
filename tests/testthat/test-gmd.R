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
    Ext          = "dta",
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

test_that("dlw_get_gmd_support ignores files that are not dta", {
  ctl <- rbind(support_ctl(),
               data.table::copy(support_ctl()[2])[, `:=`(Vermast = "V13",
                                                         Ext = "do")])
  local_mocked_bindings(
    dlw_server_catalog = \(...) ctl,
    eval_gmd_call = \(calls, verbose) calls
  )
  out <- dlw_get_gmd_support(verbose = FALSE)
  expect_length(out, 1)
  expect_match(out[[1]]$filename, "_v12_.*dta$")
})

test_that("dlw_get_gmd ignores files that are not dta", {
  inv <- data.table::data.table(
    Country = "WLD", Year = 2026L, ServerAlias = "GMD", Survey = "ALL",
    Module = "INE", Collection = "GMI", Vermast = c("V01", "V02"),
    Veralt = "V01", Ext = c("dta", "do"),
    FileName = c("WLD_2026_ALL_V01_M_V01_A_GMI_INE.dta",
                 "WLD_2026_ALL_V02_M_V01_A_GMI_INE.do")
  )
  local_mocked_bindings(
    dlw_server_inventory = \(...) inv,
    eval_gmd_call = \(calls, verbose) calls
  )
  out <- dlw_get_gmd("WLD", 2026, "INE", verbose = FALSE)
  expect_length(out, 1)
  expect_match(out[[1]]$filename, "dta$")
})
