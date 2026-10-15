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

support_files <- function() {
  data.table::data.table(
    Country  = "SUPPORT",
    Year     = 2005L,
    Ext      = "dta",
    FileName = c(rep("Monthly_CPI.dta", 2), "Yearly_CPI_Final.dta")
  )
}

capture_data_call <- function(...) {
  rec <- new.env()
  local_mocked_bindings(
    dlw_server_catalog = \(...) support_files(),
    dlw_get_data = \(...) {
      rec$args <- list(...)
      "data"
    },
    .env = parent.frame()
  )
  rec$out <- dlw_get_gmd_support(..., verbose = FALSE)
  rec
}

test_that("dlw_get_gmd_support gets any Support file by name", {
  rec <- capture_data_call(filename = "monthly_cpi.DTA")
  expect_equal(rec$out, "data")
  expect_equal(rec$args$country_code, "Support")
  expect_equal(rec$args$filename, "Monthly_CPI.dta")
  expect_equal(rec$args$year, 2005L)
  expect_false("vermast" %in% names(rec$args))
})

test_that("dlw_get_gmd_support passes vermast for a Support file", {
  rec <- capture_data_call(filename = "Yearly_CPI_Final.dta", vermast = "v15")
  expect_equal(rec$args$filename, "Yearly_CPI_Final.dta")
  expect_equal(rec$args$vermast, "v15")
})

test_that("dlw_get_gmd_support errors for unknown or ambiguous Support files", {
  local_mocked_bindings(dlw_server_catalog = \(...) support_files())
  expect_error(dlw_get_gmd_support(filename = "nope.dta", verbose = FALSE),
               "not a Support file")
  local_mocked_bindings(dlw_server_catalog = \(...) {
    ctl <- support_files()
    ctl[2, Year := 2006L]
    ctl
  })
  expect_error(dlw_get_gmd_support(filename = "Monthly_CPI.dta",
                                   verbose = FALSE),
               "several years")
})
