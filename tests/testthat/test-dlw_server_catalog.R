test_that("dlw_server_catalog returns a data.table/data.frame", {
  skip("API call - integration test only")
  # expect_s3_class(dlw_server_catalog("GMD"), "data.frame")
})

test_that("dlw_server_catalog errors for unknown server", {
  expect_error(dlw_server_catalog("x"))
})

raw_catalog <- function() {
  data.table::data.table(
    ServerAlias = "GMD",
    Country = "WLD",
    Year = 2026L,
    FilePath = c("WLD/x/Data/WLD_2026_ALL_v02_M_v01_A_GMI_INE.dta",
                 "WLD/x/Programs/WLD_2026_ALL_V02_M_V01_A_GMI_INE.do",
                 "USA/x/Doc/Technical/manual.PDF"),
    Ext = c("dta", "do", "PDF")
  )
}

catalog_with_formats <- function(formats) {
  withr::local_options(dlw.download_formats = formats)
  local_mocked_bindings(get_from_dlwenv = \(...) NULL,
                        set_in_dlwenv = \(...) NULL,
                        build_request = \(...) "req",
                        handle_resp = \(req) raw_catalog())
  dlw_server_catalog("GMD", verbose = FALSE)
}

test_that("dlw_server_catalog lists all file types by default", {
  ctl <- catalog_with_formats(NULL)
  expect_equal(ctl$Ext, c("dta", "do", "PDF"))
  expect_equal(ctl$FileName[3], "manual.PDF")
})

test_that("dlw.download_formats restricts the catalog", {
  expect_equal(catalog_with_formats("dta")$Ext, "dta")
  expect_equal(catalog_with_formats(c("DO", "pdf"))$Ext, c("do", "PDF"))
})

test_that("add_gmd_vars handles file names with a repeated extension", {
  ctl <- data.table::data.table(
    FileName = c("ZWE_2014_LFS_V01_M_V01_A_GLD_ALL.do.do",
                 "ZWE_2014_LFS_V01_M_V01_A_GLD_ALL.dta"))
  add_gmd_vars(ctl)
  expect_equal(ctl$Collection, c("GLD", "GLD"))
  expect_equal(ctl$Module, c("ALL", "ALL"))
  expect_equal(ctl$ext, c("DO", "DTA"))
})
