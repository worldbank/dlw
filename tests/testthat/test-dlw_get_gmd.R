has_token <- tryCatch(
  {
    dlw::dlw_test_token
    TRUE
  },
  error = function(e) FALSE
)

skip_if_no_token <- function() {
  if (!has_token) skip("No DLW token available")
}

test_that("dlw_get_gmd downloads, saves, and loads data from temp folder", {
  skip_if_no_token()

  # Create and initialize temp folder
  tempfld <- fs::path_temp("prj-temp-fld")
  fs::dir_create(tempfld)
  stamp::st_init(tempfld, alias = "dlw")

  # Download and save data
  result <- dlw_get_gmd(
    country_code  = "COL",
    year          = 2010,
    survey        = "GEIH",
    module        = "GPWG",
    vermast       = "v02",
    veralt        = "v09",
    local_dir     = tempfld
  )

  expected_file <- fs::path(tempfld, "COL_2010_GEIH_V02_M_V09_A_GMD_GPWG.qs2")

  # File was saved
  expect_true(fs::file_exists(expected_file))

  # Returned object is a data.table
  expect_s3_class(result, "data.table")
  expect_gt(nrow(result), 0)

  # Load from stamp and compare
  testdf <- stamp::st_load(expected_file, alias = "dlw")

  expect_s3_class(testdf, "data.table")
  expect_equal(nrow(result), nrow(testdf))
  expect_equal(ncol(result), ncol(testdf))
  expect_equal(names(result), names(testdf))

  # Cleanup
  fs::dir_delete(tempfld)
})
