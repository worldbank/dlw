test_that("dlw_server_catalog returns a data.table/data.frame", {
  skip("API call - integration test only")
  # expect_s3_class(dlw_server_catalog("GMD"), "data.frame")
})

test_that("dlw_server_catalog errors for unknown server", {
  expect_error(dlw_server_catalog("x"))
})

test_that("dlw_server_catalog returns a data.table with expected columns", {
  skip_on_cran()
  skip_if_not_installed("dlw")
  skip_if_not_installed("data.table")

  result <- dlw_server_catalog()
  expect_s3_class(result, "data.table")
  expect_true(all(
    c("FileName", "Country", "Survey", "Year", "Survey_year") %in% names(result)
  ))
  expect_gt(nrow(result), 0)
})

test_that("dlw_server_catalog can filter by server name", {
  skip_on_cran()
  skip_if_not_installed("dlw")
  skip_if_not_installed("data.table")

  result <- dlw_server_catalog(server = "GMD")
  expect_s3_class(result, "data.table")
})