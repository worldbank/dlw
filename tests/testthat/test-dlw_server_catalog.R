test_that("dlw_server_catalog returns a data.table/data.frame", {
  skip("API call - integration test only")
  # expect_s3_class(dlw_server_catalog("GMD"), "data.frame")
})

test_that("dlw_server_catalog errors for unknown server", {
  expect_error(dlw_server_catalog("x"))
})
