test_that("dlw_country_catalog returns a data.table with expected columns", {
  skip_on_cran()
  skip_if_not_installed("dlw")
  skip_if_not_installed("data.table")

  result <- dlw_country_catalog(country_code = "COL")
  expect_s3_class(result, "data.table")
  expect_true(all(
    c("ServerAlias", "Country", "Survey", "Year", "Ext", "Checksum") %in%
      names(result)
  ))
  expect_gt(nrow(result), 0)
})

test_that("dlw_country_catalog can filter by country_code", {
  skip_on_cran()
  skip_if_not_installed("dlw")
  skip_if_not_installed("data.table")

  result <- dlw_country_catalog(country_code = "COL")
  expect_true(all(result$Country == "COL"))
})