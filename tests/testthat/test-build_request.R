test_that("build_request returns an httr2 request", {
  skip_on_cran()
  req <- build_request(endpoint = "test", dlw_url = "prod", api_version = "v1")
  expect_s3_class(req, "httr2_request")
})

test_that("build_request errors with missing or wrong endpoint", {
  expect_error(build_request(dlw_url = "prod"))
  build_request(endpoint = "test",
                dlw_url = "https://example.com",
                api_version = "v1") |>
    expect_error()
})

