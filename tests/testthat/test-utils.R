
# Token ----------

if (Sys.getenv("DLW_TOKEN") != "") {
  on.exit(expr = {
    Sys.getenv("DLW_TOKEN") |>
            dlw_set_token()
    })
}


test_that("dlw_set_token and dlw_get_token work", {
  skip_on_cran()
  expect_invisible(dlw_set_token("testtoken"))
  expect_equal(dlw_get_token(prompt_if_missing = FALSE), "testtoken")
})

test_that("dlw_get_token errors if missing and prompt_if_missing = FALSE", {
  skip_on_cran()
  dlw_remove_token()
  expect_error(dlw_get_token(prompt_if_missing = FALSE))
})

test_that("dlw_remove_token works", {
  skip_on_cran()
  dlw_set_token("testtoken")
  expect_invisible(dlw_remove_token())
})


# Select server
test_that("select_server returns GMD by default", {
  expect_equal(select_server(NULL), "GMD")
  expect_equal(select_server("GMD"), "GMD")
  expect_equal(select_server("X"), "X") # this will fail when the function is fixed
})



test_that("onLoad sets options", {
  op <- options()
  expect_true("dlw.verbose" %in% names(op))
  expect_true("dlw.default_api_version" %in% names(op))
  expect_true("dlw.output_method" %in% names(op))
})
