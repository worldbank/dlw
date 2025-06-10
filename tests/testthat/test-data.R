test_that("dlwenv is an environment", {
  expect_true(exists(".dlwenv", envir = asNamespace("dlw")))
  expect_true(is.environment(get(".dlwenv", envir = asNamespace("dlw"))))
})

test_that("prod_url is set and is a character", {
  expect_true(exists("prod_url", envir = asNamespace("dlw")))
  expect_type(get("prod_url", envir = asNamespace("dlw")), "character")
})

