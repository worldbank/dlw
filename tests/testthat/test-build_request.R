test_that("build_request returns an httr2 request", {
  skip_on_cran()
  req <- build_request(endpoint = "test", dlw_url = "prod", api_version = "v1")
  expect_s3_class(req, "httr2_request")
})

csv_resp <- function(body) {
  httr2::response(status_code = 200,
                  headers = list(`Content-Type` = "text/csv"),
                  body = charToRaw(body))
}

test_that("handle_resp aborts on single-row CSV with ErrorCode", {
  local_mocked_bindings(
    handle_req_perform = \(req) csv_resp("ErrorCode,ErrorMessage\r\n676,Not found\r\n")
  )
  expect_error(handle_resp("req"), class = "dlw_api_error")
})

test_that("handle_resp returns multi-row CSV with ErrorCode as data", {
  local_mocked_bindings(
    handle_req_perform = \(req) csv_resp("FilePath,ErrorCode\r\na.dta,2\r\nb.dta,2\r\n")
  )
  out <- handle_resp("req")
  expect_s3_class(out, "data.table")
  expect_equal(nrow(out), 2)
})

test_that("handle_resp parses CSV with bare carriage-return line endings", {
  local_mocked_bindings(
    handle_req_perform = \(req) csv_resp("FileName,ErrorCode,ErrorDetail\ra.dta,2,Publicly available for download\rb.dta,2,Publicly available for download")
  )
  out <- handle_resp("req")
  expect_equal(dim(out), c(2, 3))
  expect_equal(out$FileName, c("a.dta", "b.dta"))
})

test_that("dlw_download returns version list for multi-row response", {
  versions <- data.table::data.table(FileName = c("a.dta", "a.dta"),
                                     ErrorCode = 2L)
  local_mocked_bindings(build_request = \(...) "req",
                        get_raw_data = \(req) versions)
  expect_message(
    out <- dlw_download("SUPPORT", "a.dta", board = NULL, pin_name = "a",
                        format = "qs2", year = 2005),
    "2 versions"
  )
  expect_equal(out, versions)
})

version_list <- function() {
  data.table::data.table(
    FileName = "a.dta",
    FileSharePath = paste0("GMD/Support/S_2005_CPI/S_2005_CPI_",
                           c("V09", "V15", "V10"), "_M/Data/Stata/a.dta"),
    ErrorCode = 2L
  )
}

test_that("get_version_arg builds para1 from vermast and veralt", {
  expect_null(get_version_arg(list()))
  expect_equal(get_version_arg(list(vermast = "v12")), "V12_M")
  expect_equal(get_version_arg(list(veralt = "v01")), "V01_A")
  expect_equal(get_version_arg(list(vermast = "v12", veralt = "v01")),
               "V12_M_V01_A")
  expect_equal(get_version_arg(list(para1 = "V03_M", vermast = "v12")),
               "V03_M")
})

test_that("latest_version picks highest master version", {
  expect_equal(latest_version(version_list()), "V15_M")
  expect_null(latest_version(data.table::data.table(FileName = "a.dta")))
  expect_null(latest_version(data.table::data.table(FileSharePath = "x/a.dta")))
})

test_that("dlw_download repeats request with latest version", {
  rec <- new.env()
  rec$sent <- list()
  responses <- list(version_list(), dta_bytes())
  local_mocked_bindings(
    build_request = \(...) {
      rec$sent[[length(rec$sent) + 1]] <- list(...)
      "req"
    },
    get_raw_data = \(req) {
      force(req)
      responses[[length(rec$sent)]]
    }
  )
  expect_message(
    out <- dlw_download("SUPPORT", "a.dta", board = pins::board_temp(),
                        pin_name = "a.qs2", format = "qs2", year = 2005),
    "V15_M"
  )
  expect_length(rec$sent, 2)
  expect_null(rec$sent[[1]]$para1)
  expect_equal(rec$sent[[2]]$para1, "V15_M")
  expect_equal(dim(out), c(3, 1))
})

test_that("dlw_download maps vermast to para1 without listing versions", {
  rec <- new.env()
  rec$sent <- list()
  local_mocked_bindings(
    build_request = \(...) {
      rec$sent[[length(rec$sent) + 1]] <- list(...)
      "req"
    },
    get_raw_data = \(req) {
      force(req)
      dta_bytes()
    }
  )
  dlw_download("SUPPORT", "a.dta", board = pins::board_temp(),
               pin_name = "a.qs2", format = "qs2", year = 2005,
               vermast = "v12")
  expect_length(rec$sent, 1)
  expect_equal(rec$sent[[1]]$para1, "V12_M")
})

test_that("filename_collection reads the collection of harmonized files", {
  expect_equal(filename_collection("COL_2010_GEIH_V04_M_V01_A_GMD_ALL.dta"),
               "GMD")
  expect_equal(filename_collection("WLD_2026_ALL_v02_M_v01_A_gmi_INE.dta"),
               "GMI")
  expect_null(filename_collection("Monthly_CPI.dta"))
  expect_null(filename_collection("CPS_2020_ind.dta"))
})

test_that("dlw_download sends server and collection by default", {
  rec <- new.env()
  local_mocked_bindings(
    build_request = \(...) {
      rec$sent <- list(...)
      "req"
    },
    get_raw_data = \(req) {
      force(req)
      dta_bytes()
    }
  )
  dlw_download("LAO", "LAO_2012_LECS_V02_M_V03_A_GMD_ALL.dta",
               board = pins::board_temp(), pin_name = "a.qs2", format = "qs2",
               year = 2012)
  expect_equal(rec$sent$Server, "GMD")
  expect_equal(rec$sent$collection, "GMD")

  dlw_download("LAO", "LAO_2012_LECS_V02_M_V03_A_GMD_ALL.dta",
               board = pins::board_temp(), pin_name = "a.qs2", format = "qs2",
               server = "FDP", year = 2012, collection = "GMDRAW")
  expect_equal(rec$sent$Server, "FDP")
  expect_equal(rec$sent$collection, "GMDRAW")

  dlw_download("Support", "Monthly_CPI.dta", board = pins::board_temp(),
               pin_name = "a.qs2", format = "qs2", year = 2005)
  expect_false("collection" %in% names(rec$sent))
})

test_that("dlw_get_data requires year, even when the file is cached", {
  dir <- withr::local_tempdir()
  pins::pin_write(pins::board_folder(dir), data.frame(x = 1), "a.qs2",
                  type = "qs2")
  expect_error(dlw_get_data("COL", "a.dta", local = TRUE, local_dir = dir),
               "year")
  expect_equal(dlw_get_data("COL", "a.dta", year = 2010, local = TRUE,
                            local_dir = dir)$x, 1)
})

test_that("dlw_download errors when no file matches", {
  local_mocked_bindings(build_request = \(...) "req",
                        get_raw_data = \(req) data.table::data.table(FileName = character()))
  expect_error(dlw_download("SUPPORT", "a.dta", board = NULL, pin_name = "a",
                            format = "qs2", year = 2005),
               "No file")
})

http_error <- function(status, body = "", type = "text/plain", headers = list(),
                        token = "secret-token") {
  local_mocked_bindings(dlw_get_token = \(...) token,
                        .env = parent.frame())
  httr2::local_mocked_responses(
    \(req) httr2::response(status_code = status,
                           headers = c(list(`Content-Type` = type), headers),
                           body = charToRaw(body)),
    env = parent.frame()
  )
  req <- build_request(endpoint = "FileInformationInternal/GetFileInfo",
                       method = "POST", Country = "COL", filename = "a.dta")
  rlang::catch_cnd(handle_req_perform(req), classes = "error")
}

test_that("HTTP errors keep status, endpoint, parameters and body", {
  cnd <- http_error(404, '{"title":"Not Found","status":404}',
                    type = "application/problem+json")
  expect_s3_class(cnd, "dlw_http_404")
  expect_s3_class(cnd, "dlw_http_error")
  expect_s3_class(cnd, "dlw_api_error")
  expect_equal(cnd$status, 404L)
  expect_equal(cnd$reason, "Not Found")
  expect_equal(cnd$endpoint, "FileInformationInternal/GetFileInfo")
  expect_equal(unclass(cnd$params$filename), "a.dta")
  expect_match(cnd$response_body, "Not Found")
  expect_match(conditionMessage(cnd), "API message: Not Found")
})

test_that("401, 403, 429 and 5xx give different errors", {
  e401 <- http_error(401, "Status Code: 401; Unauthorized")
  e403 <- http_error(403, "Forbidden")
  e429 <- http_error(429, "slow down", headers = list(`Retry-After` = "30"))
  e500 <- http_error(500, "System.NullReferenceException: boom\n   at x")

  expect_s3_class(e401, "dlw_http_401")
  expect_match(conditionMessage(e401), "Token invalid")
  expect_s3_class(e403, "dlw_http_403")
  expect_match(conditionMessage(e403), "does not have access")
  expect_s3_class(e429, "dlw_http_429")
  expect_equal(e429$retry_after, "30")
  expect_match(conditionMessage(e429), "Retry after 30 seconds")
  expect_s3_class(e500, "dlw_http_500")
  expect_match(conditionMessage(e500), "Server-side failure")
  expect_match(conditionMessage(e500), "NullReferenceException: boom")
  expect_no_match(conditionMessage(e500), "at x")
})

test_that("HTTP errors never contain the token", {
  cnd <- http_error(500, "Authorization: Bearer secret-token\nsecret-token")
  # the backtrace is left out: it records this test's own call
  fields <- cnd[setdiff(names(cnd), c("trace", "call"))]
  txt <- rawToChar(serialize(fields, NULL, ascii = TRUE))
  expect_false(grepl("secret-token", txt, fixed = TRUE))
  expect_match(cnd$response_body, "Authorization: Bearer [REDACTED]",
               fixed = TRUE)
  expect_null(cnd$request$headers$Authorization)
})

test_that("JSON error messages never contain the token", {
  cnd <- http_error(500,
                    '{"detail":"Header Authorization=Bearer secret-token rejected"}',
                    type = "application/problem+json")
  fields <- cnd[setdiff(names(cnd), c("trace", "call"))]
  txt <- rawToChar(serialize(fields, NULL, ascii = TRUE))
  expect_false(grepl("secret-token", txt, fixed = TRUE))
  expect_match(conditionMessage(cnd), "rejected")
})

test_that("JSON error messages redact tokens before truncating", {
  token <- paste0("secret-token-", strrep("x", 200))
  body <- paste0('{"detail":"', strrep("a", 180), token, ' rejected"}')
  cnd <- http_error(500, body, type = "application/problem+json", token = token)
  expect_s3_class(cnd, "dlw_http_500")
  expect_no_match(conditionMessage(cnd), "secret-token", fixed = TRUE)
  expect_match(conditionMessage(cnd), "[REDACTED]", fixed = TRUE)
  expect_no_match(cnd$response_body, token, fixed = TRUE)
})

test_that("transport failures keep the underlying error", {
  local_mocked_bindings(dlw_get_token = \(...) "secret-token")
  httr2::local_mocked_responses(\(req) {
    cnd <- structure(class = c("curl_error", "error", "condition"),
                     list(message = "Could not resolve host", call = NULL))
    rlang::abort("Failed to perform HTTP request.", class = "httr2_failure",
                 parent = cnd)
  })
  req <- build_request(endpoint = "x")
  cnd <- rlang::catch_cnd(handle_req_perform(req), classes = "error")
  expect_s3_class(cnd, "dlw_transport_error")
  expect_s3_class(cnd$parent, "curl_error")
})

test_that("build_request errors with missing or wrong endpoint", {
  expect_error(build_request(dlw_url = "prod"))
  build_request(endpoint = "test",
                dlw_url = "https://example.com",
                api_version = "v1") |>
    expect_error()
})
