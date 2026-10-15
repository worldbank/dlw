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
                        format = "qs2"),
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

dta_bytes <- function() {
  tmp <- withr::local_tempfile(fileext = ".dta")
  haven::write_dta(data.frame(x = 1:3), tmp)
  readBin(tmp, "raw", file.size(tmp))
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
                        pin_name = "a.qs2", format = "qs2"),
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
               pin_name = "a.qs2", format = "qs2", vermast = "v12")
  expect_length(rec$sent, 1)
  expect_equal(rec$sent[[1]]$para1, "V12_M")
})

test_that("dlw_download errors when no file matches", {
  local_mocked_bindings(build_request = \(...) "req",
                        get_raw_data = \(req) data.table::data.table(FileName = character()))
  expect_error(dlw_download("SUPPORT", "a.dta", board = NULL, pin_name = "a",
                            format = "qs2"),
               "No file")
})

test_that("build_request errors with missing or wrong endpoint", {
  expect_error(build_request(dlw_url = "prod"))
  build_request(endpoint = "test",
                dlw_url = "https://example.com",
                api_version = "v1") |>
    expect_error()
})

