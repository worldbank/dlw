file_resp <- function(body, type, disposition = NULL, status = 200) {
  headers <- list(`Content-Type` = type)
  if (!is.null(disposition)) {
    headers[["Content-Disposition"]] <- disposition
  }
  httr2::response(status_code = status, headers = headers, body = body)
}

perform_file <- function(resp) {
  local_mocked_bindings(handle_req_perform = \(req) resp,
                        .env = parent.frame())
  handle_file_resp("req")
}

test_that("resp_filename reads the Content-Disposition header", {
  fn <- \(d) resp_filename(file_resp(raw(1), "do", d))
  expect_equal(fn("attachment; filename=a.do"), "a.do")
  expect_equal(fn('attachment; filename="my file.pdf"; size=3'), "my file.pdf")
  expect_equal(fn("attachment; filename*=UTF-8''my%20file.pdf"), "my file.pdf")
  expect_null(fn("attachment"))
  expect_null(resp_filename(file_resp(raw(1), "do")))
})

test_that("resp_filename keeps only the last path component", {
  fn <- \(d) resp_filename(file_resp(raw(1), "do", d))
  expect_equal(fn("attachment; filename=../../a.do"), "a.do")
  expect_equal(fn("attachment; filename=..\\..\\a.do"), "a.do")
  expect_equal(fn("attachment; filename*=UTF-8''%2e%2e%2fa.do"), "a.do")
  expect_null(fn("attachment; filename=.."))
  expect_null(fn("attachment; filename*=UTF-8''%2e%2e"))
})

test_that("resp_filename decodes only filename*", {
  fn <- \(d) resp_filename(file_resp(raw(1), "do", d))
  # plain names are literal, whatever they contain
  expect_equal(fn("attachment; filename=100%25 sure.do"), "100%25 sure.do")
  expect_equal(fn("attachment; filename=a%00b.do"), "a%00b.do")
  expect_equal(fn("attachment; filename=l'enquete (2).pdf"),
               "l'enquete (2).pdf")
  expect_equal(fn('attachment; filename="a;b.do"'), "a;b.do")
  expect_equal(fn("attachment; filename*=UTF-8''User%E2%80%99s%20Guide.pdf"),
               "User’s Guide.pdf")
  expect_null(fn("attachment; filename*=UTF-8''a%00b.do"))
  # filename* wins over filename
  expect_equal(fn("attachment; filename=x.do; filename*=UTF-8''y%20z.do"),
               "y z.do")
})

test_that("handle_file_resp returns any file as raw bytes with metadata", {
  bytes <- as.raw(c(0x00, 0xff, 0x0d, 0x0a, 0x25, 0x50))
  out <- perform_file(file_resp(bytes, "pdf",
                                "attachment; filename=manual.pdf"))
  expect_identical(out$body, bytes)
  expect_equal(out$filename, "manual.pdf")
  expect_equal(out$content_type, "pdf")
  expect_equal(out$status, 200L)

  txt <- charToRaw("* program\r\ndisplay 1\r\n")
  out <- perform_file(file_resp(txt, "do", "attachment; filename=a.do"))
  expect_identical(out$body, txt)
})

test_that("handle_file_resp keeps a csv file but parses API csv reports", {
  body <- charToRaw("a,b\r\n1,2\r\n")
  out <- perform_file(file_resp(body, "csv", "attachment; filename=data.csv"))
  expect_identical(out$body, body)
  expect_equal(out$filename, "data.csv")

  info <- charToRaw("FileName,ErrorCode\r\na.dta,2\r\nb.dta,2\r\n")
  out <- perform_file(file_resp(info, "csv",
                                "attachment; filename=ECAFileinfo.csv"))
  expect_s3_class(out, "data.table")
  expect_equal(nrow(out), 2)

  out <- perform_file(file_resp(info, "text/csv"))
  expect_s3_class(out, "data.table")

  out <- perform_file(file_resp(body, "text/csv"))
  expect_identical(out$body, body)
  expect_null(out$filename)

  err <- charToRaw("ErrorCode,ErrorMessage\r\n674,No access\r\n")
  expect_error(perform_file(file_resp(err, "text/csv")),
               class = "dlw_api_error")
})

test_that("unnamed CSV files are saved and pinned as files, not reports", {
  body <- charToRaw("a,b\r\n1,2\r\n")
  local_mocked_bindings(
    build_request = \(...) "req",
    handle_req_perform = \(req) file_resp(body, "text/csv")
  )
  dir <- withr::local_tempdir()
  path <- file.path(dir, "a.csv")
  out <- dlw_download_file("COL", "a.csv", year = 2010, path = path)
  expect_equal(out$filename, "a.csv")
  expect_identical(readBin(path, "raw", 100), body)

  out <- dlw_get_data("COL", "a.csv", year = 2010, local = TRUE,
                      local_dir = dir)
  expect_equal(out, data.table::data.table(a = 1L, b = 2L))
  expect_equal(pins::pin_list(pins::board_folder(dir)), "a_COL_2010_csv.qs2")
})

test_that("handle_file_resp aborts on API errors and empty bodies", {
  err <- charToRaw("ErrorCode,ErrorMessage\r\n674,No access\r\n")
  expect_error(perform_file(file_resp(err, "csv",
                                      "attachment; filename=ECAFileinfo.csv")),
               class = "dlw_api_error")
  expect_error(perform_file(file_resp(raw(0), "dta",
                                      "attachment; filename=a.dta")),
               "empty", class = "dlw_api_error")
  expect_error(perform_file(file_resp(raw(0), "csv",
                                      "attachment; filename=ECAFileinfo.csv")),
               "empty", class = "dlw_api_error")
})

mock_file <- function(body = charToRaw("display 1\r\n"), filename = "a.do",
                      type = "do", env = parent.frame()) {
  rec <- new.env()
  rec$sent <- list()
  local_mocked_bindings(
    build_request = \(...) {
      rec$sent[[length(rec$sent) + 1]] <- list(...)
      "req"
    },
    get_raw_data = \(req) {
      force(req)
      file_response(body, filename, type)
    },
    .env = env
  )
  rec
}

test_that("dlw_download_file returns raw bytes with attributes", {
  body <- as.raw(c(0, 1, 2, 255))
  rec <- mock_file(body, "b.zip", "zip")
  out <- dlw_download_file("COL", "b.zip", year = 2010)
  expect_identical(as.vector(out), body)
  expect_equal(attr(out, "filename"), "b.zip")
  expect_equal(attr(out, "content_type"), "zip")
  expect_equal(attr(out, "status"), 200L)
  expect_equal(rec$sent[[1]]$Server, "GMD")
})

test_that("dlw_download_file saves the bytes unchanged and returns metadata", {
  body <- as.raw(c(0, 255, 13, 10, 26, 0, 7))
  mock_file(body, "a.bin", "bin")
  dir <- withr::local_tempdir()
  path <- file.path(dir, "sub", "x.bin")
  out <- dlw_download_file("COL", "a.bin", year = 2010, path = path)
  expect_identical(readBin(path, "raw", 100), body)
  expect_equal(out$path, as.character(fs::path_abs(path)))
  expect_equal(out$filename, "a.bin")
  expect_equal(out$size, 7)
  expect_equal(out$content_type, "bin")
  expect_equal(out$status, 200L)
  # nothing left behind besides the file
  expect_equal(list.files(dirname(path), all.files = TRUE, no.. = TRUE),
               "x.bin")
})

test_that("dlw_download_file saves a text file and a dta file", {
  dir <- withr::local_tempdir()
  txt <- charToRaw("display \"hi\"\r\n")
  mock_file(txt, "a.do", "do")
  dlw_download_file("COL", "a.do", year = 2010, path = file.path(dir, "a.do"))
  expect_identical(readBin(file.path(dir, "a.do"), "raw", 100), txt)
  expect_equal(readLines(file.path(dir, "a.do")), "display \"hi\"")

  dta <- dta_bytes()
  mock_file(dta$body, "a.dta", "dta")
  dlw_download_file("COL", "a.dta", year = 2010, path = file.path(dir, "a.dta"))
  expect_identical(readBin(file.path(dir, "a.dta"), "raw", 1e5), dta$body)
  expect_equal(as.vector(haven::read_dta(file.path(dir, "a.dta"))$x), 1:3)
})

test_that("dlw_download_file uses the API name when path is a directory", {
  mock_file(filename = "from_api.do")
  dir <- withr::local_tempdir()
  out <- dlw_download_file("COL", "a.do", year = 2010, path = dir)
  expect_equal(basename(out$path), "from_api.do")
  expect_true(file.exists(file.path(dir, "from_api.do")))

  out <- dlw_download_file("COL", "a.do", year = 2010,
                           path = paste0(file.path(dir, "new"), "/"))
  expect_true(file.exists(file.path(dir, "new", "from_api.do")))
})

test_that("dlw_download_file falls back to the requested name", {
  mock_file(filename = NULL)
  dir <- withr::local_tempdir()
  out <- dlw_download_file("COL", "requested.do", year = 2010, path = dir)
  expect_equal(basename(out$path), "requested.do")
})

test_that("dlw_download_file protects existing files", {
  rec <- mock_file(charToRaw("new"))
  dir <- withr::local_tempdir()
  path <- file.path(dir, "a.do")
  writeLines("old", path)

  expect_error(dlw_download_file("COL", "a.do", year = 2010, path = path),
               "already exists")
  expect_length(rec$sent, 0) # refused before the request
  expect_equal(readLines(path), "old")

  # also when the name comes from the API
  expect_error(dlw_download_file("COL", "a.do", year = 2010, path = dir),
               "already exists")
  expect_equal(readLines(path), "old")

  dlw_download_file("COL", "a.do", year = 2010, path = path, overwrite = TRUE)
  expect_identical(readBin(path, "raw", 10), charToRaw("new"))
})

test_that("dlw_download_file leaves no partial file when the write fails", {
  mock_file()
  dir <- withr::local_tempdir()
  path <- file.path(dir, "a.do")
  local_mocked_bindings(file_move = \(...) stop("interrupted"),
                        .package = "fs")
  expect_error(dlw_download_file("COL", "a.do", year = 2010, path = path),
               "interrupted")
  expect_length(list.files(dir, all.files = TRUE, no.. = TRUE), 0)
})

test_that("dlw_download_file requires year and returns version lists", {
  mock_file()
  expect_error(dlw_download_file("COL", "a.do"), "year")
  expect_error(dlw_download_file("COL"), "filename")

  versions <- data.table::data.table(FileName = c("a.dta", "a.dta"),
                                     ErrorCode = 2L)
  local_mocked_bindings(build_request = \(...) "req",
                        get_raw_data = \(req) versions)
  dir <- withr::local_tempdir()
  out <- suppressMessages(
    dlw_download_file("COL", "a.dta", year = 2010, path = dir)
  )
  expect_equal(out, versions)
  expect_length(list.files(dir), 0)
})

test_that("dlw_download_file selects versions like dlw_get_data", {
  rec <- mock_file()
  dlw_download_file("Support", "a.dta", year = 2005, vermast = "v12")
  expect_equal(rec$sent[[1]]$para1, "V12_M")
})
