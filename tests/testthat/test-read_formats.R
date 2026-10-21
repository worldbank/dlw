test_that("get_pin_name keeps dta names and tells other formats apart", {
  expect_equal(as.character(get_pin_name("a.dta", NULL, "qs2")), "a.qs2")
  expect_equal(as.character(get_pin_name("a.DTA", "V12_M", "parquet")),
               "a_V12_M.parquet")
  # other files also hold country and year, as names repeat across them
  pin <- \(...) as.character(get_pin_name(...))
  expect_equal(pin("a.csv", NULL, "qs2", "col", 2010), "a_COL_2010_csv.qs2")
  expect_equal(pin("a.CSV", NULL, "qs2", "COL", 2010), "a_COL_2010_csv.qs2")
  expect_equal(pin("a.do", NULL, "qs2", "COL", 2010), "a_COL_2010.do")
  expect_equal(pin("a.do", "V01_M", "qs2", "COL", 2010),
               "a_COL_2010_V01_M.do")
  expect_equal(pin("README", NULL, "qs2", "COL", 2010), "README_COL_2010")
  # not for dta, nor for harmonized names, which have them
  expect_equal(pin("a.dta", NULL, "qs2", "COL", 2010), "a.qs2")
  expect_equal(pin("WLD_2026_ALL_V02_M_V01_A_GMI_INE.do", NULL, "qs2", "WLD",
                   2026),
               "WLD_2026_ALL_V02_M_V01_A_GMI_INE.do")
})

write_fixture <- function(ext) {
  df <- data.frame(x = 1:3, y = c("a", "b", "c"))
  path <- withr::local_tempfile(fileext = paste0(".", ext), .local_envir = parent.frame())
  switch(ext,
         dta     = haven::write_dta(df, path),
         sav     = haven::write_sav(df, path),
         csv     = data.table::fwrite(df, path),
         tsv     = data.table::fwrite(df, path, sep = "\t"),
         rds     = saveRDS(df, path),
         parquet = arrow::write_parquet(df, path))
  path
}

test_that("read_table reads each tabular format", {
  skip_if_not_installed("arrow")
  for (ext in setdiff(tabular_formats, c("xls", "xlsx"))) {
    out <- read_table(write_fixture(ext), ext, paste0("a.", ext))
    expect_s3_class(out, "data.frame")
    expect_equal(dim(out), c(3, 2), info = ext)
    expect_equal(as.vector(out$x), 1:3, info = ext)
    expect_equal(as.vector(out$y), c("a", "b", "c"), info = ext)
  }
})

test_that("read_table rejects an rds that is not a table", {
  path <- withr::local_tempfile(fileext = ".rds")
  saveRDS(list(1, 2), path)
  expect_error(read_table(path, "rds", "a.rds"), "does not contain a table")
})

mock_download <- function(body, filename, env = parent.frame()) {
  # `rec$body` can be changed to serve other bytes
  rec <- new.env()
  rec$n <- 0
  rec$body <- body
  local_mocked_bindings(
    build_request = \(...) "req",
    get_raw_data = \(req) {
      rec$n <- rec$n + 1
      file_response(rec$body, filename, tools::file_ext(filename))
    },
    .env = env
  )
  rec
}

file_bytes <- function(ext) {
  path <- write_fixture(ext)
  readBin(path, "raw", file.size(path))
}

test_that("dlw_get_data pins csv and dta files under different names", {
  dir <- withr::local_tempdir()
  csv <- mock_download(file_bytes("csv"), "a.csv")
  out <- dlw_get_data("COL", "a.csv", year = 2010, local = TRUE,
                      local_dir = dir)
  expect_s3_class(out, "data.table")
  expect_equal(dim(out), c(3, 2))
  expect_equal(pins::pin_list(pins::board_folder(dir)), "a_COL_2010_csv.qs2")

  dta <- mock_download(file_bytes("dta"), "a.dta")
  dlw_get_data("COL", "a.dta", year = 2010, local = TRUE, local_dir = dir)
  expect_setequal(pins::pin_list(pins::board_folder(dir)),
                  c("a_COL_2010_csv.qs2", "a.qs2"))

  # cached: no new request
  again <- dlw_get_data("COL", "a.csv", year = 2010, local = TRUE,
                        local_dir = dir)
  expect_equal(csv$n, 1)
  expect_equal(again, out)
})

test_that("dlw_get_data pins other files as is and returns their path", {
  dir <- withr::local_tempdir()
  txt <- charToRaw("display \"hi\"\r\n")
  rec <- mock_download(txt, "a.do")
  path <- dlw_get_data("COL", "a.do", year = 2010, local = TRUE,
                       local_dir = dir)
  expect_type(path, "character")
  expect_equal(basename(path), "a.do")
  expect_identical(readBin(path, "raw", 100), txt)
  expect_equal(pins::pin_list(pins::board_folder(dir)), "a_COL_2010.do")

  # second call reads the pin
  again <- dlw_get_data("COL", "a.do", year = 2010, local = TRUE,
                        local_dir = dir)
  expect_equal(rec$n, 1)
  expect_identical(readBin(again, "raw", 100), txt)
})

test_that("dlw_get_data re-downloads a file that did not change", {
  dir <- withr::local_tempdir()
  board <- pins::board_folder(dir)
  rec <- mock_download(charToRaw("display 1\r\n"), "a.do")
  get <- \(...) dlw_get_data("COL", "a.do", year = 2010, local = TRUE,
                             local_dir = dir, ...)
  get()
  # the same bytes again, at once: no error and no new pin version
  path <- get(local_overwrite = TRUE)
  expect_equal(rec$n, 2)
  expect_equal(nrow(pins::pin_versions(board, "a_COL_2010.do")), 1)
  expect_identical(readBin(path, "raw", 100), charToRaw("display 1\r\n"))

  # other bytes: a new version
  rec$body <- charToRaw("display 2\r\n")
  get(local_overwrite = TRUE)
  expect_equal(nrow(pins::pin_versions(board, "a_COL_2010.do")), 2)
})

test_that("files with the same name in other countries and years are separate", {
  dir <- withr::local_tempdir()
  get <- \(country, year) dlw_get_data(country, "readme.do", year = year,
                                       local = TRUE, local_dir = dir)
  rec <- mock_download(raw(1), "readme.do")
  for (x in list(list("USA", 2015), list("USA", 2016), list("COL", 2015))) {
    rec$body <- charToRaw(paste("*", x[[1]], x[[2]]))
    path <- get(x[[1]], x[[2]])
    expect_equal(readLines(path, warn = FALSE), paste("*", x[[1]], x[[2]]))
  }
  expect_length(pins::pin_list(pins::board_folder(dir)), 3)
})

test_that("dlw_get_data pins a binary file byte for byte", {
  dir <- withr::local_tempdir()
  bytes <- as.raw(c(0x50, 0x4b, 0x03, 0x04, 0, 255, 13, 10))
  mock_download(bytes, "Some Manual - v2.zip")
  path <- dlw_get_data("COL", "Some Manual - v2.zip", year = 2010,
                       local = TRUE, local_dir = dir)
  expect_identical(readBin(path, "raw", 100), bytes)
  expect_equal(basename(path), "Some Manual - v2.zip")
})

test_that("read_table reads the first sheet of xls and xlsx files", {
  skip_if_not_installed("readxl")
  for (ext in c("xls", "xlsx")) {
    path <- readxl::readxl_example(paste0("datasets.", ext))
    out <- read_table(path, ext, paste0("a.", ext))
    expect_s3_class(out, "data.frame")
    expect_equal(dim(out), c(32, 11), info = ext)
    expect_equal(names(out)[1], "mpg", info = ext)
  }
})

test_that("dlw_get_data pins an Excel file as a table", {
  skip_if_not_installed("readxl")
  dir <- withr::local_tempdir()
  path <- readxl::readxl_example("datasets.xlsx")
  rec <- mock_download(readBin(path, "raw", file.size(path)), "a.xlsx")
  out <- dlw_get_data("COL", "a.xlsx", year = 2010, local = TRUE,
                      local_dir = dir)
  expect_s3_class(out, "data.table")
  expect_equal(dim(out), c(32, 11))
  expect_equal(pins::pin_list(pins::board_folder(dir)), "a_COL_2010_xlsx.qs2")
})
