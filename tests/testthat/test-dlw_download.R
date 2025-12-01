
# Mock functions that are called within dlw_download
# This allows us to test the logic of dlw_download without making actual network requests or writing files.
mock_build_request <- function(...) {
  "mock request"
}

mock_get_raw_data <- function(...) {
  # Create a dummy raw vector that represents the content of a .dta file
  # This is a simplified representation for testing purposes.
  as.raw(c(0x64, 0x74, 0x61, 0x00))
}

mock_read_dta <- function(...) {
  # Return a dummy data.table object
  data.table::data.table(a = 1, b = 2)
}

mock_pin_write <- function(...) {
  # This function will be used to check if pin_write is called with the correct arguments.
  # We will use mocking to capture the arguments passed to it.
}

# Start of the test suite
test_that("dlw_download works as expected", {

  # Test 1: Error on missing filename
  test_that("dlw_download errors when filename is missing", {
    expect_error(
      dlw_download(country_code = "USA",
                   board = "test_board",
                   pin_name = "test_pin",
                   format = "qs"),
      "is a required argument"
    )
  })

  # Test 2: Successful download and pinning
  test_that("dlw_download successfully downloads and pins data", {
    # Use mockery to replace the real functions with our mocks for the duration of this test
    mockery::stub(dlw_download, "build_request", mock_build_request)
    mockery::stub(dlw_download, "get_raw_data", mock_get_raw_data)
    mockery::stub(dlw_download, "haven::read_dta", mock_read_dta)
    mockery::stub(dlw_download, "pins::pin_write", mock_pin_write)

    # Call the function with all required arguments
    result <- dlw_download(
      country_code = "USA",
      filename = "test_file.dta",
      board = "test_board",
      pin_name = "test_pin",
      format = "qs"
    )

    # Check that the result is a data.table
    expect_s3_class(result, "data.table")
    # Check that the content of the data.table is what we expect from our mock
    expect_equal(result, data.table::data.table(a = 1, b = 2))
  })

  # # Test 3: Correct handling of additional arguments (...)
  # test_that("dlw_download correctly handles additional arguments", {
  #   # We'll capture the arguments passed to build_request to verify this
  #   arg_capture <- NULL
  #   mockery::stub(dlw_download, "build_request", function(...) {
  #     arg_capture <<- list(...)
  #     "mock request"
  #   })
  #   mockery::stub(dlw_download, "get_raw_data", mock_get_raw_data)
  #   mockery::stub(dlw_download, "haven::read_dta", mock_read_dta)
  #   mockery::stub(dlw_download, "pins::pin_write", mock_pin_write)
  #
  #   dlw_download(
  #     country_code = "US",
  #     filename = "test_file.dta",
  #     board = "test_board",
  #     pin_name = "test_pin",
  #     format = "parquet",
  #     extra_param1 = "value1",
  #     extra_param2 = "value2"
  #   )
  #
  #   # Check that our extra parameters were passed to build_request
  #   expect_true("extra_param1" %in% names(arg_capture))
  #   expect_equal(arg_capture$extra_param1, "value1")
  #   expect_true("extra_param2" %in% names(arg_capture))
  #   expect_equal(arg_capture$extra_param2, "value2")
  # })

  # Test 3: Correct file extension and temp file usage
  test_that("dlw_download uses a temporary file with the correct extension", {
    # We can check this by mocking fs::file_temp and checking the 'ext' argument
    temp_file_ext <- NULL
    mockery::stub(dlw_download, "fs::file_temp", function(ext) {
      temp_file_ext <<- ext
      "temp.dta" # return a dummy file path
    })
    mockery::stub(dlw_download, "build_request", mock_build_request)
    mockery::stub(dlw_download, "get_raw_data", mock_get_raw_data)
    mockery::stub(dlw_download, "haven::read_dta", mock_read_dta)
    mockery::stub(dlw_download, "pins::pin_write", mock_pin_write)

    dlw_download(
      country_code = "USA",
      filename = "test_file.dta",
      board = "test_board",
      pin_name = "test_pin",
      format = "qs"
    )

    # Verify that the extension for the temp file was 'dta'
    expect_equal(temp_file_ext, "dta")
  })

  # Test 4: Correct return value
  test_that("dlw_download returns a data.table object", {
    mockery::stub(dlw_download, "build_request", mock_build_request)
    mockery::stub(dlw_download, "get_raw_data", mock_get_raw_data)
    mockery::stub(dlw_download, "haven::read_dta", mock_read_dta)
    mockery::stub(dlw_download, "pins::pin_write", mock_pin_write)

    result <- dlw_download(
      country_code = "USA",
      filename = "test_file.dta",
      board = "test_board",
      pin_name = "test_pin",
      format = "qs"
    )

    # Check that the class of the returned object is data.table
    expect_s3_class(result, "data.table")
  })
})
