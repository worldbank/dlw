
# Start of the test suite for dlw_read
test_that("dlw_read function behaves as expected", {

  # Test 1: Should throw an error if the pin_name is not found in the board
  test_that("it throws an error for a non-existent pin", {
    # Mock pins::pin_list to return a list that does not include our target pin
    mockery::stub(dlw_read, "pins::pin_list", \(...) c("existing_pin_1", "existing_pin_2"))

    # Expect the function to abort with a specific message
    expect_error(
      dlw_read(board = "mock_board", pin_name = "non_existent_pin"),
      "File 'non_existent_pin' not found in the provided board."
    )
  })

  # Test 2: Should successfully read an existing pin and return a data.table
  test_that("it reads an existing pin and returns a data.table", {
    # Mock pins::pin_list to confirm the pin exists
    mockery::stub(dlw_read, "pins::pin_list", \(...) "my_pin")
    mockery::stub(dlw_read, "pins::pin_read", \(...) data.frame(id = 1:3, value = c("A", "B", "C")))

    # Call the function
    result <- dlw_read(board = "mock_board", pin_name = "my_pin")

    # Assert that the result is a data.table
    expect_s3_class(result, "data.table")
    # Assert that the content of the data.table is correct
    expect_equal(result, data.table(id = 1:3, value = c("A", "B", "C")))
  })

  # Test 3: Should correctly pass the version argument to pins::pin_read
  test_that("it correctly handles the version argument", {
    captured_args <- NULL

    mockery::stub(dlw_read, "pins::pin_list", \(...) "versioned_pin")
    mockery::stub(dlw_read, "pins::pin_read", \(...) {
      captured_args <<- list(...)
      data.frame(x = 1) # Return a minimal data.frame
    })

    # Call the function with a specific version
    dlw_read(board = "mock_board", pin_name = "versioned_pin", version = "12345")

    # Assert that the captured arguments include the correct version
    expect_equal(captured_args$version, "12345")
  })

  # Test 4: Should return a data.table object even if pin_read returns a different type
  test_that("it ensures the return type is always a data.table", {
    # Mock pins::pin_list to confirm the pin exists
    mockery::stub(dlw_read, "pins::pin_list", \(...) "my_pin")
    # Mock pins::pin_read to return a standard data.frame
    mockery::stub(dlw_read, "pins::pin_read", \(...) data.frame(col1 = "a", col2 = "b"))

    # Call the function
    result <- dlw_read(board = "mock_board", pin_name = "my_pin")

    # Assert that the final output is a data.table, thanks to setDT()
    expect_s3_class(result, "data.table")
  })
})
