make_test_df <- function() {
  DataFrame(
    columns = list(c(1, 2, 3), c("a", "b", "c")),
    colnames = c("id", "name")
  )
}

# ================== DataFrame construction ==================
test_that("DataFrame stores columns and colnames correctly", {
  df <- make_test_df()
  expect_equal(df@columns[[1]], c(1, 2, 3))
  expect_equal(df@colnames, c("id", "name"))
})

# ================== dims ==================
test_that("dims returns correct ncol and nrow", {
  df <- make_test_df()
  expect_equal(dims(df), c(2, 3))
})

# ================== df_colnames ==================
test_that("df_colnames returns the column names", {
  df <- make_test_df()
  expect_equal(df_colnames(df), c("id", "name"))
})

# ================== col_idxs ==================
test_that("col_idxs returns correct indices, and errors on invalid names", {
  df <- make_test_df()
  expect_equal(col_idxs(df, c("name", "id")), c(2, 1))
  expect_error(col_idxs(df, "age"))
})

# ================== col_name ==================
test_that("col_name returns correct names for valid indices", {
  df <- make_test_df()
  expect_equal(col_name(df, 1), "id")
  expect_equal(col_name(df, c(2, 1)), c("name", "id"))
})

# ================== fill_col ==================
test_that("fill_col uses the value's type by default, or an overridden type", {
  expect_equal(fill_col(0, 5), c(0, 0, 0, 0, 0))
  expect_equal(class(fill_col(NA, 5, type = "numeric")), "numeric")
})

# ================== validate ==================
test_that("validate passes on equal-length columns and errors otherwise", {
  df <- make_test_df()
  expect_true(validate(df))

  bad_df <- DataFrame(columns = list(c(1, 2), c("a", "b", "c")), colnames = c("x", "y"))
  expect_error(validate(bad_df))
})

# ================== get_column / set_column ==================
test_that("get_column retrieves the correct column", {
  df <- make_test_df()
  expect_equal(get_column(df, "name"), c("a", "b", "c"))
})

test_that("set_column replaces an existing column", {
  df <- make_test_df()
  result <- set_column(df, "id", c(10, 20, 30))
  expect_equal(get_column(result, "id"), c(10, 20, 30))
})

test_that("set_column adds a new column when the name doesn't exist", {
  df <- make_test_df()
  result <- set_column(df, "score", c(100, 200, 300))
  expect_equal(df_colnames(result), c("id", "name", "score"))
})

test_that("set_column broadcasts a scalar to the correct length", {
  df <- make_test_df()
  result <- set_column(df, "flag", TRUE)
  expect_equal(get_column(result, "flag"), c(TRUE, TRUE, TRUE))
})

test_that("set_column does not mutate the original dataframe", {
  df <- make_test_df()
  set_column(df, "id", c(10, 20, 30))
  expect_equal(get_column(df, "id"), c(1, 2, 3))
})

# ================== get_value / set_value ==================
test_that("get_value retrieves the correct single value", {
  df <- make_test_df()
  expect_equal(get_value(df, "id", 2), 2)
})

test_that("set_value updates a single value without mutating the original", {
  df <- make_test_df()
  result <- set_value(df, "id", 2, 99)
  expect_equal(get_value(result, "id", 2), 99)
  expect_equal(get_value(df, "id", 2), 2)
})

# ================== rename_column ==================
test_that("rename_column changes the name but not the data", {
  df <- make_test_df()
  result <- rename_column(df, "id", "ID")
  expect_equal(df_colnames(result), c("ID", "name"))
  expect_equal(get_column(result, "ID"), c(1, 2, 3))
})

# ================== change_column_type ==================
test_that("change_column_type converts data to the requested type", {
  df <- make_test_df()
  result <- change_column_type(df, "id", "character")
  expect_equal(get_column(result, "id"), c("1", "2", "3"))
})

# ================== delete_column ==================
test_that("delete_column removes the specified column", {
  df <- make_test_df()
  result <- delete_column(df, "name")
  expect_equal(df_colnames(result), "id")
})

# ================== slice_rows ==================
test_that("slice_rows returns the correct contiguous rows", {
  df <- make_test_df()
  result <- slice_rows(df, 2, 3)
  expect_equal(get_column(result, "id"), c(2, 3))
  expect_equal(get_column(result, "name"), c("b", "c"))
})

# ================== guess_type / parse_column ==================
test_that("guess_type correctly identifies numeric, boolean, and character columns", {
  expect_equal(guess_type(c("1", "2", "3")), "numeric")
  expect_equal(guess_type(c("TRUE", "FALSE")), "boolean")
  expect_equal(guess_type(c("apple", "banana")), "character")
})

test_that("parse_column converts strings to the correct type", {
  expect_equal(parse_column(c("1", "2", "3")), c(1, 2, 3))
  expect_equal(parse_column(c("TRUE", "FALSE")), c(TRUE, FALSE))
})

# ================== read_dataframe_csv ==================
test_that("read_dataframe_csv reads a CSV with correct column names and types", {
  tmp <- tempfile(fileext = ".csv")
  writeLines(c("id,name,active", "1,Alice,TRUE", "2,Bob,FALSE"), tmp)

  df <- read_dataframe_csv(tmp)

  expect_equal(df_colnames(df), c("id", "name", "active"))
  expect_equal(get_column(df, "id"), c(1, 2))
  expect_equal(class(get_column(df, "active")), "logical")

  unlink(tmp)
})
