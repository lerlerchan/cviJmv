# Tests for validator functions

test_that("validate_data errors on empty data frame", {
  empty_df <- data.frame()

  expect_error(
    validate_data(empty_df),
    "cannot be empty"
  )
})

test_that("validate_data errors on zero-row data", {
  zero_rows <- data.frame(item1 = numeric(0), item2 = numeric(0))

  expect_error(
    validate_data(zero_rows),
    "cannot be empty"
  )
})

test_that("validate_data detects all-NA rows", {
  na_row <- data.frame(
    item1 = c(4, 4, NA, 4),
    item2 = c(4, 4, NA, 4)
  )

  expect_error(
    validate_data(na_row),
    "only NA"
  )
})

test_that("validate_data detects all-NA columns", {
  na_col <- data.frame(
    item1 = c(4, 4, 4, 4),
    item2 = c(NA, NA, NA, NA)
  )

  expect_error(
    validate_data(na_col),
    "only NA"
  )
})

test_that("validate_scale warns when relevant_values ignored", {
  expect_warning(
    validate_scale("4point", relevant_values = c(3, 4)),
    "ignored"
  )

  expect_warning(
    validate_scale("3point", relevant_values = c(2, 3)),
    "ignored"
  )

  expect_warning(
    validate_scale("binary", relevant_values = c(1)),
    "ignored"
  )
})

test_that("validate_scale warns for out-of-range 4-point values", {
  ratings <- data.frame(item1 = c(1, 2, 5, 6))

  expect_warning(
    validate_scale("4point", data = ratings),
    "outside 1-4 range"
  )
})

test_that("validate_scale warns for out-of-range 3-point values", {
  ratings <- data.frame(item1 = c(0, 2, 4, 5))

  expect_warning(
    validate_scale("3point", data = ratings),
    "outside 1-3 range"
  )
})

test_that("validate_data enforces minimum experts", {
  two_experts <- data.frame(
    item1 = c(4, 4),
    item2 = c(4, 4)
  )

  expect_error(
    validate_data(two_experts, min_experts = 3),
    "At least 3 experts"
  )

  # Should pass with min_experts = 2
  expect_true(validate_data(two_experts, min_experts = 2))
})

test_that("validate_data enforces configurable minimum experts", {
  five_experts <- data.frame(
    item1 = c(4, 4, 4, 4, 4),
    item2 = c(4, 4, 4, 4, 4)
  )

  expect_error(
    validate_data(five_experts, min_experts = 6),
    "At least 6 experts"
  )

  expect_true(validate_data(five_experts, min_experts = 5))
})

test_that("validate_scale errors on invalid scale argument", {
  expect_error(
    validate_scale("5point"),
    "must be one of"
  )

  expect_error(
    validate_scale("likert"),
    "must be one of"
  )
})

test_that("validate_data works with matrix input", {
  expect_true(validate_data(matrix_ratings))

  # Matrix with all NA column
  bad_matrix <- matrix(
    c(4, 4, 4, NA, NA, NA),
    nrow = 3,
    ncol = 2
  )

  expect_error(
    validate_data(bad_matrix),
    "only NA"
  )
})

test_that("validate_data respects allow_na parameter", {
  ratings_na <- data.frame(
    item1 = c(4, 4, NA, 4),
    item2 = c(4, 4, 4, NA)
  )

  # Should pass with allow_na = TRUE (default)
  expect_true(validate_data(ratings_na, allow_na = TRUE))

  # Should fail with allow_na = FALSE
  expect_error(
    validate_data(ratings_na, allow_na = FALSE),
    "contains NA"
  )
})

test_that("prepare_data converts data frame to numeric matrix", {
  df <- data.frame(
    item1 = c(4L, 4L, 3L, 4L),
    item2 = c(4L, 4L, 4L, 4L)
  )

  result <- prepare_data(df)

  expect_true(is.matrix(result))
  expect_true(is.numeric(result))
  expect_equal(storage.mode(result), "double")
})

test_that("validate_scale errors on custom without relevant_values", {
  expect_error(
    validate_scale("custom"),
    "relevant_values.*must be provided"
  )
})
