# Tests for transform_ratings() function

test_that("transform_ratings handles matrix input", {
  result <- transform_ratings(matrix_ratings, scale = "4point")

  expect_true(is.matrix(result))
  expect_equal(dim(result), dim(matrix_ratings))
})

test_that("transform_ratings preserves dimension names", {
  result <- transform_ratings(matrix_ratings, scale = "4point")

  expect_equal(rownames(result), rownames(matrix_ratings))
  expect_equal(colnames(result), colnames(matrix_ratings))
})

test_that("transform_ratings errors on non-numeric data", {
  char_data <- data.frame(
    item1 = c("a", "b", "c"),
    item2 = c("d", "e", "f")
  )

  expect_error(
    transform_ratings(char_data, scale = "4point"),
    "numeric"
  )
})

test_that("transform_ratings errors on invalid data type", {
  expect_error(
    transform_ratings(list(a = 1, b = 2), scale = "4point"),
    "data frame or matrix"
  )

  expect_error(
    transform_ratings(c(1, 2, 3, 4), scale = "4point"),
    "data frame or matrix"
  )
})

test_that("transform_ratings errors on binary scale with >2 unique values", {
  three_values <- data.frame(
    item1 = c(1, 2, 3, 1)
  )

  expect_error(
    transform_ratings(three_values, scale = "binary"),
    "2 unique values"
  )
})

test_that("transform_ratings errors on custom scale with non-numeric relevant_values", {
  ratings <- data.frame(
    item1 = c(4, 4, 3, 4)
  )

  expect_error(
    transform_ratings(ratings, scale = "custom", relevant_values = c("a", "b")),
    "numeric"
  )
})

test_that("transform_ratings handles NA values", {
  # Use a simple 1-column test
  simple_na <- data.frame(item1 = c(4, NA, 3, 1))
  result <- transform_ratings(simple_na, scale = "4point")

  # Check NA is preserved
  expect_true(is.na(result[2, 1]))  # row 2 should be NA

  # Check transformations
  expect_equal(unname(result[1, 1]), 1L)  # 4 is relevant
  expect_equal(unname(result[3, 1]), 1L)  # 3 is relevant
  expect_equal(unname(result[4, 1]), 0L)  # 1 is not relevant

  # Test with multiple columns
  multi_col <- data.frame(
    item1 = c(4, 4, NA),
    item2 = c(NA, 4, 1)
  )
  result2 <- transform_ratings(multi_col, scale = "4point")

  expect_true(is.na(result2[3, 1]))  # item1[3] = NA
  expect_true(is.na(result2[1, 2]))  # item2[1] = NA
})

test_that("transform_ratings handles non-integer numeric values", {
  # Ratings that are doubles (e.g., 3.5 could come from averaging)
  ratings <- data.frame(
    item1 = c(3.5, 4.0, 2.5, 3.0)
  )

  # With 4-point scale, 3 and 4 are relevant, so 3.5 and 4.0 won't match exactly
  result <- transform_ratings(ratings, scale = "4point")

  # 3.5 is not in c(3, 4) exactly
  expect_equal(unname(result[1, 1]), 0L)
  expect_equal(unname(result[2, 1]), 1L)  # 4.0 is relevant
  expect_equal(unname(result[3, 1]), 0L)
  expect_equal(unname(result[4, 1]), 1L)  # 3.0 is relevant
})

test_that("transform_ratings works with single column data frame", {
  single_col <- data.frame(item1 = c(4, 4, 3, 2))

  result <- transform_ratings(single_col, scale = "4point")

  expect_equal(dim(result), c(4, 1))
  expect_equal(result[, 1], c(1L, 1L, 1L, 0L))
})

test_that("transform_ratings works with single row data frame", {
  single_row <- data.frame(item1 = 4, item2 = 3, item3 = 2)

  result <- transform_ratings(single_row, scale = "4point")

  expect_equal(dim(result), c(1, 3))
  expect_equal(unname(result[1, ]), c(1L, 1L, 0L))
})
