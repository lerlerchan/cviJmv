# Tests for CVR functions

test_that("cvr works with is_binary = TRUE", {
  binary_data <- data.frame(
    item1 = c(1, 1, 1, 1, 1),
    item2 = c(1, 1, 1, 0, 0),
    item3 = c(0, 0, 0, 0, 0)
  )

  result <- cvr(binary_data, is_binary = TRUE)

  expect_equal(result["item1"], c(item1 = 1.0))      # (5 - 2.5) / 2.5 = 1.0
  expect_equal(result["item2"], c(item2 = 0.2))     # (3 - 2.5) / 2.5 = 0.2
  expect_equal(result["item3"], c(item3 = -1.0))    # (0 - 2.5) / 2.5 = -1.0
})

test_that("cvr works with matrix input", {
  mat <- matrix(
    c(4, 4, 4, 4, 4,
      4, 4, 1, 1, 1),
    nrow = 5,
    ncol = 2,
    dimnames = list(NULL, c("item1", "item2"))
  )

  result <- cvr(mat, scale = "4point")

  expect_equal(result["item1"], c(item1 = 1.0))
  expect_equal(result["item2"], c(item2 = -0.2))  # (2 - 2.5) / 2.5 = -0.2
})

test_that("cvr handles NA values correctly", {
  # Create data with NA values but no all-NA rows
  ratings_na <- data.frame(
    item1 = c(4, 4, NA, 4, 4),  # 4 non-NA, all relevant: (4 - 2) / 2 = 1.0
    item2 = c(4, 4, 1, 1, NA)   # 4 non-NA, 2 relevant: (2 - 2) / 2 = 0
  )

  result <- cvr(ratings_na, scale = "4point")

  expect_equal(result["item1"], c(item1 = 1.0))
  expect_equal(result["item2"], c(item2 = 0.0))
})

test_that("cvr_critical interpolates correctly for n=12", {
  # n=12 is in the table, should return exact value
  expect_equal(cvr_critical(12), 0.56)
})

test_that("cvr_critical interpolates for values not in table", {
  # n=16 is between 15 (0.49) and 20 (0.42)
  # Linear interpolation: 0.49 + (0.42 - 0.49) * (16 - 15) / (20 - 15)
  # = 0.49 - 0.07 * 0.2 = 0.49 - 0.014 = 0.476
  result <- cvr_critical(16)

  expect_equal(result, 0.49 + (0.42 - 0.49) * (16 - 15) / (20 - 15), tolerance = 0.001)
})

test_that("cvr_critical extrapolates for n > 40", {
  # Should return the value for n=40
  expect_equal(cvr_critical(50), 0.29)
  expect_equal(cvr_critical(100), 0.29)
})

test_that("cvr_critical errors on alpha != 0.05", {
  expect_error(
    cvr_critical(10, alpha = 0.01),
    "alpha = 0.05"
  )

  expect_error(
    cvr_critical(10, alpha = 0.10),
    "alpha = 0.05"
  )
})

test_that("mean_cvr works with is_binary = TRUE", {
  binary_data <- data.frame(
    item1 = c(1, 1, 1, 1, 1),  # CVR = 1.0
    item2 = c(0, 0, 0, 0, 0)   # CVR = -1.0
  )

  result <- mean_cvr(binary_data, is_binary = TRUE)

  expect_equal(result, 0.0)  # (1.0 + -1.0) / 2 = 0
})

test_that("mean_cvr handles NA values", {
  # Create data with NA values but no all-NA rows
  ratings_na <- data.frame(
    item1 = c(4, 4, NA, 4, 4),  # 4 non-NA, all relevant: CVR = 1.0
    item2 = c(4, 4, 4, 1, NA)   # 4 non-NA, 3 relevant: CVR = (3-2)/2 = 0.5
  )

  result <- mean_cvr(ratings_na, scale = "4point")

  # Mean of 1.0 and 0.5 = 0.75
  expect_equal(result, (1.0 + 0.5) / 2, tolerance = 0.001)
})

test_that("cvr_critical warns for fewer than 5 experts", {
  expect_warning(result <- cvr_critical(4))
  expect_true(is.na(result))

  expect_warning(result <- cvr_critical(3))
  expect_true(is.na(result))
})

test_that("cvr produces named output", {
  ratings <- data.frame(
    myItem1 = c(4, 4, 4, 4, 4),
    myItem2 = c(4, 4, 4, 4, 4)
  )

  result <- cvr(ratings)

  expect_equal(names(result), c("myItem1", "myItem2"))
})

test_that("cvr provides default names when not provided", {
  mat <- matrix(
    c(4, 4, 4, 4, 4,
      4, 4, 4, 4, 4),
    nrow = 5,
    ncol = 2
  )

  result <- cvr(mat, scale = "4point")

  expect_equal(names(result), c("Item1", "Item2"))
})
