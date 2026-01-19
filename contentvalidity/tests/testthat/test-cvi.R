# Tests for Content Validity Index functions

test_that("transform_ratings works with 4-point scale", {
  ratings <- data.frame(
    item1 = c(4, 3, 2, 1),
    item2 = c(4, 4, 4, 4)
  )

  result <- transform_ratings(ratings, scale = "4point")

  expect_equal(dim(result), c(4, 2))
  expect_equal(result[, "item1"], c(1, 1, 0, 0))
  expect_equal(result[, "item2"], c(1, 1, 1, 1))
})

test_that("transform_ratings works with 3-point scale", {
  ratings <- data.frame(
    item1 = c(3, 2, 1)
  )

  result <- transform_ratings(ratings, scale = "3point")

  expect_equal(result[, 1], c(1, 1, 0))
})

test_that("transform_ratings works with custom scale", {
  ratings <- data.frame(
    item1 = c(5, 4, 3, 2, 1)
  )

  result <- transform_ratings(ratings, scale = "custom", relevant_values = c(4, 5))

  expect_equal(result[, 1], c(1, 1, 0, 0, 0))
})

test_that("transform_ratings requires relevant_values for custom scale", {
  ratings <- data.frame(item1 = c(1, 2, 3))

  expect_error(
    transform_ratings(ratings, scale = "custom"),
    "relevant_values"
  )
})

test_that("icvi calculates correctly", {
  # Perfect agreement (all relevant)
  ratings_perfect <- data.frame(
    item1 = c(4, 4, 4, 4)
  )
  expect_equal(icvi(ratings_perfect)[1], 1.0)

  # No agreement (none relevant)
  ratings_none <- data.frame(
    item1 = c(1, 1, 1, 1)
  )
  expect_equal(icvi(ratings_none)[1], 0.0)

  # Half relevant
  ratings_half <- data.frame(
    item1 = c(4, 4, 1, 1)
  )
  expect_equal(icvi(ratings_half)[1], 0.5)

  # 3 out of 4 relevant (I-CVI = 0.75)
  ratings_three <- data.frame(
    item1 = c(4, 4, 4, 1)
  )
  expect_equal(icvi(ratings_three)[1], 0.75)
})

test_that("ua calculates correctly", {
  # Perfect UA (all experts agree relevant)
  ratings_ua <- data.frame(
    item1 = c(4, 4, 4, 4),  # UA = 1
    item2 = c(4, 4, 4, 2)   # UA = 0
  )

  result <- ua(ratings_ua)
  expect_equal(result["item1"], c(item1 = 1L))
  expect_equal(result["item2"], c(item2 = 0L))
})

test_that("scvi_ave calculates mean of I-CVIs", {
  ratings <- data.frame(
    item1 = c(4, 4, 4, 4),  # I-CVI = 1.0
    item2 = c(4, 4, 2, 2)   # I-CVI = 0.5
  )

  # Average should be (1.0 + 0.5) / 2 = 0.75
  expect_equal(scvi_ave(ratings), 0.75)
})

test_that("scvi_ua calculates proportion of items with UA", {
  ratings <- data.frame(
    item1 = c(4, 4, 4, 4),  # UA = 1
    item2 = c(4, 4, 4, 4),  # UA = 1
    item3 = c(4, 4, 4, 2),  # UA = 0
    item4 = c(4, 4, 2, 2)   # UA = 0
  )

  # 2 out of 4 items have UA = 1, so S-CVI/UA = 0.5
  expect_equal(scvi_ua(ratings), 0.5)
})

test_that("cvr calculates Lawshe's formula correctly", {
  # 5 experts, all say relevant: CVR = (5 - 2.5) / 2.5 = 1.0
  ratings_all <- data.frame(
    item1 = c(4, 4, 4, 4, 4)
  )
  expect_equal(cvr(ratings_all)[1], 1.0)

  # 5 experts, none say relevant: CVR = (0 - 2.5) / 2.5 = -1.0
  ratings_none <- data.frame(
    item1 = c(1, 1, 1, 1, 1)
  )
  expect_equal(cvr(ratings_none)[1], -1.0)

  # 6 experts, 3 say relevant: CVR = (3 - 3) / 3 = 0
  ratings_half <- data.frame(
    item1 = c(4, 4, 4, 1, 1, 1)
  )
  expect_equal(cvr(ratings_half)[1], 0.0)

  # 5 experts, 4 say relevant: CVR = (4 - 2.5) / 2.5 = 0.6
  ratings_four <- data.frame(
    item1 = c(4, 4, 4, 4, 1)
  )
  expect_equal(cvr(ratings_four)[1], 0.6)
})

test_that("cvr processes ALL items correctly", {
  # This tests the fix for the bug where only first item was processed
  ratings <- data.frame(
    item1 = c(4, 4, 4, 4, 4),  # CVR = 1.0
    item2 = c(4, 4, 4, 1, 1),  # CVR = (3 - 2.5) / 2.5 = 0.2
    item3 = c(1, 1, 1, 1, 1)   # CVR = -1.0
  )

  result <- cvr(ratings)

  expect_equal(length(result), 3)
  expect_equal(result["item1"], c(item1 = 1.0))
  expect_equal(result["item2"], c(item2 = 0.2))
  expect_equal(result["item3"], c(item3 = -1.0))
})

test_that("cvr_critical returns correct values", {
  expect_equal(cvr_critical(5), 0.99)
  expect_equal(cvr_critical(10), 0.62)
  expect_equal(cvr_critical(15), 0.49)
  expect_equal(cvr_critical(20), 0.42)

  # Should warn for fewer than 5 experts
  expect_warning(cvr_critical(4))
})

test_that("proportion_relevance calculates correctly", {
  ratings <- data.frame(
    item1 = c(4, 4, 1),  # Expert1: 2/3, Expert2: 3/3, Expert3: 0/3
    item2 = c(4, 4, 1),
    item3 = c(1, 4, 1)
  )

  result <- proportion_relevance(ratings)

  expect_equal(result["Expert1"], c(Expert1 = 2/3))
  expect_equal(result["Expert2"], c(Expert2 = 1.0))
  expect_equal(result["Expert3"], c(Expert3 = 0.0))
})

test_that("cvi returns correct structure", {
  ratings <- data.frame(
    item1 = c(4, 4, 3, 4),
    item2 = c(4, 4, 4, 4),
    item3 = c(2, 3, 2, 3)
  )

  result <- cvi(ratings)

  expect_s3_class(result, "cvi_result")
  expect_named(result, c("item_stats", "expert_stats", "scale_stats",
                         "binary_data", "call"))

  # Check item_stats structure
  expect_equal(nrow(result$item_stats), 3)
  expect_true("icvi" %in% names(result$item_stats))
  expect_true("ua" %in% names(result$item_stats))
  expect_true("cvr" %in% names(result$item_stats))

  # Check expert_stats structure
  expect_equal(nrow(result$expert_stats), 4)
  expect_true("proportion_relevance" %in% names(result$expert_stats))

  # Check scale_stats
  expect_true("scvi_ave" %in% names(result$scale_stats))
  expect_true("scvi_ua" %in% names(result$scale_stats))
})

test_that("cvi handles compute_cvr = FALSE", {
  ratings <- data.frame(
    item1 = c(4, 4, 3, 4)
  )

  result <- cvi(ratings, compute_cvr = FALSE)

  expect_false("cvr" %in% names(result$item_stats))
  expect_null(result$scale_stats$mean_cvr)
})

test_that("validation catches invalid data", {
  # Too few experts
  expect_error(
    cvi(data.frame(item1 = c(4, 4))),
    "At least 3 experts"
  )

  # Non-numeric data
  expect_error(
    cvi(data.frame(item1 = c("a", "b", "c"))),
    "numeric"
  )
})

test_that("print and summary methods work", {
  ratings <- data.frame(
    item1 = c(4, 4, 3, 4),
    item2 = c(4, 4, 4, 4)
  )

  result <- cvi(ratings)

  # Should not error
  expect_output(print(result))
  expect_output(summary(result))
  expect_output(summary(result, show_experts = TRUE))
})

test_that("plot method works", {
  ratings <- data.frame(
    item1 = c(4, 4, 3, 4),
    item2 = c(4, 4, 4, 4)
  )

  result <- cvi(ratings)

  # Should not error (returns invisible)
  expect_silent(plot(result))
})

test_that("binary scale detection works", {
  # 0/1 coding
  ratings_01 <- data.frame(
    item1 = c(1, 1, 1, 0)
  )
  result_01 <- transform_ratings(ratings_01, scale = "binary")
  expect_equal(result_01[, 1], c(1, 1, 1, 0))

  # 1/2 coding
  ratings_12 <- data.frame(
    item1 = c(2, 2, 2, 1)
  )
  result_12 <- transform_ratings(ratings_12, scale = "binary")
  expect_equal(result_12[, 1], c(1, 1, 1, 0))
})

test_that("handles NA values appropriately", {
  ratings_with_na <- data.frame(
    item1 = c(4, 4, NA, 4),
    item2 = c(4, NA, 4, 4)
  )

  # Should calculate based on non-NA values
  result <- icvi(ratings_with_na)

  expect_equal(result["item1"], c(item1 = 1.0))  # 3/3 non-NA are relevant
  expect_equal(result["item2"], c(item2 = 1.0))  # 3/3 non-NA are relevant
})

test_that("mean_cvr calculates correctly", {
  ratings <- data.frame(
    item1 = c(4, 4, 4, 4, 4),  # CVR = 1.0
    item2 = c(1, 1, 1, 1, 1)   # CVR = -1.0
  )

  # Mean should be (1.0 + -1.0) / 2 = 0
  expect_equal(mean_cvr(ratings), 0.0)
})
