# Tests for edge cases

test_that("single item calculations work correctly", {
  result <- cvi(single_item)

  expect_equal(nrow(result$item_stats), 1)
  expect_equal(result$scale_stats$n_items, 1)

  # I-CVI should be 1.0 (all 5 experts rated 3 or 4)
  expect_equal(result$item_stats$icvi[1], 1.0)
})

test_that("large dataset calculations work", {
  result <- cvi(large_ratings)

  expect_equal(result$scale_stats$n_experts, 20)
  expect_equal(result$scale_stats$n_items, 50)
  expect_equal(nrow(result$item_stats), 50)
  expect_equal(nrow(result$expert_stats), 20)
})

test_that("non-integer numeric values are handled", {
  # Ratings that could result from data processing
  ratings <- data.frame(
    item1 = c(4.0, 4.0, 3.0, 4.0),
    item2 = c(3.0, 3.0, 3.0, 3.0)
  )

  result <- icvi(ratings)

  expect_equal(result["item1"], c(item1 = 1.0))
  expect_equal(result["item2"], c(item2 = 1.0))
})

test_that("column names are preserved", {
  ratings <- data.frame(
    my_custom_item_1 = c(4, 4, 3, 4),
    another_item = c(4, 4, 4, 4),
    third_one = c(2, 3, 2, 3)
  )

  result <- cvi(ratings)

  expect_equal(result$item_stats$item,
               c("my_custom_item_1", "another_item", "third_one"))
})

test_that("default names are generated when not provided", {
  # Matrix without dimnames
  mat <- matrix(
    c(4, 4, 3, 4,
      4, 4, 4, 4),
    nrow = 4,
    ncol = 2
  )

  result <- cvi(mat)

  expect_equal(result$item_stats$item, c("Item1", "Item2"))
  expect_equal(result$expert_stats$expert,
               c("Expert1", "Expert2", "Expert3", "Expert4"))
})

test_that("all-relevant scenario gives I-CVI=1 and CVR=1", {
  result <- cvi(perfect_agreement)

  expect_true(all(result$item_stats$icvi == 1.0))
  expect_true(all(result$item_stats$cvr == 1.0))
  expect_true(all(result$item_stats$ua == 1))
  expect_equal(result$scale_stats$scvi_ave, 1.0)
  expect_equal(result$scale_stats$scvi_ua, 1.0)
})

test_that("no-relevant scenario gives I-CVI=0 and CVR=-1", {
  result <- cvi(no_agreement)

  expect_true(all(result$item_stats$icvi == 0.0))
  expect_true(all(result$item_stats$cvr == -1.0))
  expect_true(all(result$item_stats$ua == 0))
  expect_equal(result$scale_stats$scvi_ave, 0.0)
  expect_equal(result$scale_stats$scvi_ua, 0.0)
})

test_that("I-CVI return type is numeric", {
  ratings <- data.frame(
    item1 = c(4, 4, 3, 4),
    item2 = c(4, 4, 4, 4)
  )

  result <- icvi(ratings)

  expect_type(result, "double")
  expect_true(is.numeric(result))
})

test_that("UA return type is integer", {
  ratings <- data.frame(
    item1 = c(4, 4, 3, 4),
    item2 = c(4, 4, 4, 4)
  )

  result <- ua(ratings)

  expect_type(result, "integer")
})

test_that("CVR return type is numeric", {
  ratings <- data.frame(
    item1 = c(4, 4, 3, 4, 4),
    item2 = c(4, 4, 4, 4, 4)
  )

  result <- cvr(ratings)

  expect_type(result, "double")
  expect_true(is.numeric(result))
})

test_that("3-point scale works through all functions", {
  result <- cvi(ratings_3point, scale = "3point")

  # item1: all 4 experts rated 2 or 3, I-CVI = 1.0
  expect_equal(result$item_stats$icvi[1], 1.0)

  # item2: 3 of 4 rated 2 or 3, I-CVI = 0.75
  expect_equal(result$item_stats$icvi[2], 0.75)

  # item3: 1 of 4 rated 2 or 3, I-CVI = 0.25
  expect_equal(result$item_stats$icvi[3], 0.25)

  # Direct function calls
  icvi_result <- icvi(ratings_3point, scale = "3point")
  expect_equal(icvi_result["item1"], c(item1 = 1.0))
  expect_equal(icvi_result["item2"], c(item2 = 0.75))

  ua_result <- ua(ratings_3point, scale = "3point")
  expect_equal(ua_result["item1"], c(item1 = 1L))
  expect_equal(ua_result["item3"], c(item3 = 0L))
})

test_that("custom scale edge cases work", {
  # 5-point scale with only highest value relevant
  ratings <- data.frame(
    item1 = c(5, 5, 5, 5),  # All relevant
    item2 = c(5, 4, 3, 2),  # Only 1 relevant
    item3 = c(4, 4, 4, 4)   # None relevant with only 5 as relevant
  )

  result <- icvi(ratings, scale = "custom", relevant_values = c(5))

  expect_equal(result["item1"], c(item1 = 1.0))
  expect_equal(result["item2"], c(item2 = 0.25))
  expect_equal(result["item3"], c(item3 = 0.0))
})

test_that("proportion_relevance handles edge cases", {
  # Expert who rates everything as relevant
  ratings <- data.frame(
    item1 = c(4, 1, 4),
    item2 = c(4, 1, 4),
    item3 = c(4, 1, 4)
  )

  result <- proportion_relevance(ratings)

  expect_equal(result["Expert1"], c(Expert1 = 1.0))
  expect_equal(result["Expert2"], c(Expert2 = 0.0))
  expect_equal(result["Expert3"], c(Expert3 = 1.0))
})

test_that("binary scale with 0/1 works correctly", {
  result <- icvi(binary_ratings, scale = "binary")

  expect_equal(result["item1"], c(item1 = 1.0))
  expect_equal(result["item2"], c(item2 = 0.5))
  expect_equal(result["item3"], c(item3 = 0.0))
})

test_that("binary scale with 1/2 works correctly", {
  ratings_12 <- data.frame(
    item1 = c(2, 2, 2, 2),  # All relevant (2 is high)
    item2 = c(2, 2, 1, 1),  # Half relevant
    item3 = c(1, 1, 1, 1)   # None relevant
  )

  result <- icvi(ratings_12, scale = "binary")

  expect_equal(result["item1"], c(item1 = 1.0))
  expect_equal(result["item2"], c(item2 = 0.5))
  expect_equal(result["item3"], c(item3 = 0.0))
})

test_that("expert names are preserved from row names", {
  ratings <- data.frame(
    item1 = c(4, 4, 3, 4),
    item2 = c(4, 4, 4, 4)
  )
  rownames(ratings) <- c("Dr. Smith", "Dr. Jones", "Dr. Brown", "Dr. Davis")

  result <- cvi(ratings)

  expect_equal(result$expert_stats$expert,
               c("Dr. Smith", "Dr. Jones", "Dr. Brown", "Dr. Davis"))
})
