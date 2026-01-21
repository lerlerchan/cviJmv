# Integration tests

test_that("cvi results match individual function results", {
  ratings <- data.frame(
    item1 = c(4, 4, 3, 4, 4),
    item2 = c(4, 4, 4, 4, 4),
    item3 = c(2, 3, 2, 3, 2)
  )

  cvi_result <- cvi(ratings)

  # Check I-CVI matches icvi()
  icvi_direct <- icvi(ratings)
  expect_equal(
    as.numeric(icvi_direct),
    cvi_result$item_stats$icvi
  )

  # Check UA matches ua()
  ua_direct <- ua(ratings)
  expect_equal(
    as.integer(ua_direct),
    cvi_result$item_stats$ua
  )

  # Check CVR matches cvr()
  cvr_direct <- cvr(ratings)
  expect_equal(
    as.numeric(cvr_direct),
    cvi_result$item_stats$cvr
  )

  # Check S-CVI/Ave matches scvi_ave()
  scvi_ave_direct <- scvi_ave(ratings)
  expect_equal(scvi_ave_direct, cvi_result$scale_stats$scvi_ave)

  # Check S-CVI/UA matches scvi_ua()
  scvi_ua_direct <- scvi_ua(ratings)
  expect_equal(scvi_ua_direct, cvi_result$scale_stats$scvi_ua)

  # Check mean CVR matches mean_cvr()
  mean_cvr_direct <- mean_cvr(ratings)
  expect_equal(mean_cvr_direct, cvi_result$scale_stats$mean_cvr)

  # Check proportion_relevance matches expert_stats
  prop_rel_direct <- proportion_relevance(ratings)
  expect_equal(
    as.numeric(prop_rel_direct),
    cvi_result$expert_stats$proportion_relevance
  )
})

test_that("scale parameter consistency across functions", {
  ratings_3pt <- data.frame(
    item1 = c(3, 3, 2, 3),
    item2 = c(2, 2, 2, 1)
  )

  # All functions should use 3-point scale consistently
  cvi_result <- cvi(ratings_3pt, scale = "3point")
  icvi_result <- icvi(ratings_3pt, scale = "3point")
  ua_result <- ua(ratings_3pt, scale = "3point")
  cvr_result <- cvr(ratings_3pt, scale = "3point")
  scvi_ave_result <- scvi_ave(ratings_3pt, scale = "3point")
  scvi_ua_result <- scvi_ua(ratings_3pt, scale = "3point")

  expect_equal(as.numeric(icvi_result), cvi_result$item_stats$icvi)
  expect_equal(as.integer(ua_result), cvi_result$item_stats$ua)
  expect_equal(as.numeric(cvr_result), cvi_result$item_stats$cvr)
  expect_equal(scvi_ave_result, cvi_result$scale_stats$scvi_ave)
  expect_equal(scvi_ua_result, cvi_result$scale_stats$scvi_ua)
})

test_that("documented example produces expected output", {
  # Example from cvi() documentation
  ratings <- data.frame(
    item1 = c(4, 4, 3, 4, 3),
    item2 = c(4, 4, 4, 4, 4),
    item3 = c(3, 2, 2, 3, 2),
    item4 = c(4, 3, 4, 4, 3)
  )

  result <- cvi(ratings)

  # Verify structure
  expect_s3_class(result, "cvi_result")
  expect_equal(result$scale_stats$n_experts, 5)
  expect_equal(result$scale_stats$n_items, 4)

  # item2 should have perfect agreement (all 4s)
  expect_equal(result$item_stats$icvi[result$item_stats$item == "item2"], 1.0)
  expect_equal(result$item_stats$ua[result$item_stats$item == "item2"], 1L)

  # item3 has only 2 of 5 relevant (3 or 4): I-CVI = 0.4
  expect_equal(result$item_stats$icvi[result$item_stats$item == "item3"], 0.4)
})

test_that("binary data consistency", {
  # Pre-transformed binary data
  binary_data <- data.frame(
    item1 = c(1, 1, 1, 1),
    item2 = c(1, 1, 0, 0),
    item3 = c(0, 0, 0, 0)
  )

  # Using binary scale
  result_binary <- cvi(binary_data, scale = "binary")

  # Using cvr with is_binary
  cvr_binary <- cvr(binary_data, is_binary = TRUE)

  # Results should match
  expect_equal(result_binary$item_stats$cvr, as.numeric(cvr_binary))
})

test_that("NA handling consistency across functions", {
  ratings_na <- data.frame(
    item1 = c(4, 4, NA, 4),
    item2 = c(4, NA, 4, 4)
  )

  cvi_result <- cvi(ratings_na)
  icvi_result <- icvi(ratings_na)
  ua_result <- ua(ratings_na)
  cvr_result <- cvr(ratings_na)

  # All should handle NA consistently
  expect_equal(as.numeric(icvi_result), cvi_result$item_stats$icvi)
  expect_equal(as.integer(ua_result), cvi_result$item_stats$ua)
  expect_equal(as.numeric(cvr_result), cvi_result$item_stats$cvr)
})

test_that("custom scale consistency", {
  ratings_5pt <- data.frame(
    q1 = c(5, 5, 4, 5),
    q2 = c(4, 4, 4, 5),
    q3 = c(3, 2, 4, 3)
  )

  # All functions with custom scale
  cvi_result <- cvi(ratings_5pt, scale = "custom", relevant_values = c(4, 5))
  icvi_result <- icvi(ratings_5pt, scale = "custom", relevant_values = c(4, 5))
  cvr_result <- cvr(ratings_5pt, scale = "custom", relevant_values = c(4, 5))

  expect_equal(as.numeric(icvi_result), cvi_result$item_stats$icvi)
  expect_equal(as.numeric(cvr_result), cvi_result$item_stats$cvr)
})

test_that("transform_ratings output used correctly by cvi", {
  ratings <- data.frame(
    item1 = c(4, 4, 3, 4),
    item2 = c(2, 1, 2, 1)
  )

  cvi_result <- cvi(ratings)
  transform_result <- transform_ratings(ratings, scale = "4point")

  # Binary data stored in cvi_result should match transform_ratings output
  expect_equal(cvi_result$binary_data, transform_result)
})

test_that("get_relevant_values internal function works correctly", {
  expect_equal(get_relevant_values("4point"), c(3, 4))
  expect_equal(get_relevant_values("3point"), c(2, 3))
  expect_equal(get_relevant_values("binary"), 1)

  expect_error(get_relevant_values("5point"))
})

test_that("complete workflow produces valid output", {
  # Simulate a complete content validity analysis workflow
  ratings <- create_test_ratings(n_experts = 10, n_items = 8, seed = 123)

  # Run analysis
  result <- cvi(ratings, scale = "4point", compute_cvr = TRUE)

  # Validate output structure
  expect_s3_class(result, "cvi_result")
  expect_equal(nrow(result$item_stats), 8)
  expect_equal(nrow(result$expert_stats), 10)

  # All I-CVI values should be between 0 and 1
  expect_true(all(result$item_stats$icvi >= 0))
  expect_true(all(result$item_stats$icvi <= 1))

  # All CVR values should be between -1 and 1
  expect_true(all(result$item_stats$cvr >= -1))
  expect_true(all(result$item_stats$cvr <= 1))

  # S-CVI values should be between 0 and 1
  expect_true(result$scale_stats$scvi_ave >= 0)
  expect_true(result$scale_stats$scvi_ave <= 1)
  expect_true(result$scale_stats$scvi_ua >= 0)
  expect_true(result$scale_stats$scvi_ua <= 1)

  # Print and summary should work
  expect_output(print(result))
  expect_output(summary(result))

  # Plot should work without error
  expect_silent(plot(result))
})
