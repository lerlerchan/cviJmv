# Tests for print, summary, and plot methods

test_that("print.cvi_result returns invisible object", {
  ratings <- data.frame(
    item1 = c(4, 4, 3, 4),
    item2 = c(4, 4, 4, 4)
  )
  result <- cvi(ratings)

  # Capture print output and check it returns invisibly
  output <- capture.output(returned <- print(result))

  expect_identical(returned, result)
  expect_true(length(output) > 0)
})

test_that("summary.cvi_result returns list structure", {
  ratings <- data.frame(
    item1 = c(4, 4, 3, 4),
    item2 = c(4, 4, 4, 4),
    item3 = c(2, 3, 2, 3)
  )
  result <- cvi(ratings)

  summary_output <- capture.output(summary_result <- summary(result))

  expect_type(summary_result, "list")
  expect_named(summary_result, c("item_stats", "expert_stats", "scale_stats"))
})

test_that("plot.cvi_result works with type='cvr'", {
  ratings <- data.frame(
    item1 = c(4, 4, 3, 4),
    item2 = c(4, 4, 4, 4)
  )
  result <- cvi(ratings, compute_cvr = TRUE)

  # Should not error
  expect_silent(plot(result, type = "cvr"))
})

test_that("plot.cvi_result works with type='experts'", {
  ratings <- data.frame(
    item1 = c(4, 4, 3, 4),
    item2 = c(4, 4, 4, 4)
  )
  result <- cvi(ratings)

  # Should not error
  expect_silent(plot(result, type = "experts"))
})

test_that("plot.cvi_result errors when CVR not computed", {
  ratings <- data.frame(
    item1 = c(4, 4, 3, 4),
    item2 = c(4, 4, 4, 4)
  )
  result <- cvi(ratings, compute_cvr = FALSE)

  expect_error(
    plot(result, type = "cvr"),
    "CVR was not computed"
  )
})

test_that("plot.cvi_result works with custom threshold", {
  ratings <- data.frame(
    item1 = c(4, 4, 3, 4),
    item2 = c(4, 4, 4, 4)
  )
  result <- cvi(ratings)

  # Should not error with custom threshold
  expect_silent(plot(result, threshold = 0.90))
  expect_silent(plot(result, type = "cvr", threshold = 0.5))
})

test_that("summary.cvi_result with show_experts=TRUE shows expert info", {
  ratings <- data.frame(
    item1 = c(4, 4, 3, 4),
    item2 = c(4, 4, 4, 4)
  )
  result <- cvi(ratings)

  output <- capture.output(summary(result, show_experts = TRUE))

  # Check that expert section appears
  expect_true(any(grepl("EXPERT-LEVEL", output)))
})

test_that("summary.cvi_result without show_experts hides expert info", {
  ratings <- data.frame(
    item1 = c(4, 4, 3, 4),
    item2 = c(4, 4, 4, 4)
  )
  result <- cvi(ratings)

  output <- capture.output(summary(result, show_experts = FALSE))

  # Check that expert section does not appear
  expect_false(any(grepl("EXPERT-LEVEL", output)))
})

test_that("print.cvi_result displays correct interpretation", {
  # Excellent S-CVI/Ave (>= 0.90)
  excellent_ratings <- data.frame(
    item1 = c(4, 4, 4, 4),
    item2 = c(4, 4, 4, 4)
  )
  excellent_result <- cvi(excellent_ratings)
  output_excellent <- capture.output(print(excellent_result))
  expect_true(any(grepl("Excellent", output_excellent)))

  # Below threshold S-CVI/Ave (< 0.80)
  poor_ratings <- data.frame(
    item1 = c(4, 1, 1, 1),
    item2 = c(4, 1, 1, 1)
  )
  poor_result <- cvi(poor_ratings)
  output_poor <- capture.output(print(poor_result))
  expect_true(any(grepl("Below threshold", output_poor)))
})

test_that("plot.cvi_result returns bar midpoints invisibly", {
  ratings <- data.frame(
    item1 = c(4, 4, 3, 4),
    item2 = c(4, 4, 4, 4)
  )
  result <- cvi(ratings)

  bp <- plot(result)

  expect_true(is.numeric(bp))
  expect_equal(length(bp), 2)  # Two items
})

test_that("summary output includes status interpretation", {
  ratings <- mixed_ratings

  result <- cvi(ratings)
  output <- capture.output(summary(result))

  # Check status key appears
  expect_true(any(grepl("Status key", output)))
  expect_true(any(grepl("OK:", output)))
  expect_true(any(grepl("Revise:", output)))
})
