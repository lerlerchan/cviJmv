# Helper functions and test fixtures for contentvalidity tests

#' Create Random Test Ratings
#'
#' Generates a data frame of random ratings for testing.
#'
#' @param n_experts Number of experts (rows)
#' @param n_items Number of items (columns)
#' @param scale Character string specifying scale type
#' @param seed Random seed for reproducibility
#' @return A data frame of ratings
create_test_ratings <- function(n_experts = 5, n_items = 4,
                                 scale = "4point", seed = NULL) {
  if (!is.null(seed)) set.seed(seed)

  if (scale == "4point") {
    values <- 1:4
  } else if (scale == "3point") {
    values <- 1:3
  } else if (scale == "binary") {
    values <- 0:1
  } else {
    values <- 1:5
  }

  ratings <- as.data.frame(
    matrix(
      sample(values, n_experts * n_items, replace = TRUE),
      nrow = n_experts,
      ncol = n_items
    )
  )
  colnames(ratings) <- paste0("item", seq_len(n_items))
  rownames(ratings) <- paste0("expert", seq_len(n_experts))

  return(ratings)
}

# Perfect agreement fixture - all experts rate all items as relevant
perfect_agreement <- data.frame(
  item1 = c(4, 4, 4, 4, 4),
  item2 = c(4, 4, 4, 4, 4),
  item3 = c(3, 4, 3, 4, 3)
)

# No agreement fixture - no experts rate items as relevant
no_agreement <- data.frame(
  item1 = c(1, 1, 1, 1, 1),
  item2 = c(2, 1, 2, 1, 2),
  item3 = c(1, 2, 1, 2, 1)
)

# Mixed ratings fixture - various I-CVI values
# I-CVI values: 1.0, 0.8, 0.6, 0.4, 0.2, 0.0
mixed_ratings <- data.frame(
  item_100 = c(4, 4, 4, 4, 4),  # I-CVI = 1.0 (5/5)
  item_080 = c(4, 4, 4, 4, 1),  # I-CVI = 0.8 (4/5)
  item_060 = c(4, 4, 4, 1, 1),  # I-CVI = 0.6 (3/5)
  item_040 = c(4, 4, 1, 1, 1),  # I-CVI = 0.4 (2/5)
  item_020 = c(4, 1, 1, 1, 1),  # I-CVI = 0.2 (1/5)
  item_000 = c(1, 1, 1, 1, 1)   # I-CVI = 0.0 (0/5)
)

# 3-point scale fixture
ratings_3point <- data.frame(
  item1 = c(3, 3, 2, 3),  # I-CVI = 1.0
  item2 = c(2, 2, 2, 1),  # I-CVI = 0.75
  item3 = c(1, 1, 2, 1)   # I-CVI = 0.25
)

# Binary scale fixture (0/1)
binary_ratings <- data.frame(
  item1 = c(1, 1, 1, 1),  # All relevant
  item2 = c(1, 1, 0, 0),  # Half relevant
  item3 = c(0, 0, 0, 0)   # None relevant
)

# Ratings with NA values (5 experts, 3 items) - no all-NA rows
fixture_ratings_with_na <- data.frame(
  item1 = c(4, 4, NA, 4, 4),  # 4 valid, all relevant
  item2 = c(4, NA, 4, NA, 1), # 3 valid, 2 relevant
  item3 = c(NA, NA, 4, 4, 4)  # 3 valid, all relevant
)

# Large dataset fixture
large_ratings <- create_test_ratings(n_experts = 20, n_items = 50, seed = 42)

# Single item fixture
single_item <- data.frame(
  item1 = c(4, 4, 3, 4, 4)
)

# Matrix input fixture
matrix_ratings <- matrix(
  c(4, 4, 3, 4,
    4, 4, 4, 4,
    2, 3, 2, 3),
  nrow = 4,
  ncol = 3,
  dimnames = list(
    paste0("Expert", 1:4),
    paste0("Item", 1:3)
  )
)

# Custom scale fixture (5-point)
custom_scale_ratings <- data.frame(
  item1 = c(5, 5, 4, 5),  # All relevant (4,5)
  item2 = c(4, 3, 4, 5),  # 3 relevant
  item3 = c(2, 3, 2, 1)   # 0 relevant
)
