#' Transform Ratings to Binary Relevance
#'
#' Converts expert ratings to binary values indicating whether each item
#' is rated as relevant (1) or not relevant (0) by each expert.
#'
#' @param data A data frame or matrix of ratings where rows are experts and
#'   columns are items.
#' @param scale Character string specifying the rating scale type. One of:
#'   \describe{
#'     \item{"4point"}{4-point scale (1-4), values 3-4 = relevant (default)}
#'     \item{"3point"}{3-point scale (1-3), values 2-3 = relevant}
#'     \item{"binary"}{Binary scale (0/1 or 1/2), highest value = relevant}
#'     \item{"custom"}{Custom scale, requires \code{relevant_values}}
#'   }
#' @param relevant_values Numeric vector of values considered "relevant".
#'   Required when \code{scale = "custom"}, ignored otherwise.
#'
#' @return A matrix of binary values (0/1) with the same dimensions as input.
#'   1 indicates the rating was considered "relevant", 0 indicates "not relevant".
#'
#' @details
#' Content validity calculations require binary classification of ratings.
#' This function supports common rating scale conventions:
#' \itemize{
#'   \item \strong{4-point scale}: Typically 1 = not relevant, 2 = somewhat relevant,
#'     3 = quite relevant, 4 = highly relevant. Values 3-4 are coded as relevant.
#'   \item \strong{3-point scale}: Values 2-3 are coded as relevant.
#'   \item \strong{Binary scale}: Automatically detects 0/1 or 1/2 coding.
#'   \item \strong{Custom}: Any values in \code{relevant_values} are coded as relevant.
#' }
#'
#' @examples
#' # 4-point scale example
#' ratings <- data.frame(
#'   item1 = c(3, 4, 2, 4),
#'   item2 = c(4, 4, 4, 4),
#'   item3 = c(2, 1, 2, 3)
#' )
#' transform_ratings(ratings, scale = "4point")
#'
#' # Custom scale (5-point, values 4-5 = relevant)
#' ratings_5pt <- data.frame(
#'   item1 = c(4, 5, 3, 5),
#'   item2 = c(5, 5, 4, 5)
#' )
#' transform_ratings(ratings_5pt, scale = "custom", relevant_values = c(4, 5))
#'
#' @export
transform_ratings <- function(data,
                              scale = c("4point", "3point", "binary", "custom"),
                              relevant_values = NULL) {

  # Match argument

scale <- match.arg(scale)

  # Convert to matrix if data frame
  if (is.data.frame(data)) {
    data_matrix <- as.matrix(data)
  } else if (is.matrix(data)) {
    data_matrix <- data
  } else {
    stop("'data' must be a data frame or matrix", call. = FALSE)
  }

  # Ensure numeric
  if (!is.numeric(data_matrix)) {
    stop("All values in 'data' must be numeric", call. = FALSE)
  }

  # Determine relevant values based on scale
  if (scale == "custom") {
    if (is.null(relevant_values)) {
      stop("'relevant_values' must be provided when scale = 'custom'", call. = FALSE)
    }
    if (!is.numeric(relevant_values)) {
      stop("'relevant_values' must be a numeric vector", call. = FALSE)
    }
    rel_vals <- relevant_values
  } else if (scale == "4point") {
    rel_vals <- c(3, 4)
  } else if (scale == "3point") {
    rel_vals <- c(2, 3)
  } else if (scale == "binary") {
    # Detect binary coding (0/1 or 1/2)
    unique_vals <- sort(unique(as.vector(data_matrix[!is.na(data_matrix)])))
    if (length(unique_vals) > 2) {
      stop("Binary scale expects only 2 unique values", call. = FALSE)
    }
    rel_vals <- max(unique_vals)
  }

  # Transform to binary
  binary_matrix <- matrix(
    as.integer(data_matrix %in% rel_vals),
    nrow = nrow(data_matrix),
    ncol = ncol(data_matrix),
    dimnames = dimnames(data_matrix)
  )

  return(binary_matrix)
}


#' Get Default Relevant Values for a Scale
#'
#' Returns the default relevant values for standard rating scales.
#'
#' @param scale Character string specifying the rating scale type.
#'
#' @return A numeric vector of values considered relevant.
#'
#' @keywords internal
get_relevant_values <- function(scale) {
  switch(scale,
    "4point" = c(3, 4),
    "3point" = c(2, 3),
    "binary" = 1,
    stop("Unknown scale type: ", scale, call. = FALSE)
  )
}
