#' Validate Rating Data
#'
#' Validates that the input data meets requirements for content validity analysis.
#'
#' @param data A data frame or matrix of ratings.
#' @param min_experts Minimum number of experts (rows) required. Default is 3.
#' @param min_items Minimum number of items (columns) required. Default is 1.
#' @param allow_na Logical. Whether to allow NA values. Default is TRUE.
#'
#' @return Invisibly returns TRUE if validation passes. Throws an error otherwise.
#'
#' @details
#' This function checks:
#' \itemize{
#'   \item Data is a data frame or matrix
#'   \item All values are numeric
#'   \item Minimum number of experts (rows) is met
#'   \item Minimum number of items (columns) is met
#'   \item No completely empty rows or columns (all NA)
#' }
#'
#' @keywords internal
validate_data <- function(data,
                          min_experts = 3,
                          min_items = 1,
                          allow_na = TRUE) {

  # Check data type
  if (!is.data.frame(data) && !is.matrix(data)) {
    stop("'data' must be a data frame or matrix", call. = FALSE)
  }

  # Convert to matrix for checking
  if (is.data.frame(data)) {
    data_matrix <- as.matrix(data)
  } else {
    data_matrix <- data
  }

  # Check for empty data
  if (nrow(data_matrix) == 0 || ncol(data_matrix) == 0) {
    stop("'data' cannot be empty", call. = FALSE)
  }

  # Check numeric
  if (!is.numeric(data_matrix)) {
    stop("All values in 'data' must be numeric", call. = FALSE)
  }

  # Check minimum dimensions
  n_experts <- nrow(data_matrix)
  n_items <- ncol(data_matrix)

  if (n_experts < min_experts) {
    stop(sprintf(
      "At least %d experts (rows) required. Found %d.",
      min_experts, n_experts
    ), call. = FALSE)
  }

  if (n_items < min_items) {
    stop(sprintf(
      "At least %d item(s) (columns) required. Found %d.",
      min_items, n_items
    ), call. = FALSE)
  }

  # Check for NA values
  if (!allow_na && anyNA(data_matrix)) {
    stop("'data' contains NA values. Set allow_na = TRUE to allow missing values.",
         call. = FALSE)
  }

  # Check for completely empty rows or columns
  empty_rows <- apply(data_matrix, 1, function(x) all(is.na(x)))
  empty_cols <- apply(data_matrix, 2, function(x) all(is.na(x)))

  if (any(empty_rows)) {
    stop(sprintf(
      "Row(s) %s contain only NA values. Remove or fill these rows.",
      paste(which(empty_rows), collapse = ", ")
    ), call. = FALSE)
  }

  if (any(empty_cols)) {
    stop(sprintf(
      "Column(s) %s contain only NA values. Remove or fill these columns.",
      paste(which(empty_cols), collapse = ", ")
    ), call. = FALSE)
  }

  invisible(TRUE)
}


#' Validate Scale Argument
#'
#' Validates the scale argument and relevant_values combination.
#'
#' @param scale The scale argument.
#' @param relevant_values The relevant_values argument.
#' @param data Optional data to validate against.
#'
#' @return Invisibly returns TRUE if validation passes.
#'
#' @keywords internal
validate_scale <- function(scale, relevant_values = NULL, data = NULL) {

  valid_scales <- c("4point", "3point", "binary", "custom")

  if (!scale %in% valid_scales) {
    stop(sprintf(
      "'scale' must be one of: %s",
      paste(valid_scales, collapse = ", ")
    ), call. = FALSE)
  }

  if (scale == "custom" && is.null(relevant_values)) {
    stop("'relevant_values' must be provided when scale = 'custom'", call. = FALSE)
  }

  if (scale != "custom" && !is.null(relevant_values)) {
    warning(
      "'relevant_values' is ignored when scale is not 'custom'",
      call. = FALSE
    )
  }

  # If data provided, validate values against scale
  if (!is.null(data)) {
    data_matrix <- as.matrix(data)
    data_vals <- unique(as.vector(data_matrix[!is.na(data_matrix)]))

    if (scale == "4point") {
      if (any(data_vals < 1 | data_vals > 4)) {
        warning(
          "Some values are outside 1-4 range for 4-point scale. ",
          "Consider using scale = 'custom'.",
          call. = FALSE
        )
      }
    } else if (scale == "3point") {
      if (any(data_vals < 1 | data_vals > 3)) {
        warning(
          "Some values are outside 1-3 range for 3-point scale. ",
          "Consider using scale = 'custom'.",
          call. = FALSE
        )
      }
    } else if (scale == "binary") {
      if (length(data_vals) > 2) {
        stop(
          "Binary scale expects only 2 unique values. ",
          "Found: ", paste(sort(data_vals), collapse = ", "),
          call. = FALSE
        )
      }
    }
  }

  invisible(TRUE)
}


#' Prepare Data Matrix
#'
#' Converts input data to a numeric matrix and optionally validates it.
#'
#' @param data A data frame or matrix.
#' @param validate Logical. Whether to validate the data. Default is TRUE.
#' @param ... Additional arguments passed to \code{validate_data}.
#'
#' @return A numeric matrix.
#'
#' @keywords internal
prepare_data <- function(data, validate = TRUE, ...) {

  if (validate) {
    validate_data(data, ...)
  }

  if (is.data.frame(data)) {
    data_matrix <- as.matrix(data)
  } else {
    data_matrix <- data
  }

  # Ensure numeric mode
  storage.mode(data_matrix) <- "numeric"

  return(data_matrix)
}
