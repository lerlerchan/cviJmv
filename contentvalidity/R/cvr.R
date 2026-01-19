#' Content Validity Ratio (CVR)
#'
#' Calculates Lawshe's Content Validity Ratio for each item.
#'
#' @param data A data frame, matrix, or binary matrix of ratings. If not
#'   binary, will be transformed using the specified scale.
#' @param scale Character string specifying the rating scale type. One of:
#'   \describe{
#'     \item{"4point"}{4-point scale (1-4), values 3-4 = relevant (default)}
#'     \item{"3point"}{3-point scale (1-3), values 2-3 = relevant}
#'     \item{"binary"}{Binary scale (0/1 or 1/2), highest value = relevant}
#'     \item{"custom"}{Custom scale, requires \code{relevant_values}}
#'   }
#' @param relevant_values Numeric vector of values considered "relevant".
#'   Required when \code{scale = "custom"}, ignored otherwise.
#' @param is_binary Logical. If TRUE, treats input as already transformed
#'   to binary (0/1). Default is FALSE.
#'
#' @return A named numeric vector of CVR values, one per item. Values
#'   range from -1 to +1.
#'
#' @details
#' Lawshe's Content Validity Ratio (CVR) is calculated as:
#'
#' \deqn{CVR = \frac{n_e - N/2}{N/2}}{CVR = (ne - N/2) / (N/2)}
#'
#' where:
#' \itemize{
#'   \item \eqn{n_e}{ne} = number of experts rating the item as "essential" (relevant)
#'   \item \eqn{N} = total number of experts
#' }
#'
#' ## Interpretation
#' \itemize{
#'   \item CVR = +1: All experts agree the item is essential
#'   \item CVR = 0: Exactly half the experts rate the item as essential
#'   \item CVR = -1: No experts rate the item as essential
#' }
#'
#' ## Critical Values
#' Lawshe (1975) provided critical values for CVR based on the number
#' of panelists. An item should be retained if its CVR exceeds the
#' critical value:
#' \itemize{
#'   \item 5 experts: CVR >= 0.99
#'   \item 6 experts: CVR >= 0.99
#'   \item 7 experts: CVR >= 0.99
#'   \item 8 experts: CVR >= 0.75
#'   \item 9 experts: CVR >= 0.78
#'   \item 10 experts: CVR >= 0.62
#'   \item 15 experts: CVR >= 0.49
#'   \item 20 experts: CVR >= 0.42
#' }
#'
#' @references
#' Lawshe, C. H. (1975). A quantitative approach to content validity.
#' Personnel Psychology, 28(4), 563-575.
#'
#' @examples
#' # 4-point scale example
#' ratings <- data.frame(
#'   item1 = c(4, 4, 3, 4, 3),
#'   item2 = c(4, 4, 4, 4, 4),
#'   item3 = c(2, 1, 2, 3, 2)
#' )
#' cvr(ratings)
#'
#' # Already binary data
#' binary_data <- data.frame(
#'   item1 = c(1, 1, 1, 1, 0),
#'   item2 = c(1, 1, 1, 1, 1),
#'   item3 = c(0, 0, 0, 1, 0)
#' )
#' cvr(binary_data, is_binary = TRUE)
#'
#' @seealso \code{\link{cvi}}, \code{\link{cvr_critical}}
#'
#' @export
cvr <- function(data,
                scale = c("4point", "3point", "binary", "custom"),
                relevant_values = NULL,
                is_binary = FALSE) {

  if (is_binary) {
    # Data is already binary
    if (is.data.frame(data)) {
      binary_matrix <- as.matrix(data)
    } else {
      binary_matrix <- data
    }
  } else {
    # Transform to binary
    scale <- match.arg(scale)
    data_matrix <- prepare_data(data)
    binary_matrix <- transform_ratings(data_matrix, scale = scale,
                                        relevant_values = relevant_values)
  }

  # Calculate CVR for each item (column)
  n_experts_per_item <- colSums(!is.na(binary_matrix))
  n_essential <- colSums(binary_matrix, na.rm = TRUE)

  # Lawshe's formula: CVR = (ne - N/2) / (N/2)
  cvr_values <- (n_essential - n_experts_per_item / 2) / (n_experts_per_item / 2)

  # Add names
  names(cvr_values) <- colnames(binary_matrix)
  if (is.null(names(cvr_values))) {
    names(cvr_values) <- paste0("Item", seq_along(cvr_values))
  }

  return(cvr_values)
}


#' Critical Values for CVR
#'
#' Returns the critical value for CVR based on the number of experts.
#'
#' @param n_experts Integer. The number of experts in the panel.
#' @param alpha Numeric. Significance level. Currently only 0.05 is supported.
#'
#' @return A numeric value representing the minimum CVR for statistical
#'   significance at the specified alpha level.
#'
#' @details
#' Critical values are based on Lawshe (1975) for one-tailed test at
#' alpha = 0.05. Values are linearly interpolated for panel sizes
#' not in the original table.
#'
#' @references
#' Lawshe, C. H. (1975). A quantitative approach to content validity.
#' Personnel Psychology, 28(4), 563-575.
#'
#' @examples
#' # Get critical value for 10 experts
#' cvr_critical(10)
#'
#' # Check if item meets threshold
#' my_cvr <- 0.6
#' n_exp <- 8
#' if (my_cvr >= cvr_critical(n_exp)) {
#'   message("Item meets CVR threshold")
#' }
#'
#' @export
cvr_critical <- function(n_experts, alpha = 0.05) {

  if (alpha != 0.05) {
    stop("Only alpha = 0.05 is currently supported", call. = FALSE)
  }

  if (n_experts < 5) {
    warning("CVR critical values are not defined for fewer than 5 experts",
            call. = FALSE)
    return(NA_real_)
  }

  # Lawshe's critical values at alpha = 0.05 (one-tailed)
  critical_table <- data.frame(
    n = c(5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 20, 25, 30, 35, 40),
    cvr = c(0.99, 0.99, 0.99, 0.75, 0.78, 0.62, 0.59, 0.56, 0.54, 0.51,
            0.49, 0.42, 0.37, 0.33, 0.31, 0.29)
  )

  # Exact match
  if (n_experts %in% critical_table$n) {
    return(critical_table$cvr[critical_table$n == n_experts])
  }

  # Interpolation for values between table entries
  if (n_experts > max(critical_table$n)) {
    # Extrapolate for very large panels
    return(critical_table$cvr[nrow(critical_table)])
  }

  # Linear interpolation
  idx <- max(which(critical_table$n < n_experts))
  n1 <- critical_table$n[idx]
  n2 <- critical_table$n[idx + 1]
  cvr1 <- critical_table$cvr[idx]
  cvr2 <- critical_table$cvr[idx + 1]

  # Interpolate
  critical_val <- cvr1 + (cvr2 - cvr1) * (n_experts - n1) / (n2 - n1)

  return(critical_val)
}


#' Mean Content Validity Ratio
#'
#' Calculates the mean CVR across all items.
#'
#' @inheritParams cvr
#'
#' @return A single numeric value representing the mean CVR.
#'
#' @examples
#' ratings <- data.frame(
#'   item1 = c(4, 4, 3, 4, 3),
#'   item2 = c(4, 4, 4, 4, 4),
#'   item3 = c(3, 3, 3, 3, 2)
#' )
#' mean_cvr(ratings)
#'
#' @seealso \code{\link{cvr}}
#'
#' @export
mean_cvr <- function(data,
                     scale = c("4point", "3point", "binary", "custom"),
                     relevant_values = NULL,
                     is_binary = FALSE) {

  cvr_values <- cvr(data, scale = scale, relevant_values = relevant_values,
                    is_binary = is_binary)
  return(mean(cvr_values, na.rm = TRUE))
}
