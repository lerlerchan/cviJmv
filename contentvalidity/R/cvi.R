#' Content Validity Analysis
#'
#' Performs comprehensive content validity analysis on expert ratings,
#' computing Item-level CVI (I-CVI), Scale-level CVI (S-CVI), Universal
#' Agreement (UA), and optionally Content Validity Ratio (CVR).
#'
#' @param data A data frame or matrix of ratings where rows are experts
#'   and columns are items. Must contain numeric values.
#' @param scale Character string specifying the rating scale type. One of:
#'   \describe{
#'     \item{"4point"}{4-point scale (1-4), values 3-4 = relevant (default)}
#'     \item{"3point"}{3-point scale (1-3), values 2-3 = relevant}
#'     \item{"binary"}{Binary scale (0/1 or 1/2), highest value = relevant}
#'     \item{"custom"}{Custom scale, requires \code{relevant_values}}
#'   }
#' @param relevant_values Numeric vector of values considered "relevant".
#'   Required when \code{scale = "custom"}, ignored otherwise.
#' @param compute_cvr Logical. Whether to compute Content Validity Ratio.
#'   Default is TRUE.
#'
#' @return An object of class \code{cvi_result} containing:
#'   \describe{
#'     \item{item_stats}{Data frame with per-item statistics:
#'       \itemize{
#'         \item \code{item}: Item name/number
#'         \item \code{relevant_count}: Number of experts rating item as relevant
#'         \item \code{n_experts}: Number of experts who rated this item
#'         \item \code{icvi}: Item-level Content Validity Index
#'         \item \code{ua}: Universal Agreement indicator (1 if all experts agree, 0 otherwise)
#'         \item \code{cvr}: Content Validity Ratio (if computed)
#'       }
#'     }
#'     \item{expert_stats}{Data frame with per-expert statistics:
#'       \itemize{
#'         \item \code{expert}: Expert name/number
#'         \item \code{relevant_count}: Number of items rated as relevant
#'         \item \code{n_items}: Number of items rated
#'         \item \code{proportion_relevance}: Proportion of items rated as relevant
#'       }
#'     }
#'     \item{scale_stats}{List with scale-level statistics:
#'       \itemize{
#'         \item \code{scvi_ave}: S-CVI/Ave (average of I-CVI values)
#'         \item \code{scvi_ua}: S-CVI/UA (proportion of items with UA = 1)
#'         \item \code{mean_cvr}: Mean CVR across items (if computed)
#'         \item \code{n_experts}: Total number of experts
#'         \item \code{n_items}: Total number of items
#'       }
#'     }
#'     \item{binary_data}{The transformed binary rating matrix}
#'     \item{call}{The matched call}
#'   }
#'
#' @details
#' ## Content Validity Index (CVI)
#' The CVI is the proportion of experts who rate an item as content valid
#' (relevant to the construct being measured).
#'
#' \strong{I-CVI (Item-level CVI)}: For each item, the number of experts
#' giving a rating of relevant divided by the total number of experts.
#'
#' \strong{S-CVI/Ave (Scale-level CVI, Average method)}: The mean of all
#' I-CVI values. Recommended threshold: >= 0.90.
#'
#' \strong{S-CVI/UA (Scale-level CVI, Universal Agreement)}: The proportion
#' of items that achieved universal agreement (all experts rated as relevant).
#' Recommended threshold: >= 0.80.
#'
#' ## Universal Agreement (UA)
#' An item has UA = 1 if ALL experts rated it as relevant, otherwise UA = 0.
#'
#' ## Content Validity Ratio (CVR)
#' Lawshe's CVR is calculated as: CVR = (ne - N/2) / (N/2)
#' where ne = number of experts rating "essential" (relevant) and N = total experts.
#' CVR ranges from -1 to +1.
#'
#' @section Interpretation Guidelines:
#' Based on established literature:
#' \itemize{
#'   \item I-CVI >= 0.78 is considered acceptable (Lynn, 1986)
#'   \item S-CVI/Ave >= 0.90 is considered excellent (Polit & Beck, 2006)
#'   \item S-CVI/UA >= 0.80 is considered acceptable
#' }
#'
#' @references
#' Lynn, M. R. (1986). Determination and quantification of content validity.
#' Nursing Research, 35(6), 382-385.
#'
#' Polit, D. F., & Beck, C. T. (2006). The content validity index: Are you
#' sure you know what's being reported? Critique and recommendations.
#' Research in Nursing & Health, 29(5), 489-497.
#'
#' Lawshe, C. H. (1975). A quantitative approach to content validity.
#' Personnel Psychology, 28(4), 563-575.
#'
#' @examples
#' # Example with 4-point scale ratings
#' ratings <- data.frame(
#'   item1 = c(4, 4, 3, 4, 3),
#'   item2 = c(4, 4, 4, 4, 4),
#'   item3 = c(3, 2, 2, 3, 2),
#'   item4 = c(4, 3, 4, 4, 3)
#' )
#'
#' # Run analysis
#' result <- cvi(ratings)
#' print(result)
#' summary(result)
#'
#' # Custom scale (5-point, 4-5 = relevant)
#' ratings_5pt <- data.frame(
#'   q1 = c(5, 5, 4, 5),
#'   q2 = c(4, 4, 4, 5),
#'   q3 = c(3, 2, 4, 3)
#' )
#' result_custom <- cvi(ratings_5pt, scale = "custom", relevant_values = c(4, 5))
#'
#' @export
cvi <- function(data,
                scale = c("4point", "3point", "binary", "custom"),
                relevant_values = NULL,
                compute_cvr = TRUE) {

  # Capture call
  cl <- match.call()

  # Match and validate scale argument
  scale <- match.arg(scale)
  validate_scale(scale, relevant_values, data)

  # Validate and prepare data
  data_matrix <- prepare_data(data, validate = TRUE, min_experts = 3, min_items = 1)

  # Get dimensions
  n_experts <- nrow(data_matrix)
  n_items <- ncol(data_matrix)

  # Get item names
  item_names <- colnames(data_matrix)
  if (is.null(item_names)) {
    item_names <- paste0("Item", seq_len(n_items))
  }

  # Get expert names
  expert_names <- rownames(data_matrix)
  if (is.null(expert_names)) {
    expert_names <- paste0("Expert", seq_len(n_experts))
  }

  # Transform to binary
  binary_matrix <- transform_ratings(data_matrix, scale = scale,
                                      relevant_values = relevant_values)

  # Calculate item statistics
  item_stats <- calculate_item_stats(binary_matrix, compute_cvr = compute_cvr)
  item_stats$item <- item_names

  # Reorder columns
  if (compute_cvr) {
    item_stats <- item_stats[, c("item", "relevant_count", "n_experts",
                                  "icvi", "ua", "cvr")]
  } else {
    item_stats <- item_stats[, c("item", "relevant_count", "n_experts",
                                  "icvi", "ua")]
  }

  # Calculate expert statistics
  expert_stats <- calculate_expert_stats(binary_matrix)
  expert_stats$expert <- expert_names
  expert_stats <- expert_stats[, c("expert", "relevant_count", "n_items",
                                    "proportion_relevance")]

  # Calculate scale statistics
  scale_stats <- list(
    scvi_ave = mean(item_stats$icvi, na.rm = TRUE),
    scvi_ua = mean(item_stats$ua, na.rm = TRUE),
    n_experts = n_experts,
    n_items = n_items
  )

  if (compute_cvr) {
    scale_stats$mean_cvr <- mean(item_stats$cvr, na.rm = TRUE)
  }

  # Create result object
  result <- structure(
    list(
      item_stats = item_stats,
      expert_stats = expert_stats,
      scale_stats = scale_stats,
      binary_data = binary_matrix,
      call = cl
    ),
    class = "cvi_result"
  )

  return(result)
}


#' Calculate Item Statistics
#'
#' Internal function to calculate per-item statistics from binary data.
#'
#' @param binary_matrix A binary matrix (0/1).
#' @param compute_cvr Logical. Whether to compute CVR.
#'
#' @return A data frame with item statistics.
#'
#' @keywords internal
calculate_item_stats <- function(binary_matrix, compute_cvr = TRUE) {

  n_items <- ncol(binary_matrix)

  # Calculate for each item (column)
  relevant_counts <- colSums(binary_matrix, na.rm = TRUE)
  n_experts_per_item <- colSums(!is.na(binary_matrix))

  icvi_values <- relevant_counts / n_experts_per_item

  # UA: 1 if ALL experts rated as relevant (sum equals number of experts)
  ua_values <- as.integer(relevant_counts == n_experts_per_item)

  result <- data.frame(
    relevant_count = relevant_counts,
    n_experts = n_experts_per_item,
    icvi = icvi_values,
    ua = ua_values,
    stringsAsFactors = FALSE
  )

  if (compute_cvr) {
    result$cvr <- cvr(binary_matrix, is_binary = TRUE)
  }

  return(result)
}


#' Calculate Expert Statistics
#'
#' Internal function to calculate per-expert statistics from binary data.
#'
#' @param binary_matrix A binary matrix (0/1).
#'
#' @return A data frame with expert statistics.
#'
#' @keywords internal
calculate_expert_stats <- function(binary_matrix) {

  # Calculate for each expert (row)
  relevant_counts <- rowSums(binary_matrix, na.rm = TRUE)
  n_items_per_expert <- rowSums(!is.na(binary_matrix))

  proportion_relevance <- relevant_counts / n_items_per_expert

  result <- data.frame(
    relevant_count = relevant_counts,
    n_items = n_items_per_expert,
    proportion_relevance = proportion_relevance,
    stringsAsFactors = FALSE
  )

  return(result)
}


#' Item-Level Content Validity Index (I-CVI)
#'
#' Calculates the Item-level Content Validity Index for each item.
#'
#' @inheritParams cvi
#'
#' @return A named numeric vector of I-CVI values, one per item.
#'
#' @details
#' The I-CVI is calculated as the proportion of experts who rated the
#' item as relevant (content valid). Values range from 0 to 1.
#'
#' An I-CVI >= 0.78 is generally considered acceptable when there are
#' 6 or more experts (Lynn, 1986).
#'
#' @examples
#' ratings <- data.frame(
#'   item1 = c(4, 4, 3, 4),
#'   item2 = c(4, 4, 4, 4),
#'   item3 = c(2, 2, 3, 2)
#' )
#' icvi(ratings)
#'
#' @seealso \code{\link{cvi}}, \code{\link{scvi_ave}}, \code{\link{scvi_ua}}
#'
#' @export
icvi <- function(data,
                 scale = c("4point", "3point", "binary", "custom"),
                 relevant_values = NULL) {

  scale <- match.arg(scale)
  data_matrix <- prepare_data(data)

  binary_matrix <- transform_ratings(data_matrix, scale = scale,
                                      relevant_values = relevant_values)

  relevant_counts <- colSums(binary_matrix, na.rm = TRUE)
  n_experts <- colSums(!is.na(binary_matrix))

  icvi_values <- relevant_counts / n_experts

  # Add names
  names(icvi_values) <- colnames(data_matrix)
  if (is.null(names(icvi_values))) {
    names(icvi_values) <- paste0("Item", seq_along(icvi_values))
  }

  return(icvi_values)
}


#' Scale-Level CVI (Average Method)
#'
#' Calculates the Scale-level Content Validity Index using the average method.
#'
#' @inheritParams cvi
#'
#' @return A single numeric value representing S-CVI/Ave.
#'
#' @details
#' S-CVI/Ave is calculated as the mean of all I-CVI values. This represents
#' the average proportion of items rated as content valid across all items.
#'
#' An S-CVI/Ave >= 0.90 is generally considered excellent (Polit & Beck, 2006).
#'
#' @examples
#' ratings <- data.frame(
#'   item1 = c(4, 4, 3, 4),
#'   item2 = c(4, 4, 4, 4),
#'   item3 = c(3, 3, 3, 3)
#' )
#' scvi_ave(ratings)
#'
#' @seealso \code{\link{cvi}}, \code{\link{icvi}}, \code{\link{scvi_ua}}
#'
#' @export
scvi_ave <- function(data,
                     scale = c("4point", "3point", "binary", "custom"),
                     relevant_values = NULL) {

  icvi_values <- icvi(data, scale = scale, relevant_values = relevant_values)
  return(mean(icvi_values, na.rm = TRUE))
}


#' Scale-Level CVI (Universal Agreement Method)
#'
#' Calculates the Scale-level Content Validity Index using the universal
#' agreement method.
#'
#' @inheritParams cvi
#'
#' @return A single numeric value representing S-CVI/UA.
#'
#' @details
#' S-CVI/UA is calculated as the proportion of items that achieved
#' universal agreement (all experts rated the item as relevant).
#'
#' An S-CVI/UA >= 0.80 is generally considered acceptable.
#'
#' @examples
#' ratings <- data.frame(
#'   item1 = c(4, 4, 3, 4),
#'   item2 = c(4, 4, 4, 4),
#'   item3 = c(3, 3, 3, 3)
#' )
#' scvi_ua(ratings)
#'
#' @seealso \code{\link{cvi}}, \code{\link{icvi}}, \code{\link{scvi_ave}}, \code{\link{ua}}
#'
#' @export
scvi_ua <- function(data,
                    scale = c("4point", "3point", "binary", "custom"),
                    relevant_values = NULL) {

  ua_values <- ua(data, scale = scale, relevant_values = relevant_values)
  return(mean(ua_values, na.rm = TRUE))
}


#' Universal Agreement Indicator
#'
#' Calculates the Universal Agreement (UA) indicator for each item.
#'
#' @inheritParams cvi
#'
#' @return A named integer vector of UA values (0 or 1), one per item.
#'
#' @details
#' Universal Agreement (UA) indicates whether ALL experts rated an item
#' as relevant (content valid).
#' \itemize{
#'   \item UA = 1: All experts rated the item as relevant
#'   \item UA = 0: At least one expert did not rate the item as relevant
#' }
#'
#' @examples
#' ratings <- data.frame(
#'   item1 = c(4, 4, 3, 4),  # UA = 1 (all >= 3)
#'   item2 = c(4, 4, 4, 4),  # UA = 1
#'   item3 = c(3, 2, 3, 3)   # UA = 0 (one rating = 2)
#' )
#' ua(ratings)
#'
#' @seealso \code{\link{cvi}}, \code{\link{scvi_ua}}
#'
#' @export
ua <- function(data,
               scale = c("4point", "3point", "binary", "custom"),
               relevant_values = NULL) {

  scale <- match.arg(scale)
  data_matrix <- prepare_data(data)

  binary_matrix <- transform_ratings(data_matrix, scale = scale,
                                      relevant_values = relevant_values)

  relevant_counts <- colSums(binary_matrix, na.rm = TRUE)
  n_experts <- colSums(!is.na(binary_matrix))

  # UA = 1 only if ALL experts rated as relevant
  ua_values <- as.integer(relevant_counts == n_experts)

  # Add names
  names(ua_values) <- colnames(data_matrix)
  if (is.null(names(ua_values))) {
    names(ua_values) <- paste0("Item", seq_along(ua_values))
  }

  return(ua_values)
}


#' Proportion of Items Rated Relevant per Expert
#'
#' Calculates the proportion of items each expert rated as relevant.
#'
#' @inheritParams cvi
#'
#' @return A named numeric vector of proportions, one per expert (row).
#'
#' @details
#' This function calculates what proportion of items each expert rated
#' as relevant. This can help identify experts who may be overly lenient
#' or strict in their ratings.
#'
#' @examples
#' ratings <- data.frame(
#'   item1 = c(4, 3, 2, 4),
#'   item2 = c(4, 4, 4, 4),
#'   item3 = c(3, 2, 1, 3)
#' )
#' proportion_relevance(ratings)
#'
#' @seealso \code{\link{cvi}}
#'
#' @export
proportion_relevance <- function(data,
                                  scale = c("4point", "3point", "binary", "custom"),
                                  relevant_values = NULL) {

  scale <- match.arg(scale)
  data_matrix <- prepare_data(data)

  binary_matrix <- transform_ratings(data_matrix, scale = scale,
                                      relevant_values = relevant_values)

  relevant_counts <- rowSums(binary_matrix, na.rm = TRUE)
  n_items <- rowSums(!is.na(binary_matrix))

  prop_values <- relevant_counts / n_items

  # Add names
  names(prop_values) <- rownames(data_matrix)
  if (is.null(names(prop_values))) {
    names(prop_values) <- paste0("Expert", seq_along(prop_values))
  }

  return(prop_values)
}
