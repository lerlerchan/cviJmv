#' Print Method for CVI Results
#'
#' Prints a concise summary of content validity analysis results.
#'
#' @param x An object of class \code{cvi_result}.
#' @param ... Additional arguments (currently ignored).
#'
#' @return Invisibly returns the input object.
#'
#' @examples
#' ratings <- data.frame(
#'   item1 = c(4, 4, 3, 4),
#'   item2 = c(4, 4, 4, 4),
#'   item3 = c(2, 3, 2, 3)
#' )
#' result <- cvi(ratings)
#' print(result)
#'
#' @export
print.cvi_result <- function(x, ...) {

  cat("\nContent Validity Analysis\n")
  cat(paste(rep("=", 40), collapse = ""), "\n\n")

  cat("Sample:\n")
  cat(sprintf("  Experts: %d\n", x$scale_stats$n_experts))
  cat(sprintf("  Items:   %d\n\n", x$scale_stats$n_items))

  cat("Scale-level Statistics:\n")
  cat(sprintf("  S-CVI/Ave: %.3f", x$scale_stats$scvi_ave))
  if (x$scale_stats$scvi_ave >= 0.90) {
    cat(" (Excellent)\n")
  } else if (x$scale_stats$scvi_ave >= 0.80) {
    cat(" (Acceptable)\n")
  } else {
    cat(" (Below threshold)\n")
  }

  cat(sprintf("  S-CVI/UA:  %.3f", x$scale_stats$scvi_ua))
  if (x$scale_stats$scvi_ua >= 0.80) {
    cat(" (Acceptable)\n")
  } else {
    cat(" (Below threshold)\n")
  }

  if (!is.null(x$scale_stats$mean_cvr)) {
    cat(sprintf("  Mean CVR:  %.3f\n", x$scale_stats$mean_cvr))
  }

  cat("\nUse summary() for detailed item-level results.\n")

  invisible(x)
}


#' Summary Method for CVI Results
#'
#' Provides a detailed summary of content validity analysis results
#' including item-level and expert-level statistics with interpretation.
#'
#' @param object An object of class \code{cvi_result}.
#' @param show_experts Logical. Whether to show expert-level statistics.
#'   Default is FALSE.
#' @param ... Additional arguments (currently ignored).
#'
#' @return Invisibly returns a list with the summary components.
#'
#' @examples
#' ratings <- data.frame(
#'   item1 = c(4, 4, 3, 4),
#'   item2 = c(4, 4, 4, 4),
#'   item3 = c(2, 3, 2, 3)
#' )
#' result <- cvi(ratings)
#' summary(result)
#' summary(result, show_experts = TRUE)
#'
#' @export
summary.cvi_result <- function(object, show_experts = FALSE, ...) {

  cat("\n")
  cat(paste(rep("=", 60), collapse = ""), "\n")
  cat("                CONTENT VALIDITY ANALYSIS                  \n")
  cat(paste(rep("=", 60), collapse = ""), "\n\n")

  # Sample information
  cat("SAMPLE INFORMATION\n")
  cat(paste(rep("-", 40), collapse = ""), "\n")
  cat(sprintf("  Number of experts: %d\n", object$scale_stats$n_experts))
  cat(sprintf("  Number of items:   %d\n\n", object$scale_stats$n_items))

  # Scale-level results
  cat("SCALE-LEVEL RESULTS\n")
  cat(paste(rep("-", 40), collapse = ""), "\n")

  scvi_ave <- object$scale_stats$scvi_ave
  scvi_ua <- object$scale_stats$scvi_ua

  cat(sprintf("  S-CVI/Ave (Average method):            %.3f\n", scvi_ave))
  cat(sprintf("  S-CVI/UA  (Universal Agreement):       %.3f\n", scvi_ua))

  if (!is.null(object$scale_stats$mean_cvr)) {
    cat(sprintf("  Mean CVR:                              %.3f\n",
                object$scale_stats$mean_cvr))
  }

  cat("\n  Interpretation:\n")
  if (scvi_ave >= 0.90) {
    cat("    - S-CVI/Ave >= 0.90: Excellent content validity\n")
  } else if (scvi_ave >= 0.80) {
    cat("    - S-CVI/Ave >= 0.80: Acceptable content validity\n")
  } else {
    cat("    - S-CVI/Ave < 0.80: Content validity may be insufficient\n")
  }

  if (scvi_ua >= 0.80) {
    cat("    - S-CVI/UA >= 0.80: Good universal agreement\n")
  } else {
    cat("    - S-CVI/UA < 0.80: Low universal agreement\n")
  }

  # Item-level results
  cat("\n")
  cat("ITEM-LEVEL RESULTS\n")
  cat(paste(rep("-", 40), collapse = ""), "\n")

  item_stats <- object$item_stats

  # Format for display
  display_stats <- item_stats
  display_stats$icvi <- sprintf("%.3f", display_stats$icvi)

  if (!is.null(display_stats$cvr)) {
    display_stats$cvr <- sprintf("%.3f", display_stats$cvr)
  }

  # Add interpretation column
  icvi_numeric <- object$item_stats$icvi
  display_stats$status <- ifelse(
    icvi_numeric >= 0.78, "OK",
    ifelse(icvi_numeric >= 0.70, "Review", "Revise")
  )

  print(display_stats, row.names = FALSE)

  cat("\n  Status key:\n")
  cat("    - OK:     I-CVI >= 0.78 (acceptable)\n")
  cat("    - Review: I-CVI 0.70-0.77 (borderline, review item)\n")
  cat("    - Revise: I-CVI < 0.70 (consider revision or removal)\n")

  # Expert-level results (optional)
  if (show_experts) {
    cat("\n")
    cat("EXPERT-LEVEL RESULTS\n")
    cat(paste(rep("-", 40), collapse = ""), "\n")

    expert_stats <- object$expert_stats
    expert_stats$proportion_relevance <- sprintf("%.3f",
                                                  expert_stats$proportion_relevance)
    print(expert_stats, row.names = FALSE)

    cat("\n  Note: Proportion relevance shows the fraction of items each\n")
    cat("  expert rated as relevant. Very high or low values may indicate\n")
    cat("  lenient or strict rating patterns.\n")
  }

  cat("\n")
  cat(paste(rep("=", 60), collapse = ""), "\n")

  invisible(list(
    item_stats = object$item_stats,
    expert_stats = object$expert_stats,
    scale_stats = object$scale_stats
  ))
}


#' Plot Method for CVI Results
#'
#' Creates a bar plot visualization of I-CVI values with threshold lines.
#'
#' @param x An object of class \code{cvi_result}.
#' @param type Character. Type of plot. One of:
#'   \describe{
#'     \item{"icvi"}{Bar plot of I-CVI values (default)}
#'     \item{"cvr"}{Bar plot of CVR values}
#'     \item{"experts"}{Bar plot of expert proportion relevance}
#'   }
#' @param threshold Numeric. Reference threshold line to draw.
#'   Default is 0.78 for I-CVI.
#' @param ... Additional arguments passed to \code{\link[graphics]{barplot}}.
#'
#' @return Invisibly returns the bar midpoints (from barplot).
#'
#' @examples
#' ratings <- data.frame(
#'   item1 = c(4, 4, 3, 4),
#'   item2 = c(4, 4, 4, 4),
#'   item3 = c(2, 3, 2, 3),
#'   item4 = c(4, 3, 4, 3)
#' )
#' result <- cvi(ratings)
#' plot(result)
#' plot(result, type = "cvr", threshold = 0)
#'
#' @export
plot.cvi_result <- function(x,
                            type = c("icvi", "cvr", "experts"),
                            threshold = NULL,
                            ...) {

  type <- match.arg(type)

  if (type == "icvi") {
    values <- x$item_stats$icvi
    names(values) <- x$item_stats$item
    main_title <- "Item-level Content Validity Index (I-CVI)"
    ylab <- "I-CVI"
    ylim <- c(0, 1)
    if (is.null(threshold)) threshold <- 0.78

    # Color based on threshold
    colors <- ifelse(values >= threshold, "#4CAF50", "#F44336")

  } else if (type == "cvr") {
    if (is.null(x$item_stats$cvr)) {
      stop("CVR was not computed. Run cvi() with compute_cvr = TRUE.",
           call. = FALSE)
    }
    values <- x$item_stats$cvr
    names(values) <- x$item_stats$item
    main_title <- "Content Validity Ratio (CVR)"
    ylab <- "CVR"
    ylim <- c(-1, 1)
    if (is.null(threshold)) threshold <- 0

    # Color based on threshold
    colors <- ifelse(values >= threshold, "#4CAF50", "#F44336")

  } else if (type == "experts") {
    values <- x$expert_stats$proportion_relevance
    names(values) <- x$expert_stats$expert
    main_title <- "Expert Proportion Relevance"
    ylab <- "Proportion"
    ylim <- c(0, 1)
    if (is.null(threshold)) threshold <- NULL

    # Neutral color
    colors <- "#2196F3"
  }

  # Create bar plot
  bp <- graphics::barplot(
    values,
    main = main_title,
    ylab = ylab,
    ylim = ylim,
    col = colors,
    border = NA,
    las = 2,
    ...
  )

  # Add threshold line
  if (!is.null(threshold)) {
    graphics::abline(h = threshold, col = "darkgray", lty = 2, lwd = 2)

    # Add threshold label
    graphics::text(
      x = max(bp) + 0.5,
      y = threshold,
      labels = sprintf("%.2f", threshold),
      pos = 4,
      cex = 0.8,
      col = "darkgray"
    )
  }

  # Add value labels on bars
  graphics::text(
    x = bp,
    y = values,
    labels = sprintf("%.2f", values),
    pos = 3,
    cex = 0.7
  )

  invisible(bp)
}
