#' Expert Ratings for Content Validity Assessment
#'
#' A dataset containing ratings from 10 experts on 8 items using a
#' 4-point relevance scale. This is an example dataset for demonstrating
#' content validity analysis.
#'
#' @format A data frame with 10 rows (experts) and 8 columns (items):
#' \describe{
#'   \item{item1}{Ratings for item 1 (1-4 scale)}
#'   \item{item2}{Ratings for item 2 (1-4 scale)}
#'   \item{item3}{Ratings for item 3 (1-4 scale)}
#'   \item{item4}{Ratings for item 4 (1-4 scale)}
#'   \item{item5}{Ratings for item 5 (1-4 scale)}
#'   \item{item6}{Ratings for item 6 (1-4 scale)}
#'   \item{item7}{Ratings for item 7 (1-4 scale)}
#'   \item{item8}{Ratings for item 8 (1-4 scale)}
#' }
#'
#' @details
#' Rating scale interpretation:
#' \itemize{
#'   \item 1 = Not relevant
#'   \item 2 = Somewhat relevant
#'   \item 3 = Quite relevant
#'   \item 4 = Highly relevant
#' }
#'
#' Values of 3 or 4 are considered "relevant" for CVI calculation.
#'
#' @source Simulated data for package demonstration.
#'
#' @examples
#' data(expert_ratings)
#' head(expert_ratings)
#' cvi(expert_ratings)
#'
"expert_ratings"
