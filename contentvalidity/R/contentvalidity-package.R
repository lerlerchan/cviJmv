#' @keywords internal
"_PACKAGE"

#' contentvalidity: Content Validity Analysis for Scale Development
#'
#' Comprehensive tools for calculating content validity metrics used in
#' scale development and validation. Includes Content Validity Index (CVI)
#' at item and scale levels, Content Validity Ratio (CVR) using Lawshe's
#' formula, and Universal Agreement (UA) indicators.
#'
#' @section Main Functions:
#' \describe{
#'   \item{\code{\link{cvi}}}{Main analysis function returning comprehensive results}
#'   \item{\code{\link{icvi}}}{Item-level Content Validity Index}
#'   \item{\code{\link{scvi_ave}}}{Scale-level CVI (Average method)}
#'   \item{\code{\link{scvi_ua}}}{Scale-level CVI (Universal Agreement method)}
#'   \item{\code{\link{cvr}}}{Content Validity Ratio (Lawshe's formula)}
#'   \item{\code{\link{ua}}}{Universal Agreement indicator per item}
#'   \item{\code{\link{proportion_relevance}}}{Per-expert relevance scores}
#' }
#'
#' @section Utility Functions:
#' \describe{
#'   \item{\code{\link{transform_ratings}}}{Convert ratings to binary relevance}
#'   \item{\code{\link{cvr_critical}}}{Get critical CVR values by panel size}
#' }
#'
#' @section Data Format:
#' Input data should be a data frame or matrix where:
#' \itemize{
#'   \item Rows represent experts (raters)
#'   \item Columns represent items being evaluated
#'   \item Values are numeric ratings
#' }
#'
#' @section Supported Rating Scales:
#' \describe{
#'   \item{4-point}{1=not relevant, 2=somewhat relevant, 3=quite relevant,
#'     4=highly relevant. Values 3-4 are coded as relevant.}
#'   \item{3-point}{Values 2-3 are coded as relevant.}
#'   \item{binary}{0/1 or 1/2 coding. Highest value is relevant.}
#'   \item{custom}{Any scale with user-specified relevant values.}
#' }
#'
#' @section Interpretation Guidelines:
#' \itemize{
#'   \item I-CVI >= 0.78 is acceptable (Lynn, 1986)
#'   \item S-CVI/Ave >= 0.90 is excellent (Polit & Beck, 2006)
#'   \item S-CVI/UA >= 0.80 is acceptable
#' }
#'
#' @docType package
#' @name contentvalidity-package
#' @aliases contentvalidity
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
#' # Basic example with 4-point scale
#' ratings <- data.frame(
#'   item1 = c(4, 4, 3, 4, 3),
#'   item2 = c(4, 4, 4, 4, 4),
#'   item3 = c(3, 2, 2, 3, 2),
#'   item4 = c(4, 3, 4, 4, 3)
#' )
#'
#' # Run full analysis
#' result <- cvi(ratings)
#' print(result)
#' summary(result)
#'
#' # Individual metrics
#' icvi(ratings)
#' scvi_ave(ratings)
#' cvr(ratings)
#'
NULL
