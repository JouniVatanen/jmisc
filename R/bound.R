#' bound
#'
#' Bound the variable by lower and upper limits
#' @param x Choose variable from which to calculate.
#' @param lower Lower bound to cut the values to.
#' @param upper Upper bound to limit the values to.
#' @keywords limit, bound, cut
#' @examples
#' bound(-10, 0, 10)
#' bound(c(-10, 0, 3, 10, 100, "r"), 0, 10)
#' sapply(c(-10, 0, 100, NULL, NA), bound, 0, 10)
#' @export

bound <- function(x, lower, upper) {

  # Convert to numeric
  x <- as.numeric(x)

  if (!missing(lower)) {
    x <- pmax(x, lower)
  }

  if (!missing(upper)) {
    x <- pmin(x, upper)
  }

  return(x)
}
