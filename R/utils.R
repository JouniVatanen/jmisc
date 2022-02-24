#' is_named_vector
#'
#' Check if is named vector
#' @param x Choose vector to test.
#' @param mode character string naming an atomic mode or "list" or "expression"
#' or (except for vector) "any". Allows any type (see typeof) for mode,
#' and when mode is not "any" almost the same as typeof(x) == mode.
#' @keywords vector
#' @examples
#' is_named_vector(c(letter = "a", number = 4))
#' @export

is_named_vector <- function(x, mode = "any") {
  is.vector(x, mode = mode) & !is.null(names(x)) & !any(is.na(names(x)))
}
