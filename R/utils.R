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

#' next_weekday
#'
#' Choose the next weekday for a date
#' @param x Date type.
#' @keywords date weekday bizday
#' @examples
#' next_weekday(Sys.Date())
#' @export

next_weekday <- function(x) {
  x <- x + 1
  x + setNames(c(rep(0, 5), 2:1), 1:7)[format(x, "%u")]
}

#' is_date
#'
#' Check if variable is a date or a datetime object.
#' @param x Date type.
#' @keywords date weekday bizday
#' @examples
#' is_date(Sys.Date())
#' @export

is_date <- function(x) {
  inherits(x, c("Date", "POSIXt"))
}
