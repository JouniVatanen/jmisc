#' format_date
#'
#' Formats dates to the finnish standard.
#' @param x a date object, typically a vector of dates.
#' @keywords format, date
#' @examples
#' format_date(as.Date("2020-01-01"))
#' @export
#' @importFrom stringi stri_replace_all_regex

format_date <- function(x, format = "%d.%m.%Y") {

  # Transform to date, remove leading zeros
  x <- format(as.Date(x), format)
  output <- stri_replace_all_regex(x, "(?<=(^|\\.))0", "")

  return(output)
}
