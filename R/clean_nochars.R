#' clean_nochars
#'
#' Clean string from nochars: carriage return, new line and tab
#' @param x Choose string which to clean.
#' @keywords clean, string, new line
#' @examples
#' clean_nochars("Jouni\nVatanen")
#' @export
#' @importFrom stringi stri_replace_all_regex

clean_nochars <- function(x) {

  # Warn if value is not character
  if (!is.character(x)) warning("Input is not character type")

  # Street names pattern
  remove_pattern <- "[\r\n\t]"

  # Remove pattern from string
  output <- stri_replace_all_regex(x, remove_pattern, " ")

  return(output)
}
