#' clean_ids
#'
#' Clean string from possible id numbers
#' @param x Choose string which to clean.
#' @keywords clean, string, strees
#' @examples
#' clean_ids("003904A")
#' @export
#' @importFrom stringi stri_replace_all_regex

clean_ids <- function(x) {

  # Warn if value is not character
  if (!is.character(x)) warning("Input is not character type")

  # Street names pattern
  remove_pattern <- "([0-9, .+-]{7,})|([0-9,.+-]{5,}[A-z])"

  # Remove pattern from string
  output <- stri_replace_all_regex(x, remove_pattern, "*")

  return(output)
}
