#' clean_post_numbers
#'
#' Clean string from Finnish post numbers
#' @param x Choose string which to clean.
#' @keywords clean, string, strees
#' @examples
#' clean_street_names("Jokutie 4, pikitie 7, 00500 Helsinki")
#' @export
#' @importFrom stringi stri_replace_all_regex

clean_post_numbers <- function(x) {

  # Warn if value is not character
  if (!is.character(x)) warning("Input is not character type")

  # Street names pattern
  remove_pattern <- "\\w*\\W*\\w*\\W+[0-9]{5}\\W+\\w*"

  # Remove pattern from string
  output <- stri_replace_all_regex(x, remove_pattern, "*")

  return(output)
}
