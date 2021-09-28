#' clean_emails
#'
#' Clean string from emails
#' @param x Choose string which to clean.
#' @keywords clean, string, email
#' @examples
#' clean_emails("lklkdf sari.pukkila@pukkila.fi lkjadf ef")
#' @export
#' @importFrom stringi stri_replace_all_regex

clean_emails <- function(x) {

  # Warn if value is not character
  if (!is.character(x)) warning("Input is not character type")

  # Street names pattern
  remove_pattern <- "[A-z0-9_.]*@[A-z0-9_.-]+"

  # Remove pattern from string
  output <- stri_replace_all_regex(x, remove_pattern, "*")

  return(output)
}
