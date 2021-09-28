#' clean_company_names
#'
#' Clean string from Finnish company names
#' @param x Choose string which to clean.
#' @keywords clean, string, company, name
#' @examples
#' clean_company_names("lklkdf Nokia Oyj lkjadf ef Kämmekkäsäätiö")
#' @export
#' @importFrom stringi stri_replace_all_regex

clean_company_names <- function(x) {

  # Warn if value is not character
  if (!is.character(x)) warning("Input is not character type")

  # Collapse finnish company abbreviations to string
  abbr <- paste(pull(fi_company_abbr), collapse = "|")

  # Company names pattern
  remove_pattern <- paste("\\w*\\W*\\w*\\W*(", abbr, ")((\\W+\\w*)|$)")

  # Remove pattern from string
  output <- stri_replace_all_regex(x, remove_pattern, "*")

  return(output)
}
