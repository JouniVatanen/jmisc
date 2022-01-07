#' clean_people_names
#'
#' Clean string from Finnish people
#' @param x Choose string which to clean.
#' @keywords clean, string, strees
#' @examples
#' clean_people_names("Ari, Jouni, EsaJokutie 4, pikitie 7, 00500 Helsinki")
#' @export
#' @import dplyr
#' @importFrom stringi stri_replace_all_regex stri_opts_regex

clean_people_names <- function(x) {

  # Warn if value is not character
  if (!is.character(x)) warning("Input is not character type")

  # Create people names pattern
  names <- paste(pull(fi_filtered_people_names), collapse = "|")
  remove_pattern <- paste0("\\b(?:", names, ")\\b ?")
names
  # Remove pattern from string
  output <- stri_replace_all_regex(
    x, remove_pattern, "*",
    opts_regex = stri_opts_regex(case_insensitive = TRUE))

  return(output)
}
