#' calc_gender
#'
#' Calculate gender from social security number
#' @param string Choose string from which to calculate.
#' @keywords social security, gender
#' @examples
#' calc_gender("123456-123S")
#' @export
#' @importFrom dplyr if_else
#' @importFrom stringi stri_sub

calc_gender <- function(string) {
  output <- if_else(
    is.na(string),
    NA_character_,
    if_else(
      as.logical(as.numeric(stri_sub(string, 10, 10)) %% 2),
      "M",
      "F"))

  return(output)
}
