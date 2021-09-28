#' format_phone_num
#'
#' Format phone number to a desired format..
#' @param x Choose string which to format.
#' @param prefix Choose prefix in front of the phone number. Default: "+358"
#' @keywords clean, string, phone number
#' @examples
#' format_phone_num("Matti 040 000 1234")
#' @export
#' @importFrom stringi stri_replace_all stri_sub stri_length

format_phone_num <- function(x, prefix = "+358") {

  # Remove other characters except numbers and plus
  x <- stri_replace_all_regex(x, "[^0-9+]", "")

  # Add prefix, if it does not exist yet
  x <- stri_replace_all_regex(x, "^0", prefix)

  # Calculate maximum length of phone number
  max_length <-stri_length(prefix) + 9
  output <- stri_sub(x, 1, max_length)

  return(output)
}
