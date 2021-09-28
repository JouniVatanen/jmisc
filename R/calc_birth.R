#' calc_birth
#'
#' Calculate birth date from Finnish social security number and return POSIXct
#' @param x Choose string from which to calculate.
#' @keywords social security, birth, date
#' @examples
#' calc_birth("121256-123S")
#' @export
#' @importFrom stringi stri_sub
#' @importFrom dplyr case_when

calc_birth <- function(x) {

  # TODO: Check that it is correct format

  # Substract elements
  separator <- stri_sub(x, 7, 7)
  dm <- stri_sub(x, 1, 4)
  y <- stri_sub(x, 5, 6)

  # Add a century or two based on conditions
  Y <- case_when(
    separator == "-" ~paste0("19", y),
    separator == "A" ~paste0("20", y),
    separator == "+" ~paste0("18", y),
    TRUE ~ NA_character_)

  # Parse to date
  as.Date(paste0(dm, Y), format = "%d%m%Y")
}
