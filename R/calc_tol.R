#' calc_tol
#'
#' Calculate industry upper level classification (A-X) from lower levels (00-99)
#' @param x Choose variable from which to calculate.
#' @keywords industry, calculation, classification
#' @examples
#' calc_tol("00")
#' @export
#' @importFrom dplyr case_when

calc_tol <- function(x) {

  # Stop if value is not character
  if (!is.character(x)) stop("Industry class has to be char of type 00-99")

  output <- case_when(
    x %in% c("01", "02", "03") ~ "A",
    x %in% c("04", "05", "06", "07", "08", "09")  ~ "B",
    x %in% c("10", "11", "12", "13", "14", "15", "16", "17", "18", "19",
             "20", "21", "22", "23", "24", "25", "26", "27", "28", "29",
             "30", "31", "32", "33") ~ "C",
    x %in% c("35") ~ "D",
    x %in% c("36", "37", "38", "39") ~ "E",
    x %in% c("41", "42", "43") ~ "F",
    x %in% c("45", "46", "47") ~ "G",
    x %in% c("49", "50", "51", "52", "53") ~ "H",
    x %in% c("55", "56") ~ "I",
    x %in% c("58", "59", "60", "61", "62", "63") ~ "J",
    x %in% c("64", "65", "66") ~ "K",
    x %in% c("68") ~ "L",
    x %in% c("69", "70", "71", "72", "73", "74", "75") ~ "M",
    x %in% c("77", "78", "79", "80", "81", "82") ~ "N",
    x %in% c("84") ~ "O",
    x %in% c("85") ~ "P",
    x %in% c("86", "87", "88") ~ "Q",
    x %in% c("90", "91", "92", "93") ~ "R",
    x %in% c("94", "95", "96") ~ "S",
    x %in% c("97", "98") ~ "T",
    x %in% c("99") ~ "U",
    x %in% c("00", NA) ~ "X")

  return(output)
}
