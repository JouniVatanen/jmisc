#' write_row2file
#'
#' Write a row to a file
#' @param data Dataframe which to save.
#' @param filename Output filename.
#' @param na NA value.
#' @keywords clean, string, company, name
#' @examples
#' write_row2file(mtcars[1, 1:2])
#' @export
#' @importFrom readr write_lines

write_row2file <- function(data, filename, na = "-") {

  # FIXME: Warn if value is not data frame

  # Collapse finnish company abbreviations to string
  string <- paste0(paste0(names(data), ": ",  data), collapse = "\n")

  write_lines(string, filename, na = na)
}
