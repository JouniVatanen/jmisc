#' write_csv_fi
#'
#' Save any list like data.frame, data.table or matrix to a txt-file.
#' Uses readr::write_excel_csv2.
#'
#' @param x Any list like element e.g. data.frame and data.table.
#' @param path Path or connection to write to.
#' @param na String used for missing values. Defaults to "". Missing values
#' will never be quoted; strings with the same value as na will always be quoted.
#' @param delim SDelimiter used to separate values. Defaults to "tab"
#' @param append If FALSE, will overwrite existing file. If TRUE, will append to
#'  existing file. In both cases, if file does not exist a new file is created.
#' @param col_names Write columns names at the top of the file? Must be either
#' TRUE or FALSE.
#' @param quote_escape The type of escaping to use for quoted values, one of
#' "double", "backslash" or "none". Defaults to "double".
#'
#' @keywords save txt
#' @examples
#' n <- c(1.1, 2.2, 3.3)
#' s <- c("a", "b", "c")
#' x <- data.frame(n, s)
#' write_csv_fi(x, file = "example.txt")
#' @export
#' @importFrom readr write_excel_csv2

# Write csv file with Finnish settings
write_csv_fi <- function(
  x, path = ".", na = "", delim = "\t", quote_escape = "double",
  append = FALSE, col_names = !append) {

  readr::write_excel_csv2(
    x, path = path, na = na, delim = delim, quote_escape = quote_escape,
    append = append, col_names = col_names)
}
