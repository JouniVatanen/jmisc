#' list_to_email
#'
#' This function helps you to save O365 type email addresses from list
#' @param email From where to read the data. Default: clipboard.
#' @param reverse Reverse the order of the first and the last name. Default: FALSE.
#' @param sep Separator, if data is not from clipboard. Default: tab.
#' @keywords email list
#' @examples
#' names_list <- c("Matti meikäläinen", "Maija Meikalainen")
#' writeClipboard(names_list)
#' list_to_email(email = "gmail.com")
#' @export
#' @importFrom stringi stri_replace_all_regex stri_trans_general
#' @importFrom dplyr mutate
#' @importFrom utils read.table readClipboard write.table

list_to_email <- function(email = NULL, sep = "\t", reverse = FALSE) {

  # Works only on windows at this point
  if (.Platform$OS.type != "windows") stop("Currently only works on Windows")

  clipboard <- tolower(readClipboard())

  # If email ending is not defined
  if (is.null(email)) {

    # Copy from Outlook like clipboard
    source <- stri_replace_all_regex(clipboard,
      c(";", ">", " <"),
      c("\n", "", "\t"),
      vectorize_all = FALSE)

    col_names <- c("name", "email")

  } else {

  # If email ending is defined
    # Copy from text and put
    source <- paste0(
      stri_replace_all_regex(
        stri_trans_general(clipboard, "latin-ascii"),
        c("\\s"),
        c(".")),
      "@",
      email)

    col_names <- "email"
  }

  # Read to dataframe
  table <- read.table(
    text = source,
    sep = sep,
    strip.white = TRUE,
    header = FALSE,
    col.names = col_names,
    quote = "\"")

  # Reverse order of the first name and last name
  if (reverse) {
    table <- mutate(table, name = gsub("^(\\S+)\\s+(\\S+)$", "\\2 \\1",
                                       .data$name))
  }

  # Write table to clipboard and return the result
  x <- write.table(table, "clipboard", sep = "\t",
                   row.names = FALSE, quote = FALSE)

  return(x)
}
