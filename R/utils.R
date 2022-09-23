#' is_named_vector
#'
#' Check if is named vector
#' @param x Choose vector to test.
#' @param mode character string naming an atomic mode or "list" or "expression"
#' or (except for vector) "any". Allows any type (see typeof) for mode,
#' and when mode is not "any" almost the same as typeof(x) == mode.
#' @keywords vector
#' @examples
#' is_named_vector(c(letter = "a", number = 4))
#' @export

is_named_vector <- function(x, mode = "any") {
  is.vector(x, mode = mode) & !is.null(names(x)) & !any(is.na(names(x)))
}

#' next_weekday
#'
#' Choose the next weekday for a date
#' @param x Date type.
#' @keywords date weekday bizday
#' @examples
#' next_weekday(Sys.Date())
#' @export

next_weekday <- function(x) {
  x <- x + 1
  x + setNames(c(rep(0, 5), 2:1), 1:7)[format(x, "%u")]
}

#' is_date
#'
#' Check if variable is a date or a datetime object.
#' @param x Date type.
#' @keywords date weekday bizday
#' @examples
#' is_date(Sys.Date())
#' @export

is_date <- function(x) {
  inherits(x, c("Date", "POSIXt"))
}

#' xml_path2list
#'
#' Transforms xml xpath to list. Is used especially with Questback api functions.
#' xml2 functions have some kind of problems with nested named XML lists.
#' @param x XML file.
#' @param xpath Path of the XML file.
#' @keywords XML Questback
#' @importFrom data.table data.table
#' @importFrom xml2 xml_find_all
#' @importFrom xml2 xml_length
#' @importFrom xml2 xml_children
#' @importFrom xml2 xml_text
#' @importFrom xml2 xml_name
#' @importFrom dplyr bind_rows
#' @export

xml_path2list <- function(x, xpath) {
  xml <- xml2::xml_find_all(x, xpath)
  if (all(xml2::xml_length(xml) == 0)) { return(data.table::data.table(NULL)) }
  l <- lapply(xml, function(x) {
    c <- xml2::xml_children(x)
    len <- xml2::xml_length(c)
    if (any(len == 0)) {
      n_child <- setNames(xml2::xml_text(c[len == 0]), xml2::xml_name(c[len == 0]))
    }
    # list of childrens childrend
    if (any(len > 0)) {
      l_child <- list()
      c_name <- xml2::xml_name(c[len != 0])
      cc <- xml2::xml_children(c[len != 0])
      cc_names <- xml2::xml_name(cc)
      l_child[[c_name]] <- setNames(as_list(cc), cc_names)
      out <-  append(l_child, n_child)
    } else { out <- list(n_child) }
    return(out)
  })
  dplyr::bind_rows(l)
}

