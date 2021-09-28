#' Anonymize
#'
#' Create anonymized id. Note: function is still on alpha stage.
#' TODO: create id variable from source and date_time
#' TODO: salt aka add some noise characters to variable
#'
#' # Original function and its use
#' anonymize <- function(x, algo="crc32"){
#'  unq_hashes <- vapply(unique(x), function(object) digest(object, algo=algo), FUN.VALUE="", USE.NAMES=TRUE)
#'  unname(unq_hashes[x])
#' }
#' cols_to_mask <- c("name", "address", "postal_code")
#' SURV_ORG[, cols_to_mask := lapply(.SD, anonymize), .SDcols = cols_to_mask, with = FALSE][]
#' @param x String to anonymize.
#' @param algo Algorithm to anonymize id. Default: crc32.
#' @keywords anonymize data
#' @examples
#' anonymize(0:10, algo = "crc32")
#' @importFrom digest digest

anonymize <- function(x, algo = "crc32"){

  unq_hashes <- vapply(
    unique(x),
    function(object) digest(object, algo = algo),
    FUN.VALUE = "", USE.NAMES = TRUE)
  unname(unq_hashes[x])
}

#cols_create_id <- c("source","date_time")
#z <- paste0(cols_create_id, collapse = " | ")
