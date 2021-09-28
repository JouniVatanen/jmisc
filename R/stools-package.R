#' @useDynLib stools
#' @importFrom Rcpp sourceCpp
NULL

.onUnload <- function (libpath) {
  library.dynam.unload("stools", libpath)
}
