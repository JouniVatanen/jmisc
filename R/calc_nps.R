#' calc_nps
#'
#' Calculate NPS from survey
#' @param var Choose variable from which to calculate.
#' @keywords social security, birth, date
#' @examples
#' calc_nps(0)
#' @export
#' @importFrom dplyr case_when

calc_nps <- function(var = NA) {

  val <- as.numeric(var)

  if (max(val, na.rm = TRUE) > 11 | min(val, na.rm = TRUE) < 0) {
    stop("Check the data. The scale should be 0-10.")
  } else if (max(val, na.rm = TRUE) == 11) {
    warning("The scale will be converted from 1-11 to 0-10.")
    val <- val - 1
  }

  output <- case_when(
    val %in% c(9:10) ~ 100,
    val %in% c(7:8)  ~ 0,
    val %in% c(0:6)  ~ -100,
    TRUE ~ NA_real_)

  # factor.levels <- c(100, 0, -100)
  # output <- factor(output, levels = factor.levels)

  return(output)
}
