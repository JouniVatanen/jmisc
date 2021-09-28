#' key_set_wrapper
#'
#' If else conditions for keyring::key_set
#' @param service Choose keyring service.
#' @param username Choose optional username.
#' @param keyring Choose optional keyring for extra security.
#' @keywords keyring, wrapper
#' @examples
#' library(keyring)
#' key_set_wrapper("test_keyring_20200101")
#' key_delete("test_keyring_20200101")
#' @export
#' @importFrom keyring key_set_with_value key_list
#' @importFrom rstudioapi askForPassword

key_set_wrapper <- function(service, username = NULL, keyring = NULL) {

  # If key_list exists, then do nothing
  if (length(key_list(service)[[1]]) == 0) {

    # Set key and stop with an error message
    key_set_with_value(
      service,
      username,
      askForPassword(
        paste0(
          "Please enter password/key to parameter: '", service, "'.",
          "\nIt will be saved your personal keyring.")),
      keyring)
  }
}
