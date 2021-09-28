#' send_error_sms
#'
#' Send SMS on error.
#' @param to phone number or a vector of phone numbers to send the error message.
#' @param twilio_phone Twilio phone number from your twilio account.
#' @keywords SMS, error
#' @examples
#' \dontrun{
#'to <- "0401112222"
#'twilio_phone <- "04011113333"
#'send_error_sms(to, twilio_phone)
#'stop("Error")
#'}
#' @importFrom twilio tw_send_message
#' @export

# Send error SMS
send_error_sms <- function(to, twilio_phone) {

  # Send sms on error
  options(error = function() {

    body <- paste(
      "R command failed in path:", getwd(),
      "and with error message:", geterrmessage())

    # Send SMS error message
    tw_send_message(to, twilio_phone, body)
    })
}
