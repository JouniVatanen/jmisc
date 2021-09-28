#' send_error_mail
#'
#' Send email on error.
#' @param to email or a vector of emails.
#' @param subject of an email.
#' @param body of en email.
#' @param user user of sender's email account
#' @param password password of sender's email account
#' @param smtp smtp. If NULL, then gmail.
#' @keywords email, error
#' @examples
#' \dontrun{
#'to <- "address@email.com"
#'subject <- "You have error"
#'body <- "Hi,
#'you received an error. Check your logs."
#'user <- "erroraddress@email.com"
#'password <- "1234567"
#'send_error_mail(to, subject, body, user, password)
#'stop("Error")
#'}
#' @importFrom mailR send.mail


# Send error mail
send_error_mail <- function(to, user, password, smtp = NULL) {

  # Send email on error
  options(error = function() {

    # Create gmail setting, if smtp is missing
    if (is.null(smtp)) {
      host.name <- "smtp.gmail.com"
      port <- 465
      ssl <- TRUE
    }

    subject = "Alert!"

    body <- paste(
      "R command failed in path:", getwd(),
      "and with error message:", geterrmessage())

    send.mail(
      from = user, to = to, subject = subject,
      body = body,
      smtp = list(
        host.name = host.name, port = port, ssl = ssl, user.name = user,
        passwd = password),
      authenticate = TRUE, send = TRUE)
    })
}
