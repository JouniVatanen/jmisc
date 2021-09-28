#' send_mail
#'
#' Send email from Outlook.
#' @param to email or a vector of emails.
#' @param subject of an email.
#' @param body of en email.
#' @param attachment path to attachment file. Default: NULL
#' @param signature adds signature taken from Outlook. Default: TRUE
#' @param encoding encoding of the body. Default: "UTF-8"
#' @keywords email, outlook
#' @examples
#' \dontrun{
#'to <- c("jane.doe@address.com", "john.doe@address.com)
#'subject <- "Ääkkösiä"
#'body <- "Hei,
#'meillä on fiksuja asioita
#'ääkkösiä
#'vai miten se oli"
#'send_mail(to, subject, body)
#'}
#' @import RDCOMClient
#' @importFrom stringi stri_trim_both stri_replace_all_regex
#' @importFrom fs path

send_mail <- function(to, subject = "", body = "", attachment = NULL,
                      signature = TRUE, encoding = "UTF-8") {

  # Transform a vectors to ; separated character
  to <- paste(to, collapse = ";")

  # Transform subject and body to character
  subject <- as.character(subject)
  body <- as.character(
      stri_trim_both(
        stri_replace_all_regex(body, "\n", "<br/>")
      )
    )

  # Convert to Latin1
  body <- iconv(body, from = encoding, to = "Latin1")
  subject <- iconv(subject, to = "Latin1")

  # Initialize item_id
  mail_item <- 0

  # Create app and mail item
  out_app <- COMCreate("Outlook.Application")
  out_mail <- out_app$CreateItem(mail_item)

  # Save signature
  if (signature) {
    out_mail$GetInspector()
    s <- out_mail[["HTMLBody"]]
  }

  # Choose to and subject
  out_mail[["To"]] <- to
  out_mail[["subject"]] <- subject
  out_mail[["BodyFormat"]] <- 2

  # Add body and signature, if chosen in parameters
  if (signature) {
    out_mail[["HTMLBody"]] <- paste(body, s)
  } else {
    out_mail[["HTMLBody"]] <- body
  }

  # Normalize path
  if (!is.null(attachment)) {
    #attachment <- path(attachment)
    out_mail[["Attachments"]]$Add(attachment)
  }

  # Send
  out_mail$Send()
}
