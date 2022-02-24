#' sp_get_responses
#'
#' Get responses to a survey from Surveypals.
#' @param survey_id Quest id from Surveypal.
#' @param api_token Api-token from Surveypal.
#' @param filename Filename to save the imported data.
#' @param last_updated If time is less than chosen, then do not execute.
#' Possible time choises are secs, mins, hours, days and weeks. Default is 12 hours.
#' @param latest_days From how many days you get the data.
#' @param from_date First datetime to get the data. In the form YYYY-MM-DD or
#' YYYY-MM-DD HH:mm:ss
#' @param to_date Last datetime to get the data. In the form YYYY-MM-DD or
#' YYYY-MM-DD HH:mm:ss
#' @keywords surveypal, survey, api
#' @export
#' @importFrom fs path_abs
#' @importFrom readr write_excel_csv2


sp_get_responses <- function(
  filename, quest_id, sid, username, password, last_updated = "12 hours",
  latest_days = NULL, from_date = NULL, to_date = NULL) {

  # Split last_updated to time unit and amount
  time_amount <- as.numeric(strsplit(last_updated, " ")[[1]][1])
  time_unit <- strsplit(last_updated, " ")[[1]][2]

  # Change latest_days to integer
  latest_days_int <- as.integer(latest_days)

  # Stop if not
  stopifnot(
    is.null(latest_days) | is.integer(latest_days_int),
    is.null(from_date) | is.POSIXct(from_date),
    is.null(to_date) | is.POSIXct(to_date),
    time_unit %in% c("secs", "mins", "hours", "days", "weeks"),
    is.numeric(time_amount))

  # Calculate when the file was last updated
  file_updated <- difftime(Sys.time(), file.info(filename)$mtime, units = time_unit)

  if (!file.exists(filename) | (file_updated > time_amount)) {

    url <- paste0(
      "https://my.surveypal.com/api/rest/answer/",
      survey_id,
      "/data")

    # Get the results
    r <- GET(
      url, add_headers(`X-Auth-Token` = api_token, Accept = "application/json"),
      content_type('application/json'), query = list(
        preferredLocaleId = locale_id))

    c <- content(r)

    # TODO: Add error handling on failed response
    # TODO: Parse the data from json to csv
    # TODO: Save the data as a csv
  }
}

#' sp_send_invitees
#'
#' Send invitees to a survey from Surveypal.
#' @param data Data to import the invitees from.
#' @param survey_id Survey id from Surveypal.
#' @param api_token Api-token from Surveypal.
#' @param filename Filename to save the sent list.
#' @param email Name of the email field.
#' @param unique Is the email unique.
#' @param anonymous Is the answer anonymous.
#' @param subject Email subject.
#' @param message Email message.
#' @param language Language of the survey.
#' @param from From email address.
#' @param reply_to Reply to email address.
#' @param send_at When to send the survey emails.
#' @keywords surveypal, survey, api
#' @export
#' @import httr
#' @importFrom jsonlite toJSON
#' @importFrom fs path_abs
#' @importFrom readr write_excel_csv2

sp_send_invitees <- function(
  data, survey_id, api_token, email = "email", unique = FALSE, filename = NULL,
  anonymous = FALSE, subject, message, language, from, reply_to, send_at) {

  # Add error handling on bad parameters
  email <- quo(email)

  # Parse data to json
  json <- data %>%
    pivot_longer(
      cols = -!!email, names_to = "key", values_to = "value",
      values_transform = list(value = "as.character")) %>%
    nest(meta = c(-!!email)) %>%
    toJSON()

  url <- paste0(
    "https://my.surveypal.com/api/rest/survey/",
    survey_id,
    "/answer/email/invite/batch")

  # Send the results
  r <- PUT(
    url, add_headers(`X-Auth-Token` = api_token, Accept = "application/json"),
    content_type('application/json'), query = list(
      anonymous = anonymous,
      unique = unique,
      subject = subject,
      message = message,
      localeId = language,
      author = from,
      replyTo = reply_to,
      send_at = send_at
    ),
    body = json, encode = "json")

  c <- content(r)

  # TODO: Add error handling on failed response
  # TODO: Add option to save the sent list as a file
}
