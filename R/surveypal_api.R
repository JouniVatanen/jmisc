#' sp_test_connection
#'
#' Test connection to Surveypal.
#' @param api_key Api-key from Surveypal.
#' @keywords surveypal, survey, api
#' @export
#' @import httr

sp_test_connection <- function(api_key) {
  out <- tryCatch({

    url <- "https://my.surveypal.com/api/rest/surveys"

    res <- httr::GET(
      url, httr::add_headers(`X-Auth-Token` = api_key),
      httr::accept_json(), httr::content_type_json(), query = list(limit = 0))

    message("Connection test result: ", httr::http_status(res)$category)
  },
  error = function(e) { message(e) }
  )
}

#' sp_get_surveys
#'
#' Get surveys from Surveypal.
#' @param api_key Api-key from Surveypal.
#' @param published Filter survey list by survey published status: TRUE/FALSE
#' @param from Include survey if modified after date.
#' @param to Include survey if modified before date.
#' @param limit The maximum number of surveys to return.
#' @param offset The offset of first survey to return.
#' @keywords surveypal, survey, api
#' @export
#' @import httr

sp_get_surveys <- function(
    api_key, published = NULL, from = NULL, to = NULL, limit = NULL,
    offset = NULL, encoding = "UTF-8") {
  out <- tryCatch({

    # Parse datetimes to correct form %Y-%m-%dT%H:%M:%S
    if (!is.null(from)) { from <- sp_parse_datetime(from) }
    if (!is.null(to)) { to <- sp_parse_datetime(to) }

    url <- "https://my.surveypal.com/api/rest/surveys"
    query <- list(
      published = published, from = from, to = to, limit = limit,
      offset = offset)

    res <- httr::GET(
      url, httr::add_headers(`X-Auth-Token` = api_key),
      httr::accept_json(), httr::content_type_json(), query = query)

    stopifnot(httr::http_status(res)$category == "Success")

    httr::content(res, "text", encoding = encoding)
  },
  error = function(e) { message(e) }
  )
}

#' sp_get_answers
#'
#' Get answers to a survey from Surveypals.
#' @param survey_id Quest id from Surveypal.
#' @param api_key Api-key from Surveypal.
#' @param locale Locale. Default: first locale in the survey.
#' @param answer_state Choises are "[all]", "[complete]", "[incomplete]", "[unstarted]",
#' @param from First datetime to get the data. In the form YYYY-MM-DD,
#' YYYY-MM-DD HH:mm:ss or DD.MM.YYYY.
#' @param to Last datetime to get the data. In the form YYYY-MM-DD,
#' YYYY-MM-DD HH:mm:ss or DD.MM.YYYY.
#' @param limit Limit the number of returned answers.
#' @param use_question_id Show element question id instead of question text.
#' Default: false.
#' @keywords surveypal, survey, api
#' @export
#' @importFrom lubridate parse_date_time2
#' @import httr

sp_get_answers <- function(
    survey_id, api_key, locale = NULL, answer_state = NULL, from = NULL,
    to = NULL, limit = NULL, use_question_id = FALSE, encoding = "UTF-8") {
  out <- tryCatch({

    # Parse datetimes to correct form %Y-%m-%dT%H:%M:%S
    if (!is.null(from)) { from <- sp_parse_datetime(from) }
    if (!is.null(to)) { to <- sp_parse_date_time(to) }

    # Send request
    url <- sprintf(
      "https://my.surveypal.com/api/rest/survey/%s/answers",
      survey_id)
    query <- list(
      preferredLocaleId = locale, answerState = answer_state, from = from,
      to = to, limit = limit, useQuestionId = use_question_id)
    res <- httr::GET(
      url, httr::add_headers(`X-Auth-Token` = api_key), httr::accept_json(),
      query = query)

    stopifnot(httr::http_status(res)$category == "Success")

    # Parse response to json
    httr::content(res, "text", encoding = encoding)

  },
  error = function(e) {
    message("Error in the survey ", survey_id, ": ", e)
  }
  )
}

#' sp_send_invites
#'
#' Send invites to a survey from Surveypal.
#' @param json JSON to import the invitees from.
#' @param survey_id Survey id from Surveypal.
#' @param api_key Api-key from Surveypal.
#' @param anonymous Is the answer anonymous. Default: FALSE.
#' @param unique_emails Is the email unique. Default: TRUE.
#' @param subject Email subject. NULL uses subject defined at the Surveypal.
#' @param message Email message. NULL uses message defined at the Surveypal.
#' @param language Language of the survey. Default: first language of the survey.
#' @param author The invitation sender name.
#' @param reply_to The invitation reply to email address.
#' @param send_at The date and time when to send invitations.
#' @keywords surveypal, survey, api
#' @export
#' @import httr

sp_send_invites <- function(
    json, survey_id, api_key, anonymous = FALSE, unique_emails = TRUE,
    subject = NULL, message = NULL, locale = NULL, author = NULL,
    reply_to = NULL, send_at = NULL) {
  out <- tryCatch({

    # Parse datetimes to correct form %Y-%m-%dT%H:%M:%S
    if (!is.null(send_at)) { send_at <- sp_parse_datetime(send_at) }

    # Send request to Surveypal
    url <- sprintf(
      "https://my.surveypal.com/api/rest/survey/%s/answer/email/invite/batch",
      survey_id)
    query <- list(
      anonymous = anonymous, unique = unique_emails, subject = subject,
      message = message, localeId = locale, author = author, replyTo = reply_to,
      sendAt = send_at)
    res <- httr::PUT(
      url, httr::add_headers(`X-Auth-Token` = api_key), httr::accept_json(),
      httr::content_type_json(), body = json, encode = "json", query = query)

    stopifnot(httr::http_status(res)$category == "Success")

    # Print number of sent invitations
    n <- length(httr::content(res, "parse")$new)
    message(sprintf("Sent %s invitations.", n))
  },
  error = function(e) {
    message("Error in sending invitations in the survey ", survey_id, ": ", e)
  }
  )
}

#' sp_get_elements
#'
#' Get elements from a single Surveypal survey.
#' @param survey_id Survey id from Surveypal.
#' @param api_key Api-token from Surveypal.
#' @param locale Locale. Default: First locale of the survey. Example value: "fi_FI".
#' @param use_question_id Use question id. Default is FALSE.
#' @keywords surveypal, survey, api
#' @export
#' @import httr

sp_get_elements <- function(
    survey_id, api_key, locale = NULL, use_question_id = FALSE, encoding = "UTF-8") {
  out <- tryCatch({

    url <- sprintf(
      "https://my.surveypal.com/api/rest/survey/%s/elements",
      survey_id)
    query <- list(preferredLocaleId = locale, useQuestionId = use_question_id)

    res <- httr::GET(
      url, httr::add_headers(`X-Auth-Token` = api_key), httr::accept_json(),
      query = query)

    stopifnot(httr::http_status(res)$category == "Success")

    httr::content(res, "text", encoding = encoding)
  },
  error = function(e) {
    message("Error in getting elements from the survey,", survey_id, ": ", e)
  }
  )
}

#' sp_toJSON
#'
#' Parse data to JSON to Surveypal format.
#' @param .data Data to parse to JSON.
#' @param email Name of the email field.
#' @keywords surveypal, survey, api
#' @export
#' @import dplyr
#' @importFrom tidyr pivot_longer
#' @importFrom tidyr nest
#' @importFrom jsonlite toJSON

sp_toJSON <- function(.data, email) {

  out <- tryCatch({

    email <- enquo(email)

    .data %>%
      dplyr::mutate(
        dplyr::across(dplyr::everything(), as.character),
        rowid = dplyr::row_number()) %>%
      tidyr::pivot_longer(-c(rowid, !!email), names_to = "key", values_to = "value") %>%
      tidyr::nest(meta = c(key, value)) %>%
      dplyr::select(-rowid) %>%
      jsonlite::toJSON()
  },
  error = function(e) {
    message("Failed to parse to JSON: ", e)
  }
  )
}

#' sp_fromJSON
#'
#' Parse JSON from Surveypal to dataframe.
#' @param json JSON type value.
#' @keywords surveypal, survey, api
#' @export
#' @importFrom dplyr select
#' @importFrom tidyr unnest
#' @importFrom tidyr pivot_wider
#' @importFrom jsonlite fromJSON

sp_fromJSON <- function(json) {

  out <- tryCatch({

    stopifnot("JSON is empty." = json != "[]")

    # Flatten JSON
    dt <- json %>%
      jsonlite::fromJSON(flatten = TRUE)

    # Parse answers if exists
    if ("answers" %in% names(dt)) {
      dt <- dt %>%
        dplyr::select(answers) %>%
        tidyr::unnest(c(answers))

      # Parse metadata if it exists in any answer
      # TODO: How about saving the metadata other way like to its own table?
      if (!is.null(rapply(dt$meta, length))) {
        dt <- dt %>%
          tidyr::unnest(meta) %>%
          tidyr::pivot_wider(names_from = key, values_from = value)
      }

      dt <- dt %>%
        tidyr::unnest(elements) %>%
        tidyr::unnest(values)
    }

    # Parse labels if exists
    if ("labels" %in% names(dt)) {
      dt <- dt %>%
        tidyr::unnest(labels)
    }
    return(dt)
  },
  error = function(e) {
    message("Parse failed: ", e)
  }
  )
}

#' sp_parse_datetime
#'
#' Transform datetime object to the format Surveypal recognizes. Or tries to
#' convert a character to correct format.
#' @param x Date or datetime object or an object convertable to datetime object.
#' @keywords surveypal, survey, api
#' @export

sp_parse_datetime <- function(x) {
  if (!is_date(x)) { x <- as.POSIXct(x) }
  format(x, "%Y-%m-%dT%H:%M:%S")
}
