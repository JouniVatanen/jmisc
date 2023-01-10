#' qb_test_connection
#'
#' Test connection to Questback.
#' @param username Username to Questback.
#' @param password Password to Questback.
#' @keywords Questback, survey, api
#' @export
#' @import httr

qb_test_connection <- function(username, password) {
  out <- tryCatch({
  xml <- paste0('<s:Envelope xmlns:s="http://schemas.xmlsoap.org/soap/envelope/">
 <s:Body>
 <TestConnection xmlns="https://integration.questback.com/2011/03">
 <userInfo xmlns:i="http://www.w3.org/2001/XMLSchema-instance">
 <Username>', username, '</Username>
 <Password>', password, '</Password>
 </userInfo>
 </TestConnection>
 </s:Body>
</s:Envelope>')

  url <- "https://integration.questback.com/integration.svc?wsdl"

  res <- httr::POST(
    url, httr::add_headers(soapaction = "https://integration.questback.com/2011/03/QuestBackIntegrationLibrary/TestConnection"),
    httr::content_type("text/xml;charset=utf-8"), body = xml)

  message("Connection test result: ", httr::http_status(res)$category)
  },
  error = function(e) { message(e) }
  )
}

#' qb_get_quests
#'
#' Get responses to a survey from Questback Essentials.
#' @param username Username to Questback.
#' @param password Password to Questback.
#' @param n Maximum number of results. Max: 1000.
#' @keywords Questback, survey, api
#' @export
#' @import httr readr xml2

qb_get_quests <- function(username, password, n = 1000) {

  xml <- paste0('<s:Envelope xmlns:s="http://schemas.xmlsoap.org/soap/envelope/">
 <s:Body>
 <GetQuests xmlns="https://integration.questback.com/2011/03">
 <userInfo xmlns:i="http://www.w3.org/2001/XMLSchema-instance">
 <Username>', username, '</Username>
 <Password>', password, '</Password>
 </userInfo>
 <pagingInfo xmlns:i="http://www.w3.org/2001/XMLSchema-instance">
 <PageNo>0</PageNo>
 <PageSize>', n, '</PageSize>
 </pagingInfo>
 <questFilter>
 <QuestState>Draft|Active|Scheduled|Closed</QuestState>
 </questFilter>
 </GetQuests>
</s:Body>
</s:Envelope>')

  res <- httr::POST(
    "https://integration.questback.com/integration.svc?wsdl",
    httr::add_headers(soapaction = "https://integration.questback.com/2011/03/QuestBackIntegrationLibrary/GetQuests"),
    httr::content_type("text/xml;charset=utf-8"), body = xml)

  stopifnot(httr::http_status(res)$category == "Success")

  httr::content(res, "parsed", "text/xml", encoding = "UTF-8") %>%
    xml2::xml_ns_strip() %>%
    xml2::xml_find_all("//Quest") %>%
    {data.table(
      QuestId = xml2::xml_text(xml2::xml_find_first(., "QuestId")),
      SecurityLock = xml2::xml_text(xml2::xml_find_first(., "SecurityLock")),
      QuestTitle = xml2::xml_text(xml2::xml_find_first(., "QuestTitle")),
      StartDate = xml2::xml_text(xml2::xml_find_first(., "Startdate")),
      EndDate = xml2::xml_text(xml2::xml_find_first(., "EndDate")),
      State = xml2::xml_text(xml2::xml_find_first(., "State")),
      TotalInvitationsSent = xml2::xml_text(xml2::xml_find_first(., "TotalInvitationsSent")),
      TotalResponseCount = xml2::xml_text(xml2::xml_find_first(., "TotalResponseCount")),
      LastResponseDate = xml2::xml_text(xml2::xml_find_first(., "LastResponseDate")))} %>%
    readr::type_convert(
      cols(
        QuestId = readr::col_integer(),
        SecurityLock = readr::col_character(),
        QuestTitle = readr::col_character(),
        StartDate = readr::col_datetime(format = ""),
        EndDate = readr::col_datetime(format = ""),
        State = readr::col_character(),
        TotalInvitationsSent = readr::col_integer(),
        TotalResponseCount = readr::col_integer(),
        LastResponseDate = readr::col_datetime(format = "")
      )
    )
}

#' qb_get_questions
#'
#' Get responses to a survey from Questback Essentials.
#' @param username Username to Questback.
#' @param password Password to Questback.
#' @param quest_id Quest id.
#' @param security_id Security id.
#' @keywords Questback, survey, api
#' @export
#' @import httr readr xml2

qb_get_questions <- function(username, password, quest_id, security_id) {
  out <- tryCatch({
    xml <- paste0('<s:Envelope xmlns:s="http://schemas.xmlsoap.org/soap/envelope/">
 <s:Body>
 <GetQuestQuestions xmlns="https://integration.questback.com/2011/03">
 <userInfo xmlns:i="http://www.w3.org/2001/XMLSchema-instance">
 <Username>', username,'</Username>
 <Password>', password,'</Password>
 </userInfo>
 <questInfo xmlns:i="http://www.w3.org/2001/XMLSchema-instance">
 <QuestId>', quest_id, '</QuestId>
 <SecurityLock>', security_id, '</SecurityLock>
 </questInfo>
 </GetQuestQuestions>
 </s:Body>
</s:Envelope>
')

    res <- httr::POST(
      "https://integration.questback.com/integration.svc?wsdl",
      httr::add_headers(soapaction = "https://integration.questback.com/2011/03/QuestBackIntegrationLibrary/GetQuestQuestions"),
      httr::content_type("text/xml;charset=utf-8"), body = xml)

    if (http_status(res)$reason != "OK") { stop("Failed to get questions.") }

    content(res, "parsed", "text/xml", encoding = "UTF-8") %>%
      xml_ns_strip() %>%
      xml_find_all("//QuestQuestion")
      {data.table(
        QuestionNumber = xml_text(xml_find_first(., "QuestionNumber")),
        QuestionId = xml_text(xml_find_first(., "QuestionId")),
        QuestionTitle = map(., xml_path2list, "QuestionTitle/QuestionTitle"),
        QuestionType = xml_text(xml_find_first(., "QuestionType")),
        IsVisible = xml_text(xml_find_first(., "IsVisible")),
        Alternatives = map(., xml_path2list, "Alternatives/Alternatives"))} %>%
      # Filter only visible questions
      filter(IsVisible == "true") %>%
      select(-IsVisible) %>%
      type_convert(
        cols(
          QuestionNumber = col_integer(),
          QuestionId = col_integer(),
          QuestionType = col_character()
        )
      )
    # TODO: Data needs to be transformed to a better form
  },
  error = function(e) { message(e) }
  )
}

#' qb_get_responses
#'
#' Get responses to a survey from Questback Essentials.
#' @param username Username to Questback.
#' @param password Password to Questback.
#' @param quest_id Quest id.
#' @param security_id Security id.
#' @keywords Questback, survey, api
#' @export
#' @import httr readr xml2

qb_get_responses <- function(username, password, quest_id, security_id) {
  out <- tryCatch({
    xml <- paste0('<s:Envelope xmlns:s="http://schemas.xmlsoap.org/soap/envelope/">
 <s:Body>
 <GetResponses xmlns="https://integration.questback.com/2011/03">
 <userInfo xmlns:i="http://www.w3.org/2001/XMLSchema-instance">
 <Username>', username, '</Username>
 <Password>', password, '</Password>
 </userInfo>
 <questInfo xmlns:i="http://www.w3.org/2001/XMLSchema-instance">
 <QuestId>', quest_id, '</QuestId>
 <SecurityLock>', security_id, '</SecurityLock>
 </questInfo>
 <pagingInfo xmlns:i="http://www.w3.org/2001/XMLSchema-instance">
 <PageNo>0</PageNo>
 <PageSize>1000</PageSize>
 </pagingInfo>
 <responseFilter xmlns:i="http://www.w3.org/2001/XMLSchema-instance">
 <ToDate i:nil="true">2022-06-01T10:00:00</ToDate>
 </responseFilter>
 </GetResponses>
 </s:Body>
</s:Envelope>
')

    res <- httr::POST(
      "https://integration.questback.com/integration.svc?wsdl",
      httr::add_headers(soapaction = "https://integration.questback.com/2011/03/QuestBackIntegrationLibrary/GetResponses"),
      httr::content_type("text/xml;charset=utf-8"),
      body = xml)

    if (http_status(res)$reason != "OK") { stop("Failed to get responses.") }

    content(res, "parsed", "text/xml", encoding = "UTF-8") %>%
      xml_ns_strip()
      xml_find_all("//Answer")
      {data.table(
        RespondentId = xml_text(xml_find_first(., "../../RespondentId")),
        ResponseId = xml_text(xml_find_first(., "../../ResponseId")),
        Email = xml_text(xml_find_first(., "../../Email")),
        Start = xml_text(xml_find_first(., "../../Start")),
        Completed = xml_text(xml_find_first(., "../../Completed")),
        Anonymous = xml_text(xml_find_first(., "../../Anonymous")),
        QuestionId = xml_text(xml_find_all(., "QuestionId")),
        OrderNo = xml_text(xml_find_all(., "OrderNo")),
        Value = xml_text(xml_find_all(., "Value")),
        AnswerType = xml_text(xml_find_all(., "AnswerType")))} %>%
      type_convert(
        cols(
          RespondentId = col_integer(),
          ResponseId = col_integer(),
          Email = col_character(),
          Start = col_datetime(format = ""),
          Completed = col_datetime(format = ""),
          Anonymous = col_logical(),
          QuestionId = col_integer(),
          OrderNo = col_integer(),
          Value = col_character(),
          AnswerType = col_character()
        )
      )

    # TODO: Data needs to be transformed to a better form
  },
  error = function(e) { message(e) }
  )
}
