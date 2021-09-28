#' api_essentials
#'
#' API to POST/GET Questback Essentials survey information
#' @param soap_action API action: TestConnection / AddRespondentsData / AddEmailInvitees.
#' @param username Username for Questback Essentials.
#' @param password Password for Questback Essentials integration.
#' @param x the file name to save the dataframe.
#' @param quest_id Quest id from Questback Essentials.
#' @param security_lock Security lock from Questback Essentials.
#' @param send_duplicate Is it allowed to send survey to the same respondent.
#' @param sep Choose separator. Default: ";"
#' @keywords API survey
#' @export
#' @importFrom httr add_headers POST http_status
#' @importFrom XML xmlNode saveXML
#' @importFrom glue glue
#' @importFrom xml2 xml_find_all xml_text
#' @importFrom purrr map_df

api_essentials <- function(soap_action, username, password, quest_id = NULL,
                           security_lock = NULL, x = NULL,
                           send_duplicate = TRUE, sep = ";") {

  # Stop if required arguments are missing
  if (any(missing(soap_action), missing(username), missing(password))) {
    stop("You need to give action, username and password.")
  }

  # Url for call
  url <- "https://integration.questback.com/integration.svc?wsdl"

  # Headers for GET and POST calls
  header <- add_headers(c(
    "soapAction" = paste0(
      "https://integration.questback.com/2011/03/QuestBackIntegrationLibrary/",
      soap_action),
    "Content-Type" = "text/xml;charset=utf-8"))

  # CREATE CORRECT XML FOR EACH ACTION
  # Namespaces for XML
  xmlns <-  c(xmlns = "https://integration.questback.com/2011/03")
  `xmlns:s` <- c(`xmlns:s` = "http://schemas.xmlsoap.org/soap/envelope/")
  `xmlns:i` <- c(`xmlns:i` = "http://www.w3.org/2001/XMLSchema-instance")
  `xmlns:a` <- c(`xmlns:a` = "http://schemas.microsoft.com/2003/10/Serialization/Arrays")

  # Add email invitees XML
  AddEmailInvitees <- function(username, password, quest_id, security_lock, x,
                               send_duplicate = TRUE) {

    xml <- xmlNode(
      "s:Envelope", attrs = `xmlns:s`,
      xmlNode(
        "s:Body",
        xmlNode(
          soap_action, attrs = xmlns,
          xmlNode(
            "userInfo", attrs = `xmlns:i`,
            xmlNode("Username", username),
            xmlNode("Password", password)
          ),
          xmlNode(
            "questInfo", attrs = `xmlns:i`,
            xmlNode("QuestId", quest_id),
            xmlNode("SecurityLock", security_lock)
          ),
          xmlNode(
            "emails", attrs = c(`xmlns:a`, `xmlns:i`),
            .children = lapply(x, function(x) { xmlNode("a:string", x) })
          ),
          xmlNode("sendduplicate", tolower(send_duplicate)),
          xmlNode("languageId", "0")
        )
      )
    )
    body <- saveXML(xml)
    return(body)
  }

  # Auxiliary functions for AddRespondentsData
  is_email <- function(x) {
    if (tolower(x) %in% c("email")) tolower(TRUE) else tolower(FALSE)
  }

  is_factor <- function(x) {
    if (x == "factor") "numeric" else "text"
  }

  #purrr::imap(x, names(x), function(x) {
  #  xmlNode(
  #    "RespondentDataHeader",
  #    xmlNode("Title", names(x)),
  #    xmlNode("Type", is_factor(x)),
  #    xmlNode("isEmailField", is_email(x)), simplify = FALSE, USE.NAMES = TRUE)
  #})

  # Add respondents data XML
  AddRespondentsData <- function(
    username, password, quest_id, security_lock, x, delimiter = sep,
    allow_duplicate = TRUE) {

    xml <- xmlNode(
      "s:Envelope", attrs = `xmlns:s`,
      xmlNode(
        "s:Body",
        xmlNode(
          soap_action, attrs = xmlns,
          xmlNode(
            "userInfo", attrs = `xmlns:i`,
            xmlNode("Username", username),
            xmlNode("Password", password)
          ),
          xmlNode(
            "questInfo", attrs = `xmlns:i`,
            xmlNode("QuestId", quest_id),
            xmlNode("SecurityLock", security_lock)
          ),
          xmlNode(
            "respondentsData", attrs = `xmlns:i`,
            xmlNode(
              "RespondentDataHeader",
              .children = sapply(x, function(x) {
                xmlNode(
                  "RespondentDataHeader",
                  xmlNode("Title", x),
                  xmlNode("Type", is_factor(x)),
                  xmlNode("isEmailField", is_email(names(x))))
              })
              )
            ),
            xmlNode(
              "RespondentData", attrs = `xmlns:a`,
              xmlNode("a:string", row)
            ),
            xmlNode("Delimiter", delimiter),
            xmlNode("AllowDuplicate", tolower(allow_duplicate)),
            xmlNode("AddAsInvitee", "true"),
            xmlNode("DistributionPrioritization", "email")
          ),
          xmlNode("languageId", "0")
        )
      )
    body <- saveXML(xml)
    return(body)
  }

  # Test connection XML
  TestConnection <- function(username, password) {
    xml <- xmlNode(
      "s:Envelope", attrs = `xmlns:s`,
      xmlNode(
        "s:Body",
        xmlNode(
          "TestConnection", attrs = xmlns,
          xmlNode(
            "userInfo", attrs = `xmlns:i`,
            xmlNode("Username", username),
            xmlNode("Password", password)
          )
        )
      )
    )
    body <- saveXML(xml)
    return(body)
  }

  # Function to load data
  GetResponses <- function(username, password, quest_id, security_lock) {

    xml <- xmlNode(
      "s:Envelope", attrs = `xmlns:s`,
      xmlNode(
        "s:Body",
        xmlNode(
          soap_action, attrs = xmlns,
          xmlNode(
            "userInfo", attrs = `xmlns:i`,
            xmlNode("Username", username),
            xmlNode("Password", password)
          ),
          xmlNode(
            "questInfo", attrs = `xmlns:i`,
            xmlNode("QuestId", quest_id),
            xmlNode("SecurityLock", security_lock)
          ),
          xmlNode("Delimiter", sep),
          xmlNode(
            "pagingInfo", attrs = `xmlns:i`,
            xmlNode("PageSize", "10000"),
            xmlNode("PageNo", "0"),
            xmlNode("TotalCount", "1")
          )
        )
      )
    )
    body <- saveXML(xml)
    return(body)
  }

  # Function to get question names
  GetQuestQuestions <- function(username, password, quest_id, security_lock) {

    xml <- xmlNode(
      "s:Envelope", attrs = `xmlns:s`,
      xmlNode(
        "s:Body",
        xmlNode(
          soap_action, attrs = xmlns,
          xmlNode(
            "userInfo", attrs = `xmlns:i`,
            xmlNode("Username", username),
            xmlNode("Password", password)
          ),
          xmlNode(
            "questInfo", attrs = `xmlns:i`,
            xmlNode("QuestId", quest_id),
            xmlNode("SecurityLock", security_lock)
          )
        )
      )
    )
    body <- saveXML(xml)
    return(body)
  }

  # Return correct XML depending on soap_action
  if (soap_action == "TestConnection") {
    body <- TestConnection(username, password)
  } else if (soap_action == "AddRespondentsData") {
    body <- AddRespondentsData(username, password, quest_id, security_lock, x)
  } else if (soap_action == "AddEmailInvitees") {
    body <- AddEmailInvitees(username, password, quest_id, security_lock, x,
                             send_duplicate)
  } else if (soap_action == "GetResponses") {
    body <- GetResponses(username, password, quest_id, security_lock)
  } else {
    stop(glue("Currently SOAP call: {soap_action} is not possible"))
  }

  # Send XML file to Essentials
  post <- POST(url, config = header, body = body)

  # Return results if post was a success, else stop
  if (http_status(post)$reason != "OK") {
    stop(glue(
      "{soap_action} failed because {http_status(post)$reason}"))
  } else {
    return(post)
  }
}

parse_responses <- function(xml) {

  # Write results to file and delete unnecessary results
  #xml <- reader$value()
  xml <- gsub('.*(<Responses>.*</Responses>).*', '\\1', xml)

  # Modify XML file to data frame
  output <- read_xml(xml) %>%
    xml_find_all('.//Response') %>%
    map_df(function(x) {
      ResponseId  <- xml_find_all(x, './ResponseId')  %>% xml_text()
      Email       <- xml_find_all(x, './Email')       %>% xml_text()
      Start       <- xml_find_all(x, './Start')       %>% xml_text() %>% as.POSIXct(format = '%Y-%m-%dT%H:%M:%S')
      Completed   <- xml_find_all(x, './Completed')   %>% xml_text() %>% as.POSIXct(format = '%Y-%m-%dT%H:%M:%S')
      Answer      <- xml_find_all(x, './/Value')      %>% xml_text()
      QuestionId  <- xml_find_all(x, './/QuestionId') %>% xml_text()
      data_frame(ResponseId, Email, Start, Completed, Answer, QuestionId)
    })
  return(output)
}

parse_questions <- function(xml) {

  # Write results to file and delete unnecessary results
  #xml <- reader$value()
  xml <- gsub('.*(<GetQuestQuestionsResult.*</GetQuestQuestionsResult>).*', '\\1', xml)

  # Modify XML file to data frame
  output <- read_xml(xml) %>%
    xml_find_all('//QuestQuestion') %>%
    map_df(function(x) {
      Title       <- xml_find_all(x, './/Title')      %>% xml_text()
      QuestionId  <- xml_find_all(x, './/QuestionId') %>% xml_text()
      data_frame(Title, QuestionId)
    })
  return(output)
}
