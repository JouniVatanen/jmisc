#' db_get_query
#'
#' Get query results and close connection safely

#' @param sql Sql query or sql file
#' @param drv Driver for database. Default odbc::odbc().
#' @param ... Connection parameters like dsn or server, username and password
#' also extra parameters like encoding.
#' @param params List of parameters to replace question marks in sql query.
#' Default NULL
#' @keywords DBI, odbc, database, dbGetQuery
#' @export
#' @importFrom odbc odbc
#' @importFrom fs path_ext
#' @importFrom readr read_lines
#' @importFrom glue glue_collapse
#' @import DBI

db_get_query <- function(sql, drv = odbc::odbc(), ..., params = NULL) {

  # Create connection
  con <- dbConnect(drv = drv, ...)

  # Disconnect on exit
  on.exit(dbDisconnect(con))

  # Check if sql is valid by select and from keywords
  # TODO: better validation
  if (str_detect(tolower(sql), "select") & str_detect(tolower(sql), "from")) {
    sql <- sql
  } else if (tolower(path_ext(sql)) == "sql") {
    # If sql is sql file, then parse the file to sql
    sql <- glue_collapse(read_lines(sql), "\n")
  } else {
    stop("Check your sql query!")
  }

  # Return query with params, if defined
  if (!is.null(params)) {
    dbGetQuery(con, sql, params = params)
  } else {
    # Return query without params
    dbGetQuery(con, sql)
  }


}
