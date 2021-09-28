#' db_write_table2
#'
#' Write table to database
#' @param data Data.
#' @param con Connection string.
#' @param schema Schema name.
#' @param table Table name.
#' @param fields Named vector of column names and datatypes. If null, then
#' automatic fetch by DBI::dbDataType.
#' @param to_nvarchar Convert varchar datatype to nvarchar for easier encoding.
#' @param overwrite Overwrite table.
#' @param append Append table.
#' @param unique_cols Columns, which makes appended data unique. You can use
#' tidyselect styled helpers like starts_with(). Default: everything().
#' @param to_utf16 Convert to UTF-16LE, which is required for proper Microsoft
#' SQL Server encoding.
#' @param batch_rows Number of rows for a batch. Default: 1000. Suggestion: 10000.
#' @param bulk Not yet implemented. Allows bulk insert using bcp.exe tool.
#' @keywords write, sql, database
#' @export
#' @importFrom DBI dbGetInfo dbDataType Id dbExistsTable dbRemoveTable dbCreateTable dbAppendTable
#' @importFrom stringi stri_encode stri_enc_tonative
#' @importFrom dplyr mutate_if distinct_at
#' @importFrom data.table rbindlist fwrite
#' @importFrom readr write_tsv
#' @importFrom rlang enquo
#' @import tidyselect

# Write table to database
db_write_table2 <- function(
  data, con, schema, table, fields = NULL, to_nvarchar = TRUE,
  overwrite = TRUE,  append = !overwrite, unique_cols = everything(),
  to_utf16 = TRUE, temporary = FALSE, logging = TRUE, batch_rows = 1000,
  bulk = FALSE) {

  # Check arguments
  stopifnot(
    is_named_vector(fields, "character") || is.null(fields))

  # Get server and database name from connection
  server_name <- dbGetInfo(con)$servername
  db_name <- dbGetInfo(con)$dbname

  # Choose the number of rows for a batch e.g. 10000
  options(odbc.batch_rows = batch_rows)

  # Warn if bcp.exe tool is not found and bulk = TRUE
  if (bulk & Sys.which("bcp") == "") {
    warning("bcp.exe tool is not found. Switching bulk to FALSE.")
    bulk <- FALSE
  }

  # Define table catalog and schema
  table_id <- Id(
    catalog = db_name,
    schema = schema,
    table = table)

  # Fetch old data from database, if append is TRUE
  if (append) {

    # Fetch old data
    data_old <- dbGetQuery(
      con, paste0("SELECT * FROM ", paste(db_name, schema, table, sep = ".")))

    # Unite dataframes
    data_stacked <- rbindlist(list(data_old, data), fill = TRUE)

    # Choose distinct rows
    # Return unique rows comparing selected variables if null, then compare everyhing
    data <- distinct_at(data_stacked, vars(!!enquo(unique_cols)), .keep_all = TRUE)
  }

  # Replace all varchar with nvarchar, if fields is missing
  if (to_nvarchar) {
    # FIXME: dbDataType gives warning with NA charvar datatypes
    fields_auto <- gsub("^varchar", "nvarchar", dbDataType(con, data))
  } else {
    fields_auto <- dbDataType(con, data)
  }

  # Replace some datatypes, if fields is not null
  if (!is.null(fields)) {
    # Replace fields_auto values by fields values, where names match
    fields_auto[names(fields)] <- fields
  }

  # Remove table if it exists
  if (dbExistsTable(con, table_id)) {
    dbRemoveTable(con, table_id)
  }

  # create table
  dbCreateTable(con, table_id, fields_auto)

  # Append to table
  if (bulk) {
    # If bulk, then write to table using bcp.exe tool
    # TODO: Absoute path?
    format_file <- tempfile()

    # Write format file
    cmd_format <- paste(
      "bcp.exe",
      paste(db_name, schema, table, sep = "."),
      "format nul",
      "-c",
      "-f", format_file,
      "-t \\t",
      "-T",
      "-S", server_name)

    shell(cmd_format)

    # TODO: Use format file in bcp write

    # Write data to tempfile
    temp_file <- tempfile()

    # Convert characters to native encoding
    # TODO: Cyrillics etc. are not encoded properly
    data <- data %>%
      mutate_if(is.factor, as.character) %>%
      mutate_if(is.character, stri_enc_tonative)

    # TODO: write.csv datetime removes second decimals
    fwrite(data, temp_file, sep = "\t", eol = "\r\n", quote = FALSE,
           col.names = FALSE, na = "", dateTimeAs = "write.csv", scipen = 999)

    # Create bulk command
    # FIXME: use \n as end of line option
    cmd <- paste(
      "bcp.exe",
      paste(db_name, schema, table, sep = "."),
      "in", temp_file,
      #"-f", format_file,
      "-T",
      "-S", server_name,
      "-t \\t",
      "-c",
      "-C ACP",
      "-a 65535",
      "-k",
      '-h "TABLOCK"'
      #,"-r \\n"
    )
    # Use bulk tool in shell
    shell(cmd)

    # Remove temp_file
    unlink(temp_file)

  } else {
    # Else use dbAppendTable
    # Convert character data to UTF-16LE
    #https://stackoverflow.com/questions/48105277/writing-unicode-from-r-to-sql-server
    data <- data %>%
      mutate_if(is.factor, as.character) %>%
      mutate_if(is.character, ~stri_encode(., to = "UTF-16LE", to_raw = TRUE))

    dbAppendTable(con, table_id, data)
  }

  # Check that all the rows were written and log number of rows
  # TODO: If fail, then save data to temp file and show temp file location
  if (logging) {
    # Count rows in the database
    db_count_rows <- dbGetQuery(
      con,
      paste0("SELECT count(*) FROM ", paste(db_name, schema, table, sep = ".")))

    # Check if all the rows were loaded to database
    if (db_count_rows != nrow(data)) {
      warning("The data was not loaded to the database!")
    } else {
      cat(paste0("Uploaded ", db_count_rows, " rows to the database."))
    }
  }
}
