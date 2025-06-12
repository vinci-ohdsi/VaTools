#' Create standard CDM schema
#'
#' @param connectionDetails DatabaseConnector connection details
#' @param database Database in which to create the new views (e.g. "ORD_Researcher_xyz")
#' @param startingSchema Schema containing provisioned CDM views (e.g. "src")
#' @param destinationSchema Destination schema for compliant views
#' @param tableNamePrefix Prefix of non-standard view names (e.g. "omopv5_")
#'
#' @returns invisible NULL
#' @export
#'
#' @examples
createStandardCdmSchema <- function(connectionDetails,
                                    database,
                                    startingSchema,
                                    destinationSchema = "OMPV5",
                                    tableNamePrefix = "OMOPV5_") {

  connection <- DatabaseConnector::connect(connectionDetails)

  on.exit(DatabaseConnector::disconnect(connection))

  ## Get list of OMOP table names

  tables <- DatabaseConnector::dbListTables(
    connection,
    paste0(database, ".", startingSchema)
  )

  tables <- tables[grepl(tableNamePrefix, tables, ignore.case = TRUE)]


  ## Create schema if it doesn't exist

  DatabaseConnector::executeSql(
    connection,
    sprintf("use %s;",
            database)
  )

  schemas <- DatabaseConnector::querySql(
    connection,
    "select * from sys.schemas"
  )

  if (destinationSchema %in% schemas$NAME == FALSE) {
    message(sprintf("Creating schema %s.%s",
                    database, destinationSchema))

    DatabaseConnector::executeSql(
      connection,
      sprintf("create schema %s;",
              destinationSchema)
    )

  } else {
    message(sprintf("Schema %s.%s already exists",
                    database, destinationSchema))
  }


  ## Create views with CDM compliant names

  for(x in tables) {

    newViewTableName <- gsub(tableNamePrefix,
                             "",
                             x,
                             ignore.case = TRUE)

    newViewName <- paste0(destinationSchema, ".", newViewTableName)

    oldViewName <- paste0(startingSchema, ".", x)

    message(sprintf("%s -> %s", oldViewName, newViewName))

    DatabaseConnector::executeSql(
      connection,
      sprintf("create view %s as
              select *
              from %s",
              newViewName,
              oldViewName)
    )
  }

  invisible(NULL)

}
