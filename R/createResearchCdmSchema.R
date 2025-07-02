#' Create a research CDM schema with filtered views from CDW OMOP and supplied cohort table
#'
#' @param connectionDetails DatabaseConnector connection details
#' @param database Database in which to create the new views
#' @param startingSchema Schema containing provisioned CDM views (e.g. "src")
#' @param destinationSchema Destination schema for compliant views
#' @param tableNamePrefix Prefix of non-standard view names (e.g. "omopv5_")
#'
#' @returns invisible NULL
#' @export
#'
#' @examples
createResearchCdmSchema <- function(connectionDetails,
                                    sourceDatabase,
                                    sourceSchema,
                                    targetDatabase,
                                    targetSchema,
                                    cohortSchema) {

  connection <- DatabaseConnector::connect(connectionDetails)

  on.exit(DatabaseConnector::disconnect(connection))

  ## Get list of OMOP table names
  tables <- DatabaseConnector::dbListTables(
    connection,
    paste0(sourceDatabase, ".", sourceSchema)
  )

  ## Create schema if it doesn't exist
  DatabaseConnector::executeSql(
    connection,
    sprintf("use %s;",
            targetDatabase)
  )

  schemas <- DatabaseConnector::querySql(
    connection,
    "select * from sys.schemas"
  )

  if (targetSchema %in% schemas$NAME == FALSE) {
    message(sprintf("Creating schema %s.%s",
                    targetDatabase, targetSchema))

    DatabaseConnector::executeSql(
      connection,
      sprintf("create schema %s;",
              targetSchema)
    )

  } else {
    message(sprintf("Schema %s.%s already exists",
                    targetDatabase, targetSchema))
  }

  ## list of person-level omop tables
  query <- sprintf(
    "select table_schema,
      table_name,
      column_name
    from %s.information_schema.columns
      where table_schema = '%s'  and
        column_name = 'person_id'",
    sourceDatabase,
    sourceSchema)

  personTables <- DatabaseConnector::querySql(connection, query)

  ## Create research views from the source CDW tables
  for(x in tables) {

    newViewName <- paste0(targetSchema, ".", x)

    sourceTableName <- paste0(sourceSchema, ".", x)

    message(sprintf("Creating view %s -> %s", newViewName, sourceTableName))

    if (toupper(x) %in% toupper(personTables$TABLE_NAME)) {
      DatabaseConnector::executeSql(
        connection,
        sprintf("create view %s as
              select b.*
              from %s.%s.cohort a
              left join %s.%s b
              on a.person_id = b.person_id",
                newViewName,
                targetDatabase,
                cohortSchema,
                sourceDatabase,
                sourceTableName)
      )
    } else {
      DatabaseConnector::executeSql(
        connection,
        sprintf("create view %s as
              select *
              from %s.%s",
                newViewName,
                sourceDatabase,
                sourceTableName)
      )
    }
  }

  invisible(NULL)

}
