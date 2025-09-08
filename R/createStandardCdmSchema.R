#' Create standard CDM schema
#'
#' VINCI-provisioned OMOP CDM views have non-standard names (ORD_Researcher_xyz.src.omopv5_person).
#' This function creates properly named views (ORD_Researcher_xyz.omopv5.person) that work with Hades packages.
#' The provisioned PERSON table may also have more than one record per person, which causes errors with feature
#' extraction and CohortMethod estimation. If includeAllCohorts is TRUE, the person table will be de-duped and all
#' distinct person_id values included. In cases where the researcher wants to limit to selected VINCI-provisioned
#' cohorts, cohortsToInclude can be specified.
#' 
#' @param connectionDetails DatabaseConnector connection details
#' @param database Database in which to create the new views (e.g. "ORD_Researcher_xyz")
#' @param startingSchema Schema containing provisioned CDM views (e.g. "src")
#' @param destinationSchema Destination schema for compliant views
#' @param tableNamePrefix Prefix of non-standard view names (e.g. "omopv5_")
#' @param cohortColumnName Cohort table name in VINCI-provisioned person table 
#' @param includeAllCohorts Logical
#' @param cohortsToInclude If includeAllCohorts is FALSE, which values of cohortName to include? 
#' @param verbose Print progress to console 
#' @returns invisible NULL
#' @export
createStandardCdmSchema <- function(connectionDetails,
                                    database,
                                    startingSchema = "src",
                                    destinationSchema = "OMOPV5",
                                    tableNamePrefix = "OMOPV5_",
                                    cohortColumnName = "CohortName",
                                    includeAllCohorts = TRUE,
                                    cohortsToInclude = NULL,
                                    verbose = TRUE) {

  if (includeAllCohorts == FALSE & is.null(cohortsToInclude)) {
    abort("Must specify cohortsToInclude if includeAllCohorts is FALSE")
  } else if (includeAllCohorts == TRUE & !is.null(cohortsToInclude)) {
    abort("Cannot specify cohortsToInclude if includeAllCohorts is TRUE")
  }
  
  connection <- DatabaseConnector::connect(connectionDetails)
  on.exit(DatabaseConnector::disconnect(connection))

  ## Get list of OMOP table names except person 
  tables <- DatabaseConnector::dbListTables(
    connection,
    paste0(database, ".", startingSchema)
  )
  tables <- tables[grepl(tableNamePrefix, tables, ignore.case = TRUE)]
  tables <- tables[tolower(tables) != paste0(tolower(tableNamePrefix), "person")]

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
    if (verbose) message(sprintf("%s -> %s", oldViewName, newViewName))

    DatabaseConnector::executeSql(
      connection,
      sprintf("create view %s as
              select *
              from %s",
              newViewName,
              oldViewName)
    )
  }

  ## Person table
  personColumnNames <- DatabaseConnector::renderTranslateQuerySql(
    connection,
    "SELECT name
     FROM sys.columns
     WHERE object_id = OBJECT_ID('@startingSchema.@tableNamePrefixperson')",
    startingSchema = startingSchema,
    tableNamePrefix = tableNamePrefix) |>
    dplyr::pull(1)
  
  personColumnNames <- personColumnNames[tolower(personColumnNames) != "cohortname"]
  personColumnNames <- paste0(personColumnNames, collapse = ", ")

  DatabaseConnector::renderTranslateExecuteSql(
    connection,
    "create view @destinationSchema.person as
     select distinct @personColumnNames 
     from @startingSchema.@tableNamePrefixperson
     {@includeAllCohorts == FALSE} ? {where @cohortColumnName in (@cohortsToInclude)}" ,
    destinationSchema = destinationSchema,
    personColumnNames = personColumnNames,
    tableNamePrefix = tableNamePrefix,
    startingSchema = startingSchema,
    cohortColumnName = cohortColumnName,
    cohortsToInclude = paste0("'", cohortsToInclude, "'", collapse = ", "),
    includeAllCohorts = includeAllCohorts
  ) 
    
  invisible(NULL)
}
