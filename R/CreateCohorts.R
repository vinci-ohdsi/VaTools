#' Create the exposure cohorts
#'
#' @details
#' This function will create the exposure cohorts following the definitions included in this package.
#'
#' @param connectionDetails      An object of type \code{connectionDetails} as created using the
#'                               \code{\link[DatabaseConnector]{createConnectionDetails}} function in
#'                               the DatabaseConnector package.
#' @param cdmDatabaseSchema      Schema name where your patient-level data in OMOP CDM format resides.
#'                               Note that for SQL Server, this should include both the database and
#'                               schema name, for example 'cdm_data.dbo'.
#' @param vocabularyDatabaseSchema   Schema name where your vocabulary tables in OMOP CDM format resides.
#'                               Note that for SQL Server, this should include both the database and
#'                               schema name, for example 'cdm_data.dbo'.
#' @param cohortDatabaseSchema   Schema name where intermediate data can be stored. You will need to
#'                               have write priviliges in this schema. Note that for SQL Server, this
#'                               should include both the database and schema name, for example
#'                               'cdm_data.dbo'.
#' @param tablePrefix            A prefix to be used for all table names created for this study.
#' @param indicationId           A string denoting the indicationId for which the exposure cohorts
#'                               should be created.
#' @param oracleTempSchema       Should be used in Oracle to specify a schema where the user has write
#'                               priviliges for storing temporary tables.
#' @param outputFolder           Name of local folder to place results; make sure to use forward
#'                               slashes (/)
#' @param databaseId             A short string for identifying the database (e.g. 'Synpuf').
#'
#' @export
createCohorts <- function(connectionDetails,
                          cdmDatabaseSchema,
                          vocabularyDatabaseSchema = cdmDatabaseSchema,
                          cohortDatabaseSchema,
                          cohortToCreateFile,
                          packageName = "VaTools",
                          cohortTable,
                          oracleTempSchema,
                          outputFolder,
                          databaseId,
                          generateInclusionStats = TRUE) {

  if (!file.exists(outputFolder)) {
    dir.create(outputFolder, recursive = TRUE)
  }

  ParallelLogger::logInfo("Creating cohorts")
  ParallelLogger::logInfo("- Populating table ", cohortTable)

  CohortDiagnostics::instantiateCohortSet(connectionDetails = connectionDetails,
                                          cdmDatabaseSchema = cdmDatabaseSchema,
                                          vocabularyDatabaseSchema = vocabularyDatabaseSchema,
                                          cohortDatabaseSchema = cohortDatabaseSchema,
                                          oracleTempSchema = oracleTempSchema,
                                          cohortTable = cohortTable,
                                          packageName = packageName,
                                          createCohortTable = TRUE,
                                          cohortToCreateFile = cohortToCreateFile,
                                          generateInclusionStats = generateInclusionStats,
                                          inclusionStatisticsFolder = outputFolder)

  ParallelLogger::logInfo("Counting cohorts")
  sql <- SqlRender::loadRenderTranslateSql("GetCounts.sql",
                                           "VaTools",
                                           dbms = connectionDetails$dbms,
                                           oracleTempSchema = oracleTempSchema,
                                           cdm_database_schema = cdmDatabaseSchema,
                                           work_database_schema = cohortDatabaseSchema,
                                           study_cohort_table = cohortTable)
  connection <- DatabaseConnector::connect(connectionDetails)
  counts <- DatabaseConnector::querySql(connection, sql, snakeCaseToCamelCase = TRUE)
  DatabaseConnector::disconnect(connection)
  counts$databaseId <- databaseId

  write.csv(counts, file.path(outputFolder, "cohortCounts.csv"), row.names = FALSE)
}

#' @export
writeCohortSql <- function(inputJsonFile,
                           outputSqlFile,
                           #packageName,
                           baseUrl = "https://atlas-covid19.ohdsi.org/WebAPI",
                           generateStats = FALSE) {
  json <- SqlRender::readSql(inputJsonFile)
  cohort <- RJSONIO::fromJSON(json)
  sql <- ROhdsiWebApi::getCohortSql(cohortDefinition = cohort, baseUrl = baseUrl, generateStats = generateStats)
  SqlRender::writeSql(sql, outputSqlFile)
}

