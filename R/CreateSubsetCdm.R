#' @export
createSubsetCdmFromCohort <- function(connectionDetails,
                                      outputCdmDatabaseSchema,
                                      inputCdmDatabaseSchema,
                                      cohortDatabaseSchema,
                                      cohortTable) {

  sql <- SqlRender::readSql(system.file("sql", "postgresql", "SubsetCdm.sql",
                                        package = "VaTools"))
  sql <- SqlRender::render(sql,
                           output_cdm_database_schema = outputCdmDatabaseSchema,
                           input_cdm_database_schema = inputCdmDatabaseSchema,
                           cohort_database_schema = cohortDatabaseSchema,
                           cohort_table = cohortTable)

  cat(sql)

}
