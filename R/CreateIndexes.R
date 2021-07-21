#' @export
createIndexes <- function(connectionDetails,
                          cdmDatabaseSchema) {
  sql <- SqlRender::readSql(system.file("sql", "postgresql", "Indexes.sql",
                                        package = "VaTools"))

  sql <- SqlRender::render(sql,
                           cdm_database_schema = cdmDatabaseSchema)

  cat(sql)
}
