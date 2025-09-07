##' Get distinct values of CohortName from person table
##'
##' Due to the way VINCI provisions cohorts, there may be duplicate rows for a person_id in the CDM person table.
##' This helper function lists the distinct values of the non-standard column `CohortName`. 
##' @title getCohortNames 
##' @param connectionDetails DatabaseConnector connectionDetails object
##' @param database "ORD_Name_xyz" 
##' @param schema "src"
##' @param table "omopv5_person" 
##' @return Distinct values of CohortName (character vector)
getCohortNames <- function(connectionDetails,
                           database,
                           schema = "src",
                           table = "omopv5_person",
                           columnName = "CohortName") {

  con <- DatabaseConnector::connect(connectionDetails)
  
  results <- DatabaseConnector::renderTranslateQuerySql(
    con,
    "select distinct CohortName
     from @database.@schema.@table",
    database = database,
    schema = schema,
    table = table) |>
    dplyr::pull(COHORTNAME)

  DatabaseConnector::disconnect(con)

  return(results)
}

