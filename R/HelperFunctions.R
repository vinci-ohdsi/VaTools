#' @export
refactor <- function(cohortDefinitionSet) {
  for (i in c(1:nrow(cohortDefinitionSet))) {
    oldSql <- cohortDefinitionSet$sql[i]
    cohortDefinitionSet$sql[i] <- translateToCustomVaSqlUsingJava(oldSql)
    cohortDefinitionSet$oldSql <- oldSql
  }
  return (cohortDefinitionSet)
}
