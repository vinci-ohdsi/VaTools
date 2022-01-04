library(VaTools)

outputFolder <- "D:/Users/msuchard/Documents/VaToolsOutput"
pathToDriver = "D:/Users/msuchard/Documents/Drivers"

oracleTempSchema <- NULL
cdmDatabaseSchema <- "cdm_truven_mdcr_v1838"
cohortDatabaseSchema <- "scratch_msuchard"
cohortTable <- "insulin_test"

databaseId<- "MDCR"
databaseName <- "IBM Health MarketScan Medicare Supplemental and Coordination of Benefits Database"
databaseDescription <- "IBM Health MarketScan® Medicare Supplemental and Coordination of Benefits Database (MDCR) represents health services of retirees in the United States with primary or Medicare supplemental coverage through privately insured fee-for-service, point-of-service, or capitated health plans. These data include adjudicated health insurance claims (e.g. inpatient, outpatient, and outpatient pharmacy). Additionally, it captures laboratory tests for a subset of the covered lives."

connectionDetails <- DatabaseConnector::createConnectionDetails(
  dbms = "redshift",
  server = paste0(keyring::key_get("redshiftServer"), "/", "truven_mdcr"),
  port = 5439,
  user = keyring::key_get("redshiftUser"),
  password = keyring::key_get("redshiftPassword"),
  extraSettings = "ssl=true&sslfactory=com.amazon.redshift.ssl.NonValidatingFactory")

createCohorts(connectionDetails = connectionDetails,
              cdmDatabaseSchema = cdmDatabaseSchema,
              cohortDatabaseSchema = cohortDatabaseSchema,
              cohortTable = cohortTable,
              cohortToCreateFile = "settings/CohortsToCreate.csv",
              oracleTempSchema = oracleTempSchema,
              outputFolder = outputFolder,
              databaseId = databaseId,
              generateInclusionStats = TRUE)
