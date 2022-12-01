######################  Function AllCodesets###########################################################
#   creates dataframe of all codesets from all sql scripts in package, with concept domains, names, and vocabularies
#     Parameters
#           a folder path
#           connectionDetails
#           cohortDatabaseSchema
#           rebuild (TRUE or FALSE) <-If TRUE, the database table containing codesets will be rebuilt even if it already exists, and even if contains a codeset for all cohort IDs
#     Requires
#           DatabaseConnector
#           Sqlrender
#     This process can fail for a number of reasons, the most common of which are
#             (1) an issue with the SQL;
#             (2) an issue with the connection; I think this causes issues with not dropping temp tables between subsequent cohorts
#                     This could likely be fixed with a closing of the connection between cohort runs
#
############################################################################################

#path<-"D:/OHDSI/bv/CovidVaccineSafetyv1_2_0/Output/cohortSql/cte"

AllCodesets<-function(path, connectionDetails, cohortDatabaseSchema, rebuild=TRUE){
  ##get all the files in a folder
  files<-list.files(path, full.names = TRUE)

  ### Get just the files that end in .sql
  files<-files[grep('.sql', files)] #if this is the CTE folder, then none should be removed

  ### Get the actual cohorts from the files (assuming that removing the path and .sql will return the cohort number)
  cohorts_files<-gsub('.sql','',gsub(paste(path, '/', sep=""), '', files))
  cohorts_files<-as.numeric(cohorts_files)
  ### create full concept set table
  # check first if it already exists
  connection <- DatabaseConnector::connect(connectionDetails)
  if (DatabaseConnector::existsTable(connection,cohortDatabaseSchema,'AllCodeSets')){
    print('An existing codeset table already exists.')

    ##if exists, pull in existing table and compare number of cohort_definition_ids to number of cohorts in folder
    codesetsql<-sprintf("SELECT cohort_definition_ID, COUNT(*) conceptct
                          FROM %s
                          GROUP BY cohort_definition_ID
                          ORDER BY cohort_definition_ID", paste(cohortDatabaseSchema, '.','AllCodeSets', sep=""))

    codesettable<-DatabaseConnector::querySql(connection, codesetsql)
    currentcodesets<-codesettable$COHORT_DEFINITION_ID
    if (identical(intersect(cohorts_files,currentcodesets),cohorts_files)){
      print('A codeset for all files already exists')
    } else if (length(cohorts_files)>length(currentcodesets)){
      print('There are more cohorts than codesets in current table.')
    } else if (length(cohorts_files)<length(currentcodesets)){
      print('There are fewer cohorts than codesets in current table.')
    }
  }
  DatabaseConnector::disconnect(connection)
  #rebuild<-TRUE

  ###build table
  connection <- DatabaseConnector::connect(connectionDetails)
  if (DatabaseConnector::existsTable(connection,cohortDatabaseSchema,'AllCodeSets')==FALSE|rebuild==TRUE){
    tablename<-paste(cohortDatabaseSchema, '.','AllCodeSets', sep="")
    createtablesql<-sprintf("DROP TABLE IF EXISTS %s;
CREATE TABLE %s (
	cohort_definition_ID int,
	codeset_id int,
	concept_id bigint,
	CONCEPT_NAME varchar(500),
	DOMAIN_ID varchar(20),
	STANDARD_CONCEPT varchar(1),
	VOCABULARY_ID varchar(20)
); ",tablename, tablename)
    createtable<-executeSql(connection, createtablesql, progressBar = TRUE, reportOverallTime = TRUE)
  }
  DatabaseConnector::disconnect(connection)

  ### get all codesets (all sql script before 2nd semi-colon)

  ############# Loop through files and run part of each SQL query ############
  start<-Sys.time()
  for (i in 1:length(files)){
    #i=1
    print(paste('Running ', files[i], sep=""))
    connection <- DatabaseConnector::connect(connectionDetails)
    ##### read in a sql file
    sql <- SqlRender::readSql(files[i])
    ##Locate end of concept_set creation
    conceptsetend<-stringr::str_locate_all(sql, ';')[1]
    #the second semi-colon is the end of the conceptcode script
    sql <- stringr::str_sub(sql, 1, conceptsetend[[1]][2])
    #Add insert statement to push all of the concepts into AllCodeSets table
    sql <- paste0(sql," INSERT INTO ", paste(cohortDatabaseSchema, '.','AllCodeSets', sep=""),
                  sprintf(' SELECT %s , a.codeset_id, a.concept_id, c.CONCEPT_NAME, c.DOMAIN_ID, c.STANDARD_CONCEPT, c.VOCABULARY_ID
FROM #Codesets a
INNER JOIN CDW_OMOP.OMOPV5.concept c
on a.concept_id = c.CONCEPT_ID; ', as.character(cohorts_files[i])), sep="\n")

    print(paste0('Executing SQL Codeset Creation of Cohort ID: ',as.character(cohorts_files[i]), sep=""))
    ##execute sql in database
    R.utils::withTimeout(DatabaseConnector::executeSql(connection, sql, progressBar = TRUE, reportOverallTime = TRUE),
                         timeout = 300,
                         onTimeout = 'warning')
    DatabaseConnector::disconnect(connection)
  }
  end<-Sys.time()
  print(paste('Total time to run all codesets was ', end-start, sep = ""))

  ### Pull into R AllCodesets table
  ##if exists, pull in existing table and compare number of cohort_definition_ids to number of cohorts in folder
  Allcodesetsql<-sprintf("SELECT *
                          FROM %s", paste(cohortDatabaseSchema, '.','AllCodeSets', sep=""))
  connection <- DatabaseConnector::connect(connectionDetails)
  AllCodesets<-DatabaseConnector::querySql(connection, Allcodesetsql)
  DatabaseConnector::disconnect(connection)
  return(AllCodesets)
}



######################  Function createDomainCrit ###########################################################
#   for identifying the search strings for each possible domain (all the the current ones are those in any janssen study cohort, its likely that there are domains not represented here)
#     Parameters
#           none
#     Requires
#           none
#
############################################################################################

#' @export
createDomaincrit<-function(){
  domains<-c('Measurement', 'Condition', 'Drug', 'Visit', 'Device', 'Observation', 'Procedure')

  beginnings<-c(
    '-- Begin Measurement Criteria'
    ,'-- Begin Condition Occurrence Criteria'
    ,'-- Begin Drug Exposure Criteria'
    ,'-- Begin Visit Occurrence Criteria'
    ,'-- Begin Device Exposure Criteria'
    ,'-- Begin Observation Criteria'
    ,'-- Begin Procedure Occurrence Criteria'
  )

  ends<-c(
    '-- End Measurement Criteria'
    ,'-- End Condition Occurrence Criteria'
    ,'-- End Drug Exposure Criteria'
    ,'-- End Visit Occurrence Criteria'
    ,'-- End Device Exposure Criteria'
    ,'-- End Observation Criteria'
    ,'-- End Procedure Occurrence Criteria'
  )

  domaincrit<-as.data.frame(cbind(domains,beginnings,ends))
  return(domaincrit)
}
