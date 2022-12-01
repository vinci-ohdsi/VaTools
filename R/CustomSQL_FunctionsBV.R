
###########################  FUNCTIONS ##########################
# SQLCohortsCreate()


######################  Function SQLCohortsCreate###########################################################
#   for identifying sql-defined ohorts in a folder that are not yet created in a cohort table and creating them
#     Parameters
#           a folder path
#           a max timeout time in seconds
#           cohortDatabaseSchema
#           cohortTable
#           connectionDetails
#     Requires
#           DatabaseConnector
#           Sqlrender
#           Codesets function
#     This process can fail for a number of reasons, the most common of which are
#             (1) an issue with the SQL;
#
############################################################################################

#path<-"D:/OHDSI/bv/VaccineSafety_V1_Results/cohortSql/bv_SQL"
#TimeoutInSeconds<-600 #600 seconds=10 min

# library(DatabaseConnector)
# library(SqlRender)

SQLCohortsCreate <- function(path,
                             timeoutInSeconds,
                             cohortDatabaseSchema,
                             cohortTable,
                             connectionDetails) {
    #path<-pathtoSQL
    ## establish connection
    connection <- DatabaseConnector::connect(connectionDetails)

    ##get all the files in a folder
    files <- list.files(path, full.names = TRUE)

    ### Get just the files that end in .sql
    files <- files[grep('.sql', files)]

    ############# remove the already completed cohorts

    ##automatic identification
    completesql <-
      sprintf(
        "SELECT cohort_definition_ID, COUNT(*) subjectCt
                          FROM %s
                          GROUP BY cohort_definition_ID
                          ORDER BY cohort_definition_ID",
        paste(cohortDatabaseSchema, '.', cohortTable, sep = "")
      )
    # completesql<-"SELECT cohort_definition_ID, COUNT(*) subjectCt
    #                         FROM ORD_OHDSI.AESI.VaccineSafetyPackage
    #                         GROUP BY cohort_definition_ID
    #                         ORDER BY cohort_definition_ID"
    complete <- DatabaseConnector::querySql(connection, completesql)
    complete <- complete$COHORT_DEFINITION_ID

    ###get the position numbers of files to exclude

    if (length(complete) == 0) {
      print('None Complete')
    } else {
      completevector <- c()
      for (i in complete) {
        #print(i)
        x = grep(i, files)
        completevector <- c(completevector, x)
      }
      files <- files[-completevector]
    }
    DatabaseConnector::disconnect(connection)


    ############# Loop through files and run each SQL query, kill any query that takes longer than 10 min ############

    for (i in files) {
      connection <- DatabaseConnector::connect(connectionDetails)
      ##### read in a sql file and run it
      #i=files[1] #for testing
      ##read in sql
      sql <- SqlRender::readSql(i)
      print(paste0('Executing SQL Cohort Creation of Cohort ID: ', i, sep =
                     ""))
      ##execute sql in database
      R.utils::withTimeout(
        DatabaseConnector::executeSql(
          connection,
          sql,
          progressBar = TRUE,
          reportOverallTime = TRUE
        ),
        timeout = timeoutInSeconds,
        onTimeout = 'warning'
      )
      DatabaseConnector::disconnect(connection)
    }
  }


