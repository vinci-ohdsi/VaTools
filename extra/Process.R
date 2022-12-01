
library(CovidVaccineSafety)

############# Prepare COhort SQL files using package functions prepareCohortSql() ###

Sys.setenv(DATABASECONNECTOR_JAR_FOLDER="D:/JDBC/sqljdbc_6.0/enu/jre8")

# The folder where the study intermediate and result files will be written:
outputFolder <- "D:/OHDSI/bv/CovidVaccineSafetyv1_2_0/Output"


# Details for connecting to the server:
connectionDetails <- DatabaseConnector::createConnectionDetails(
  dbms = "sql server",
  server = "vhacdwdwhdbs102")

# The name of the database schema where the CDM data can be found:
cdmDatabaseSchema <- "ORD_OHDSI.OMOPview"

# The name of the database schema and table where the study-specific cohorts will be instantiated:
cohortDatabaseSchema <- "ORD_OHDSI.AESI"
cohortTable <- "VaccineSafetyPackage20220812"


prepareCohortSql(connectionDetails = connectionDetails,
                 cdmDatabaseSchema = cdmDatabaseSchema,
                 cohortDatabaseSchema = cohortDatabaseSchema,
                 cohortTable = cohortTable,
                 outputFolder = outputFolder)


#folder path
pathtoSQL<-"D:/OHDSI/bv/CovidVaccineSafetyv1_2_0/Output/cohortSql/cte"

files<-list.files(pathtoSQL, full.names = TRUE)

### Get just the files that end in .sql
files<-files[grep('.sql', files)]
files[13]

customsqlfolder<-"D:/OHDSI/bv/CovidVaccineSafetyv1_2_0/Output/customSQL"

####### translate all prepared sql files in OHDSI package
source("D:/OHDSI/bv/CustomCohortExecution/OHDSI_SQLDomainCritSubquery2Temp.R")
source("D:/OHDSI/bv/CustomCohortExecution/createdomaincrit.R")

### Get the actual cohorts from the files (assuming that removing the path and .sql will return the cohort number)
cohorts_files<-gsub('.sql','',gsub(paste(pathtoSQL, '/', sep=""), '', files))
cohorts_files<-as.numeric(cohorts_files)
#cohorts_files
#files
##get all concept sets & find the domains used in this cohort sql file
codesettable<-AllCodesets(pathtoSQL, connectionDetails, cohortDatabaseSchema, rebuild=TRUE)

#create domain criteria dataset with known OHDSI cohort OMOP domains
domaincrit<-createDomaincrit()

#cohorts_files[1]
#recreate all cohort files in loop into customsqlfolder location
for (f in 1:length(files)){
  #f<-13
  print(paste("Currently editing the following file: ",files[f], sep=""))
  ##Pull in SQL file
  sql <- SqlRender::readSql(files[f])
  ##get all possible OMOP domains for sql file
  posdomains<-codesettable %>%
    filter(COHORT_DEFINITION_ID==cohorts_files[f]) %>%
    distinct(DOMAIN_ID)
  
  domainsInfile<-posdomains$DOMAIN_ID
  newfile<-paste(customsqlfolder,"/",cohorts_files[f], ".sql", sep="")
  
  VAcustomSQL(ogfilepath=files[f], 
              domainsInfile=domainsInfile, 
              newfilepath=newfile)
  }


##run all of the sql cohorts (THE BELOW FUNCTION DOES NOT OVERWRITE COHORTS), unless they run longer than 20 minutes
pathtoSQL<-paste0(outputFolder,"/customSQL")
source("D:/OHDSI/bv/CustomCohortExecution/CustomSQL_FunctionsBV.R")
st<-Sys.time()
print(paste("process started at ", st))
SQLCohortsCreate(path = pathtoSQL, 
                 timeoutInSeconds = 300, 
                 cohortDatabaseSchema, 
                 cohortTable,
                 connectionDetails)  ##this function still freezes R sometimes, likely due to issues with underlying SQL scripts
#started around 11:20PM
end<-Sys.time()
print(paste("process started at ", st, "; ended at ", end, ";", sep=""))





