#####################################################################
#
#  Convert a prepared (already has databases) OHDSI SQL cohort file
#    into a custom VA file with temp tables and indexes
#     Inputs needed:
#           (1) .sql filepath
#           (2) vector of domains in the cohort codeset [e.g., c(condition, procedure)]
#           (3) filepath for newly created .sql file
# requires SqlRender, dplyr, and createdomaincrit.R file
###################################################################


############ THINGS TO CHANGE
#  Change to nonclustered rowstore indexes (clustered rowstore also take longer to build b/c they take more space)



##find file in folder
# #folder path
# pathtoSQL<-"O:/VINCI_COVIDNLP/prone/OHDSI_Definitions/ProneSQL"
# files<-list.files(pathtoSQL, full.names = TRUE)
#
# ### Get just the files that end in .sql
# files<-files[grep('.sql', files)]
# files
# # #inputs
# ogfilepath<-files[17]
# f<-17
# #domainsInfile<-c('Visit','Measurement','Condition', 'Observation')
# #newfilepath<-"O:/VINCI_COVIDNLP/prone/OHDSI_Definitions/ProneSQL/tempsql_translated.sql"
# #get all codesets in a table
# source("D:/OHDSI/bv/CustomCohortExecution/createdomaincrit.R")
# codesettable<-AllCodesets(pathtoSQL, connectionDetails, cohortDatabaseSchema, rebuild=TRUE)
# ### Get the actual cohorts from the files (assuming that removing the path and .sql will return the cohort number)
# cohorts_files<-gsub('.sql','',gsub(paste(pathtoSQL, '/', sep=""), '', files))
# cohorts_files<-as.numeric(cohorts_files)
#
#
# print(paste("Currently editing the following file: ",files[f], sep=""))
# ##Pull in SQL file
# sql <- SqlRender::readSql(files[f])
# ##get all possible OMOP domains for sql file
# posdomains<-codesettable %>%
#   filter(COHORT_DEFINITION_ID==cohorts_files[f]) %>%
#   distinct(DOMAIN_ID)
#
# domainsInfile<-posdomains$DOMAIN_ID
# newfile<-paste(customsqlfolder,"/",cohorts_files[f], ".sql", sep="")
#
# VAcustomSQL(ogfilepath=files[f],
#             domainsInfile=domainsInfile,
#             newfilepath=newfile)










#
# VAcustomSQL(ogfilepath, domainsInfile, newfilepath)
#
#function

#' @importFrom dplyr %>%
#' @export
translateToCustomVaSql <- function(ogfilepath,
                                   domainsInfile,
                                   newfilepath) {
  # library(dplyr)
  ##Pull in SQL file
  sql <- SqlRender::readSql(ogfilepath)

  ##get all possible OMOP domains for sql file
  # source("D:/OHDSI/bv/CustomCohortExecution/createdomaincrit.R")
  domaincrit2<-createDomaincrit()
  # domaincrit2<-domaincrit %>%
  #   filter(domains %in% domainsInfile)


  ####### Locate domain criteria subqueries (Easy to find with comments)
  ###    --Begin 'domain' Criteria (e.g.,'-- Begin Condition Occurrence Criteria')
  ###    AND
  ###    --End 'domain' Criteria (e.g.,'-- End Condition Occurrence Criteria')

  alllocs<-data.frame()
  for (i in 1:length(domaincrit2$domains)){
    #i<-2
    print(domaincrit2$domains[i])
    startlocs<-stringr::str_locate_all(sql, domaincrit2$beginnings[i])[[1]][,1]
    #startlocs
    if (length(startlocs)>0){ #sometimes a domain exists in codeset but is unused in cohort (see 2367)
      endlocs<-stringr::str_locate_all(sql, domaincrit2$ends[i])[[1]][,2]
      locs<-as.data.frame(cbind(domaincrit2$domains[i], startlocs, endlocs))
      alllocs<-rbind(alllocs,locs)
    }

  }
  #colnames(alllocs)[1]<-"domains"
  print('OMOP Domains contained in this concept set include:')
  print(unique(alllocs$V1))

  locs<-alllocs %>%
    dplyr::arrange(as.numeric(startlocs)) %>% ##order by character location in script
    dplyr::mutate(critord=dplyr::row_number())


  ##### ALTER SQL
  #Extract subquery of domain criteria,
  #identify codeset_id number,
  #create tablename  =  paste("#", [domain],"crit",[codesetid])
  #ensure that subqueries with the same tablename are all the same (if they differ, add sub-id)
  #insert tablename into subquery in INTO statement and CREATE INDEX ON statement
  #insert into original sql after #codeset creation
  #and replace every instance of the extracted subquery with the name of the temp tablename

  #extract subquery
  allcrit<-c()
  for (i in 1:length(locs$critord)){
    #i<-2
    print(locs$V1[i])
    sqlcrit<-substr(sql, locs$startlocs[i], locs$endlocs[i])
    #sqlcrit
    allcrit<-c(allcrit,sqlcrit)
  }
  locs<-cbind(locs, allcrit)


  #identify codeset_id and create tablename
  tablenames<-c()
  for (i in 1:length(locs$critord)){
    #i<-1
    print(locs$V1[i])
    csid<-stringr::str_extract(locs$allcrit[i], "codeset_id =\\s*(\\d+)")
    #csid
    tabname<-paste0("#",locs$V1[i],"Crit",stringr::str_extract(csid, "\\d+"))
    #tabname
    tablenames<-c(tablenames,tabname)
  }
  locs<-cbind(locs, tablenames)

  #identify if a criteria is different while the table name is the same (codeset is the same)
  #many of the locs rows identify the same subquery used multiple times within the cohort script
  #for these, find the unique subqueries and insert in the new temp table script after codeset creation

  locs2<-locs %>%
    dplyr::group_by( tablenames) %>%
    #arrange(allcrit) %>%
    dplyr::mutate(table_ID=dplyr::dense_rank(allcrit))

  locs$tablenames<-paste0(locs2$tablenames,'_', locs2$table_ID)


  #create new temp table query
  newtemps<-c()
  for (i in 1:length(locs$critord)){
    #i<-1
    FirstFrom<-stringr::str_locate(tolower(locs$allcrit[i]), 'from')[1] #find the first 'from' in subquery to put INTO #domaincrit before it
    EndCrit<-stringr::str_locate(locs$allcrit[i], '-- End')[1]

    newtemp<-paste0(stringr::str_sub(locs$allcrit[i], 1, FirstFrom-1),
                    'INTO ', locs$tablenames[i], '\r\n',
                    stringr::str_sub(locs$allcrit[i], FirstFrom, EndCrit-1),
                    ';\r\n CREATE CLUSTERED COLUMNSTORE INDEX idx ON ',
                    locs$tablenames[i], ';\r\n',
                    stringr::str_sub(locs$allcrit[i], EndCrit, -1L), sep="")
    newtemps<-c(newtemps, newtemp)

  }
  locs<-cbind(locs, newtemps)

  ##Locate end of concept_set creation (the second semicolon is the end)
  conceptsetend<-stringr::str_locate_all(sql, ';')[[1]][2,1]

  ##add index for #codeset and unique subqueries transformed to temp tables to script after conceptsetend
  newsql<-paste0(stringr::str_sub(sql, 1, conceptsetend),
                 '\r\n CREATE CLUSTERED COLUMNSTORE INDEX idx ON #Codesets;\r\n',
                 paste(unique(locs$newtemps), collapse = "\r\n")
  )


  ##replace subqueries with temptables
  sqlparts<-c(newsql)
  if (length(locs$critord)==1){
    #When there is only 1 location to change, Add conceptset and new temps to (select * FROM #temp) and remainder of script
    print(paste0('Domain: ',locs$V1[i], '; ',
                 'Character start:', locs$startlocs[i], '; ',
                 'Character end:', locs$endlocs[i], sep = ""))
    sql_next<-paste0(stringr::str_sub(sql, conceptsetend+1, as.numeric(locs$startlocs[1])-1),
                     "SELECT * FROM ", locs$tablenames[i])
    sql_end<-stringr::str_sub(sql, as.numeric(locs$endlocs[1])+1, -1L)
    sqlparts<-c(sqlparts,sql_next, sql_end)
  } else {
    for (i in 1:length(locs$critord)){
      #i<-1
      print(paste0('Domain: ',locs$V1[i], '; ',
                   'Character start:', locs$startlocs[i], '; ',
                   'Character end:', locs$endlocs[i], sep = ""))
      #get next section of unrevised code and paste it to section of revised code
      if (i==1){ #first row starts from conceptsetend to end of current row
        sql_next<-paste0(stringr::str_sub(sql, conceptsetend+1, as.numeric(locs$startlocs[i])-1),
                         "SELECT * FROM ", locs$tablenames[i])
      } else if (i==length(locs$critord)){ #last row starts from end of row prior to end of code
        sql_next<-paste0(stringr::str_sub(sql, as.numeric(locs$endlocs[i-1])+1, as.numeric(locs$startlocs[i])-1),
                         "SELECT * FROM ", locs$tablenames[i],
                         stringr::str_sub(sql, as.numeric(locs$endlocs[i])+1, -1L))
      } else {#all other rows start from end of row prior to end of current row
        sql_next<-paste0(stringr::str_sub(sql, as.numeric(locs$endlocs[i-1])+1, as.numeric(locs$startlocs[i])-1),
                         "SELECT * FROM ", locs$tablenames[i])
      }
      sqlparts<-c(sqlparts,sql_next)
    }
  }

  ##combine sql parts all back together
  newsql<-paste(sqlparts, collapse="\r\n")

  # Delete temp tables
  deleteString <- paste(
    lapply(locs$tablenames, function(tmpTableName) {
      paste0("TRUNCATE TABLE ", tmpTableName, ";\n",
             "DROP TABLE ", tmpTableName, ";\n")
    }),
    collapse = "\n")

  newsql <- paste(newsql, "\n-- DELETE TEMP TABLES\n", deleteString, collapse = "\n")

  SqlRender::writeSql(newsql, targetFile=newfilepath)
}







