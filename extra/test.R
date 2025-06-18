domainsInfile <- createDomaincrit()

translateToCustomVaSql("~/Dropbox/Projects/VaTools/test/ID101100000.sql", domainsInfile, "~/Dropbox/Projects/VaTools/new.sql")



# old_sql <- SqlRender::readSql("~/Dropbox/Projects/VaTools/test/ID101100000.sql")

library(Capr)

# Hypertension
essentialHypertension <- cs(
  descendants(320128),
  name = "Essential hypertension"
)

sbp <- cs(3004249, name = "SBP")
dbp <- cs(3012888, name = "DBP")

hypertensionCohort <- cohort(
  entry = entry(                 # Entrance based on any of these
    conditionOccurrence(essentialHypertension),
    measurement(sbp, valueAsNumber(gte(130)), unit(8876)),
    measurement(dbp, valueAsNumber(gte(80)), unit(8876))
  ),
  exit = exit(
    endStrategy = observationExit()
  )
)

# Acute myocardial infarction
myocardialInfarction <- cs(
  descendants(4329847),
  exclude(descendants(314666)), # Old myocardial infarction
  name = "Myocardial infarction"
)
inpatientOrEr <- cs(
  descendants(9201),
  descendants(262),
  name = "Inpatient or ER"
)
amiCohort <- cohort(
  entry = entry(
    conditionOccurrence(myocardialInfarction),
    additionalCriteria = withAll(
      atLeast(1,
              visit(inpatientOrEr),
              aperture = duringInterval(eventStarts(-Inf, 0), eventEnds(0, Inf)))
    ),
    primaryCriteriaLimit = "All",
    qualifiedLimit = "All"
  ),
  attrition = attrition(
    "No prior AMI" = withAll(
      exactly(0,
              conditionOccurrence(myocardialInfarction),
              duringInterval(eventStarts(-365, -1)))
    )
  ),
  exit = exit(
    endStrategy = fixedExit(index = "startDate", offsetDays = 1)
  )
)
cohortDefinitionSet <- makeCohortSet(hypertensionCohort, amiCohort)

old_sql <- cohortDefinitionSet$sql[2]

new_sql <- VaTools::translateToCustomVaSqlUsingJava(old_sql,
                                                    refactorNestedCriteria = TRUE,
                                                    addIndicesToDomainCriteria = FALSE,
                                                    addIndicesToNestedCriteria = FALSE)
SqlRender::writeSql(new_sql, targetFile = "new.sql")
