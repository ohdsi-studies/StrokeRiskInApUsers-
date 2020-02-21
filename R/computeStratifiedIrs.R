# Copyright 2018 Observational Health Data Sciences and Informatics
#
# This file is part of epi581v4
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

#' @export
computeCrudeIrs <- function(connectionDetails, 
                            studyFolder,
                            databases) {

  resultsFolders <- file.path(studyFolder, databases)
  reportFolder <- file.path(studyFolder, "report")
  if (!file.exists(reportFolder)) {
    dir.create(reportFolder)
  }
  dataFolder <- file.path(studyFolder, "shinyDataAll")
  
  relabel <- function(var, oldLabel, newLabel) {
    levels(var)[levels(var) == oldLabel] <- newLabel
    return(var)
  }
  toChar <- function(df) {
    facs <- sapply(df, is.factor)
    df[facs] <- lapply(df[facs], as.character)  
    return(df)
  }
  
  exposureRef <- readRDS(file.path(dataFolder, "exposure_of_interest.rds"))[, -3]
  exposureRef$exposure_name <- relabel(exposureRef$exposure_name, "[Epi 581] T1. New users of typical antipsychotics age 18-64 without a recent dementia diagnosis", "Typicals, 18-64y, no dementia")
  exposureRef$exposure_name <- relabel(exposureRef$exposure_name, "[Epi 581] T2. New users of haloperidol age 18-64 without a recent dementia diagnosis", "Haloperidol, 18-64y, no dementia")
  exposureRef$exposure_name <- relabel(exposureRef$exposure_name, "[Epi 581] T3. New users of typical antipsychotics age 65 years and older", "Typicals, >=65y")
  exposureRef$exposure_name <- relabel(exposureRef$exposure_name, "[Epi 581] T4. New users of haloperidol age 65 years and older", "Haloperidol, >=65y")
  exposureRef$exposure_name <- relabel(exposureRef$exposure_name, "[Epi 581] T5. New users of typical antipsychotics age 18-64", "Typicals, 18-64y")
  exposureRef$exposure_name <- relabel(exposureRef$exposure_name, "[Epi 581] T6. New users of haloperidol aged 18 to 64 years", "Haloperidol, 18-64y")
  exposureRef$exposure_name <- relabel(exposureRef$exposure_name, "[Epi 581] C1. New users of atypical antipsychotics age 18-64 without a recent dementia diagnosis", "Atypicals, 18-64y, no dementia")
  exposureRef$exposure_name <- relabel(exposureRef$exposure_name, "[Epi 581] C2. New users of atypical antipsychotics aged 65 years and older", "Atypicals, >=65y")
  exposureRef$exposure_name <- relabel(exposureRef$exposure_name, "[Epi 581] C3. New users of atypical antipsychotics age 18-64", "Atypicals, 18-64y")
  exposureRef <- toChar(exposureRef)
  
  outcomeRef <- readRDS(file.path(dataFolder, "outcome_of_interest.rds"))[, -3]
  outcomeRef$outcome_name <- relabel(outcomeRef$outcome_name, "[Epi 581] O2: Stroke as a Principal Inpatient Diagnosis ICD10 Update", "Stroke, principal inpatient diagnosis")
  outcomeRef <- toChar(outcomeRef)
  
  comparisonSummary <- readRDS(file.path(dataFolder, "comparison_summary.rds"))[, 3:5]
  comparisonSummary <- toChar(comparisonSummary)
  
  analysisRef <- readRDS(file.path(dataFolder, "cohort_method_analysis.rds"))[, -3]
  analysisRef <- analysisRef[analysisRef$analysis_id %in% 1:2, ]
  analysisRef$description <- relabel(analysisRef$description, "1. Unadjusted", "[2001.01.01-2017.12.31] Unadjusted")
  analysisRef$description <- relabel(analysisRef$description, "2. Unadjusted 2015", "[2001.01.01-2015.09.30] Unadjusted")
  analysisRef <- toChar(analysisRef)
  analysisRef <- merge(analysisRef, comparisonSummary)
  analysisRef$min_date <- gsub("-", "", analysisRef$min_date)
  analysisRef$max_date <- "20171231"
  analysisRef$max_date[analysisRef$analysis_id == 2] <- "20150930"
  analysisRef <- unique(analysisRef)
  analysisRef <- merge(analysisRef, exposureRef)
  analysisRef <- merge(analysisRef, outcomeRef)
  
  drops <- (analysisRef$database_id == "CCAE" & analysisRef$exposure_id %in% c(8163, 8172, 8175)) | 
    (analysisRef$database_id == "MDCR" & analysisRef$exposure_id %in% c(8173, 8176, 8174, 8571, 8572, 8573))
  analysisRef <- analysisRef[!drops, ]
  
  analysisRef$analysis_order <- match(analysisRef$analysis_id, c(1,2))
  analysisRef$db_order <- match(analysisRef$database_id, c("CCAE", "MDCR"))
  analysisRef <- analysisRef[order(analysisRef$analysis_order, analysisRef$db_order), ]
  analysisRef$analysis_order <- NULL
  analysisRef$db_order <- NULL
  
  covariateSettings <- FeatureExtraction::createCovariateSettings(useDemographicsGender = TRUE,
                                                                  useConditionGroupEraLongTerm = TRUE,
                                                                  includedCovariateConceptIds = c(435783, 8532),
                                                                  addDescendantsToInclude = TRUE)
  
  getIrIp <- function(studyPop) {
    targetPatients <- sum(studyPop$treatment == 1)
    targetPersonYears <- sum(studyPop$survivalTime[studyPop$treatment == 1]) / 365.25
    targetEvents <- sum(ifelse(studyPop$outcomeCount[studyPop$treatment == 1] > 0, 1, 0))
    result <- data.frame(tPatients = formatC(targetPatients, big.mark = ",", format = "d"),
                         tEvents = formatC(targetEvents, big.mark = ",", format = "d"),
                         tPy = formatC(targetPersonYears, big.mark = ",", format = "d"),
                         tIr1k = round(1000 * targetEvents / targetPersonYears, 3),
                         tIp1k = round(1000 * targetEvents / targetPatients, 3))
    return(result)
  }
  
  createIrIpTable <- function(df, type) {
    irOverall <- cbind(strata = "Overall", getIrIp(df))
    irFemale <- cbind(strata = "Female", getIrIp(df[df$female == 1, ]))
    irMale <- cbind(strata = "Male", getIrIp(df[df$female == 0, ]))
    irSchiz <- cbind(strata = "Schizophrenia", getIrIp(df[df$schizophrenia == 1, ]))
    irNoSchiz <- cbind(strata = "No schizophrenia", getIrIp(df[df$schizophrenia == 0, ]))
    result <- cbind(type, rbind(irOverall,
                                irFemale,
                                irMale,
                                irSchiz,
                                irNoSchiz))
    return(result)
  }
  
  getIrs <- function(row) {  
    irDataFolder <- file.path(dbIrFolder, paste0("cmData_a", row$analysis_id, "_e", row$exposure_id))
    if (!file.exists(irDataFolder)) {
      dir.create(irDataFolder, recursive = TRUE)
      cmData <- CohortMethod::getDbCohortMethodData(connectionDetails = connectionDetails,
                                                    studyStartDate = row$min_date,
                                                    studyEndDate = row$max_date,
                                                    cdmDatabaseSchema = cdmDatabaseSchema,
                                                    targetId = row$exposure_id,
                                                    comparatorId = 0,
                                                    outcomeIds = row$outcome_id,
                                                    exposureDatabaseSchema = cohortDatabaseSchema,
                                                    exposureTable = cohortTable,
                                                    outcomeDatabaseSchema = cohortDatabaseSchema,
                                                    outcomeTable = cohortTable,
                                                    covariateSettings = covariateSettings)
      CohortMethod::saveCohortMethodData(cmData, irDataFolder)
      covariates <- as.data.frame(cmData$covariates)
      covariates <- reshape(covariates, idvar = "rowId", timevar = "covariateId", direction = "wide")
      covariates[is.na(covariates)] <- 0
      skips <- c("rowId", "covariateValue.8532001")
      keeps <- names(covariates)[names(covariates) != skips]
      covariates$schizophrenia <- ifelse(rowSums(covariates[, c(keeps)] == 1) > 0, 1, 0)
      covariates$female <- covariates$covariateValue.8532001
      covariates <- covariates[, c("rowId", "female", "schizophrenia")]
    } else {
      cmData <- CohortMethod::loadCohortMethodData(irDataFolder)
      covariates <- as.data.frame(cmData$covariates)
      covariates <- reshape(covariates, idvar = "rowId", timevar = "covariateId", direction = "wide")
      covariates[is.na(covariates)] <- 0
      skips <- c("rowId", "covariateValue.8532001")
      keeps <- names(covariates)[names(covariates) != skips]
      covariates$schizophrenia <- ifelse(rowSums(covariates[, c(keeps)] == 1) > 0, 1, 0)
      covariates$female <- covariates$covariateValue.8532001
      covariates <- covariates[, c("rowId", "female", "schizophrenia")]
    }
    firstPostIndexStudyPop <- CohortMethod::createStudyPopulation(cohortMethodData = cmData, 
                                                                  outcomeId = row$outcome_id,
                                                                  removeSubjectsWithPriorOutcome = FALSE,
                                                                  minDaysAtRisk = 1,
                                                                  riskWindowStart = 0,
                                                                  addExposureDaysToStart = FALSE,
                                                                  riskWindowEnd = 0,
                                                                  addExposureDaysToEnd = TRUE)
    firstPostAttrition <- attr(firstPostIndexStudyPop, "metaData")$attrition
    firstPostIndexStudyPop <- merge(firstPostIndexStudyPop, covariates, all.x = TRUE)
    firstPostIndexStudyPop[is.na(firstPostIndexStudyPop)] <- 0

    firstEverStudyPop <- CohortMethod::createStudyPopulation(cohortMethodData = cmData, 
                                                             outcomeId = row$outcome_id,
                                                             removeSubjectsWithPriorOutcome = TRUE,
                                                             priorOutcomeLookback = 9999,
                                                             minDaysAtRisk = 1,
                                                             riskWindowStart = 0,
                                                             addExposureDaysToStart = FALSE,
                                                             riskWindowEnd = 0,
                                                             addExposureDaysToEnd = TRUE)
    firstEverAttrition <- attr(firstEverStudyPop, "metaData")$attrition
    firstEverStudyPop <- merge(firstEverStudyPop, covariates, all.x = TRUE)
    firstEverStudyPop[is.na(firstEverStudyPop)] <- 0
    
    irIpTable <- rbind(createIrIpTable(firstPostIndexStudyPop, "First post-index event"),
                       createIrIpTable(firstEverStudyPop, "First ever event"))
    irIpTable <- cbind(exposure = row$exposure_name, irIpTable)
    irIpTable <- cbind(description = row$description, irIpTable)
    irIpTable$description <- gsub("\\[|\\]", "", irIpTable$description)
    irIpTable$description <- gsub("\\[|\\]", "", irIpTable$description)
    irIpTable$description <- gsub(" Unadjusted", "", irIpTable$description)
    names(irIpTable) <- c("Exposure cohort", "Study period", "Event type", "Strata", "Persons", "Events", "PY", "IR/1k PY", "IP/1k Persons")

    attritionTable <- rbind(cbind(type = "First post-index event", firstPostAttrition),
                            cbind(type = "First ever event", firstEverAttrition))[, 1:3]
    attritionTable <- cbind(exposure = row$exposure_name, attritionTable)
    attritionTable <- cbind(description = row$description, attritionTable)
    attritionTable$description <- gsub("\\[|\\]", "", attritionTable$description)
    attritionTable$description <- gsub("\\[|\\]", "", attritionTable$description)
    attritionTable$description <- gsub(" Unadjusted", "", attritionTable$description)
    names(attritionTable) <- c("Exposure cohort", "Study period", "Event type", "Attrition description", "Persons")

    resultList <- list(irIpTable, attritionTable)
    return(resultList)
  }
  
  for (databaseId in unique(analysisRef$database_id)) { # databaseId <- "CCAE"
    if (databaseId == "CCAE") {
      cdmDatabaseSchema <- "cdm_ibm_ccae_v870.dbo"
      cohortDatabaseSchema <- "scratch.dbo"
      cohortTable <- "epi_581_ccae"
    }
    if (databaseId == "MDCR") {
      cdmDatabaseSchema = "cdm_ibm_mdcr_v871.dbo"
      cohortDatabaseSchema <- "scratch.dbo"
      cohortTable <- "epi_581_mdcr"
    }
    dbIrFolder <- file.path(studyFolder, databaseId, "irData")
    if (!file.exists(dbIrFolder)) {
      dir.create(dbIrFolder)
    }
    dbAnalysisRef <- analysisRef[analysisRef$database_id == databaseId, ]
    analysisIrIpTable <- data.frame()
    analysisAttritionTable <- data.frame()
    for (analysisId in unique(analysisRef$analysis_id)) { # analysisId <- 1
      aDbAnalysisRef <- dbAnalysisRef[dbAnalysisRef$analysis_id == analysisId, ]
      irIpTable <- data.frame()
      attritionTable <- data.frame()
      for (i in 1:nrow(aDbAnalysisRef)) { # i <- 1
        row <- aDbAnalysisRef[i, ]
        resultList <- getIrs(row)
        irIpTable <- rbind(irIpTable, resultList[[1]])
        attritionTable <- rbind(attritionTable, resultList[[2]])
      }
      analysisIrIpTable <- rbind(analysisIrIpTable, irIpTable)
      analysisAttritionTable <- rbind(analysisAttritionTable, attritionTable)
    }
    write.csv(analysisIrIpTable, file.path(dbIrFolder, "irIpTable.csv"), row.names = FALSE)
    write.csv(analysisAttritionTable, file.path(dbIrFolder, "attritionTable.csv"), row.names = FALSE)
  }
}
