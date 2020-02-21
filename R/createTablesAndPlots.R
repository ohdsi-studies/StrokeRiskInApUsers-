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
createTablesAndPlots <- function(studyFolder,
                                 createTable1,
                                 createHrTable,
                                 createHrAttritionTable,
                                 createMdrrTable,
                                 createDiagnosticsPlot,
                                 createForestPlot) {
  
  if (createTable1 | createHrTable | createHrAttritionTable | createMdrrTable | createDiagnosticsPlot | createForestPlot) {
    source("inst/shiny/EvidenceExplorer/DataPulls.R")
    source("inst/shiny/EvidenceExplorer/PlotsAndTables.R")
    dataFolder <- file.path(studyFolder, "shinyDataAll")
    reportFolder <- file.path(studyFolder, "report")
    if (!file.exists(reportFolder)) {
      dir.create(reportFolder)
    }
    splittableTables <- c("covariate_balance", "preference_score_dist", "kaplan_meier_dist")
    files <- list.files(dataFolder, pattern = ".rds")
    
    # load files
    loadFile <- function(file) {
      tableName <- gsub("(_t[0-9]+_c[0-9]+)*\\.rds", "", file) 
      camelCaseName <- SqlRender::snakeCaseToCamelCase(tableName)
      if (!(tableName %in% splittableTables)) {
        newData <- readRDS(file.path(dataFolder, file))
        colnames(newData) <- SqlRender::snakeCaseToCamelCase(colnames(newData))
        if (exists(camelCaseName, envir = .GlobalEnv)) {
          existingData <- get(camelCaseName, envir = .GlobalEnv)
          newData <- rbind(existingData, newData)
        }
        assign(camelCaseName, newData, envir = .GlobalEnv)
      }
      invisible(NULL)
    }
    lapply(files, loadFile)
    
    # reference
    ref <- unique(cohortMethodResult[, c("analysisId", "targetId", "comparatorId", "outcomeId")])
    ref <- ref[ref$outcomeId %in% outcomeOfInterest$outcomeId, ]
    ref <- merge(ref, database[, c("databaseId", "databaseName")])
    drops <- (ref$databaseId == "CCAE" & ref$targetId %in% c(8163, 8172)) | 
             (ref$databaseId == "MDCR" & ref$targetId %in% c(8173, 8174, 8571, 8572))
    ref <- ref[!drops, ]
    
    relabel <- function(var, oldLabel, newLabel) {
      levels(var)[levels(var) == oldLabel] <- newLabel
      return(var)
    }
    exposureOfInterest$exposureName <- relabel(exposureOfInterest$exposureName, "[Epi 581] T1. New users of typical antipsychotics age 18-64 without a recent dementia diagnosis", "Typicals, 18-64y, no dementia")
    exposureOfInterest$exposureName <- relabel(exposureOfInterest$exposureName, "[Epi 581] T2. New users of haloperidol age 18-64 without a recent dementia diagnosis", "Haloperidol, 18-64y, no dementia")
    exposureOfInterest$exposureName <- relabel(exposureOfInterest$exposureName, "[Epi 581] T3. New users of typical antipsychotics age 65 years and older", "Typicals, >=65y")
    exposureOfInterest$exposureName <- relabel(exposureOfInterest$exposureName, "[Epi 581] T4. New users of haloperidol age 65 years and older", "Haloperidol, >=65y")
    exposureOfInterest$exposureName <- relabel(exposureOfInterest$exposureName, "[Epi 581] T5. New users of typical antipsychotics age 18-64", "Typicals, 18-64y")
    exposureOfInterest$exposureName <- relabel(exposureOfInterest$exposureName, "[Epi 581] T6. New users of haloperidol aged 18 to 64 years", "Haloperidol, 18-64y")
    exposureOfInterest$exposureName <- relabel(exposureOfInterest$exposureName, "[Epi 581] C1. New users of atypical antipsychotics age 18-64 without a recent dementia diagnosis", "Atypicals, 18-64y, no dementia")
    exposureOfInterest$exposureName <- relabel(exposureOfInterest$exposureName, "[Epi 581] C2. New users of atypical antipsychotics aged 65 years and older", "Atypicals, >=65y")
    exposureOfInterest$exposureName <- relabel(exposureOfInterest$exposureName, "[Epi 581] C3. New users of atypical antipsychotics age 18-64", "Atypicals, 18-64y")
    outcomeOfInterest$outcomeName <- relabel(outcomeOfInterest$outcomeName, "[Epi 581] O2: Stroke as a Principal Inpatient Diagnosis ICD10 Update", "Stroke, principal inpatient diagnosis")
    cohortMethodAnalysis$description <- relabel(cohortMethodAnalysis$description, "1. Unadjusted", "[2001.01.01-2017.12.31] Unadjusted")
    cohortMethodAnalysis$description <- relabel(cohortMethodAnalysis$description, "2. Unadjusted 2015", "[2001.01.01-2015.09.30] Unadjusted")
    cohortMethodAnalysis$description <- relabel(cohortMethodAnalysis$description, "3. 1:1 PS Match on Selected Covariates", "[2001.01.01-2017.12.31] 1:1 PS match on select covariates")
    cohortMethodAnalysis$description <- relabel(cohortMethodAnalysis$description, "4. 1:1 PS Match on Selected Covariates 2015", "[2001.01.01-2015.09.30] 1:1 PS match on select covariates")
    cohortMethodAnalysis$description <- relabel(cohortMethodAnalysis$description, "5. 1:10 PS Match on Full Covariates", "[2001.01.01-2017.12.31] 1:10 PS match on full covariates")
    cohortMethodAnalysis$description <- relabel(cohortMethodAnalysis$description, "6. 1:10 PS Match on Full Covariates 2015", "[2001.01.01-2015.09.30] 1:10 PS match on full covariates")
  }
  
  if (createTable1) {
    for (databaseId in unique(ref$databaseId)) { # databaseId=database$databaseId[2]
    dbRef <- ref[ref$databaseId == databaseId, ]
    wb <- openxlsx::createWorkbook()
      for (i in 1:nrow(dbRef)) { # i=1
        analysisId <- dbRef$analysisId[i]
        targetId <- dbRef$targetId[i]
        comparatorId <- dbRef$comparatorId[i]
        outcomeId <- dbRef$outcomeId[i]
        databaseId <- factor(databaseId, levels(database$databaseId))
        balance <- getCovariateBalance2(connection = NULL,
                                        analysisId = analysisId,
                                        targetId = targetId,
                                        comparatorId = comparatorId,
                                        databaseId = databaseId,
                                        outcomeId = outcomeId,
                                        dataFolder = dataFolder)
        if (!is.null(balance)) {
          if (analysisId %in% c(3,4)) {
            spec <- "inst/shiny/EvidenceExplorer/Table1SpecsSelect.csv"
          } else {
            spec <- "inst/shiny/EvidenceExplorer/Table1Specs.csv"
          }
          table1 <- prepareTable1(balance = balance,
                                  beforeLabel = "Before PS adjustment",
                                  afterLabel = "After PS adjustment",
                                  targetLabel = "Target",
                                  comparatorLabel = "Comparator",
                                  percentDigits = 1,
                                  stdDiffDigits = 2,
                                  output = "latex",
                                  pathToCsv = spec)
          sheetName <- paste0("a", analysisId, "_t", targetId, "_c", comparatorId)
          openxlsx::addWorksheet(wb = wb, sheetName = sheetName)
          openxlsx::mergeCells(wb, sheetName, cols = 2:4, rows = 1)
          openxlsx::mergeCells(wb, sheetName, cols = 5:7, rows = 1)
          openxlsx::setColWidths(wb = wb,
                                 sheet = sheetName,
                                 cols = 1:ncol(table1),
                                 widths = "auto",
                                 ignoreMergedCells = TRUE)
          openxlsx::writeData(wb = wb,
                              sheet = sheetName,
                              x = table1,
                              colNames = TRUE,
                              rowNames = FALSE,
                              withFilter = FALSE)
        }
      }
      openxlsx::saveWorkbook(wb = wb,
                             file = file.path(reportFolder, paste0("pop_char_", databaseId, ".xlsx")),
                             overwrite = TRUE)
    }
  }
  
  if (createHrTable) {
    header1 <- c(rep("", 3),
                 "Target", rep("", 3),
                 "Comparator", rep("", 3),
                 rep("", 4))
    header2 <- c("Comparison", "Study period", "Analysis",
                 "N", "PY", "Events", "IR",
                 "N", "PY", "Events", "IR",
                 "HR (95% CI)", "p",
                 "Cal. HR (95% CI)", "Cal. p")
    header <- data.frame(rbind(header1, header2))
    for (databaseId in unique(ref$databaseId)) { # databaseId=database$databaseId[1]
      dbRef <- ref[ref$databaseId == databaseId, ] 
      dbRef$studyPeriod[dbRef$analysisId %in% c(1, 3, 5)] <- "2001.01.01 - 2017.12.31"
      dbRef$studyPeriod[dbRef$analysisId %in% c(2, 4, 6)] <- "2001.01.01 - 2015.09.30"
      dbRef$studyPeriodOrder <- match(dbRef$studyPeriod, c("2001.01.01 - 2017.12.31", "2001.01.01 - 2015.09.30"))
      dbRef <- dbRef[order(dbRef$studyPeriodOrder), ]
      dbRef$analysisOrder <- match(dbRef$analysisId, c(1, 3, 5, 2, 4, 6))
      dbRef <- dbRef[order(dbRef$analysisOrder), ]
      dbRef$targetOrder <- match(dbRef$targetId, c(8173, 8174, 8163, 8172, 8571, 8572))
      dbRef <- dbRef[order(dbRef$targetOrder), ]
      dbRef$comparatorOrder <- match(dbRef$comparatorId, c(8176, 8175, 8573))
      dbRef <- dbRef[order(dbRef$comparatorOrder), ]
      mainTable <- data.frame()
      for (i in 1:nrow(dbRef)) { # i=1
        analysisId <- dbRef$analysisId[i]
        targetId <- dbRef$targetId[i]
        comparatorId <- dbRef$comparatorId[i]
        outcomeId <- dbRef$outcomeId[i]
        databaseId <- factor(databaseId, levels(database$databaseId))
        Comparison <- paste(exposureOfInterest$exposureName[exposureOfInterest$exposureId == targetId],
                            exposureOfInterest$exposureName[exposureOfInterest$exposureId == comparatorId],
                            sep = " vs. ")
        studyPeriod <- dbRef$studyPeriod[i]
        mainResults <- getMainResults(connection = NULL,
                                      targetIds = targetId,
                                      comparatorIds = comparatorId,
                                      outcomeIds = outcomeId,
                                      databaseIds = databaseId,
                                      analysisIds = analysisId)
        hrs <- prepareMainResultsTable(mainResults = mainResults, analyses = cohortMethodAnalysis)
        irs <- preparePowerTable(mainResults = mainResults, analyses = cohortMethodAnalysis)
        names(irs)[names(irs) == "description"] <- "Analysis"
        irs <- irs[, c("Analysis",
                       "targetSubjects", "targetYears", "targetOutcomes", "targetIr",
                       "comparatorSubjects", "comparatorYears", "comparatorOutcomes", "comparatorIr")]
        row <- merge(irs, hrs)
        row <- cbind(Comparison, studyPeriod, row)
        mainTable <- rbind(mainTable, row)
        mainTable$Analysis <- gsub(".*]", "", mainTable$Analysis)
        
      }
      names(mainTable) <- names(header)
      mainTable <- rbind(header, mainTable)
      file <- file.path(reportFolder, paste0("hr_", databaseId, ".csv"))
      write.csv(mainTable, file, row.names = FALSE)
    }
  }
  
  if (createHrAttritionTable) {
    for (databaseId in unique(ref$databaseId)) { # databaseId=database$databaseId[1]
      dbRef <- ref[ref$databaseId == databaseId, ] 
      dbRef <- merge(dbRef, exposureOfInterest[, 1:2], by.x = "targetId", by.y = "exposureId")
      dbRef <- merge(dbRef, exposureOfInterest[, 1:2], by.x = "comparatorId", by.y = "exposureId")
      dbRef <- merge(dbRef, cohortMethodAnalysis[, 1:2])
      dbRef$studyPeriod[dbRef$analysisId %in% c(1, 3, 5)] <- "2001.01.01 - 2017.12.31"
      dbRef$studyPeriod[dbRef$analysisId %in% c(2, 4, 6)] <- "2001.01.01 - 2015.09.30"
      dbRef$studyPeriodOrder <- match(dbRef$studyPeriod, c("2001.01.01 - 2017.12.31", "2001.01.01 - 2015.09.30"))
      dbRef <- dbRef[order(dbRef$studyPeriodOrder), ]
      dbRef$analysisOrder <- match(dbRef$analysisId, c(1, 3, 5, 2, 4, 6))
      dbRef <- dbRef[order(dbRef$analysisOrder), ]
      dbRef$targetOrder <- match(dbRef$targetId, c(8173, 8174, 8163, 8172, 8571, 8572))
      dbRef <- dbRef[order(dbRef$targetOrder), ]
      dbRef$comparatorOrder <- match(dbRef$comparatorId, c(8176, 8175, 8573))
      dbRef <- dbRef[order(dbRef$comparatorOrder), ]
      attritionTable <- data.frame()
      for (i in 1:nrow(dbRef)) { # i = 1
        analysisId <- dbRef$analysisId[i]
        targetId <- dbRef$targetId[i]
        comparatorId <- dbRef$comparatorId[i]
        outcomeId <- dbRef$outcomeId[i]
        targetName <- dbRef$exposureName.x[i]
        comparatorName <- dbRef$exposureName.y[i]
        analysisName <- gsub(".*] ", "",  dbRef$description[i])
        comparison <- paste(targetName, comparatorName, sep = " vs. ")
        studyPeriod <- dbRef$studyPeriod[i]
        omFile <- file.path(studyFolder, databaseId, "cmOutput", paste0("Analysis_", analysisId),
                                                                 paste0("om_t", targetId,
                                                                        "_c", comparatorId,
                                                                        "_o", outcomeId,
                                                                        ".rds"))
        om <- readRDS(omFile)
        attrition <- om$attrition[, 1:3]
        attrition <- cbind(comparison = comparison,
                           studyPeriod = studyPeriod,
                           analysisName = analysisName,
                           attrition)
        attritionTable <- rbind(attritionTable, attrition)
      }
      attritionTable$description <- as.character(attritionTable$description)
      attritionTable$description[attritionTable$description == "First exp. only & first cohort only & 183 days of obs. prior"] <- "First cohort only"
      names(attritionTable) <- c("Comparison", "Study period", "Analysis", "Attrition description", "Target persons", "Comparator persons")
      attritionFile <- file.path(reportFolder, paste0("hr_attrition_", databaseId, ".csv"))
      write.csv(attritionTable, attritionFile, row.names = FALSE)
    }
  }
  
  if (createMdrrTable) {
    for (databaseId in unique(ref$databaseId)) { 
      idx <- cohortMethodResult$outcomeId == 8997 & cohortMethodResult$databaseId == databaseId
      powerTable <- preparePowerTable2(mainResults = cohortMethodResult[idx, ], analyses = cohortMethodAnalysis)
      powerTable <- merge(powerTable, exposureOfInterest[, 1:2], by.x = "targetId", by.y = "exposureId")
      powerTable <- merge(powerTable, exposureOfInterest[, 1:2], by.x = "comparatorId", by.y = "exposureId")
      powerTable$studyPeriod[powerTable$analysisId %in% c(1, 3, 5)] <- "2001.01.01 - 2017.12.31"
      powerTable$studyPeriod[powerTable$analysisId %in% c(2, 4, 6)] <- "2001.01.01 - 2015.09.30"
      powerTable$analysisOrder <- match(powerTable$analysisId, c(1, 3, 5, 2, 4, 6))
      powerTable$targetOrder <- match(powerTable$targetId, exposureOfInterest$exposureId)
      powerTable <- powerTable[order(powerTable$targetOrder, powerTable$analysisOrder), ]
      powerTable$analysisOrder <- NULL
      powerTable$targetOrder <- NULL
      powerTable$analysisId <- NULL
      powerTable$targetId <- NULL
      powerTable$comparatorId <- NULL
      powerTable$Comparison <- paste(powerTable$exposureName.x, powerTable$exposureName.y, sep = " vs. ")
      powerTable$description <- gsub(".*]", "", powerTable$description)
      powerTable <- powerTable[, c(14, 13, 1, 10)]
      names(powerTable) <- c("Comparison", "Study period", "Analysis", "MDRR")
      powerTable$Analysis <- gsub(".*]", "", powerTable$Analysis)
      file <- file.path(reportFolder, paste0("power_", databaseId, ".csv"))
      write.csv(powerTable, file, row.names = FALSE)
    }
  }
  
  if (createDiagnosticsPlot) {
    exposureOfInterest$exposureName <- relabel(exposureOfInterest$exposureName, "Typicals, 18-64y, no dementia", "T1")
    exposureOfInterest$exposureName <- relabel(exposureOfInterest$exposureName, "Haloperidol, 18-64y, no dementia", "T2")
    exposureOfInterest$exposureName <- relabel(exposureOfInterest$exposureName, "Typicals, >=65y", "T3")
    exposureOfInterest$exposureName <- relabel(exposureOfInterest$exposureName, "Haloperidol, >=65y", "T4")
    exposureOfInterest$exposureName <- relabel(exposureOfInterest$exposureName, "Typicals, 18-64y", "T5")
    exposureOfInterest$exposureName <- relabel(exposureOfInterest$exposureName, "Haloperidol, 18-64y", "T6")
    exposureOfInterest$exposureName <- relabel(exposureOfInterest$exposureName, "Atypicals, 18-64y, no dementia", "C1")
    exposureOfInterest$exposureName <- relabel(exposureOfInterest$exposureName, "Atypicals, >=65y", "C2")
    exposureOfInterest$exposureName <- relabel(exposureOfInterest$exposureName, "Atypicals, 18-64y", "C3")
    
    remove_geom <- function(ggplot2_object,
                            geom_type) {
      layers <- lapply(ggplot2_object$layers, function(x) {
        if (class(x$geom)[1] == geom_type) {
          NULL
        } else {
          x
        }
      })
      layers <- layers[!sapply(layers, is.null)]
      ggplot2_object$layers <- layers
      ggplot2_object
    }
    for (databaseId in unique(ref$databaseId)) { # databaseId=database$databaseId[1]
      dbRef <- ref[ref$databaseId == databaseId, ]
      tcs <- unique(dbRef[, c("targetId", "comparatorId")])
      for (i in 1:nrow(tcs)) { # i=1
        targetId <- tcs$targetId[i]
        comparatorId <- tcs$comparatorId[i]
        dbTcRef <- dbRef[dbRef$targetId == targetId & dbRef$comparatorId == comparatorId, ]
        
        psPlotList <- list()
        covBalPlotList <- list()
        nullPlotList <- list()
        kmPlotList <- list()
        
        for (j in 1:nrow(dbTcRef)) { # j=3
          analysisId <- dbRef$analysisId[j]
          outcomeId <- dbRef$outcomeId[j]
          databaseId <- factor(databaseId, levels(database$databaseId))
          targetName <- paste(exposureOfInterest$exposureName[exposureOfInterest$exposureId == targetId])
          comparatorName <- paste(exposureOfInterest$exposureName[exposureOfInterest$exposureId == comparatorId])
          
          if (analysisId %in% c(1,2)) {
            psPlotList[[length(psPlotList) + 1]] <- list(NULL)
            covBalPlotList[[length(covBalPlotList) + 1]] <- list(NULL)
            #null plot
            controlResults <- getControlResults(connection = NULL,
                                                analysisId = analysisId,
                                                targetId = targetId,
                                                comparatorId = comparatorId,
                                                databaseId = databaseId)
            # dbNullPlotList[[length(dbNullPlotList) + 1]] <- plotLargeScatter2(d = controlResults[controlResults$effectSize == 1, ], xLabel = "Hazard ratio")
            controlResultsHr1 <- controlResults[controlResults$effectSize == 1, ]
            controlResultsHr1$Significant <- controlResultsHr1$ci95Lb > 1 | controlResultsHr1$ci95Ub < 1
            oneRow <- data.frame(nLabel = paste0(formatC(nrow(controlResultsHr1), big.mark = ","), " estimates"),
                                 meanLabel = paste0(formatC(100 * mean(!controlResultsHr1$Significant, na.rm = TRUE), digits = 1, format = "f"), "% of CIs includes 1"))
            nullPlot <- EmpiricalCalibration::plotCalibrationEffect(logRrNegatives = controlResultsHr1$logRr,
                                                                    seLogRrNegatives = controlResultsHr1$seLogRr,
                                                                    xLabel = "Hazard ratio",
                                                                    showCis = TRUE)
            nullPlot <- remove_geom(nullPlot, "GeomPoint")
            nullPlot <- nullPlot + 
              ggplot2::geom_point(shape = 16,
                                  ggplot2::aes(x, y),
                                  data = data.frame(x = exp(controlResultsHr1$logRr), y = controlResultsHr1$seLogRr),
                                  size = 6,
                                  alpha = 0.5,
                                  color = rgb(0, 0, 0.8)) +
              ggplot2::theme(axis.text.x = ggplot2::element_text(size = 20),
                             axis.text.y = ggplot2::element_text(size = 20),
                             axis.title = ggplot2::element_text(size = 30))
            nullPlot <- nullPlot +
              ggplot2::geom_label(x = log(0.55), y = 1.5, alpha = 1, hjust = "left", ggplot2::aes(label = nLabel), size = 10, data = oneRow) +
              ggplot2::geom_label(x = log(0.55), y = 1.35, alpha = 1, hjust = "left", ggplot2::aes(label = meanLabel), size = 10, data = oneRow)
            nullPlotList[[length(nullPlotList) + 1]] <- nullPlot
            
            # km plot
            km <- getKaplanMeier2(connection = NULL,
                                  analysisId = analysisId,
                                  targetId = targetId,
                                  comparatorId = comparatorId,
                                  outcomeId = outcomeId,
                                  databaseId = databaseId,
                                  dataFolder = dataFolder)
            kmPlotList[[length(kmPlotList) + 1]] <- plotKaplanMeier2(kaplanMeier = km,
                                                                     targetName = targetName,
                                                                     comparatorName = comparatorName)
          } else {
            # ps plot
            ps <- getPs2(connection = NULL,
                         analysisId = analysisId,
                         targetIds = targetId,
                         comparatorIds = comparatorId,
                         databaseId = databaseId,
                         dataFolder = dataFolder)
            psPlotList[[length(psPlotList) + 1]] <- plotPs2(ps = ps,
                                                            targetName = targetName,
                                                            comparatorName = comparatorName)
            # bal plot
            balance <- getCovariateBalance2(connection = NULL,
                                            analysisId = analysisId,
                                            targetId = targetId,
                                            comparatorId = comparatorId,
                                            databaseId = databaseId,
                                            outcomeId = outcomeId,
                                            dataFolder = dataFolder)
            if (analysisId %in% c(3,4)) {
              covBalPlotList[[length(covBalPlotList) + 1]] <- plotCovariateBalanceScatterPlot2(balance = balance,
                                                                                               beforeLabel = "Before PS adjustment",
                                                                                               afterLabel = "After PS adjustment",
                                                                                               pathToCsv = "inst/shiny/EvidenceExplorer/selectCovariateIds.csv",
                                                                                               legendPosition = "none",
                                                                                               pointSize = 6,
                                                                                               textSize = 30)
            } else {
              covBalPlotList[[length(covBalPlotList) + 1]] <- plotCovariateBalanceScatterPlot(balance = balance,
                                                                                              beforeLabel = "Before PS adjustment",
                                                                                              afterLabel = "After PS adjustment",
                                                                                              pointSize = 6,
                                                                                              textSize = 30)
            }
            # null plot
            controlResults <- getControlResults(connection = NULL,
                                                analysisId = analysisId,
                                                targetId = targetId,
                                                comparatorId = comparatorId,
                                                databaseId = databaseId)
            # dbNullPlotList[[length(dbNullPlotList) + 1]] <- plotLargeScatter2(d = controlResults[controlResults$effectSize == 1, ], xLabel = "Hazard ratio")
            controlResultsHr1 <- controlResults[controlResults$effectSize == 1, ]
            controlResultsHr1$Significant <- controlResultsHr1$ci95Lb > 1 | controlResultsHr1$ci95Ub < 1
            oneRow <- data.frame(nLabel = paste0(formatC(nrow(controlResultsHr1), big.mark = ","), " estimates"),
                                 meanLabel = paste0(formatC(100 * mean(!controlResultsHr1$Significant, na.rm = TRUE), digits = 1, format = "f"), "% of CIs includes 1"))
            nullPlot <- EmpiricalCalibration::plotCalibrationEffect(logRrNegatives = controlResultsHr1$logRr,
                                                                    seLogRrNegatives = controlResultsHr1$seLogRr,
                                                                    xLabel = "Hazard ratio",
                                                                    showCis = TRUE)
            nullPlot <- remove_geom(nullPlot, "GeomPoint")
            nullPlot <- nullPlot + 
              ggplot2::geom_point(shape = 16,
                                  ggplot2::aes(x, y),
                                  data = data.frame(x = exp(controlResultsHr1$logRr), y = controlResultsHr1$seLogRr),
                                  size = 6,
                                  alpha = 0.5,
                                  color = rgb(0, 0, 0.8)) +
              ggplot2::theme(axis.text.x = ggplot2::element_text(size = 20),
                             axis.text.y = ggplot2::element_text(size = 20),
                             axis.title = ggplot2::element_text(size = 30))
            nullPlot <- nullPlot +
              ggplot2::geom_label(x = log(0.55), y = 1.5, alpha = 1, hjust = "left", ggplot2::aes(label = nLabel), size = 10, data = oneRow) +
              ggplot2::geom_label(x = log(0.55), y = 1.35, alpha = 1, hjust = "left", ggplot2::aes(label = meanLabel), size = 10, data = oneRow)
            nullPlotList[[length(nullPlotList) + 1]] <- nullPlot
            # km plot
            km <- getKaplanMeier2(connection = NULL,
                                  analysisId = analysisId,
                                  targetId = targetId,
                                  comparatorId = comparatorId,
                                  outcomeId = outcomeId,
                                  databaseId = databaseId,
                                  dataFolder = dataFolder)
            kmPlotList[[length(kmPlotList) + 1]] <- plotKaplanMeier2(kaplanMeier = km,
                                                                     targetName = targetName,
                                                                     comparatorName = comparatorName)
          }
        }
        # db specific trellis
        # 1. Unadjusted
        # 2. Unadjusted 2015
        # 3. 1:1 PS select
        # 4. 1:1 PS select 2015
        # 5. 1:10 PS full
        # 6. 1:10 PS full 2015
        
        col0 <- grid::textGrob("")
        col1 <- grid::textGrob("Preference score distribution", gp = grid::gpar(fontsize = 40))
        col2 <- grid::textGrob("Covariate balance", gp = grid::gpar(fontsize = 40))
        col3 <- grid::textGrob("Empirical null distribution", gp = grid::gpar(fontsize = 40))
        col4 <- grid::textGrob("Kaplan-Meier plot", gp = grid::gpar(fontsize = 40))
        row1 <- grid::textGrob("Unadjusted", rot = 90, gp = grid::gpar(fontsize = 40))
        row2 <- grid::textGrob("1:1 PS match select", rot = 90, gp = grid::gpar(fontsize = 40))
        row3 <- grid::textGrob("1:10 PS match full", rot = 90, gp = grid::gpar(fontsize = 40))
        blank <- grid::rectGrob(gp = grid::gpar(fill = NA, col = "white"))
        
        # full
        trellisPlot <- gridExtra::grid.arrange(col0, col1, col2, col3, col4,
                                               row1, blank, blank, nullPlotList[[1]], kmPlotList[[1]],
                                               row2, psPlotList[[3]], covBalPlotList[[3]], nullPlotList[[3]], kmPlotList[[3]],
                                               row3, psPlotList[[5]], covBalPlotList[[5]], nullPlotList[[5]], kmPlotList[[5]],
                                               nrow = 4,
                                               heights = c(5, rep(20, 3)),
                                               widths = c(5, rep(20, 4)))
        plotFile <- file.path(reportFolder, paste0("diagnostics_t", targetId, "_c", comparatorId, "_", databaseId, ".png"))
        ggplot2::ggsave(filename = plotFile, plot = trellisPlot, height = 35, width = 45)
        
        # up to 2015
        trellisPlot2015 <- gridExtra::grid.arrange(col0, col1, col2, col3, col4,
                                                   row1, blank, blank, nullPlotList[[2]], kmPlotList[[2]],
                                                   row2, psPlotList[[4]], covBalPlotList[[4]], nullPlotList[[4]], kmPlotList[[4]],
                                                   row3, psPlotList[[6]], covBalPlotList[[6]], nullPlotList[[6]], kmPlotList[[6]],
                                                   nrow = 4,
                                                   heights = c(5, rep(20, 3)),
                                                   widths = c(5, rep(20, 4)))
        plotFile <- file.path(reportFolder, paste0("diagnostics_t", targetId, "_c", comparatorId, "_", databaseId, "_2015.png"))
        ggplot2::ggsave(filename = plotFile, plot = trellisPlot2015, height = 35, width = 45)
      }
    }
  }
  
  if (createForestPlot) {
    forestFolder <- file.path(reportFolder, "forestPlots")
    if (!file.exists(forestFolder)) {
      dir.create(forestFolder, recursive = TRUE)
    }
    # 8173 typical no dementia
    # 8176 atypical no dementia
    # 8174 haldol no dementia
    # 2. Unadjusted 2015
    # 4. 1:1 PS Match on Selected Covariates 2015
    # 6. 1:10 PS Match on Full Covariates 2015
    ref <- data.frame(analysisId = c(2, 2, 4, 4, 6, 6),
                      targetId = c(8173, 8174, 8173, 8174, 8173, 8174),
                      comparatorId = c(8176, 8176, 8176, 8176, 8176, 8176))
    janssenData <- merge(ref, cohortMethodResult[cohortMethodResult$outcomeId == 8997, ])
    janssenData <- merge(exposureOfInterest[, c(1,2)], janssenData, by.x = "exposureId", by.y = "comparatorId")
    names(janssenData)[names(janssenData) == "exposureName"] <- "comparatorName"
    names(janssenData)[names(janssenData) == "exposureId"] <- "comparatorId"
    janssenData <- merge(exposureOfInterest[, c(1,2)], janssenData, by.x = "exposureId", by.y = "targetId")
    names(janssenData)[names(janssenData) == "exposureName"] <- "targetName"
    names(janssenData)[names(janssenData) == "exposureId"] <- "targetId"
    janssenData <- merge(cohortMethodAnalysis[, c(1,2)], janssenData)
    janssenData <- janssenData[, c("description", "targetName", "comparatorName", "calibratedRr", "calibratedCi95Lb", "calibratedCi95Ub", "calibratedP")]
    janssenDataTypical <- janssenData[janssenData$targetName == "Typicals, 18-64y, no dementia", ]
    janssenDataHaldol <- janssenData[janssenData$targetName == "Haloperidol, 18-64y, no dementia", ]
    sentinelData <- data.frame(description = c("[2001.01.01-2015.09.30] Sentinel unadjusted",
                                               "[2001.01.01-2015.09.30] Sentinel unadjusted",
                                               "[2001.01.01-2015.09.30] Sentinel 1:1 PS match on select",
                                               "[2001.01.01-2015.09.30] Sentinel 1:1 PS match on select"), 
                               targetName = c("Typicals, 18-64y, no dementia",
                                              "Haloperidol, 18-64y, no dementia",
                                              "Typicals, 18-64y, no dementia",
                                              "Haloperidol, 18-64y, no dementia"), 
                               comparatorName = c("Atypicals, 18-64y, no dementia",
                                                  "Atypicals, 18-64y, no dementia",
                                                  "Atypicals, 18-64y, no dementia",
                                                  "Atypicals, 18-64y, no dementia"),
                               calibratedRr = c(1.75, 1.80, 0.87, 1.31),
                               calibratedCi95Lb = c(1.17, 0.93, 0.54, 0.54),
                               calibratedCi95Ub = c(2.63, 3.48, 1.41, 3.21),
                               calibratedP = c(0.007, 0.083, 0.566, 0.553))
    sentinelDataTypical <- sentinelData[sentinelData$targetName == "Typicals, 18-64y, no dementia", ]
    sentinelDataHaldol <- sentinelData[sentinelData$targetName == "Haloperidol, 18-64y, no dementia", ]
    janssenTypicalForest <- generateForestPlot(janssenDataTypical)
    janssenTypicalForestFile <- file.path(forestFolder, "janssenTypicalForest.png")
    ggplot2::ggsave(filename = janssenTypicalForestFile, plot = janssenTypicalForest, height = 10, width = 10, units = "cm")
    janssenHaldolForest <- generateForestPlot(janssenDataHaldol)
    janssenHaldolForestFile <- file.path(forestFolder, "janssenHaldolForest.png")
    ggplot2::ggsave(filename = janssenHaldolForestFile, plot = janssenHaldolForest, height = 10, width = 10, units = "cm")
    sentinelTypicalForest <- generateForestPlot(sentinelDataTypical)
    sentinelTypicalForestFile <- file.path(forestFolder, "sentinelTypicalForest.png")
    ggplot2::ggsave(filename = sentinelTypicalForestFile, plot = sentinelTypicalForest, height = 10, width = 10, units = "cm")
    sentinelHaldolForest <- generateForestPlot(sentinelDataHaldol)
    sentinelHaldolForestFile <- file.path(forestFolder, "sentinelHaldolForest.png")
    ggplot2::ggsave(filename = sentinelHaldolForestFile, plot = sentinelHaldolForest, height = 10, width = 10, units = "cm")
  }
}

generateForestPlot <- function(plotData,
                               limits = c(0.25, 10)) {
  data <- data.frame(calibratedRr = plotData$calibratedRr,
                     calibratedCi95Lb = plotData$calibratedCi95Lb,
                     calibratedCi95Ub = plotData$calibratedCi95Ub,
                     analysis = plotData$description)
  calibratedRr <- data$calibratedRr
  calibratedCi95Lb <- data$calibratedCi95Lb
  calibratedCi95Ub <- data$calibratedCi95Ub
  labels <- data$analysis
  targetName <- sub(",.*", "", as.character(unique(plotData$targetName)))
  comparatorName <- sub(",.*", "", as.character(unique(plotData$comparatorName)))
  d <- data.frame(calibratedRr = calibratedRr,
                  calibratedCi95Lb = calibratedCi95Lb,
                  calibratedCi95Ub = calibratedCi95Ub,
                  name = labels)
  d$name <- factor(d$name, levels = rev(as.character(labels)))
  breaks <- c(0.1, 0.25, 0.5, 1, 2, 4, 6, 8, 10)
  breaksLabels <- c(0.1, 0.25, paste("0.5\nFavors", targetName), 1, paste("2\nFavors", comparatorName), 4, 6, 8, 10)
  p <- ggplot2::ggplot(d, ggplot2::aes(x = calibratedRr, y = name, xmin = calibratedCi95Lb, xmax = calibratedCi95Ub)) +
    ggplot2::geom_vline(xintercept = breaks, colour = "#AAAAAA", lty = 1, size = 0.5) +
    ggplot2::geom_vline(xintercept = 1, colour = "#000000", lty = 1, size = 0.8) +
    ggplot2::geom_errorbarh(height = 0, size = 2, alpha = 0.6, colour = "red") +
    ggplot2::geom_point(alpha = 0.6, ggplot2::aes(shape = 16, size = 4, colour = "red")) +
    ggplot2::scale_shape_identity() +
    ggplot2::scale_colour_manual(values = c(rgb(0.8,0,0), rgb(0,0,0))) +
    ggplot2::scale_x_continuous("Calibrated HR", trans = "log10", breaks = breaks, labels = breaksLabels) +
    ggplot2::coord_cartesian(xlim = limits) +
    ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   panel.background = ggplot2::element_blank(),
                   legend.position = "none",
                   panel.border = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_blank(),
                   axis.title.y = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank(),
                   plot.margin = grid::unit(c(0.1,0,0.1,0), "lines"))
  return(p)
}




