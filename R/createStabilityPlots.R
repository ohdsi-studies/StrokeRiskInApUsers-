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
createStabilityPlots <- function(connectionDetails,
                                 outputFolder,
                                 cohortDatabaseSchema,
                                 cohortTable,
                                 cohortDefinitionId,
                                 cdmDatabaseSchema,
                                 databaseId) {
  ipDataFolder <- file.path(outputFolder, "ipData")
  if (!file.exists(ipDataFolder)) {
    dir.create(ipDataFolder)
  }
  ipDataFile <- file.path(ipDataFolder, "ipDataAgeGender.rds")
  if (databaseId == "CCAE") {
    ageGroups <- c("10-19", "20-29", "30-39", "40-49", "50-59", "60-69")
  }
  if (databaseId == "MDCR") {
    ageGroups <- c("60-69", "70-79", "80-89", "90-99", "100-109", "110-119")
  }
  if (!file.exists(ipDataFile)) {
    ipData <- phenotypeStability::getIntidenceProportionData(connectionDetails = connectionDetails,
                                                             cohortDatabaseSchema = cohortDatabaseSchema,
                                                             cohortTable = cohortTable,
                                                             cdmDatabaseSchema = cdmDatabaseSchema,
                                                             cohortDefinitionId = cohortDefinitionId,
                                                             stratified = "age_gender",
                                                             ageGroups = ageGroups,
                                                             fileName = ipDataFile)
  } else {
    ipData <- readRDS(ipDataFile)
  }
  ipPlotFile <- file.path(ipDataFolder, "ipPlotAgeGender.png")
  phenotypeStability::generateStabilityPlot(ipData = ipData, fileName = ipPlotFile)
}

