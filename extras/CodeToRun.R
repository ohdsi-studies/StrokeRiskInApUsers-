library(epi581v4)
options(fftempdir = "S:/FFTemp")
maxCores <- parallel::detectCores()
studyFolder <- "S:/StudyResults/epi_581_5"

# server connection:
source("S:/MiscCode/SetEnvironmentVariables.R")
connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = "pdw",
                                                                server = Sys.getenv("server"),
                                                                user = NULL,
                                                                password = NULL,
                                                                port = as.numeric(Sys.getenv("port")))

mailSettings <- list(from = Sys.getenv("emailAddress"),
                     to = c(Sys.getenv("emailAddress")),
                     smtp = list(host.name = Sys.getenv("emailHost"), port = 25,
                                 user.name = Sys.getenv("emailAddress"),
                                 passwd = Sys.getenv("emailPassword"), ssl = FALSE),
                     authenticate = FALSE,
                     send = TRUE)

# MDCR settings ----------------------------------------------------------------
databaseId <- "MDCR"
databaseName <- "MDCR"
databaseDescription <- "MDCR"
cdmDatabaseSchema = "cdm_ibm_mdcr_v871.dbo"
outputFolder <- file.path(studyFolder, databaseId)
cohortDatabaseSchema <- "scratch.dbo"
cohortTable <- "epi_581_mdcr"

# CCAE settings ----------------------------------------------------------------
databaseId <- "CCAE"
databaseName <- "CCAE"
databaseDescription <- "CCAE"
cdmDatabaseSchema = "cdm_ibm_ccae_v870.dbo"
outputFolder <- file.path(studyFolder, databaseId)
cohortDatabaseSchema <- "scratch.dbo"
cohortTable <- "epi_581_ccae"

# Run --------------------------------------------------------------------------
OhdsiRTools::runAndNotify(expression = {
  execute(connectionDetails = connectionDetails,
          cdmDatabaseSchema = cdmDatabaseSchema,
          cohortDatabaseSchema = cohortDatabaseSchema,
          cohortTable = cohortTable,
          oracleTempSchema = NULL,
          outputFolder = outputFolder,
          databaseId = databaseId,
          databaseName = databaseName,
          databaseDescription = databaseDescription,
          createCohorts = FALSE,
          synthesizePositiveControls = FALSE,
          runAnalyses = FALSE,
          runDiagnostics = FALSE,
          packageResults = FALSE,
          maxCores = maxCores)
}, mailSettings = mailSettings, label = paste0("Epi 581 ", databaseId), stopOnWarning = FALSE)

# prepare for shiny per database -----------------------------------------------
resultsZipFile <- file.path(outputFolder, "export", paste0("Results", databaseId, ".zip"))
dataFolder <- file.path(outputFolder, "shinyData")
prepareForEvidenceExplorer(resultsZipFile = resultsZipFile, dataFolder = dataFolder)

# create stability plots per database ------------------------------------------
createStabilityPlots(connectionDetails = connectionDetails,
                     outputFolder = outputFolder,
                     cohortDatabaseSchema = cohortDatabaseSchema,
                     cohortTable = cohortTable,
                     cohortDefinitionId = 8997,
                     cdmDatabaseSchema = cdmDatabaseSchema,
                     databaseId = databaseId)

# compile results for Shiny accross databases ----------------------------------
compileShinyData(studyFolder = studyFolder, databases = c("CCAE", "MDCR"))
# source("extras/attritionDataFix.R") # execute extras/attritionDataFix.R once to fix attrition table counts

shinyDataAllFolder <- file.path(studyFolder, "shinyDataAll")
launchEvidenceExplorer(dataFolder = shinyDataAllFolder, blind = FALSE, launch.browser = FALSE)

# create tables and plots for report across databases --------------------------
createTablesAndPlots(studyFolder = studyFolder,
                     createTable1 = FALSE,
                     createHrTable = FALSE,
                     createHrAttritionTable = FALSE,
                     createMdrrTable = FALSE,
                     createDiagnosticsPlot = FALSE,
                     createForestPlot = TRUE)
# source("extras/hrAttritionDataFix.R") # execute extras/hrAttritionDataFix.R once to fix attrition table counts

# create HR tables for 31 Oct 2015 study end date, keep 31 Oct 2015 result only-
createTablesAndPlots(studyFolder = "S:/StudyResults/epi_581_4",
                     createTable1 = FALSE,
                     createHrTable = TRUE,
                     createHrAttritionTable = FALSE,
                     createMdrrTable = FALSE,
                     createDiagnosticsPlot = FALSE)

# compute crude, stratified IRs across databases -------------------------------
computeCrudeIrs(connectionDetails = connectionDetails, 
                studyFolder = studyFolder,
                databases = c("CCAE", "MDCR"))

