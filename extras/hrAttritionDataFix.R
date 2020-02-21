
studyFolder <- "S:/StudyResults/epi_581_5"

toChar <- function(df) {
  facs <- sapply(df, is.factor)
  df[facs] <- lapply(df[facs], as.character)  
  return(df)
}

tab <- function(x){
  freq <- table(x)
  prop <- prop.table(freq)
  table <- cbind(freq, prop)
  n <- c(sum(table[, 1]), sum(table[, 2]))
  table <- rbind(table, n)
  table
}

fixAttrition <- function(databaseId) {
  hrAttrition <- read.csv(paste0("S:/StudyResults/epi_581_5/report/hr_attrition_", databaseId, ".csv"))
  hrAttrition$targetName <- sub("\\vs.*", "",  hrAttrition$Comparison)
  hrAttrition$targetName <- trimws(hrAttrition$targetName, "left")
  hrAttrition$targetName <- trimws(hrAttrition$targetName, "right")
  hrAttrition$comparatorName <- sub(".*\\vs.", "",  hrAttrition$Comparison)
  hrAttrition$comparatorName <- trimws(hrAttrition$comparatorName, "left")
  hrAttrition$comparatorName <- trimws(hrAttrition$comparatorName, "right")
  hrAttrition <- toChar(hrAttrition)
  fixAttrition <- read.csv(paste0("S:/StudyResults/epi_581_4/", databaseId, "/irData/attritionTable.csv"))
  fixAttrition <- fixAttrition[fixAttrition$description.1 == "Original cohorts", ]
  fixAttrition$type <- NULL
  fixAttrition <- unique(fixAttrition)
  fixAttrition$Study.period[fixAttrition$description == "[2001.01.01-2017.12.31] Unadjusted"] <- "2001.01.01 - 2017.12.31"
  fixAttrition$Study.period[fixAttrition$description == "[2001.01.01-2015.09.30] Unadjusted"] <- "2001.01.01 - 2015.09.30"
  fixAttrition$Analysis <- gsub(".*] ", "",  fixAttrition$description)
  fixAttrition <- toChar(fixAttrition)
  names(fixAttrition)[3] <- "Attrition.description"
  names(fixAttrition)[2] <- "targetName"
  hrAttrition <- merge(hrAttrition, 
                       fixAttrition[, c("Study.period", "Attrition.description", "targetName", "targetPersons")],
                       by = c("Study.period", "Attrition.description", "targetName"),
                       all.x = TRUE)
  hrAttrition$Target.persons[!is.na(hrAttrition$targetPersons)] <- hrAttrition$targetPersons[!is.na(hrAttrition$targetPersons)]
  hrAttrition$targetPersons <- NULL
  names(fixAttrition)[2] <- "comparatorName"
  hrAttrition <- merge(hrAttrition, 
                       fixAttrition[, c("Study.period", "Attrition.description", "comparatorName", "targetPersons")],
                       by = c("Study.period", "Attrition.description", "comparatorName"),
                       all.x = TRUE)
  hrAttrition$Comparator.persons[!is.na(hrAttrition$targetPersons)] <- hrAttrition$targetPersons[!is.na(hrAttrition$targetPersons)]
  hrAttrition$targetPersons <- NULL
  hrAttrition <- hrAttrition[, c("Comparison", "Study.period", "Analysis", "Attrition.description", "Target.persons", "Comparator.persons")]
  if (databaseId == "CCAE") {
  hrAttrition$comparisonOrder <- match(hrAttrition$Comparison, c("Typicals, 18-64y, no dementia vs. Atypicals, 18-64y, no dementia",
                                                                 "Haloperidol, 18-64y, no dementia vs. Atypicals, 18-64y, no dementia",
                                                                 "Typicals, 18-64y vs. Atypicals, 18-64y",
                                                                 "Haloperidol, 18-64y vs. Atypicals, 18-64y"))
  }
  if (databaseId == "MDCR") {
    hrAttrition$comparisonOrder <- match(hrAttrition$Comparison, c("Typicals, >=65y vs. Atypicals, >=65y",
                                                                   "Haloperidol, >=65y vs. Atypicals, >=65y"))
  }
  hrAttrition$studyPeriodOrder <- match(hrAttrition$Study.period, c("2001.01.01 - 2017.12.31",
                                                                    "2001.01.01 - 2015.09.30"))
  hrAttrition$analysisOrder <- match(hrAttrition$Analysis, c("Unadjusted",
                                                             "1:1 PS match on select covariates",
                                                             "1:10 PS match on full covariates"))
  hrAttrition$descriptionOrder <- match(hrAttrition$Attrition.description, c("Original cohorts",
                                                                             "First cohort only",
                                                                             "Have at least 1 days at risk",
                                                                             "Matched on propensity score"))
  hrAttrition <- hrAttrition[order(hrAttrition$comparisonOrder,
                                             hrAttrition$studyPeriodOrder,
                                             hrAttrition$analysisOrder,
                                             hrAttrition$descriptionOrder), ][, 1:6]
  write.csv(hrAttrition, paste0("S:/StudyResults/epi_581_5/report/hr_fix_attrition_", databaseId, ".csv"), row.names = FALSE)
  return(hrAttrition)
}

hrAttrition_CCAE <- fixAttrition("CCAE")
hrAttrition_MDCR <- fixAttrition("MDCR")
