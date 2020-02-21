
studyFolder <- "S:/StudyResults/epi_581_5"
dataFolder <- file.path(studyFolder, "shinyDataAll")

relabel <- function(var, oldLabel, newLabel) {
  levels(var)[levels(var) == oldLabel] <- newLabel
  return(var)
}

attrition <- readRDS(file.path(dataFolder, "attrition.rds"))

exposure_of_interest <- readRDS(file.path(dataFolder, "exposure_of_interest.rds"))[, 1:2]
exposure_of_interest$exposure_name <- relabel(exposure_of_interest$exposure_name, "[Epi 581] T1. New users of typical antipsychotics age 18-64 without a recent dementia diagnosis", "Typicals, 18-64y, no dementia")
exposure_of_interest$exposure_name <- relabel(exposure_of_interest$exposure_name, "[Epi 581] T2. New users of haloperidol age 18-64 without a recent dementia diagnosis", "Haloperidol, 18-64y, no dementia")
exposure_of_interest$exposure_name <- relabel(exposure_of_interest$exposure_name, "[Epi 581] T3. New users of typical antipsychotics age 65 years and older", "Typicals, >=65y")
exposure_of_interest$exposure_name <- relabel(exposure_of_interest$exposure_name, "[Epi 581] T4. New users of haloperidol age 65 years and older", "Haloperidol, >=65y")
exposure_of_interest$exposure_name <- relabel(exposure_of_interest$exposure_name, "[Epi 581] T5. New users of typical antipsychotics age 18-64", "Typicals, 18-64y")
exposure_of_interest$exposure_name <- relabel(exposure_of_interest$exposure_name, "[Epi 581] T6. New users of haloperidol aged 18 to 64 years", "Haloperidol, 18-64y")
exposure_of_interest$exposure_name <- relabel(exposure_of_interest$exposure_name, "[Epi 581] C1. New users of atypical antipsychotics age 18-64 without a recent dementia diagnosis", "Atypicals, 18-64y, no dementia")
exposure_of_interest$exposure_name <- relabel(exposure_of_interest$exposure_name, "[Epi 581] C2. New users of atypical antipsychotics aged 65 years and older", "Atypicals, >=65y")
exposure_of_interest$exposure_name <- relabel(exposure_of_interest$exposure_name, "[Epi 581] C3. New users of atypical antipsychotics age 18-64", "Atypicals, 18-64y")

cohort_method_analysis <- readRDS(file.path(dataFolder, "cohort_method_analysis.rds"))[, 1:2]
cohort_method_analysis <- cohort_method_analysis[cohort_method_analysis$analysis_id %in% 1:2, ]
cohort_method_analysis$description <- relabel(cohort_method_analysis$description, "1. Unadjusted", "[2001.01.01-2017.12.31] Unadjusted")
cohort_method_analysis$description <- relabel(cohort_method_analysis$description, "2. Unadjusted 2015", "[2001.01.01-2015.09.30] Unadjusted")

attrition_fix_ccae <- read.csv("S:/StudyResults/epi_581_4/CCAE/irData/attritionTable.csv")
attrition_fix_ccae$database_id = "CCAE"

attrition_fix_mdcr <- read.csv("S:/StudyResults/epi_581_4/MDCR/irData/attritionTable.csv")
attrition_fix_mdcr$database_id = "MDCR"

attrition_fix <- rbind(attrition_fix_ccae, attrition_fix_mdcr)
attrition_fix <- attrition_fix[attrition_fix$description.1 == "Original cohorts", ]
attrition_fix$type <- NULL
attrition_fix <- unique(attrition_fix)

attrition_fix <- merge(exposure_of_interest, attrition_fix, by.x = "exposure_name", by.y = "exposure")
attrition_fix <- merge(cohort_method_analysis, attrition_fix, by.x = "description", by.y = "description")

attrition_fix_3_4 <- attrition_fix
attrition_fix_3_4$analysis_id <- attrition_fix_3_4$analysis_id + 2
attrition_fix_5_6 <- attrition_fix_3_4
attrition_fix_5_6$analysis_id <- attrition_fix_3_4$analysis_id + 2
attrition_fix <- rbind(attrition_fix, attrition_fix_3_4, attrition_fix_5_6)
attrition_fix$description <- NULL

common_levels <- unique(levels(attrition$description), levels(attrition_fix$description.1))
attrition_fix$description.1 <- factor(attrition_fix$description.1, levels = common_levels)
attrition$description <- factor(attrition$description, levels = common_levels)

attrition <- merge(attrition, 
                   attrition_fix[, c("database_id", "analysis_id", "exposure_id", "description.1", "targetPersons")],
                   by.x = c("database_id", "analysis_id", "exposure_id", "description"),
                   by.y = c("database_id", "analysis_id", "exposure_id", "description.1"),
                   all.x = TRUE)

attrition$subjects[!is.na(attrition$targetPersons)] <- attrition$targetPersons[!is.na(attrition$targetPersons)]
attrition$targetPersons <- NULL

saveRDS(attrition, file.path(dataFolder, "attrition.rds"))

