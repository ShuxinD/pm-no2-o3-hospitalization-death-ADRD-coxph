###############################################################################
# Project: Air Pollution on mortality and readmission in Medicare AD/ADRD     #
# Code: extract ADRD hopspitalization info from hospitalization files         #
# Input: hospitalization files                                                #
# Output: "ADRDhospitalization.csv"                                           #
# Author: Shuxin Dong                                                         #
# Date: 2021-02-03                                                            #
###############################################################################

############################# 0. Setup ########################################
rm(list = ls())
gc()

library(data.table)
library(fst)
library(NSAPHutils)
library(icd)

setDTthreads(threads = 0)
setwd("/nfs/home/S/shd968/shared_space/ci3_shd968/dementia")
dir_hospital <- "/nfs/home/S/shd968/shared_space/ci3_health_data/medicare/gen_admission/1999_2016/targeted_conditions/cache_data/admissions_by_year/"
dir_output <- "/nfs/home/S/shd968/shared_space/ci3_shd968/dementia/ADRDhospitalization/"

######################## 1. ICD code info #####################################
outcomes <- list()
outcomes[["ADRD"]] <- list()
outcomes[["ADRD"]][["icd9"]] <- c("3310",expand_range("290","290"), expand_range("294", "294"), "794")
outcomes[["ADRD"]][["icd9"]] <- outcomes[["ADRD"]][["icd9"]][outcomes[["ADRD"]][["icd9"]]!="2949"]
outcomes[["ADRD"]][["icd10"]] <- c(expand_range("G30","G30"), expand_range("F015", "F015"), 
                                   expand_range("F028", "F028"), expand_range("F039", "F039"),
                                   "F04", "R4181")

##################### 2. extract hospitalization info #########################
## clear out old data in case of re-run
for (outcome in names(outcomes)) {
  for (diag_type in c("primary", "secondary")) {
    file.remove(list.files(file.path(dir_output, outcome, diag_type),
                           pattern = ".fst",
                           full.names = T))
  }
}

for (year_ in 2000:2016) {
  admissions <- read_data(dir_hospital, years = year_,
                          columns = c("QID",
                                      "ADATE",
                                      "DDATE",
                                      "DIAG1",
                                      "DIAG2",
                                      "DIAG3",
                                      "DIAG4",
                                      "DIAG5",
                                      "DIAG6",
                                      "DIAG7",
                                      "DIAG8",
                                      "DIAG9",
                                      "DIAG10"))
  admissions[, ADATE := dmy(ADATE)]
  admissions[, DDATE := dmy(DDATE)]
  admissions[, year := year(ADATE)]
  admissions <- admissions[year %in% 2000:2016]

  for (outcome in names(outcomes)) {
    admissions[DDATE < "2015-10-01", (paste0(outcome, "_primary")) := DIAG1 %in% outcomes[[outcome]][["icd9"]]]
    admissions[DDATE >= "2015-10-01", (paste0(outcome, "_primary")) := DIAG1 %in% outcomes[[outcome]][["icd10"]]]
    admissions[DDATE < "2015-10-01", (paste0(outcome, "_secondary")) := DIAG1 %in% outcomes[[outcome]][["icd9"]]|
                 DIAG2 %in% outcomes[[outcome]][["icd9"]]|
                 DIAG3 %in% outcomes[[outcome]][["icd9"]]|
                 DIAG4 %in% outcomes[[outcome]][["icd9"]]|
                 DIAG5 %in% outcomes[[outcome]][["icd9"]]|
                 DIAG6 %in% outcomes[[outcome]][["icd9"]]|
                 DIAG7 %in% outcomes[[outcome]][["icd9"]]|
                 DIAG8 %in% outcomes[[outcome]][["icd9"]]|
                 DIAG9 %in% outcomes[[outcome]][["icd9"]]|
                 DIAG10 %in% outcomes[[outcome]][["icd9"]]]
    admissions[DDATE >= "2015-10-01", (paste0(outcome, "_secondary")) := DIAG1 %in% outcomes[[outcome]][["icd10"]]|
                 DIAG2 %in% outcomes[[outcome]][["icd10"]]|
                 DIAG3 %in% outcomes[[outcome]][["icd10"]]|
                 DIAG4 %in% outcomes[[outcome]][["icd10"]]|
                 DIAG5 %in% outcomes[[outcome]][["icd10"]]|
                 DIAG6 %in% outcomes[[outcome]][["icd10"]]|
                 DIAG7 %in% outcomes[[outcome]][["icd10"]]|
                 DIAG8 %in% outcomes[[outcome]][["icd10"]]|
                 DIAG9 %in% outcomes[[outcome]][["icd10"]]|
                 DIAG10 %in% outcomes[[outcome]][["icd10"]]]
  }
  for (i in 2000:2016) {
    for (outcome in names(outcomes)) {
      for (type in c("primary", "secondary")) {
        varname <- paste0(outcome, "_", type)
        if (file.exists(paste0(file.path(dir_output, outcome, type),
                               "/", varname, "_", i, ".fst"))) {
          year_admissions <- read_fst(paste0(file.path(dir_output, outcome, type),
                                             "/", varname, "_", i, ".fst"))
        } else {
          year_admissions <- NULL
        }
        year_admissions <- rbind(year_admissions, admissions[year == i & get(varname) == T])
        
        if (nrow(year_admissions) != 0) {
          write_fst(year_admissions,paste0(file.path(dir_output, outcome, type),
                                           "/", varname, "_", i, ".fst"))
        }
      }
    }
  }
}
gc()
gc()

