###############################################################################
# Project: Medicare Mortality and Air Pollution in AD/ADRD                    #
# Code: covariates correlations, create table one                             #
# Input: "ADRDmort_cplt.csv"                                                  #
# Output: "corr.csv" as correlations between covariates                       #
# Output: "table1.doc"                                                        #
# Author: Shuxin Dong                                                         #
# Date: Dec 9, 2020                                                           #
###############################################################################

############################# 0. Setup ########################################
rm(list = ls())
gc()

library(data.table)
library(dplyr)

setwd("/nfs/home/S/shd968/shared_space/ci3_shd968/dementia")

dir_data <- "/nfs/home/S/shd968/shared_space/ci3_shd968/dementia/"
dir_output <- "/nfs/home/S/shd968/shared_space/ci3_shd968/dementia/descrip/"

setDTthreads(threads = 0)

dt <- fread(paste0(dir_data, "ADRDmort_cplt.csv"))
names(dt)

########################## 1. calculate corr ##################################
corr_data <- dt[,.(PctEye, PctLDL, Pctmam, LungCancerRate, poverty, 
                   popdensity, medianhousevalue, pct_blk, 
                   medhouseholdincome, pct_owner_occ, hispanic,
                   education, smoke_rate, mean_bmi, amb_visit_pct,
                   a1c_exm_pct, nearest_hospital_km, pm25, no2, ozone)]
corr <- cor(corr_data)
write.csv(corr, file = paste0(dir_output, "corr.csv"))

############################# 2. create table one #############################
listVars <- c("pm25", "no2", "ozone", "po", "poverty", "popdensity",
              "medhouseholdincome", "education", "pct_blk", "PctEye",
              "PctLDL", "Pctmam", "LungCancerRate", "medianhousevalue", 
              "pct_owner_occ", "hispanic", "smoke_rate", "mean_bmi",
              "amb_visit_pct","a1c_exm_pct", "nearest_hospital_km")
table1.personyr <- tableone::CreateTableOne(vars = listVars, 
                                            data = dt)
table1.personyr <- print(table1.personyr)

## for individual
dt_ind <- dt[,.SD[1], by=QID]
head(dt_ind)
dt_ind <- dt_ind[,.(QID, AGE, age_gp, year_admit, Sex_gp, Dual_gp, race, death)]

listVars <- c("death", "Sex_gp", "race", "Dual_gp", "AGE", "age_gp" ,"year_admit")
catVars <- c("death", "Sex_gp", "age_gp", "race", "Dual_gp", "year_admit")
table1.individual <- tableone::CreateTableOne(vars = listVars, 
                                              data = dt_ind, 
                                              factorVars = catVars)
table1.individual <- print(table1.individual)

library(rtf)
rtffile <- RTF(paste0(dir_output, "table1.doc"))
addParagraph(rtffile, "Table1_person year")
addTable(rtffile, cbind(rownames(table1.personyr), table1.personyr))
addParagraph(rtffile, "\n \n Table1_individual")
addTable(rtffile, cbind(rownames(table1.individual), table1.individual))
done(rtffile)
