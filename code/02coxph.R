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
dir_output <- "/nfs/home/S/shd968/shared_space/ci3_shd968/dementia/modelResult/"

setDTthreads(threads = 0)

dt <- fread(paste0(dir_data, "ADRDmort_cplt.csv"))
# > names(dt)
# [1] "QID"                 "year"                "zip"                 "AGE"                 "STATECODE"          
# [6] "Sex_gp"              "age_gp"              "Dual_gp"             "race"                "bene_dod"           
# [11] "ADATE"               "DDATE"               "DIAG1"               "DIAG2"               "diabetes"           
# [16] "year_admit"          "pm25"                "no2"                 "PctEye"              "PctLDL"             
# [21] "Pctmam"              "LungCancerRate"      "poverty"             "popdensity"          "medianhousevalue"   
# [26] "pct_blk"             "medhouseholdincome"  "pct_owner_occ"       "hispanic"            "education"          
# [31] "smoke_rate"          "mean_bmi"            "amb_visit_pct"       "a1c_exm_pct"         "nearest_hospital_km"
# [36] "ozone"               "firstADRDyr"         "mort_yr"             "death" 
dt$dead_peryear <- 0
dt$dead_peryear[dt$death==1 & (dt$year_admit==dt$mort_yr_admit)] <- 1
dt[, followupyr := year_admit - firstADRDyr]
dt[, followupyr_plusone := followupyr +1][]
dt

############################# 1. coxph model ##################################
library(survival)
cox_all <- coxph(Surv(time = followupyr, time2 = followupyr_plusone, event = dead_peryear) ~ 
                   pm25 + no2 + ozone + 
                   mean_bmi + smoke_rate + hispanic + pct_blk + 
                   medhouseholdincome + medianhousevalue + poverty + 
                   education + popdensity + pct_owner_occ +
                   as.factor(year_admit) +  
                   strata(as.factor(age_gp)) + strata(as.factor(Sex_gp)) + 
                   strata(as.factor(race)) + strata(as.factor(Dual_gp)),
                 data = dt,
                 tie = c("efron"), na.action = na.omit)
s <- summary(cox_all)
write.csv(s$coefficients, paste0(dir_output, "cox_all.csv"))
