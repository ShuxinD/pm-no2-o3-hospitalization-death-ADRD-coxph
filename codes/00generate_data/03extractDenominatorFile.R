###############################################################################
# Project: Air Pollution on mortality and readmission in Medicare AD/ADRD     #
# Code: extract ADRD population based on enrolled info from denominator files #
# Input: "EnrolledInfo.csv"
# Input: denominator files                             
# Output: "ADRDpeople_denom.csv" the data extracted from denominator files          
# Author: Shuxin Dong                                                         #
# Date: 2021-01-19                                                            #
###############################################################################

############################# 0. Setup ########################################
rm(list = ls())
gc()

library(data.table)
library(fst)
library(dplyr)

setDTthreads(threads = 0)
setwd("/nfs/home/S/shd968/shared_space/ci3_shd968/dementia")
dir_enrolledInfo <- "/nfs/home/S/shd968/shared_space/ci3_shd968/dementia/"
dir_denominator <- "/nfs/home/S/shd968/shared_space/ci3_health_data/medicare/mortality/1999_2016/wu/cache_data/merged_by_year_v2"
dir_output <- "/nfs/home/S/shd968/shared_space/ci3_shd968/dementia/"

f <- list.files(dir_denominator, pattern = "\\.fst", full.names = TRUE)

## example to read fst file
# example <- read_fst(f[1])
# names(example)
# > names(example)
# [1] "zip"                          "year"                         "qid"                          "dodflag"                     
# [5] "bene_dod"                     "sex"                          "race"                         "age"                         
# [9] "hmo_mo"                       "hmoind"                       "statecode"                    "latitude"                    
# [13] "longitude"                    "dual"                         "death"                        "dead"                        
# [17] "entry_age"                    "entry_year"                   "entry_age_break"              "followup_year"               
# [21] "followup_year_plus_one"       "pm25_ensemble"                "pm25_no_interp"               "pm25_nn"                     
# [25] "ozone"                        "ozone_no_interp"              "zcta"                         "poverty"                     
# [29] "popdensity"                   "medianhousevalue"             "pct_blk"                      "medhouseholdincome"          
# [33] "pct_owner_occ"                "hispanic"                     "education"                    "population"                  
# [37] "zcta_no_interp"               "poverty_no_interp"            "popdensity_no_interp"         "medianhousevalue_no_interp"  
# [41] "pct_blk_no_interp"            "medhouseholdincome_no_interp" "pct_owner_occ_no_interp"      "hispanic_no_interp"          
# [45] "education_no_interp"          "population_no_interp"         "smoke_rate"                   "mean_bmi"                    
# [49] "smoke_rate_no_interp"         "mean_bmi_no_interp"           "amb_visit_pct"                "a1c_exm_pct"                 
# [53] "amb_visit_pct_no_interp"      "a1c_exm_pct_no_interp"        "tmmx"                         "rmax"                        
# [57] "pr"                           "cluster_cat"                  "fips_no_interp"               "fips"                        
# [61] "summer_tmmx"                  "summer_rmax"                  "winter_tmmx"                  "winter_rmax" 
myvars <- c("qid", "year", #"zip", "sex", "race", "age", "dual", "entry_age_break", "statecode",
            #"dead",
            #"mean_bmi", "smoke_rate", "hispanic", "pct_blk", "medhouseholdincome", "medianhousevalue",
            #"poverty", "education", "popdensity", "pct_owner_occ",
            "summer_tmmx", "winter_tmmx", "summer_rmax", "winter_rmax")
dt <- rbindlist(lapply(f,
                       read_fst,
                       columns = myvars,
                       as.data.table = TRUE))

enrolledInfo <- fread(paste0(dir_enrolledInfo, "EnrolledInfo.csv"))
dim(enrolledInfo)[1]
# [1] 8046408

################################# 1. Subset ###################################
names(enrolledInfo)
ADRDpeople <- dt[qid %in% enrolledInfo[,QID], ]
uniqueN(ADRDpeople[,qid])
# [1] 8037547
dim(ADRDpeople)
# [1] 87476969       20

ADRDpeople <- merge(ADRDpeople, enrolledInfo, by.x = "qid", by.y = "QID", all = FALSE)
ADRDpeople <- ADRDpeople[year >= firstADRDyr, ]
uniqueN(ADRDpeople[,qid])
# [1] 8037303
dim(ADRDpeople)
# [1] 27049496       21
setorder(ADRDpeople, qid, year)
fwrite(ADRDpeople, paste0(dir_output, "ADRDpeople_denom.csv"))
