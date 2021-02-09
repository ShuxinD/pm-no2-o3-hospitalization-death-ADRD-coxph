###############################################################################
# Project: Air Pollution on mortality and readmission in Medicare AD/ADRD     #
# Code: clean dataset for mortality analyses
# Input: "ADRDpeople.csv"
# Output: "ADRD_mortality.csv" 
# Author: Shuxin Dong                                                         #
# Date: 2021-02-08                                                            #
###############################################################################

############################# 0. Setup ########################################
rm(list = ls())
gc()

library(data.table)
setDTthreads(threads = 0)

dir_in <- "/nfs/home/S/shd968/shared_space/ci3_shd968/dementia/"
dir_out <- "/nfs/home/S/shd968/shared_space/ci3_shd968/dementia/"

########################### 1. Load data ######################################
ADRDpeople <- fread(paste0(dir_in, "ADRDpeople.csv"))
names(ADRDpeople)
# [1] "zip"                "year"               "qid"                "dead"               "sex"               
# [6] "race"               "age"                "dual"               "statecode"          "entry_age_break"   
# [11] "mean_bmi"           "smoke_rate"         "hispanic"           "pct_blk"            "medhouseholdincome"
# [16] "medianhousevalue"   "poverty"            "education"          "popdensity"         "pct_owner_occ"     
# [21] "firstADRDyr"        "pm25"               "no2"                "ozone"    
dt <- na.omit(ADRDpeople)[year!=firstADRDyr, ] # remove NAs and firstADRDyr
uniqueN(dt, by = "qid")
# [1] 5162518

########################### 2. Clean data #####################################
temp <- dt[, .(start_yr = min(year),
               end_yr = max(year),
               count = uniqueN(year)), by = .(qid)]
dim(temp)
head(temp)
temp <- merge(temp, unique(dt[,.(qid,firstADRDyr)]), by = "qid", all.x = TRUE)
dim(temp)
gc()

## remove those not followed-up from the year following firstADRDyr
dim(temp[start_yr != (firstADRDyr+1)]) # number of subjects to remove
# [1] 65290     5
dt[!(qid %in% temp[start_yr != (firstADRDyr+1)][, qid]), ]
dt <- dt[qid %in% temp[start_yr == (firstADRDyr+1)][, qid], ]
gc()

dim(ADRDpeople)[1] - dim(dt)[1] # number of person-years to removed
# [1] 218613

## remove those not having each year’s info during follow-up
dim(temp[(end_yr-start_yr+1) != count,]) # number of subjects to remove
# [1] 14804     5
dt <- dt[qid %in% temp[(end_yr-start_yr+1) == count, qid],]
gc()

dim(ADRDpeople)[1] - dim(dt)[1]
# [1] 10235313

head(dt)

fwrite(dt, paste0(dir_out, "ADRD_mortality.csv"))


