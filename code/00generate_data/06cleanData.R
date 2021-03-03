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
# [1] "zip"                "year"               "qid"                "summer_tmmx"        "winter_tmmx"        "summer_rmax"       
# [7] "winter_rmax"        "dead"               "sex"                "race"               "age"                "dual"              
# [13] "statecode"          "entry_age_break"    "mean_bmi"           "smoke_rate"         "hispanic"           "pct_blk"           
# [19] "medhouseholdincome" "medianhousevalue"   "poverty"            "education"          "popdensity"         "pct_owner_occ"     
# [25] "firstADRDyr"        "pm25"               "no2"                "ozone"  
dt <- na.omit(ADRDpeople)[year!=firstADRDyr, ] # remove NAs and firstADRDyr
dim(dt)
# [1] 17081631       28
dim(ADRDpeople)[1] - dim(dt)[1] # number of person-years to be removed due to NAs
# > dim(ADRDpeople)[1] - dim(dt)[1]
# [1] 9967865
uniqueN(dt, by = "qid")
# [1] 5154447

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
dim(temp[start_yr > (firstADRDyr+1)]) # number of subjects to remove
# [1] 64530     5
dt[!(qid %in% temp[start_yr != (firstADRDyr+1)][, qid]), ]
dt <- dt[!(qid %in% temp[start_yr != (firstADRDyr+1)][, qid]), ]
gc()

dim(ADRDpeople)[1] - dim(dt)[1] # number of person-years to removed
# [1] 10183511

## remove those not having each year’s info during follow-up
dim(temp[(end_yr-start_yr+1) != count,]) # number of subjects to remove
# [1] 13669     5
dt <- dt[qid %in% temp[(end_yr-start_yr+1) == count, qid],]
gc()

dim(ADRDpeople)[1] - dim(dt)[1]
# [1] 10258411

head(dt)

fwrite(dt, paste0(dir_out, "ADRD_mortality.csv"))


