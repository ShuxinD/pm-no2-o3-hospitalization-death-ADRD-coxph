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
dt <- na.omit(ADRDpeople)[year!=firstADRDyr, ] # remove NAs and firstADRDyr

########################### 2. Clean data #####################################
temp <- dt[, .(start_yr = min(year),
               end_yr = max(year),
               count = uniqueN(year)), by = qid]
temp <- merge(temp, dt[,.(qid,firstADRDyr)], by = "qid", all.x = TRUE)
gc()

## remove those not followed-up from the year following firstADRDyr
temp[start_yr != (firstADRDyr+1)]
dt[!(qid %in% temp[start_yr != (firstADRDyr+1)][, qid]), ]
dt <- dt[!(qid %in% temp[start_yr != (firstADRDyr+1)][, qid]), ]
gc()

dim(temp)[1] - dim(dt)[1]
# [1] 218613

## remove those not having each year’s info during follow-up
dt[!(qid %in% temp[(end_yr-start_yr+1) != count,][,qid]),]
dt <- dt[!(qid %in% temp[(end_yr-start_yr+1) != count,][,qid]),]
gc()

dim(temp)[1] - dim(dt)[1]
# [1] 297473

fwrite(dt, paste0(dir_out, "ADRD_mortality.csv"))


