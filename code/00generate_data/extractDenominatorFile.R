###############################################################################
# Project: Air Pollution on mortality and readmission in Medicare AD/ADRD     #
# Code: extract ADRD population based on enrolled info from denominator files #
# Input: "enrolledInfo.csv" and denominator files #
# Output: #
# Author: Shuxin Dong                                                         #
# Date: 2021-01-19                                                            #
###############################################################################

############################# 0. Setup ########################################
rm(list = ls())
gc()

library(data.table)
library(fst)

setDTthreads(threads = 0)
setwd("/nfs/home/S/shd968/shared_space/ci3_shd968/dementia")
dir_enrolledInfo <- "/nfs/home/S/shd968/shared_space/ci3_shd968/dementia/"
dir_denominator <- "/nfs/home/S/shd968/shared_space/ci3_health_data/medicare/mortality/1999_2016/wu/cache_data/merged_by_year_v2"

f <- list.files(dir_denominator, pattern = "\\.fst", full.names = TRUE)
myvars <- c("qid", "year", "zip", "dead", "sex", "race", "age", "dual", 
            "entry_age_break",
            "mean_bmi", "smoke_rate",
            "hispanic", "pct_blk", "medhouseholdincome", "medianhousevalue",
            "poverty","education", "popdensity", "pct_owner_occ")
dt <- rbindlist(lapply(f,
                       read_fst,
                       columns = myvars,
                       as.data.table = TRUE))

enrolledInfo <- fread(paste0(dir_enrolledInfo,"enrolledInfo.csv"))

################################# 1. Subset ###################################
names(enrolledInfo)
ADRDpeople <- dt[qid %in% enrolledInfo[,QID], ]
dim(ADRDpeople)

ADRDpeople <- merge(ADRDpeople, enrolledInfo, by.x = "qid", by.y = "QID")
dim(ADRDpeople)

ADRDpeople <- ADRDpeople[year >= firstADRDyr, ]
setorder(ADRDpeople, qid, year)

##################### 2. check completeness of follow-up ######################
temp <- ADRDpeople[, .(start_yr = min(year),
                       end_yr = max(year),
                       count = uniqueN(year)), by = qid]
## check all people were followed-up from the firstADRDyr
temp1 <- merge(temp, enrolledInfo, by.x = "qid", by.y = "QID")
temp1[start_yr != firstADRDyr]

## check all alive people were followed-up till the end of study period (2016)
temp[qid %in% ADRDpeople[!dead,qid], ][, end_yr] %>% table()

## check whether there is duplication for qid and year
any(duplicated(ADRDpeople[,.(qid, year)]))
## check all people have each year's info during follow-up
temp[(end_yr-start_yr+1) != count,]
