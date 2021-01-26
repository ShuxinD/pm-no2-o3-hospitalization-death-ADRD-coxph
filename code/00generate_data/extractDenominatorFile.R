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
library(dplyr)

setDTthreads(threads = 0)
setwd("/nfs/home/S/shd968/shared_space/ci3_shd968/dementia")
dir_enrolledInfo <- "/nfs/home/S/shd968/shared_space/ci3_shd968/dementia/"
dir_denominator <- "/nfs/home/S/shd968/shared_space/ci3_health_data/medicare/mortality/1999_2016/wu/cache_data/merged_by_year_v2"

f <- list.files(dir_denominator, pattern = "\\.fst", full.names = TRUE)

## example to read fst file
example <- read_fst(f[1])
names(example)
myvars <- c("qid", "year", "zip", "dead", "sex", "race", "age", "dual", "statecode", 
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
# [1] 48130535       20
ADRDpeople <- merge(ADRDpeople, enrolledInfo, by.x = "qid", by.y = "QID")

ADRDpeople <- ADRDpeople[year >= firstADRDyr, ]
dim(ADRDpeople)
# [1] 15180886       21
setorder(ADRDpeople, qid, year)

##################### 2. check completeness of follow-up ######################
temp <- ADRDpeople[, .(start_yr = min(year),
                       end_yr = max(year),
                       count = uniqueN(year)), by = qid]
## check all people were followed-up from the firstADRDyr
temp1 <- merge(temp, enrolledInfo, by.x = "qid", by.y = "QID")
temp1[start_yr != firstADRDyr]
# qid start_yr end_yr count firstADRDyr
# 1:       A00118072     2002   2002     1        2001
# 2:       A00135173     2002   2002     1        2001
# 3:       A00155904     2001   2002     2        2000
# 4:       A00284464     2001   2002     2        2000
# 5:       A00424257     2002   2002     1        2001
# ---                                                  
#   969: lllllllooXlX4lo     2001   2006     6        2000
# 970: lllllllooo84l8S     2014   2014     1        2013
# 971: llllllloooSXUoX     2012   2016     5        2011
# 972: llllllloooU0O08     2014   2016     3        2012
# 973: llllllloooXUol0     2002   2010     9        2001
ADRDpeople[qid %in% temp1[start_yr != firstADRDyr][, qid], ]
# 3673 rows

## check all alive people were followed-up till the end of study period (2016)
temp[qid %in% ADRDpeople[!(dead),qid], ][, end_yr] %>% table()

## check whether there is duplication for qid and year
any(duplicated(ADRDpeople[,.(qid, year)]))
## check all people have each year's info during follow-up
temp1[(end_yr-start_yr+1) != count,]
temp1[(end_yr-start_yr+1) != count,][,qid]
ADRDpeople[qid %in% temp1[(end_yr-start_yr+1) != count,][,qid],]
