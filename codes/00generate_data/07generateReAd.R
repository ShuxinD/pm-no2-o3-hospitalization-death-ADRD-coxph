###############################################################################
# Project: Air Pollution on mortality and readmission in Medicare AD/ADRD     #
# Code: clean dataset for mortality analyses
# Input: "ADRD_for_mortality.csv"
# Output: "ADRD_for_ReAd.csv" 
# Author: Shuxin Dong                                                         #
# Date: 2021-04-21                                                         
###############################################################################

############################# 0. Setup ########################################
rm(list = ls())
gc()

library(data.table)
setDTthreads(threads = 23)

dir_in <- "/nfs/home/S/shd968/shared_space/ci3_shd968/dementia/data/"
dir_out <- "/nfs/home/S/shd968/shared_space/ci3_shd968/dementia/data/"

dt_mortliaty <- fread(paste0(dir_in, "ADRD_for_mortality.csv"), colClasses = c("zip"="character"))
ReAdInfo <- fread(paste0(dir_in, "ReAdmissionInfo.csv"))
head(ReAdInfo)
names(ReAdInfo)

############################# 1. Subset ########################################
dt_ReAd_event <- dt_mortliaty[qid %in% ReAdInfo[,QID],]
# uniqueN(dt_ReAd_event[,qid])
# uniqueN(ReAdInfo[,QID])
dt_ReAd_event <- merge(dt_ReAd_event, ReAdInfo, by.x = "qid", by.y = "QID", all.x = TRUE)
head(dt_ReAd_event)
dt_ReAd_event$ReAd <- FALSE
dt_ReAd_event[year==first_ReAdyr, ReAd:=TRUE][]
dt_ReAd_event <- dt_ReAd_event[year<=first_ReAdyr, ]

dt_ReAd_noevent <- dt_mortliaty[!(qid %in% ReAdInfo[,QID]),]
head(dt_ReAd_noevent)
dt_ReAd_noevent[, first_ReAdyr := 0000][]
dt_ReAd_noevent[, ReAd := FALSE]

dt_ReAD <- rbind(dt_ReAd_event, dt_ReAd_noevent)
fwrite(dt_ReAD, paste0(dir_out, "ADRD_for_ReAd.csv"))
