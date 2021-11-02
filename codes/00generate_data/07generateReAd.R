#' Project: airPollution_ADRD
# Code: generate ReAd and mortality data
# Input: "ADRDcohort_clean.fst"
# Output: "ADRDcohort_ReAd.fst" 
# Author: Shuxin Dong 
# Date: 2021-04-21                                                         

## setup ----
rm(list = ls())
gc()

library(data.table)
library(fst)
setDTthreads(threads = 0)

dir_in <- "/nfs/home/S/shd968/shared_space/ci3_shd968/dementia/data/"
dir_out <- "/nfs/home/S/shd968/shared_space/ci3_shd968/dementia/data/"

## load data ---
cohort <- read_fst(paste0(dir_in, "ADRDcohort_clean.fst"), as.data.table = T)
ReAdInfo <- fread(paste0(dir_in, "ReAdmissionInfo.csv"))
head(ReAdInfo)
names(ReAdInfo)

## subset ----
dt_ReAd_event <- cohort[qid %in% ReAdInfo[,QID],]
# uniqueN(dt_ReAd_event[,qid])
# uniqueN(ReAdInfo[,QID])
dt_ReAd_event <- merge(dt_ReAd_event, ReAdInfo, by.x = "qid", by.y = "QID", all.x = TRUE)
head(dt_ReAd_event)
dt_ReAd_event$ReAd <- FALSE
dt_ReAd_event[year==first_ReAdyr, ReAd:=TRUE][]
dt_ReAd_event <- dt_ReAd_event[year<=first_ReAdyr, ]

dt_ReAd_noevent <- cohort[!(qid %in% ReAdInfo[,QID]),]
head(dt_ReAd_noevent)
dt_ReAd_noevent[, first_ReAdyr := 0000][]
dt_ReAd_noevent[, ReAd := FALSE]

dt_ReAD <- rbind(dt_ReAd_event, dt_ReAd_noevent)
write_fst(dt_ReAD, paste0(dir_out, "ADRDcohort_ReAd.fst"))
