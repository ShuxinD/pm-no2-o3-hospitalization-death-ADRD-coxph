## setup ----
rm(list = ls())
gc()

library(data.table)
library(fst)
library(lubridate)
# library(NSAPHutils)
setDTthreads(threads = 0)

wkdir <- "/n/dominici_nsaph_l3/Lab/projects/pm-no2-o3-hospitalization-death-ADRD-coxph"
rundir <- file.path(wkdir, "code", "revise_all_hospitalization")

## load cohort enrollment info ----
enrollInfo <- read_fst(file.path(rundir, "All_EnrolledInfo.fst"), as.data.table = T)
names(enrollInfo)
summary(enrollInfo)

## load ReAd info ----
ReAdInfo <- read_fst(file.path(rundir, "All_ReAdInfo.fst"), as.data.table = T)
dim(ReAdInfo)
names(ReAdInfo)

table(ReAdInfo[,firstReAdyr])
# 2001   2002   2003   2004   2005   2006   2007   2008   2009   2010   2011   2012   2013   2014   2015   2016
# 227029 263466 275973 275394 280187 269377 262687 256008 246500 247995 247408 244919 231994 224024 226428 221832

## load ADRD cohort data ---
cohort <- read_fst(file.path(rundir, "ALLcohort_clean.fst"), as.data.table = T)
uniqueN(cohort[,qid])
# [1] 5552221

## subset for analyzing ReAd ----
#' those with ReAd, followup until ReAd
dt_ReAd_event <- cohort[qid %in% ReAdInfo[,QID],]
uniqueN(dt_ReAd_event[,qid])
uniqueN(ReAdInfo[,QID])

dt_ReAd_event <- merge(dt_ReAd_event, ReAdInfo, by.x = "qid", by.y = "QID", all.x = TRUE)
anyNA(dt_ReAd_event)
names(dt_ReAd_event)
summary(dt_ReAd_event[,firstReAdyr]- dt_ReAd_event[, firstHOSPyr])
dt_ReAd_event$ReAd <- FALSE
dt_ReAd_event[year==firstReAdyr, ReAd:=TRUE][]
dt_ReAd_event <- dt_ReAd_event[year<=firstReAdyr, ]
uniqueN(dt_ReAd_event[,qid])
# [1] 3888787

#' those without ReAd event, followup as the same as in mortality cohort
dt_ReAd_noevent <- cohort[!(qid %in% ReAdInfo[,QID]),]
uniqueN(dt_ReAd_noevent[,qid])
head(dt_ReAd_noevent)
dt_ReAd_noevent[, firstReAdyr := 0000][]
dt_ReAd_noevent[, ReAd := FALSE]

dt_ReAd <- rbind(dt_ReAd_event, dt_ReAd_noevent)
uniqueN(dt_ReAd[,qid]) # number of subjects
# [1] 5552221

dim(dt_ReAd[(dead)&(ReAd),]) # the person-year death and readmission happened at the same time
# [1] 1263845      35
uniqueN(dt_ReAd[year>firstHOSPyr,qid])
write_fst(dt_ReAd[year>firstHOSPyr,], file.path(rundir, "ALLcohort_ReAd.fst")) # subset dataset to starting from the next year of firstADRDyr
rm(dt_ReAd);gc()

## subset to mortality cohort ----
cohort <- read_fst(file.path(rundir, "ALLcohort_clean.fst"), as.data.table = T)
write_fst(cohort[year>firstHOSPyr,], file.path(rundir, "ALLcohort_dead.fst")) # subset datasets starting from the next year of firstADRDyr

uniqueN(cohort[year>firstHOSPyr,qid])