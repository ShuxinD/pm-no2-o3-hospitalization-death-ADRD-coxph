#' Project: airPollution_ADRD
# Code: generate ReAd and mortality data
# Input: "ADRDcohort_clean.fst"
# Output: "ADRDcohort_ReAd.fst" "ADRDcohort_dead.fst"
# Author: Shuxin Dong 
# Date: 2021-04-21                                                         

## setup ----
rm(list = ls())
gc()

library(data.table)
library(fst)
library(lubridate)
library(NSAPHutils)
setDTthreads(threads = 0)

dir_in <- "/nfs/home/S/shd968/shared_space/ci3_shd968/medicareADRD/data/"
dir_out <- "/nfs/home/S/shd968/shared_space/ci3_shd968/medicareADRD/data/"

## load cohort enrollment info ----
enrollInfo <- fread(paste0(dir_in, "EnrolledInfo.csv"))
head(enrollInfo)

## load inpatient records ----
dir_hospital <- "/nfs/home/S/shd968/shared_space/ci3_health_data/medicare/gen_admission/1999_2016/targeted_conditions/cache_data/admissions_by_year/"
dir_cohortAd <- "/nfs/home/S/shd968/shared_space/ci3_shd968/medicareADRD/data/ADRDcohort_hospitalization/"

file.remove(list.files(dir_cohortAd, 
                       pattern = ".fst",
                       full.names = T))

for (year_ in 2000:2016) {
  cat("Loading", year_, "hospitalization file... \n")
  admissions <- read_data(dir_hospital, years = year_,
                          columns = c("QID",
                                      "ADATE",
                                      "DDATE"))
  admissions <- admissions[QID %in% enrollInfo[,QID]]
  admissions[, ADATE := dmy(ADATE)][, DDATE := dmy(DDATE)]
  admissions[, year := year(ADATE)]
  admissions <- admissions[year %in% 2000:2016]
  
  write_fst(admissions, paste0(dir_cohortAd, "ADRDcohort_hospitalization", year_, ".fst"))
}
rm(admissions)
gc()

## combine cohort hospitalization files together ----
ADRDcohort_hosp <- NULL
for (i in 2000:2016) {
  adm_ <- read_fst( paste0(dir_cohortAd, "ADRDcohort_hospitalization", i, ".fst"))
  ADRDcohort_hosp <- rbind(ADRDcohort_hosp, adm_)
  cat("finish loading file:", "ADRDcohort_hospitalization", i,".fst", "\n")
}
rm(adm_)
gc()

## clean the ADRD cohort inpatient records ----
setDT(ADRDcohort_hosp)
names(ADRDcohort_hosp)
#' year as Admission year
dim(ADRDcohort_hosp) # [1] 44937905       4

any(duplicated(ADRDcohort_hosp))  # TRUE
sum(duplicated(ADRDcohort_hosp)) # 4785 number of duplicates
ADRDcohort_hosp <- unique(ADRDcohort_hosp) # remove all the duplicates
dim(ADRDcohort_hosp) # [1] 44933120       31 # dimention after removing duplicates
gc()

ADRDcohort_hosp_time <- ADRDcohort_hosp[,.(QID, year)] # drop all diagnosis codes, only save QID, admission year
gc()
head(ADRDcohort_hosp_time)
ADRDcohort_hosp_time <- unique(ADRDcohort_hosp_time) # remove duplicate of Admission year (some may be admitted several times in one year)
dim(ADRDcohort_hosp_time) # [1] 25622884        2
gc()

ADRDcohort_hosp_time <- merge(ADRDcohort_hosp_time, enrollInfo, by = "QID", all.x=T) # merge firstADRDyr to cohort hospitalization
ADRDcohort_hosp_time[year>firstADRDyr,] # subset to those admitted after firstADRDyr
ADRDcohort_hosp_time <- ADRDcohort_hosp_time[year>firstADRDyr,]

## generate ReAd info ----
setorder(ADRDcohort_hosp_time, QID, year)
ReAdInfo <- ADRDcohort_hosp_time[, .(firstReAdyr = min(year)), by = QID]
dim(ReAdInfo)
# [1] 4001221       2

table(ReAdInfo[,firstReAdyr])
# 2001   2002   2003   2004   2005   2006   2007   2008   2009   2010   2011   2012   2013   2014   2015   2016
# 227029 263466 275973 275394 280187 269377 262687 256008 246500 247995 247408 244919 231994 224024 226428 221832
fwrite(ReAdInfo, paste0(dir_in, "ReAdmissionInfo.csv"))

## load ADRD cohort data ---
cohort <- read_fst(paste0(dir_in, "ADRDcohort_clean.fst"), as.data.table = T)
ReAdInfo <- fread(paste0(dir_in, "ReAdmissionInfo.csv"))

## subset for analyzing ReAd ----
#' those with ReAd, followup until ReAd
dt_ReAd_event <- cohort[qid %in% ReAdInfo[,QID],]
# uniqueN(dt_ReAd_event[,qid])
# uniqueN(ReAdInfo[,QID])
dt_ReAd_event <- merge(dt_ReAd_event, ReAdInfo, by.x = "qid", by.y = "QID", all.x = TRUE)
head(dt_ReAd_event)
summary(dt_ReAd_event[,firstReAdyr]- dt_ReAd_event[,firstADRDyr])
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.000   1.000   1.000   2.114   3.000  16.000
dt_ReAd_event$ReAd <- FALSE
dt_ReAd_event[year==firstReAdyr, ReAd:=TRUE][]
dt_ReAd_event <- dt_ReAd_event[year<=firstReAdyr, ]

#' those without ReAd event, followup as the same as in mortality cohort
dt_ReAd_noevent <- cohort[!(qid %in% ReAdInfo[,QID]),]
head(dt_ReAd_noevent)
dt_ReAd_noevent[, firstReAdyr := 0000][]
dt_ReAd_noevent[, ReAd := FALSE]

dt_ReAd <- rbind(dt_ReAd_event, dt_ReAd_noevent)

dim(dt_ReAd[(dead)&(ReAd),]) # the person-year death and readmission happened at the same time
# [1] 1263845      35
write_fst(dt_ReAd[year>firstADRDyr,], paste0(dir_out, "ADRDcohort_ReAd.fst")) # subset dataset to starting from the next year of firstADRDyr

## subset to mortality cohort ----
cohort <- read_fst(paste0(dir_in, "ADRDcohort_clean.fst"), as.data.table = T)
write_fst(cohort[year>firstADRDyr,], paste0(dir_out, "ADRDcohort_dead.fst")) # subset datasets starting from the next year of firstADRDyr
