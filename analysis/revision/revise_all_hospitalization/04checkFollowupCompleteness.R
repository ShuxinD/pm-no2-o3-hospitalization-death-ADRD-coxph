#' The dataset `ALLcohort.fst` was directly generated from the denominator files, which follow all people till their death or the end of 2016.

## setup ----
rm(list = ls())
gc()

library(data.table)
setDTthreads(threads = 0)
library(fst)
library(dplyr)

wkdir <- "/n/dominici_nsaph_l3/Lab/projects/pm-no2-o3-hospitalization-death-ADRD-coxph"
rundir <- file.path(wkdir, "code", "revise_all_hospitalization")

ALLcohort <- read_fst(file.path(rundir, "ALLcohort.fst"), as.data.table = T)
enrolledInfo <- read_fst(file.path(rundir, "All_EnrolledInfo.fst"), as.data.table = T)

## basic info of original dataset ----
cat("number of subjects in original all cohort dataset", uniqueN(ALLcohort[,qid]), "\n")
cat("number of subjects in cohorts after removing NAs",uniqueN(na.omit(ALLcohort)[,qid]),"\n")

#' Those who admitted for the first time with all code and died in the same year don't contribute to the risk set. **Consider deleting those subjects**

A <- ALLcohort[, .(end_yr = max(year)), by = qid]
A <- merge(A, enrolledInfo, by.x = "qid", by.y = "QID", all.x = T)
no_contribution <- A[end_yr==firstHOSPyr,]
cat("The number of person-years/subjects considered to be deleted due to no-contribution is", dim(no_contribution)[1])

temp <- ALLcohort[!(qid %in% no_contribution[,qid]),]
cat("number of subjects after removing no-contribution subjects", uniqueN(temp[,qid]), "\n")
temp <- na.omit(ALLcohort[!(qid %in% no_contribution[,qid]),]) # remove all the NAs and those without contribution to risk set
cat("number of subjects after removing no-contribution subjects and NAs", uniqueN(temp[,qid]), "\n")
sum_temp <- temp[, .(start_yr = min(year),
                     end_yr = max(year),
                     count = uniqueN(year)), by = qid]
sum_temp <- merge(sum_temp, enrolledInfo, by.x = "qid", by.y = "QID", all.x = TRUE)

dim(sum_temp)[1]

#' We constructed a temporary dataset named `temp` which is a subset of `ALLcohort` after removing NA (not remove rows with `year==firstHOSPyr`, but remove those making no contribution to the risk set), and summarize each person as one row in total `r dim(sum_temp)[1]` subjects:
#' - generated `start_yr` as the minimum of calendar year (should equal to `firstHOSPyr`)
#' - `end_yr` as the maximum of calendar year
#' - `count` as the count number of unique calendar year for each subject. 
#' 
#' We also merged the enroll information (`firstALLyear`) to `sum_temp`. `firstALLyr` indicates the year that subjects should start to be followed-up.
#' `sum_temp` is a one-row-per-person dataset.

cat("the number of subjects in all cohort (after removing NAs, and non-contribution subjects) is", dim(sum_temp)[1], "\n")
cat("the number of person-years in all cohort (after removing NAs, and non-contribution subjects) is", dim(temp)[1], "\n")
cat("is there any duplication of the combination of `qid` and calendar year: ", any(duplicated(temp[,.(qid,year)])), "\n")
cat("is there any duplication of the combination of `qid` and age: ", any(duplicated(temp[,.(qid,age)])), "\n")

## Check the completeness of follow-up after removing no-contribution subjects ----
### 1. how many subjects were not followed-up from `firstHOSPyr` ----
cat("the number of subjects not followed from firstHOSPyr is", dim(sum_temp[start_yr != (firstHOSPyr)])[1], "\n")
cat("the number of person-years of related subjects is", dim(temp[qid %in% sum_temp[start_yr != firstHOSPyr, qid], ])[1], "\n")
### 2. how many people don't have each year's info ----
setorder(temp,qid,year)
cat("the number of subjects not having each year's info (contain those from 1.) is", dim(sum_temp[(end_yr-firstHOSPyr+1) != count,])[1], "\n")
cat("the number of person-years (contain those from 1.) is", dim(temp[qid %in% sum_temp[(end_yr-firstHOSPyr+1) != count,qid],])[1], "\n")

## Omit those without complete follow-up and contribution to the risk set ----
cat("the number of subjects without complete follow-up is", dim(sum_temp[(end_yr-firstHOSPyr+1) != count,])[1], "\n")
cat("the number of person-years of related subjects is", dim(temp[qid %in% sum_temp[(end_yr-firstHOSPyr+1) != count,qid],])[1], "\n")

cat("the number of person-years/subjects making no-contribution to the risk set is", dim(no_contribution)[1], "\n")

omitInfo <- rbind(sum_temp[(end_yr-firstHOSPyr+1) != count, .(qid,end_yr, firstHOSPyr)], no_contribution)
dim(omitInfo)[1]
#' The above subject (`r dim(omitInfo)[1]` subjects in total) do not have each year's info during follow-up or (mostly) make no contribution to the risk set. **Deleting them**

## Check right-censoring of the processed data ----
#' Check whether alive people were followed-up till the end of study period (2016) after excluding those without complete follow-up
#' We could see some alive subjects weren't followed-up till 2016. This should be considered as right-censored subjects in the analyses.
sum_temp[!(qid %in% temp[(dead),qid]), ][!(qid %in% omitInfo[,qid]),end_yr] %>% table()
names(omitInfo)
write_fst(omitInfo, path = file.path(rundir, "omitInfo.fst"))