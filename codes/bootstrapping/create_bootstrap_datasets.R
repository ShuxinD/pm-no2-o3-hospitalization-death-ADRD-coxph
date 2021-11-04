#' Project: airPollution_ADRD
#' Code: bootstrapping dataset
#' Input: "ADRDcohort_clean.fst"
#' Output: bootstrap datasets
#' Author: Shuxin Dong                                                         
#' Date: 2021-07-07

## setup ----
rm(list = ls())
gc()

library(data.table)
setDTthreads(threads = 0)
library(fst)

setwd("/nfs/home/S/shd968/shared_space/ci3_shd968/dementia/")
dir_data <- "/nfs/home/S/shd968/shared_space/ci3_shd968/dementia/data/"

## load mortality cohort ----
dt <- read_fst(paste0(dir_data, "ADRDcohort_clean.fst"), as.data.table = T)
all_zip <- unique(dt[,zip])
num_uniq_zip <- uniqueN(dt[,zip])

## create bootstrap data for mortality ----
#' Save the bootstrapped data to accelerate computing
dir.create(file.path("/nfs/home/S/shd968/shared_space/ci3_shd968/dementia/data/bootstrap/mortality/"), showWarnings = FALSE)

lapply(1:200, function(boots_id){
  set.seed(boots_id)
  zip_sample <- sample(1:num_uniq_zip, floor((num_uniq_zip)^0.7), replace=T) 
  dt_boots <- subset(dt, zip %in% all_zip[zip_sample]) 
  write_fst(dt_boots, paste0("/nfs/home/S/shd968/shared_space/ci3_shd968/dementia/data/bootstrap/mortality/", boots_id,".fst"))
  cat("finish creating data", boots_id, "of 200\n")
})

## load ReAd cohort ----
dt <- read_fst(paste0(dir_data, "ADRDcohort_ReAd.fst"), as.data.table = T)
dt_ipw <- read_fst(paste0(dir_data, "ADRDcohort_ReAd_deadipw.fst"), as.data.table = T)
dt[dt_ipw, on = .(qid = qid, year = year)][]
dt <- dt[dt_ipw, on = .(qid = qid, year = year)]

all_zip <- unique(dt[,zip])
num_uniq_zip <- uniqueN(dt[,zip])

## create bootstrap data for ReAd ----
#' Save the bootstrapped data to accelerate computing
dir.create(file.path("/nfs/home/S/shd968/shared_space/ci3_shd968/dementia/data/bootstrap/ReAd/"), showWarnings = FALSE)

lapply(1:200, function(boots_id){
  set.seed(boots_id)
  zip_sample <- sample(1:num_uniq_zip, floor((num_uniq_zip)^0.7), replace=T) 
  dt_boots <- subset(dt, zip %in% all_zip[zip_sample]) 
  write_fst(dt_boots, paste0("/nfs/home/S/shd968/shared_space/ci3_shd968/dementia/data/bootstrap/ReAd/", boots_id,".fst"))
  cat("finish creating data", boots_id, "of 200\n")
})


temp <- coxph(Surv(time=year_prev, time2 = year, event = dead)~as.factor(sex) + strata(as.factor(sex)),
              data=dt,
              ties = "efron")
summary(temp)

