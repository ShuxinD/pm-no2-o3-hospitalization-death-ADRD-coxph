#' Project: airPollution_ADRD
#' Code: bootstrapping dataset
#' Input: "ADRDcohort_dead.fst"
#' Input: "ADRDcohort_ReAd.fst"
#' Output: bootstrap datasets
#' Author: Shuxin Dong                                                         
#' Date: 2021-07-07

## setup ----
rm(list = ls())
gc()

library(data.table)
setDTthreads(threads = 0)
library(fst)
#setwd("/nfs/home/S/shd968/shared_space/ci3_shd968/dementia/")
dir_data <- paste0(getwd(),"/data/")

## load mortality cohort ----
dt <- read_fst(paste0(dir_data, "ADRDcohort_dead.fst"), as.data.table = T)
all_zip <- unique(dt[,zip])
num_uniq_zip <- uniqueN(dt[,zip])

## create bootstrap data for mortality ----
## Save the bootstrapped data to accelerate computing
dir.create(file.path(paste0(getwd(),"/data/bootstrap/mortality/")), showWarnings = FALSE)

lapply(1:100, function(boots_id){
  set.seed(boots_id)
  zip_sample <- sample(1:num_uniq_zip, floor((num_uniq_zip)^0.7), replace=T) 
  dt_boots <- subset(dt, zip %in% all_zip[zip_sample]) 
  write_fst(dt_boots, paste0(getwd(),"/data/bootstrap/mortality/",boots_id,".fst"))
  cat("finish creating data", boots_id, "of 100\n")
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
dir.create(file.path(paste0(getwd(),"/data/bootstrap/ReAd/")), showWarnings = FALSE)

lapply(1:100, function(boots_id){
  set.seed(boots_id)
  zip_sample <- sample(1:num_uniq_zip, floor((num_uniq_zip)^0.7), replace=T) 
  dt_boots <- subset(dt, zip %in% all_zip[zip_sample]) 
  write_fst(dt_boots, paste0(getwd(),"/data/bootstrap/ReAd/", boots_id,".fst"))
  cat("finish creating data", boots_id, "of 100\n")
})

