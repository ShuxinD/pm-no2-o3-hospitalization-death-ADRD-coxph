#' Project: airPollution_ADRD
#' Code: Cox PH 
#' Input: "ADRDcohort_dead.fst"                                                  
#' Output: model specific results
#' Author: Shuxin Dong
#' First create date: 2021-07-07                                                         

## setup ----
rm(list = ls())
gc()

library(data.table)
library(fst)
setDTthreads(threads = 0)
library(survival)

dir_iqrdata <- paste0(getwd(),"/data/")
dir_bootsdata <- paste0(getwd(),"/data/bootstrap/mortality/")
dir_estimate0 <- paste0(getwd(),"/results/main_analysis/1plain/coxph_mortality/")

dir_bootstrap_results <- paste0(getwd(), "/results/bootstrapping/1plain/")

num_uniq_zip <- 33608L # mortality
# num_uniq_zip <- 33532L # ReAd

## load IQRs----
IQRs <- readRDS(paste0(dir_iqrdata,"IQRs_mortality.rds"))

## single pollutant model ----
pollutants <- c("pm25", "no2", "ozone_summer", "ox")
for (pollutants_i in pollutants) {
  cox_coefs_boots<-NULL
  for (boots_id in 1:100) {
    # set.seed(boots_id)
    dt_boots<- read_fst(paste0(dir_bootsdata, boots_id,".fst"), as.data.table = T)
    ## single pollutant model
    cox <- coxph(Surv(time = followupyr_start, time2 = followupyr_end, event = dead) ~ 
                   get(pollutants_i) + 
                   mean_bmi + smoke_rate + 
                   hispanic + pct_blk + medhouseholdincome + medianhousevalue + poverty + education + popdensity + pct_owner_occ +
                   summer_tmmx + winter_tmmx + summer_rmax + winter_rmax +
                   as.factor(year) +  as.factor(region) +
                   strata(as.factor(entry_age_break), as.factor(sex), as.factor(race_collapsed), as.factor(dual)) + cluster(qid),
                 data = dt_boots,
                 tie = c("efron"),
                 na.action = na.omit)
    cox_coefs_boots<-c(cox_coefs_boots, coef(cox)[1])
    rm(dt_boots)
    gc()
    cat("finish", pollutants_i, "sample", boots_id, "of 100\n")
  }
  estimate0 <- fread(paste0(dir_estimate0, "cox_mortality_", pollutants_i, ".csv"))
  coef0 <- estimate0[1,coef]
  var_boots <- floor((num_uniq_zip)^0.7)/num_uniq_zip*var(cox_coefs_boots)
  HR_results <- data.table(HR = exp(coef0*IQRs[,get(pollutants_i)]),
                           HR_lci_bootstrap = exp((coef0-qnorm(.975)*sqrt(var_boots))*IQRs[,get(pollutants_i)]),
                           HR_uci_bootstrap = exp((coef0+qnorm(.975)*sqrt(var_boots))*IQRs[,get(pollutants_i)]))
  fwrite(HR_results, paste0(dir_bootstrap_results, "mortality_", pollutants_i, ".csv"))
}
rm(estimate0)
rm(coef0)
rm(var_boots)
rm(HR_results)
gc()

## multi-pollutant model ----

## pm25+no2+summer ozone
cox_coefs_boots_1 <- NULL
cox_coefs_boots_2 <- NULL
cox_coefs_boots_3 <- NULL
for (boots_id in 1:100) {
  # set.seed(boots_id)
  dt_boots<- read_fst(paste0(dir_bootsdata, boots_id,".fst"), as.data.table = T)
  ## multi pollutant model
  cox <- coxph(Surv(time = followupyr_start, time2 = followupyr_end, event = dead) ~ 
                 pm25 + no2 + ozone_summer +
                 mean_bmi + smoke_rate + 
                 hispanic + pct_blk + medhouseholdincome + medianhousevalue + poverty + education + popdensity + pct_owner_occ +
                 summer_tmmx + winter_tmmx + summer_rmax + winter_rmax +
                 as.factor(year) +  as.factor(region) +
                 strata(as.factor(entry_age_break), as.factor(sex), as.factor(race_collapsed), as.factor(dual)) + cluster(qid),
               data = dt_boots,
               tie = c("efron"),
               na.action = na.omit)
  cox_coefs_boots_1<-c(cox_coefs_boots_1, coef(cox)[1])
  cox_coefs_boots_2<-c(cox_coefs_boots_2, coef(cox)[2])
  cox_coefs_boots_3<-c(cox_coefs_boots_3, coef(cox)[3])
  rm(dt_boots)
  gc()
  cat("finish", pollutants_i, "sample", boots_id, "of 100\n")
}
estimate0 <- fread(paste0(dir_estimate0, "cox_mortality_all3.csv"))
coef0_1 <- estimate0[1,coef]
coef0_2 <- estimate0[2,coef]
coef0_3 <- estimate0[3,coef]
var_boots_1 <- floor((num_uniq_zip)^0.7)/num_uniq_zip*var(cox_coefs_boots_1)
var_boots_2 <- floor((num_uniq_zip)^0.7)/num_uniq_zip*var(cox_coefs_boots_2)
var_boots_3 <- floor((num_uniq_zip)^0.7)/num_uniq_zip*var(cox_coefs_boots_3)
HR_results <- data.table(HR = c(exp(coef0_1*IQRs[,pm25]),exp(coef0_2*IQRs[,no2]),exp(coef0_3*IQRs[,ozone_summer])),
                         HR_lci_bootstrap = c(exp((coef0_1-qnorm(.975)*sqrt(var_boots_1))*IQRs[,pm25]), exp((coef0_2-qnorm(.975)*sqrt(var_boots_2))*IQRs[,no2]), exp((coef0_3-qnorm(.975)*sqrt(var_boots_3))*IQRs[,ozone_summer])),
                         HR_uci_bootstrap = c(exp((coef0_1+qnorm(.975)*sqrt(var_boots_1))*IQRs[,pm25]), exp((coef0_2+qnorm(.975)*sqrt(var_boots_2))*IQRs[,no2]), exp((coef0_3+qnorm(.975)*sqrt(var_boots_3))*IQRs[,ozone_summer])))
fwrite(HR_results, paste0(dir_bootstrap_results, "mortality_all3.csv"))

## pm25+ox
cox_coefs_boots_1 <- NULL
cox_coefs_boots_2 <- NULL
for (boots_id in 1:100) {
  # set.seed(boots_id)
  dt_boots<- read_fst(paste0(dir_bootsdata, boots_id,".fst"), as.data.table = T)
  ## multi pollutant model
  cox <- coxph(Surv(time = followupyr_start, time2 = followupyr_end, event = dead) ~ 
                 pm25 + ox +
                 mean_bmi + smoke_rate + 
                 hispanic + pct_blk + medhouseholdincome + medianhousevalue + poverty + education + popdensity + pct_owner_occ +
                 summer_tmmx + winter_tmmx + summer_rmax + winter_rmax +
                 as.factor(year) +  as.factor(region) +
                 strata(as.factor(entry_age_break), as.factor(sex), as.factor(race_collapsed), as.factor(dual)) + cluster(qid),
               data = dt_boots,
               tie = c("efron"),
               na.action = na.omit)
  cox_coefs_boots_1<-c(cox_coefs_boots_1, coef(cox)[1])
  cox_coefs_boots_2<-c(cox_coefs_boots_2, coef(cox)[2])
  rm(dt_boots)
  gc()
  cat("finish", pollutants_i, "sample", boots_id, "of 100\n")
}
estimate0 <- fread(paste0(dir_estimate0, "cox_mortality_all2.csv"))
coef0_1 <- estimate0[1,coef]
coef0_2 <- estimate0[2,coef]
var_boots_1 <- floor((num_uniq_zip)^0.7)/num_uniq_zip*var(cox_coefs_boots_1)
var_boots_2 <- floor((num_uniq_zip)^0.7)/num_uniq_zip*var(cox_coefs_boots_2)
HR_results <- data.table(HR = c(exp(coef0_1*IQRs[,pm25]),exp(coef0_2*IQRs[,ox])),
                         HR_lci_bootstrap = c(exp((coef0_1-qnorm(.975)*sqrt(var_boots_1))*IQRs[,pm25]), exp((coef0_2-qnorm(.975)*sqrt(var_boots_2))*IQRs[,ox])),
                         HR_uci_bootstrap = c(exp((coef0_1+qnorm(.975)*sqrt(var_boots_1))*IQRs[,pm25]), exp((coef0_2+qnorm(.975)*sqrt(var_boots_2))*IQRs[,ox])))
fwrite(HR_results, paste0(dir_bootstrap_results, "mortality_all2.csv"))