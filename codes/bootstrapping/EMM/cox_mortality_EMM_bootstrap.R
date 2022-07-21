#' Project: airPollution_ADRD  
#' Code: Cox PH model with effect modification
#' Input: bootstrap datasets                                                  
#' Output: model specific results
#' Author: Shuxin Dong                                                         
#' Date: 2021-07-07 

## setup ----
rm(list = ls())
gc()

library(data.table)
library(fst)
setDTthreads(threads = 0)
library(survival)

dir_iqrdata <- paste0(getwd(),"/data/")
dir_bootsdata <- paste0(getwd(),"/data/bootstrap/mortality/")
dir_estimate0 <- paste0(getwd(),"/results/main_analysis/EMM/coxph_mortality/")

dir_bootstrap_results <- paste0(getwd(), "/results/bootstrapping/EMM/mortality/")

num_uniq_zip <- 33608L # mortality
# num_uniq_zip <- 33532L # ReAd

## load IQRs----
IQRs <- readRDS(paste0(dir_iqrdata,"IQRs_mortality.rds"))

## EMM regression ----
pollutants <- c("pm25", "no2", "ozone_summer", "ox")
## by sex ----
for (pollutants_i in pollutants) {
  cox_coefs_boots_level0<-NULL
  cox_coefs_boots_level1<-NULL
  for (boots_id in 1:100) {
    # set.seed(boots_id)
    dt_boots<- read_fst(paste0(dir_bootsdata, boots_id,".fst"), as.data.table = T)
    # model
    cox <- coxph(Surv(time = followupyr_start, time2 = followupyr_end, event = dead) ~ 
                   get(pollutants_i)*as.factor(sex) + 
                   mean_bmi + smoke_rate + 
                   hispanic + pct_blk + medhouseholdincome + medianhousevalue + poverty + education + popdensity + pct_owner_occ +
                   summer_tmmx + winter_tmmx + summer_rmax + winter_rmax +
                   as.factor(year) +  as.factor(region) +
                   strata(as.factor(entry_age_break), as.factor(sex), as.factor(race_collapsed), as.factor(dual)) + cluster(qid),
                 data = dt_boots,
                 tie = c("efron"),
                 na.action = na.omit)
    cox_coefs_boots_level0 <- c(cox_coefs_boots_level0, coef(cox)[1])
    cox_coefs_boots_level1 <- c(cox_coefs_boots_level0, coef(cox)[1] + coef(cox)[length(coef(cox))])
    # rm(dt_boots)
    gc()
    cat("finish", pollutants_i, "sample", boots_id, "of 100\n")
  }
  estimate0 <- fread(paste0(dir_estimate0, "cox_mortality_EMM_sex_", pollutants_i, ".csv"))
  coef0_level0 <- estimate0[1,coef]
  coef0_level1 <- estimate0[1,coef] + estimate0[dim(estimate0)[1],coef]
  var_boots_level0 <- floor((num_uniq_zip)^0.7)/num_uniq_zip*var(cox_coefs_boots_level0)
  var_boots_level1 <- floor((num_uniq_zip)^0.7)/num_uniq_zip*var(cox_coefs_boots_level1)
  HR_results <- data.table(HR = c(exp(coef0_level0*IQRs[,get(pollutants_i)]), exp(coef0_level1*IQRs[,get(pollutants_i)])),
                           HR_lci_bootstrap = c(exp((coef0_level0-qnorm(.975)*sqrt(var_boots_level0))*IQRs[,get(pollutants_i)]), exp((coef0_level1-qnorm(.975)*sqrt(var_boots_level1))*IQRs[,get(pollutants_i)])),
                           HR_uci_bootstrap = c(exp((coef0_level0+qnorm(.975)*sqrt(var_boots_level0))*IQRs[,get(pollutants_i)]), exp((coef0_level1+qnorm(.975)*sqrt(var_boots_level1))*IQRs[,get(pollutants_i)]))
                           )
  HR_results <- as.data.frame(HR_results)
  rownames(HR_results) <- paste0("level", levels(dt_boots[,as.factor(sex)]))
  setDT(HR_results, keep.rownames = T)
  print(HR_results)
  fwrite(HR_results, paste0(dir_bootstrap_results, "cox_mortality_EMM_sex_", pollutants_i, "_HR.csv"))
  cat("save HR for cox mortality EMM bootstrapping sex", pollutants_i, "\n")
}

## by dual ----
for (pollutants_i in pollutants) {
  cox_coefs_boots_level0<-NULL
  cox_coefs_boots_level1<-NULL
  for (boots_id in 1:100) {
    # set.seed(boots_id)
    dt_boots<- read_fst(paste0(dir_bootsdata, boots_id,".fst"), as.data.table = T)
    # model
    cox <- coxph(Surv(time = followupyr_start, time2 = followupyr_end, event = dead) ~ 
                   get(pollutants_i)*as.factor(dual) + 
                   mean_bmi + smoke_rate + 
                   hispanic + pct_blk + medhouseholdincome + medianhousevalue + poverty + education + popdensity + pct_owner_occ +
                   summer_tmmx + winter_tmmx + summer_rmax + winter_rmax +
                   as.factor(year) +  as.factor(region) +
                   strata(as.factor(entry_age_break), as.factor(dual), as.factor(race_collapsed), as.factor(dual)) + cluster(qid),
                 data = dt_boots,
                 tie = c("efron"),
                 na.action = na.omit)
    cox_coefs_boots_level0 <- c(cox_coefs_boots_level0, coef(cox)[1])
    cox_coefs_boots_level1 <- c(cox_coefs_boots_level0, coef(cox)[1] + coef(cox)[length(coef(cox))])
    # rm(dt_boots)
    gc()
    cat("finish", pollutants_i, "sample", boots_id, "of 100\n")
  }
  estimate0 <- fread(paste0(dir_estimate0, "cox_mortality_EMM_dual_", pollutants_i, ".csv"))
  coef0_level0 <- estimate0[1,coef]
  coef0_level1 <- estimate0[1,coef] + estimate0[dim(estimate0)[1],coef]
  var_boots_level0 <- floor((num_uniq_zip)^0.7)/num_uniq_zip*var(cox_coefs_boots_level0)
  var_boots_level1 <- floor((num_uniq_zip)^0.7)/num_uniq_zip*var(cox_coefs_boots_level1)
  HR_results <- data.table(HR = c(exp(coef0_level0*IQRs[,get(pollutants_i)]), exp(coef0_level1*IQRs[,get(pollutants_i)])),
                           HR_lci_bootstrap = c(exp((coef0_level0-qnorm(.975)*sqrt(var_boots_level0))*IQRs[,get(pollutants_i)]), exp((coef0_level1-qnorm(.975)*sqrt(var_boots_level1))*IQRs[,get(pollutants_i)])),
                           HR_uci_bootstrap = c(exp((coef0_level0+qnorm(.975)*sqrt(var_boots_level0))*IQRs[,get(pollutants_i)]), exp((coef0_level1+qnorm(.975)*sqrt(var_boots_level1))*IQRs[,get(pollutants_i)]))
  )
  HR_results <- as.data.frame(HR_results)
  rownames(HR_results) <- paste0("level", levels(dt_boots[,as.factor(dual)]))
  setDT(HR_results, keep.rownames = T)
  print(HR_results)
  fwrite(HR_results, paste0(dir_bootstrap_results, "cox_mortality_EMM_dual_", pollutants_i, "_HR.csv"))
  cat("save HR for cox mortality EMM bootstrapping dual", pollutants_i, "\n")
}

## by population density (above median) ----
for (pollutants_i in pollutants) {
  cox_coefs_boots_level0<-NULL
  cox_coefs_boots_level1<-NULL
  for (boots_id in 1:100) {
    # set.seed(boots_id)
    dt_boots<- read_fst(paste0(dir_bootsdata, boots_id,".fst"), as.data.table = T)
    # model
    cox <- coxph(Surv(time = followupyr_start, time2 = followupyr_end, event = dead) ~ 
                   get(pollutants_i)*as.factor(above_median_popdensity) + 
                   mean_bmi + smoke_rate + 
                   hispanic + pct_blk + medhouseholdincome + medianhousevalue + poverty + education + popdensity + pct_owner_occ +
                   summer_tmmx + winter_tmmx + summer_rmax + winter_rmax +
                   as.factor(year) +  as.factor(region) +
                   strata(as.factor(entry_age_break), as.factor(above_median_popdensity), as.factor(race_collapsed), as.factor(dual)) + cluster(qid),
                 data = dt_boots,
                 tie = c("efron"),
                 na.action = na.omit)
    cox_coefs_boots_level0 <- c(cox_coefs_boots_level0, coef(cox)[1])
    cox_coefs_boots_level1 <- c(cox_coefs_boots_level0, coef(cox)[1] + coef(cox)[length(coef(cox))])
    # rm(dt_boots)
    gc()
    cat("finish", pollutants_i, "sample", boots_id, "of 100\n")
  }
  estimate0 <- fread(paste0(dir_estimate0, "cox_mortality_EMM_above_median_popdensity_", pollutants_i, ".csv"))
  coef0_level0 <- estimate0[1,coef]
  coef0_level1 <- estimate0[1,coef] + estimate0[dim(estimate0)[1],coef]
  var_boots_level0 <- floor((num_uniq_zip)^0.7)/num_uniq_zip*var(cox_coefs_boots_level0)
  var_boots_level1 <- floor((num_uniq_zip)^0.7)/num_uniq_zip*var(cox_coefs_boots_level1)
  HR_results <- data.table(HR = c(exp(coef0_level0*IQRs[,get(pollutants_i)]), exp(coef0_level1*IQRs[,get(pollutants_i)])),
                           HR_lci_bootstrap = c(exp((coef0_level0-qnorm(.975)*sqrt(var_boots_level0))*IQRs[,get(pollutants_i)]), exp((coef0_level1-qnorm(.975)*sqrt(var_boots_level1))*IQRs[,get(pollutants_i)])),
                           HR_uci_bootstrap = c(exp((coef0_level0+qnorm(.975)*sqrt(var_boots_level0))*IQRs[,get(pollutants_i)]), exp((coef0_level1+qnorm(.975)*sqrt(var_boots_level1))*IQRs[,get(pollutants_i)]))
  )
  HR_results <- as.data.frame(HR_results)
  rownames(HR_results) <- paste0("level", levels(dt_boots[,as.factor(above_median_popdensity)]))
  setDT(HR_results, keep.rownames = T)
  print(HR_results)
  fwrite(HR_results, paste0(dir_bootstrap_results, "cox_mortality_EMM_above_median_popdensity_", pollutants_i, "_HR.csv"))
  cat("save HR for cox mortality EMM bootstrapping above_median_popdensity", pollutants_i, "\n")
}

## by entry age ----
for (pollutants_i in pollutants) {
  cox_coefs_boots_level0<-NULL
  cox_coefs_boots_level1<-NULL
  for (boots_id in 1:100) {
    # set.seed(boots_id)
    dt_boots<- read_fst(paste0(dir_bootsdata, boots_id,".fst"), as.data.table = T)
    dt_boots[, entry_age_over85 := entry_age>85]
    # model
    cox <- coxph(Surv(time = followupyr_start, time2 = followupyr_end, event = dead) ~ 
                   get(pollutants_i)*as.factor(entry_age_over85) + 
                   mean_bmi + smoke_rate + 
                   hispanic + pct_blk + medhouseholdincome + medianhousevalue + poverty + education + popdensity + pct_owner_occ +
                   summer_tmmx + winter_tmmx + summer_rmax + winter_rmax +
                   as.factor(year) +  as.factor(region) +
                   strata(as.factor(entry_age_break), as.factor(entry_age_over85), as.factor(race_collapsed), as.factor(dual)) + cluster(qid),
                 data = dt_boots,
                 tie = c("efron"),
                 na.action = na.omit)
    cox_coefs_boots_level0 <- c(cox_coefs_boots_level0, coef(cox)[1])
    cox_coefs_boots_level1 <- c(cox_coefs_boots_level0, coef(cox)[1] + coef(cox)[length(coef(cox))])
    # rm(dt_boots)
    gc()
    cat("finish", pollutants_i, "sample", boots_id, "of 100\n")
  }
  estimate0 <- fread(paste0(dir_estimate0, "cox_mortality_EMM_entry_age_over85_", pollutants_i, ".csv"))
  coef0_level0 <- estimate0[1,coef]
  coef0_level1 <- estimate0[1,coef] + estimate0[dim(estimate0)[1],coef]
  var_boots_level0 <- floor((num_uniq_zip)^0.7)/num_uniq_zip*var(cox_coefs_boots_level0)
  var_boots_level1 <- floor((num_uniq_zip)^0.7)/num_uniq_zip*var(cox_coefs_boots_level1)
  HR_results <- data.table(HR = c(exp(coef0_level0*IQRs[,get(pollutants_i)]), exp(coef0_level1*IQRs[,get(pollutants_i)])),
                           HR_lci_bootstrap = c(exp((coef0_level0-qnorm(.975)*sqrt(var_boots_level0))*IQRs[,get(pollutants_i)]), exp((coef0_level1-qnorm(.975)*sqrt(var_boots_level1))*IQRs[,get(pollutants_i)])),
                           HR_uci_bootstrap = c(exp((coef0_level0+qnorm(.975)*sqrt(var_boots_level0))*IQRs[,get(pollutants_i)]), exp((coef0_level1+qnorm(.975)*sqrt(var_boots_level1))*IQRs[,get(pollutants_i)]))
  )
  HR_results <- as.data.frame(HR_results)
  rownames(HR_results) <- paste0("level", levels(dt_boots[,as.factor(entry_age_over85)]))
  setDT(HR_results, keep.rownames = T)
  print(HR_results)
  fwrite(HR_results, paste0(dir_bootstrap_results, "cox_mortality_EMM_entry_age_over85_", pollutants_i, "_HR.csv"))
  cat("save HR for cox mortality EMM bootstrapping entry_age_over85", pollutants_i, "\n")
}

## by race_collapsed ----
for (pollutants_i in pollutants) {
  cox_coefs_boots_level0<-NULL
  cox_coefs_boots_level1<-NULL
  cox_coefs_boots_level2<-NULL
  cox_coefs_boots_level2<-NULL
  for (boots_id in 1:100) {
    # set.seed(boots_id)
    dt_boots<- read_fst(paste0(dir_bootsdata, boots_id,".fst"), as.data.table = T)
    # model
    cox <- coxph(Surv(time = followupyr_start, time2 = followupyr_end, event = dead) ~ 
                   get(pollutants_i)*as.factor(race_collapsed) + 
                   mean_bmi + smoke_rate + 
                   hispanic + pct_blk + medhouseholdincome + medianhousevalue + poverty + education + popdensity + pct_owner_occ +
                   summer_tmmx + winter_tmmx + summer_rmax + winter_rmax +
                   as.factor(year) +  as.factor(region) +
                   strata(as.factor(entry_age_break), as.factor(race_collapsed), as.factor(race_collapsed), as.factor(dual)) + cluster(qid),
                 data = dt_boots,
                 tie = c("efron"),
                 na.action = na.omit)
    cox_coefs_boots_level0 <- c(cox_coefs_boots_level0, coef(cox)[1])
    cox_coefs_boots_level1 <- c(cox_coefs_boots_level0, coef(cox)[1] + coef(cox)[length(coef(cox))])
    cox_coefs_boots_level2 <- c(cox_coefs_boots_level0, coef(cox)[1] + coef(cox)[length(coef(cox))-1])
    cox_coefs_boots_level3 <- c(cox_coefs_boots_level0, coef(cox)[1] + coef(cox)[length(coef(cox))-2])
    # rm(dt_boots)
    gc()
    cat("finish", pollutants_i, "sample", boots_id, "of 100\n")
  }
  estimate0 <- fread(paste0(dir_estimate0, "cox_mortality_EMM_race_collapsed_", pollutants_i, ".csv"))
  coef0_level0 <- estimate0[1,coef]
  coef0_level1 <- estimate0[1,coef] + estimate0[dim(estimate0)[1],coef]
  coef0_level2 <- estimate0[1,coef] + estimate0[dim(estimate0)[1]-1,coef]
  coef0_level3 <- estimate0[1,coef] + estimate0[dim(estimate0)[1]-2,coef]
  var_boots_level0 <- floor((num_uniq_zip)^0.7)/num_uniq_zip*var(cox_coefs_boots_level0)
  var_boots_level1 <- floor((num_uniq_zip)^0.7)/num_uniq_zip*var(cox_coefs_boots_level1)
  var_boots_level2 <- floor((num_uniq_zip)^0.7)/num_uniq_zip*var(cox_coefs_boots_level2)
  var_boots_level3 <- floor((num_uniq_zip)^0.7)/num_uniq_zip*var(cox_coefs_boots_level3)
  
  HR_results <- data.table(HR = c(exp(coef0_level0*IQRs[,get(pollutants_i)]), exp(coef0_level1*IQRs[,get(pollutants_i)]), exp(coef0_level2*IQRs[,get(pollutants_i)]), exp(coef0_level3*IQRs[,get(pollutants_i)])),
                           HR_lci_bootstrap = c(exp((coef0_level0-qnorm(.975)*sqrt(var_boots_level0))*IQRs[,get(pollutants_i)]), exp((coef0_level1-qnorm(.975)*sqrt(var_boots_level1))*IQRs[,get(pollutants_i)]), exp((coef0_level2-qnorm(.975)*sqrt(var_boots_level0))*IQRs[,get(pollutants_i)]), exp((coef0_level3-qnorm(.975)*sqrt(var_boots_level0))*IQRs[,get(pollutants_i)])),
                           HR_uci_bootstrap = c(exp((coef0_level0+qnorm(.975)*sqrt(var_boots_level0))*IQRs[,get(pollutants_i)]), exp((coef0_level1+qnorm(.975)*sqrt(var_boots_level1))*IQRs[,get(pollutants_i)]), exp((coef0_level2+qnorm(.975)*sqrt(var_boots_level0))*IQRs[,get(pollutants_i)]), exp((coef0_level3+qnorm(.975)*sqrt(var_boots_level0))*IQRs[,get(pollutants_i)]))
  )
  HR_results <- as.data.frame(HR_results)
  rownames(HR_results) <- paste0("level", levels(dt_boots[,as.factor(race_collapsed)]))
  setDT(HR_results, keep.rownames = T)
  print(HR_results)
  fwrite(HR_results, paste0(dir_bootstrap_results, "cox_mortality_EMM_race_collapsed_", pollutants_i, "_HR.csv"))
  cat("save HR for cox mortality EMM bootstrapping race_collapsed", pollutants_i, "\n")
}