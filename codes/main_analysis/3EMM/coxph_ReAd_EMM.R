## Project: airPollution_ADRD  
## Code: Cox PH model with effect modification
## Input: "ADRDcohort_ReAd.fst"                                                   
## Output: model specific results
## Author: Shuxin Dong                                                         
## Date: 2021-02-17                                                            

## setup ----
rm(list = ls())
gc()

library(data.table)
library(fst)
setDTthreads(threads = 0)
library(survival)

# setwd("/nfs/home/S/shd968/shared_space/ci3_shd968/medicareADRD/")
dir_data <- paste0(getwd(),"/data/")

dir_out <- paste0(getwd(),"/results/main_analysis/EMM/coxph_ReAd/")

## load data ----
dt <- read_fst(paste0(dir_data, "ADRDcohort_ReAd.fst"), as.data.table = T)
dt_ipwdeath <- read_fst(paste0(dir_data,"ADRDcohort_ReAd_deadipw.fst"), as.data.table = T)
dt <- merge(dt, dt_ipwdeath, by = c("qid", "year"))
# names(dt)

dt[, followupyr_start := (year - firstADRDyr-1)]
dt[, followupyr_end := (year - firstADRDyr)]

IQRs <- data.table(IQR(dt$pm25), IQR(dt$no2), IQR(dt$ozone), IQR(dt$ox), IQR(dt$ozone_summer))
colnames(IQRs) <- c("pm25", "no2", "ozone", "ox", "ozone_summer")
print(IQRs)

## EMM by sex -----
pollutants <- c("pm25", "no2", "ozone_summer", "ox")

for (pollutants_i in pollutants) {
  cat("fit coxph model EMM sex", pollutants_i, "\n")
  cox <- coxph(Surv(time = followupyr_start, time2 = followupyr_end, event = ReAd) ~ 
                 get(pollutants_i)*as.factor(sex) + 
                 mean_bmi + smoke_rate + 
                 hispanic + pct_blk + medhouseholdincome + medianhousevalue + poverty + education + popdensity + pct_owner_occ +
                 summer_tmmx + winter_tmmx + summer_rmax + winter_rmax +
                 as.factor(year) +  as.factor(region) +
                 strata(as.factor(entry_age_break), as.factor(sex), as.factor(race_collapsed), as.factor(dual)) + cluster(qid),
               weights = get(paste0("deadipw_",pollutants_i)),
               data = dt,
               tie = c("efron"),
               na.action = na.omit)
  tb <- summary(cox)$coefficients
  tb <- as.data.frame(tb)
  setDT(tb, keep.rownames = TRUE)
  fwrite(tb, paste0(dir_out, "cox_ReAd_EMM_sex_", pollutants_i, ".csv"))
  cov_matrix <- vcov(cox)
  cat("output coefs...\n")
  HR_0 <- c(tb[1,coef], IQRs[, get(pollutants_i)], exp(tb[1,coef]*IQRs[, get(pollutants_i)]), exp((tb[1,coef]-tb[1,`robust se`])*IQRs[, get(pollutants_i)]), exp((tb[1,coef]+tb[1,`robust se`])*IQRs[, get(pollutants_i)]))
  se <- sqrt(cov_matrix[1,1] + cov_matrix[dim(cov_matrix)[1],dim(cov_matrix)[1]] + 2*cov_matrix[1, dim(cov_matrix)[1]])
  HR_1 <- c(tb[dim(cov_matrix)[1],coef], IQRs[, get(pollutants_i)], exp(tb[dim(cov_matrix)[1],coef]*IQRs[, get(pollutants_i)]), exp((tb[dim(cov_matrix)[1],coef]-se)*IQRs[, get(pollutants_i)]),  exp((tb[dim(cov_matrix)[1],coef]+se)*IQRs[, get(pollutants_i)]))
  HR <- rbind(HR_0, HR_1)
  print(HR)
  HR <- as.data.frame(HR)
  colnames(HR) <- c("coef", "IQRunit", "HR_IQR", "HR_lci", "HR_uci")
  rownames(HR) <- paste0("level", levels(dt[,as.factor(sex)]))
  setDT(HR, keep.rownames = T)
  print(HR)
  fwrite(HR, paste0(dir_out, "cox_ReAd_EMM_sex_", pollutants_i, "_HR.csv"))
  cat("save HR for cox ReAd EMM sex", pollutants_i, "\n")
}

## EMM by dual ----
pollutants <- c("pm25", "no2", "ozone_summer", "ox")

for (pollutants_i in pollutants) {
  cat("fit coxph model EMM dual", pollutants_i, "\n")
  cox <- coxph(Surv(time = followupyr_start, time2 = followupyr_end, event = ReAd) ~ 
                 get(pollutants_i)*as.factor(dual) + 
                 mean_bmi + smoke_rate + 
                 hispanic + pct_blk + medhouseholdincome + medianhousevalue + poverty + education + popdensity + pct_owner_occ +
                 summer_tmmx + winter_tmmx + summer_rmax + winter_rmax +
                 as.factor(year) +  as.factor(region) +
                 strata(as.factor(entry_age_break), as.factor(sex), as.factor(race_collapsed), as.factor(dual)) + cluster(qid),
               weights = get(paste0("deadipw_",pollutants_i)),
               data = dt,
               tie = c("efron"),
               na.action = na.omit)
  tb <- summary(cox)$coefficients
  tb <- as.data.frame(tb)
  setDT(tb, keep.rownames = TRUE)
  fwrite(tb, paste0(dir_out, "cox_ReAd_EMM_dual_", pollutants_i, ".csv"))
  cov_matrix <- vcov(cox)
  cat("output coefs...\n")
  HR_0 <- c(tb[1,coef], IQRs[, get(pollutants_i)], exp(tb[1,coef]*IQRs[, get(pollutants_i)]), exp((tb[1,coef]-tb[1,`robust se`])*IQRs[, get(pollutants_i)]), exp((tb[1,coef]+tb[1,`robust se`])*IQRs[, get(pollutants_i)]))
  se <- sqrt(cov_matrix[1,1] + cov_matrix[dim(cov_matrix)[1],dim(cov_matrix)[1]] + 2*cov_matrix[1, dim(cov_matrix)[1]])
  HR_1 <- c(tb[dim(cov_matrix)[1],coef], IQRs[, get(pollutants_i)], exp(tb[dim(cov_matrix)[1],coef]*IQRs[, get(pollutants_i)]), exp((tb[dim(cov_matrix)[1],coef]-se)*IQRs[, get(pollutants_i)]),  exp((tb[dim(cov_matrix)[1],coef]+se)*IQRs[, get(pollutants_i)]))
  HR <- rbind(HR_0, HR_1)
  print(HR)
  HR <- as.data.frame(HR)
  colnames(HR) <- c("coef", "IQRunit", "HR_IQR", "HR_lci", "HR_uci")
  rownames(HR) <- paste0("level", levels(dt[,as.factor(dual)]))
  setDT(HR, keep.rownames = T)
  print(HR)
  fwrite(HR, paste0(dir_out, "cox_ReAd_EMM_dual_", pollutants_i, "_HR.csv"))
  cat("save HR for cox ReAd EMM dual", pollutants_i, "\n")
}

## EMM by population density (above median)----
median_popdensity <- median(dt[, popdensity])
dt[, above_median_popdensity := popdensity>median_popdensity]

pollutants <- c("pm25", "no2", "ozone_summer", "ox")

for (pollutants_i in pollutants) {
  cat("fit coxph model EMM above median popdensity", pollutants_i, "\n")
  cox <- coxph(Surv(time = followupyr_start, time2 = followupyr_end, event = ReAd) ~ 
                 get(pollutants_i)*as.factor(above_median_popdensity) + 
                 mean_bmi + smoke_rate + 
                 hispanic + pct_blk + medhouseholdincome + medianhousevalue + poverty + education + popdensity + pct_owner_occ +
                 summer_tmmx + winter_tmmx + summer_rmax + winter_rmax +
                 as.factor(year) +  as.factor(region) +
                 strata(as.factor(entry_age_break), as.factor(sex), as.factor(race_collapsed), as.factor(dual)) + cluster(qid),
               weights = get(paste0("deadipw_",pollutants_i)),
               data = dt,
               tie = c("efron"),
               na.action = na.omit)
  tb <- summary(cox)$coefficients
  tb <- as.data.frame(tb)
  setDT(tb, keep.rownames = TRUE)
  fwrite(tb, paste0(dir_out, "cox_ReAd_EMM_above_median_popdensity_", pollutants_i, ".csv"))
  cov_matrix <- vcov(cox)
  cat("output coefs...\n")
  HR_0 <- c(tb[1,coef], IQRs[, get(pollutants_i)], exp(tb[1,coef]*IQRs[, get(pollutants_i)]), exp((tb[1,coef]-tb[1,`robust se`])*IQRs[, get(pollutants_i)]), exp((tb[1,coef]+tb[1,`robust se`])*IQRs[, get(pollutants_i)]))
  se <- sqrt(cov_matrix[1,1] + cov_matrix[dim(cov_matrix)[1],dim(cov_matrix)[1]] + 2*cov_matrix[1, dim(cov_matrix)[1]])
  HR_1 <- c(tb[dim(cov_matrix)[1],coef], IQRs[, get(pollutants_i)], exp(tb[dim(cov_matrix)[1],coef]*IQRs[, get(pollutants_i)]), exp((tb[dim(cov_matrix)[1],coef]-se)*IQRs[, get(pollutants_i)]),  exp((tb[dim(cov_matrix)[1],coef]+se)*IQRs[, get(pollutants_i)]))
  HR <- rbind(HR_0, HR_1)
  print(HR)
  HR <- as.data.frame(HR)
  colnames(HR) <- c("coef", "IQRunit", "HR_IQR", "HR_lci", "HR_uci")
  rownames(HR) <- paste0("level", levels(dt[,as.factor(above_median_popdensity)]))
  setDT(HR, keep.rownames = T)
  print(HR)
  fwrite(HR, paste0(dir_out, "cox_ReAd_EMM_above_median_popdensity_", pollutants_i, "_HR.csv"))
  cat("save HR for cox ReAd EMM above median popdensity", pollutants_i, "\n")
}

## EMM by entry age ----
dt[, entry_age_over85 := entry_age>85]
pollutants <- c("pm25", "no2", "ozone_summer", "ox")

for (pollutants_i in pollutants) {
  cat("fit coxph model EMM entry_age_over85", pollutants_i, "\n")
  cox <- coxph(Surv(time = followupyr_start, time2 = followupyr_end, event = ReAd) ~ 
                 get(pollutants_i)*as.factor(entry_age_over85) + 
                 mean_bmi + smoke_rate + 
                 hispanic + pct_blk + medhouseholdincome + medianhousevalue + poverty + education + popdensity + pct_owner_occ +
                 summer_tmmx + winter_tmmx + summer_rmax + winter_rmax +
                 as.factor(year) +  as.factor(region) +
                 strata(as.factor(entry_age_break), as.factor(sex), as.factor(race_collapsed), as.factor(dual)) + cluster(qid),
               weights = get(paste0("deadipw_",pollutants_i)),
               data = dt,
               tie = c("efron"),
               na.action = na.omit)
  tb <- summary(cox)$coefficients
  tb <- as.data.frame(tb)
  setDT(tb, keep.rownames = TRUE)
  fwrite(tb, paste0(dir_out, "cox_ReAd_EMM_entry_age_over85_", pollutants_i, ".csv"))
  cov_matrix <- vcov(cox)
  cat("output coefs...\n")
  HR_0 <- c(tb[1,coef], IQRs[, get(pollutants_i)], exp(tb[1,coef]*IQRs[, get(pollutants_i)]), exp((tb[1,coef]-tb[1,`robust se`])*IQRs[, get(pollutants_i)]), exp((tb[1,coef]+tb[1,`robust se`])*IQRs[, get(pollutants_i)]))
  se <- sqrt(cov_matrix[1,1] + cov_matrix[dim(cov_matrix)[1],dim(cov_matrix)[1]] + 2*cov_matrix[1, dim(cov_matrix)[1]])
  HR_1 <- c(tb[dim(cov_matrix)[1],coef], IQRs[, get(pollutants_i)], exp(tb[dim(cov_matrix)[1],coef]*IQRs[, get(pollutants_i)]), exp((tb[dim(cov_matrix)[1],coef]-se)*IQRs[, get(pollutants_i)]),  exp((tb[dim(cov_matrix)[1],coef]+se)*IQRs[, get(pollutants_i)]))
  HR <- rbind(HR_0, HR_1)
  print(HR)
  HR <- as.data.frame(HR)
  colnames(HR) <- c("coef", "IQRunit", "HR_IQR", "HR_lci", "HR_uci")
  rownames(HR) <- paste0("level", levels(dt[,as.factor(entry_age_over85)]))
  setDT(HR, keep.rownames = T)
  print(HR)
  fwrite(HR, paste0(dir_out, "cox_ReAd_EMM_entry_age_over85_", pollutants_i, "_HR.csv"))
  cat("save HR for cox ReAd EMM entry_age_over85_", pollutants_i, "\n")
}

## EMM by race_collapsed ----
pollutants <- c("pm25", "no2", "ozone_summer", "ox")

for (pollutants_i in pollutants) {
  cat("fit coxph model EMM race", pollutants_i, "\n")
  cox <- coxph(Surv(time = followupyr_start, time2 = followupyr_end, event = ReAd) ~ 
                 get(pollutants_i)*as.factor(race_collapsed) + 
                 mean_bmi + smoke_rate + 
                 hispanic + pct_blk + medhouseholdincome + medianhousevalue + poverty + education + popdensity + pct_owner_occ +
                 summer_tmmx + winter_tmmx + summer_rmax + winter_rmax +
                 as.factor(year) +  as.factor(region) +
                 strata(as.factor(entry_age_break), as.factor(sex), as.factor(race_collapsed), as.factor(dual)) + cluster(qid),
               weights = get(paste0("deadipw_",pollutants_i)),
               data = dt,
               tie = c("efron"),
               na.action = na.omit)
  tb <- summary(cox)$coefficients
  tb <- as.data.frame(tb)
  setDT(tb, keep.rownames = TRUE)
  fwrite(tb, paste0(dir_out, "cox_ReAd_EMM_race_collapsed_", pollutants_i, ".csv"))
  cov_matrix <- vcov(cox)
  cat("output coefs...\n")
  HR_0 <- c(tb[1,coef], IQRs[, get(pollutants_i)], exp(tb[1,coef]*IQRs[, get(pollutants_i)]), exp((tb[1,coef]-tb[1,`robust se`])*IQRs[, get(pollutants_i)]), exp((tb[1,coef]+tb[1,`robust se`])*IQRs[, get(pollutants_i)]))
  
  se_1 <- sqrt(cov_matrix[1,1] + cov_matrix[dim(cov_matrix)[1],dim(cov_matrix)[1]] + 2*cov_matrix[1, dim(cov_matrix)[1]])
  HR_1 <- c(tb[dim(cov_matrix)[1],coef], IQRs[, get(pollutants_i)], exp(tb[dim(cov_matrix)[1],coef]*IQRs[, get(pollutants_i)]), exp((tb[dim(cov_matrix)[1],coef]-se_1)*IQRs[, get(pollutants_i)]),  exp((tb[dim(cov_matrix)[1],coef]+se_1)*IQRs[, get(pollutants_i)]))
  se_2 <- sqrt(cov_matrix[1,1] + cov_matrix[dim(cov_matrix)[1]-1, dim(cov_matrix)[1]-1] + 2*cov_matrix[1, dim(cov_matrix)[1]-1])
  HR_2 <- c(tb[dim(cov_matrix)[1]-1,coef], IQRs[, get(pollutants_i)], exp(tb[dim(cov_matrix)[1]-1,coef]*IQRs[, get(pollutants_i)]), exp((tb[dim(cov_matrix)[1]-1,coef]-se_2)*IQRs[, get(pollutants_i)]),  exp((tb[dim(cov_matrix)[1]-1,coef]+se_2)*IQRs[, get(pollutants_i)]))
  se_3 <- sqrt(cov_matrix[1,1] + cov_matrix[dim(cov_matrix)[1]-2,dim(cov_matrix)[1]] + 2*cov_matrix[1, dim(cov_matrix)[1]-2])
  HR_3 <- c(tb[dim(cov_matrix)[1]-2,coef], IQRs[, get(pollutants_i)], exp(tb[dim(cov_matrix)[1]-2,coef]*IQRs[, get(pollutants_i)]), exp((tb[dim(cov_matrix)[1]-2,coef]-se_3)*IQRs[, get(pollutants_i)]),  exp((tb[dim(cov_matrix)[1]-2,coef]+se_1)*IQRs[, get(pollutants_i)]))
  
  HR <- rbind(HR_0, HR_3, HR_2, HR_1)
  print(HR)
  HR <- as.data.frame(HR)
  colnames(HR) <- c("coef", "IQRunit", "HR_IQR", "HR_lci", "HR_uci")
  rownames(HR) <- paste0("level", levels(dt[,as.factor(race_collapsed)]))
  setDT(HR, keep.rownames = T)
  print(HR)
  fwrite(HR, paste0(dir_out, "cox_ReAd_EMM_race_collapsed_", pollutants_i, "_HR.csv"))
  cat("save HR for cox ReAd EMM race_collapsed_", pollutants_i, "\n")
}