## Project: airPollution_ADRD  
## Code: Cox PH model with effect modification
## Input: "ADRDcohort_dead.fst"                                                   
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

dir_out <- paste0(getwd(),"/results/main_analysis/EMM/coxph_mortality/")

## load data ----
dt <- read_fst(paste0(dir_data, "ADRDcohort_dead.fst"), as.data.table = T)
names(dt)
# [1] "qid"                "zip"                "year"               "sex"                "race"              
# [6] "age"                "dual"               "statecode"          "dead"               "mean_bmi"          
# [11] "smoke_rate"         "hispanic"           "pct_blk"            "medhouseholdincome" "medianhousevalue"  
# [16] "poverty"            "education"          "popdensity"         "pct_owner_occ"      "summer_tmmx"       
# [21] "winter_tmmx"        "summer_rmax"        "winter_rmax"        "firstADRDyr"        "pm25"              
# [26] "no2"                "ozone"              "ozone_summer"       "ox"                 "entry_age"         
# [31] "entry_age_break"    "race_collapsed"     "region" 

# dt[, dual := as.numeric(dual)]
dt[, followupyr_start := (year - firstADRDyr-1)]
dt[, followupyr_end := (year - firstADRDyr)]

IQRs <- data.table(IQR(dt$pm25), IQR(dt$no2), IQR(dt$ozone), IQR(dt$ox), IQR(dt$ozone_summer))
colnames(IQRs) <- c("pm25", "no2", "ozone", "ox", "ozone_summer")
print(IQRs)


## EMM by sex -----
pollutants <- c("pm25", "no2", "ozone_summer", "ox")

for (pollutants_i in pollutants) {
  cat("fit coxph model EMM sex", pollutants_i, "\n")
  cox <- coxph(Surv(time = followupyr_start, time2 = followupyr_end, event = dead) ~ 
                 get(pollutants_i)*as.factor(sex) + 
                 mean_bmi + smoke_rate + 
                 hispanic + pct_blk + medhouseholdincome + medianhousevalue + poverty + education + popdensity + pct_owner_occ +
                 summer_tmmx + winter_tmmx + summer_rmax + winter_rmax +
                 as.factor(year) +  as.factor(region) +
                 strata(as.factor(entry_age_break), as.factor(sex), as.factor(race_collapsed), as.factor(dual)) + cluster(qid),
               data = dt,
               tie = c("efron"),
               na.action = na.omit)
  tb <- summary(cox)$coefficients
  tb <- as.data.frame(tb)
  setDT(tb, keep.rownames = TRUE)
  fwrite(tb, paste0(dir_out, "cox_mortality_EMM_sex_", pollutants_i, ".csv"))
  # cov_matrix <- vcov(cox)
  cat("output coefs...\n")
  HR_0 <- c(tb[1,coef], IQRs[, get(pollutants_i)], exp(tb[1,coef]*IQRs[, get(pollutants_i)]))
  # se <- sqrt(cov_matrix[1,1] + cov_matrix[dim(cov_matrix)[1],dim(cov_matrix)[1]] + 2*cov_matrix[1, dim(cov_matrix)[1]])
  HR_1 <- c(tb[1,coef] + tb[dim(tb)[1],coef], IQRs[, get(pollutants_i)], exp((tb[1,coef] + tb[dim(tb)[1],coef])*IQRs[, get(pollutants_i)]))
  HR <- rbind(HR_0, HR_1)
  print(HR)
  HR <- as.data.frame(HR)
  colnames(HR) <- c("coef", "IQRunit", "HR_IQR")
  rownames(HR) <- paste0("level", levels(dt[,as.factor(sex)]))
  setDT(HR, keep.rownames = T)
  print(HR)
  fwrite(HR, paste0(dir_out, "cox_mortality_EMM_sex_", pollutants_i, "_HR.csv"))
  cat("save HR for cox mortality EMM sex", pollutants_i, "\n")
}

## EMM by dual ----
pollutants <- c("pm25", "no2", "ozone_summer", "ox")

for (pollutants_i in pollutants) {
  cat("fit coxph model EMM dual", pollutants_i, "\n")
  cox <- coxph(Surv(time = followupyr_start, time2 = followupyr_end, event = dead) ~ 
                 get(pollutants_i)*as.factor(dual) + 
                 mean_bmi + smoke_rate + 
                 hispanic + pct_blk + medhouseholdincome + medianhousevalue + poverty + education + popdensity + pct_owner_occ +
                 summer_tmmx + winter_tmmx + summer_rmax + winter_rmax +
                 as.factor(year) +  as.factor(region) +
                 strata(as.factor(entry_age_break), as.factor(sex), as.factor(race_collapsed), as.factor(dual)) + cluster(qid),
               data = dt,
               tie = c("efron"),
               na.action = na.omit)
  tb <- summary(cox)$coefficients
  tb <- as.data.frame(tb)
  setDT(tb, keep.rownames = TRUE)
  fwrite(tb, paste0(dir_out, "cox_mortality_EMM_dual_", pollutants_i, ".csv"))
  # cov_matrix <- vcov(cox)
  cat("output coefs...\n")
  HR_0 <- c(tb[1,coef], IQRs[, get(pollutants_i)], exp(tb[1,coef]*IQRs[, get(pollutants_i)]))
  # se <- sqrt(cov_matrix[1,1] + cov_matrix[dim(cov_matrix)[1],dim(cov_matrix)[1]] + 2*cov_matrix[1, dim(cov_matrix)[1]])
  HR_1 <- c(tb[1,coef] + tb[dim(tb)[1],coef], IQRs[, get(pollutants_i)], exp((tb[1,coef] + tb[dim(tb)[1],coef])*IQRs[, get(pollutants_i)]))
  HR <- rbind(HR_0, HR_1)
  print(HR)
  HR <- as.data.frame(HR)
  colnames(HR) <- c("coef", "IQRunit", "HR_IQR")
  rownames(HR) <- paste0("level", levels(dt[,as.factor(dual)]))
  setDT(HR, keep.rownames = T)
  print(HR)
  fwrite(HR, paste0(dir_out, "cox_mortality_EMM_dual_", pollutants_i, "_HR.csv"))
  cat("save HR for cox mortality EMM dual", pollutants_i, "\n")
}

## EMM by population density (above median)----
median_popdensity <- median(dt[, popdensity])
dt[, above_median_popdensity := popdensity>median_popdensity]

pollutants <- c("pm25", "no2", "ozone_summer", "ox")

for (pollutants_i in pollutants) {
  cat("fit coxph model EMM above median popdensity", pollutants_i, "\n")
  cox <- coxph(Surv(time = followupyr_start, time2 = followupyr_end, event = dead) ~ 
                 get(pollutants_i)*as.factor(above_median_popdensity) + 
                 mean_bmi + smoke_rate + 
                 hispanic + pct_blk + medhouseholdincome + medianhousevalue + poverty + education + popdensity + pct_owner_occ +
                 summer_tmmx + winter_tmmx + summer_rmax + winter_rmax +
                 as.factor(year) +  as.factor(region) +
                 strata(as.factor(entry_age_break), as.factor(sex), as.factor(race_collapsed), as.factor(dual)) + cluster(qid),
               data = dt,
               tie = c("efron"),
               na.action = na.omit)
  tb <- summary(cox)$coefficients
  tb <- as.data.frame(tb)
  setDT(tb, keep.rownames = TRUE)
  fwrite(tb, paste0(dir_out, "cox_mortality_EMM_above_median_popdensity_", pollutants_i, ".csv"))
  # cov_matrix <- vcov(cox)
  cat("output coefs...\n")
  HR_0 <- c(tb[1,coef], IQRs[, get(pollutants_i)], exp(tb[1,coef]*IQRs[, get(pollutants_i)]))
  # se <- sqrt(cov_matrix[1,1] + cov_matrix[dim(cov_matrix)[1],dim(cov_matrix)[1]] + 2*cov_matrix[1, dim(cov_matrix)[1]])
  HR_1 <- c(tb[1,coef] + tb[dim(tb)[1],coef], IQRs[, get(pollutants_i)], exp((tb[1,coef] + tb[dim(tb)[1],coef])*IQRs[, get(pollutants_i)]))
  HR <- rbind(HR_0, HR_1)
  print(HR)
  HR <- as.data.frame(HR)
  colnames(HR) <- c("coef", "IQRunit", "HR_IQR")
  rownames(HR) <- paste0("level", levels(dt[,as.factor(above_median_popdensity)]))
  setDT(HR, keep.rownames = T)
  print(HR)
  fwrite(HR, paste0(dir_out, "cox_mortality_EMM_above_median_popdensity_", pollutants_i, "_HR.csv"))
  cat("save HR for cox mortality EMM above median popdensity", pollutants_i, "\n")
}

## EMM by entry age ----
dt[, entry_age_over85 := entry_age>85]
pollutants <- c("pm25", "no2", "ozone_summer", "ox")

for (pollutants_i in pollutants) {
  cat("fit coxph model EMM entry_age_over85", pollutants_i, "\n")
  cox <- coxph(Surv(time = followupyr_start, time2 = followupyr_end, event = dead) ~ 
                 get(pollutants_i)*as.factor(entry_age_over85) + 
                 mean_bmi + smoke_rate + 
                 hispanic + pct_blk + medhouseholdincome + medianhousevalue + poverty + education + popdensity + pct_owner_occ +
                 summer_tmmx + winter_tmmx + summer_rmax + winter_rmax +
                 as.factor(year) +  as.factor(region) +
                 strata(as.factor(entry_age_break), as.factor(sex), as.factor(race_collapsed), as.factor(dual)) + cluster(qid),
               data = dt,
               tie = c("efron"),
               na.action = na.omit)
  tb <- summary(cox)$coefficients
  tb <- as.data.frame(tb)
  setDT(tb, keep.rownames = TRUE)
  fwrite(tb, paste0(dir_out, "cox_mortality_EMM_entry_age_over85_", pollutants_i, ".csv"))
  # cov_matrix <- vcov(cox)
  cat("output coefs...\n")
  HR_0 <- c(tb[1,coef], IQRs[, get(pollutants_i)], exp(tb[1,coef]*IQRs[, get(pollutants_i)]))
  # se <- sqrt(cov_matrix[1,1] + cov_matrix[dim(cov_matrix)[1],dim(cov_matrix)[1]] + 2*cov_matrix[1, dim(cov_matrix)[1]])
  HR_1 <- c(tb[1,coef] + tb[dim(tb)[1],coef], IQRs[, get(pollutants_i)], exp((tb[1,coef] + tb[dim(tb)[1],coef])*IQRs[, get(pollutants_i)]))
  HR <- rbind(HR_0, HR_1)
  print(HR)
  HR <- as.data.frame(HR)
  colnames(HR) <- c("coef", "IQRunit", "HR_IQR")
  rownames(HR) <- paste0("level", levels(dt[,as.factor(entry_age_over85)]))
  setDT(HR, keep.rownames = T)
  print(HR)
  fwrite(HR, paste0(dir_out, "cox_mortality_EMM_entry_age_over85_", pollutants_i, "_HR.csv"))
  cat("save HR for cox mortality EMM entry_age_over85_", pollutants_i, "\n")
}

## EMM by race_collapsed ----
pollutants <- c("pm25", "no2", "ozone_summer", "ox")

for (pollutants_i in pollutants) {
  cat("fit coxph model EMM race", pollutants_i, "\n")
  cox <- coxph(Surv(time = followupyr_start, time2 = followupyr_end, event = dead) ~ 
                 get(pollutants_i)*as.factor(race_collapsed) + 
                 mean_bmi + smoke_rate + 
                 hispanic + pct_blk + medhouseholdincome + medianhousevalue + poverty + education + popdensity + pct_owner_occ +
                 summer_tmmx + winter_tmmx + summer_rmax + winter_rmax +
                 as.factor(year) +  as.factor(region) +
                 strata(as.factor(entry_age_break), as.factor(sex), as.factor(race_collapsed), as.factor(dual)) + cluster(qid),
               data = dt,
               tie = c("efron"),
               na.action = na.omit)
  tb <- summary(cox)$coefficients
  tb <- as.data.frame(tb)
  setDT(tb, keep.rownames = TRUE)
  fwrite(tb, paste0(dir_out, "cox_mortality_EMM_race_collapsed_", pollutants_i, ".csv"))
  # cov_matrix <- vcov(cox)
  cat("output coefs...\n")
  HR_0 <- c(tb[1,coef], IQRs[, get(pollutants_i)], exp(tb[1,coef]*IQRs[, get(pollutants_i)]))
  
  # se_1 <- sqrt(cov_matrix[1,1] + cov_matrix[dim(cov_matrix)[1],dim(cov_matrix)[1]] + 2*cov_matrix[1, dim(cov_matrix)[1]])
  HR_1 <- c(tb[1,coef] + tb[dim(tb)[1],coef], IQRs[, get(pollutants_i)], exp((tb[1,coef] + tb[dim(tb)[1],coef])*IQRs[, get(pollutants_i)]))
  # se_2 <- sqrt(cov_matrix[1,1] + cov_matrix[dim(cov_matrix)[1]-1, dim(cov_matrix)[1]-1] + 2*cov_matrix[1, dim(cov_matrix)[1]-1])
  HR_2 <- c(tb[1,coef] + tb[dim(tb)[1]-1,coef], IQRs[, get(pollutants_i)], exp((tb[1,coef] + tb[dim(tb)[1]-1,coef])*IQRs[, get(pollutants_i)]))
  # se_3 <- sqrt(cov_matrix[1,1] + cov_matrix[dim(cov_matrix)[1]-2,dim(cov_matrix)[1]] + 2*cov_matrix[1, dim(cov_matrix)[1]-2])
  HR_3 <- c(tb[1,coef] + tb[dim(tb)[1]-2,coef], IQRs[, get(pollutants_i)], exp((tb[1,coef] + tb[tb(tb)[1]-2,coef])*IQRs[, get(pollutants_i)]))
  
  HR <- rbind(HR_0, HR_3, HR_2, HR_1)
  print(HR)
  HR <- as.data.frame(HR)
  colnames(HR) <- c("coef", "IQRunit", "HR_IQR")
  rownames(HR) <- paste0("level", levels(dt[,as.factor(race_collapsed)]))
  setDT(HR, keep.rownames = T)
  print(HR)
  fwrite(HR, paste0(dir_out, "cox_mortality_EMM_race_collapsed_", pollutants_i, "_HR.csv"))
  cat("save HR for cox mortality EMM race_collapsed_", pollutants_i, "\n")
}

## revised added % low income ----
median_poverty <- median(dt[, poverty])
dt[, above_median_poverty := poverty > median_poverty]

pollutants <- c("pm25", "no2", "ozone_summer", "ox")
for (pollutants_i in pollutants) {
  cat("fit coxph model EMM above median poverty", pollutants_i, "\n")
  cox <- coxph(Surv(time = followupyr_start, time2 = followupyr_end, event = dead) ~ 
                 get(pollutants_i)*as.factor(above_median_poverty) + 
                 mean_bmi + smoke_rate + 
                 hispanic + pct_blk + medhouseholdincome + medianhousevalue + poverty + education + popdensity + pct_owner_occ +
                 summer_tmmx + winter_tmmx + summer_rmax + winter_rmax +
                 as.factor(year) +  as.factor(region) +
                 strata(as.factor(entry_age_break), as.factor(sex), as.factor(race_collapsed), as.factor(dual)) + cluster(qid),
               data = dt,
               tie = c("efron"),
               na.action = na.omit)
  tb <- summary(cox)$coefficients
  tb <- as.data.frame(tb)
  setDT(tb, keep.rownames = TRUE)
  fwrite(tb, paste0(dir_out, "cox_mortality_EMM_above_median_poverty_", pollutants_i, ".csv"))
  # cov_matrix <- vcov(cox)
  cat("output coefs...\n")
  HR_0 <- c(tb[1,coef], IQRs[, get(pollutants_i)], exp(tb[1,coef]*IQRs[, get(pollutants_i)]))
  # se <- sqrt(cov_matrix[1,1] + cov_matrix[dim(cov_matrix)[1],dim(cov_matrix)[1]] + 2*cov_matrix[1, dim(cov_matrix)[1]])
  HR_1 <- c(tb[1,coef] + tb[dim(tb)[1],coef], IQRs[, get(pollutants_i)], exp((tb[1,coef] + tb[dim(tb)[1],coef])*IQRs[, get(pollutants_i)]))
  HR <- rbind(HR_0, HR_1)
  print(HR)
  HR <- as.data.frame(HR)
  colnames(HR) <- c("coef", "IQRunit", "HR_IQR")
  rownames(HR) <- paste0("level", levels(dt[,as.factor(above_median_poverty)]))
  setDT(HR, keep.rownames = T)
  print(HR)
  fwrite(HR, paste0(dir_out, "cox_mortality_EMM_above_median_poverty_", pollutants_i, "_HR.csv"))
  cat("save HR for cox mortality EMM above median poverty", pollutants_i, "\n")
}

## revised EMM by race_collapsed dual == 1----
pollutants <- c("pm25", "no2", "ozone_summer", "ox")
dt_dual1 <- dt[dual==1,]

for (pollutants_i in pollutants) {
  cat("fit coxph model EMM race", pollutants_i, "\n")
  cox <- coxph(Surv(time = followupyr_start, time2 = followupyr_end, event = dead) ~ 
                 get(pollutants_i)*as.factor(race_collapsed) + 
                 mean_bmi + smoke_rate + 
                 hispanic + pct_blk + medhouseholdincome + medianhousevalue + poverty + education + popdensity + pct_owner_occ +
                 summer_tmmx + winter_tmmx + summer_rmax + winter_rmax +
                 as.factor(year) +  as.factor(region) +
                 strata(as.factor(entry_age_break), as.factor(sex), as.factor(race_collapsed), as.factor(dual)) + cluster(qid),
               data = dt_dual1,
               tie = c("efron"),
               na.action = na.omit)
  tb <- summary(cox)$coefficients
  tb <- as.data.frame(tb)
  setDT(tb, keep.rownames = TRUE)
  fwrite(tb, paste0(dir_out, "cox_mortality_EMM_race_collapsed_dual1", pollutants_i, ".csv"))
  # cov_matrix <- vcov(cox)
  cat("output coefs...\n")
  HR_0 <- c(tb[1,coef], IQRs[, get(pollutants_i)], exp(tb[1,coef]*IQRs[, get(pollutants_i)]))
  
  # se_1 <- sqrt(cov_matrix[1,1] + cov_matrix[dim(cov_matrix)[1],dim(cov_matrix)[1]] + 2*cov_matrix[1, dim(cov_matrix)[1]])
  HR_1 <- c(tb[1,coef] + tb[dim(tb)[1],coef], IQRs[, get(pollutants_i)], exp((tb[1,coef] + tb[dim(tb)[1],coef])*IQRs[, get(pollutants_i)]))
  # se_2 <- sqrt(cov_matrix[1,1] + cov_matrix[dim(cov_matrix)[1]-1, dim(cov_matrix)[1]-1] + 2*cov_matrix[1, dim(cov_matrix)[1]-1])
  HR_2 <- c(tb[1,coef] + tb[dim(tb)[1]-1,coef], IQRs[, get(pollutants_i)], exp((tb[1,coef] + tb[dim(tb)[1]-1,coef])*IQRs[, get(pollutants_i)]))
  # se_3 <- sqrt(cov_matrix[1,1] + cov_matrix[dim(cov_matrix)[1]-2,dim(cov_matrix)[1]] + 2*cov_matrix[1, dim(cov_matrix)[1]-2])
  HR_3 <- c(tb[1,coef] + tb[dim(tb)[1]-2,coef], IQRs[, get(pollutants_i)], exp((tb[1,coef] + tb[dim(tb)[1]-2,coef])*IQRs[, get(pollutants_i)]))
  
  HR <- rbind(HR_0, HR_3, HR_2, HR_1)
  print(HR)
  HR <- as.data.frame(HR)
  colnames(HR) <- c("coef", "IQRunit", "HR_IQR")
  rownames(HR) <- paste0("level", levels(dt_dual1[,as.factor(race_collapsed)]))
  setDT(HR, keep.rownames = T)
  print(HR)
  fwrite(HR, paste0(dir_out, "cox_mortality_EMM_race_collapsed_dual1", pollutants_i, "_HR.csv"))
  cat("save HR for cox mortality EMM race_collapsed_dual1", pollutants_i, "\n")
}

## revised EMM by race_collapsed dual == 0----
pollutants <- c("pm25", "no2", "ozone_summer", "ox")
dt_dual0 <- dt[dual==0,]

for (pollutants_i in pollutants) {
  cat("fit coxph model EMM race", pollutants_i, "\n")
  cox <- coxph(Surv(time = followupyr_start, time2 = followupyr_end, event = dead) ~ 
                 get(pollutants_i)*as.factor(race_collapsed) + 
                 mean_bmi + smoke_rate + 
                 hispanic + pct_blk + medhouseholdincome + medianhousevalue + poverty + education + popdensity + pct_owner_occ +
                 summer_tmmx + winter_tmmx + summer_rmax + winter_rmax +
                 as.factor(year) +  as.factor(region) +
                 strata(as.factor(entry_age_break), as.factor(sex), as.factor(race_collapsed), as.factor(dual)) + cluster(qid),
               data = dt_dual0,
               tie = c("efron"),
               na.action = na.omit)
  tb <- summary(cox)$coefficients
  tb <- as.data.frame(tb)
  setDT(tb, keep.rownames = TRUE)
  fwrite(tb, paste0(dir_out, "cox_mortality_EMM_race_collapsed_dual0", pollutants_i, ".csv"))
  # cov_matrix <- vcov(cox)
  cat("output coefs...\n")
  HR_0 <- c(tb[1,coef], IQRs[, get(pollutants_i)], exp(tb[1,coef]*IQRs[, get(pollutants_i)]))
  
  # se_1 <- sqrt(cov_matrix[1,1] + cov_matrix[dim(cov_matrix)[1],dim(cov_matrix)[1]] + 2*cov_matrix[1, dim(cov_matrix)[1]])
  HR_1 <- c(tb[1,coef] + tb[dim(tb)[1],coef], IQRs[, get(pollutants_i)], exp((tb[1,coef] + tb[dim(tb)[1],coef])*IQRs[, get(pollutants_i)]))
  # se_2 <- sqrt(cov_matrix[1,1] + cov_matrix[dim(cov_matrix)[1]-1, dim(cov_matrix)[1]-1] + 2*cov_matrix[1, dim(cov_matrix)[1]-1])
  HR_2 <- c(tb[1,coef] + tb[dim(tb)[1]-1,coef], IQRs[, get(pollutants_i)], exp((tb[1,coef] + tb[dim(tb)[1]-1,coef])*IQRs[, get(pollutants_i)]))
  # se_3 <- sqrt(cov_matrix[1,1] + cov_matrix[dim(cov_matrix)[1]-2,dim(cov_matrix)[1]] + 2*cov_matrix[1, dim(cov_matrix)[1]-2])
  HR_3 <- c(tb[1,coef] + tb[dim(tb)[1]-2,coef], IQRs[, get(pollutants_i)], exp((tb[1,coef] + tb[dim(tb)[1]-2,coef])*IQRs[, get(pollutants_i)]))
  
  HR <- rbind(HR_0, HR_3, HR_2, HR_1)
  print(HR)
  HR <- as.data.frame(HR)
  colnames(HR) <- c("coef", "IQRunit", "HR_IQR")
  rownames(HR) <- paste0("level", levels(dt_dual0[,as.factor(race_collapsed)]))
  setDT(HR, keep.rownames = T)
  print(HR)
  fwrite(HR, paste0(dir_out, "cox_mortality_EMM_race_collapsed_dual0", pollutants_i, "_HR.csv"))
  cat("save HR for cox mortality EMM race_collapsed_dual0", pollutants_i, "\n")
}
