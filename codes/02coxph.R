###############################################################################
# Project: Air Pollution and mortality / readmission in AD/ADRD Medicare      #
# Code: Cox PH model
# Input: "ADRD_mortality.csv"                                                  
# Output: 
# Author: Shuxin Dong                                                         #
# Date: 2021-02-17                                                            #
###############################################################################

############################# 0. Setup ########################################
rm(list = ls())
gc()

library(data.table)
setDTthreads(threads = 0)

setwd("/nfs/home/S/shd968/shared_space/ci3_shd968/dementia/")

dir_data <- "/nfs/home/S/shd968/shared_space/ci3_shd968/dementia/"
dir_results <- "/nfs/home/S/shd968/shared_space/ci3_shd968/dementia/"

dt <- fread(paste0(dir_data, "ADRD_mortality.csv"))
names(dt)
# > names(dt)
# [1] "zip"                "year"               "qid"                "summer_tmmx"        "winter_tmmx"        "summer_rmax"       
# [7] "winter_rmax"        "dead"               "sex"                "race"               "age"                "dual"              
# [13] "statecode"          "entry_age_break"    "mean_bmi"           "smoke_rate"         "hispanic"           "pct_blk"           
# [19] "medhouseholdincome" "medianhousevalue"   "poverty"            "education"          "popdensity"         "pct_owner_occ"     
# [25] "firstADRDyr"        "pm25"               "no2"                "ozone"  

dt[, followupyr := (year - firstADRDyr)][]
dt[, followupyr_plusone := followupyr +1][]
head(dt)

############################# 1. coxph model ##################################
library(survival)
## Wu's national causal paper Cox model
# Cox_raw <- coxph(Surv(followup_year, followup_year_plus_one,dead) ~ pm25_ensemble +
#                    mean_bmi + smoke_rate + hispanic + pct_blk +
#                    medhouseholdincome + medianhousevalue +
#                    poverty + education + popdensity + pct_owner_occ +
#                    summer_tmmx + winter_tmmx + summer_rmax + winter_rmax +
#                    as.factor(year) + as.factor(region) +
#                    strata(as.factor(entry_age_break)) + strata(as.factor(sex)) + strata(as.factor(race)) + strata(as.factor(dual)),
#                  data = national_merged2016,
#                  ties = c("efron"),
#                  na.action = na.omit)
cox_raw <- coxph(Surv(time = followupyr, time2 = followupyr_plusone, event = dead) ~ 
                   pm25 + no2 + ozone + 
                   mean_bmi + smoke_rate + hispanic + pct_blk + 
                   medhouseholdincome + medianhousevalue +  
                   poverty + education + popdensity + pct_owner_occ +
                   summer_tmmx + winter_tmmx + summer_rmax + winter_rmax +
                   as.factor(year) +  
                   strata(as.factor(entry_age_break)) + strata(as.factor(sex)) + 
                   strata(as.factor(race)) + strata(as.factor(dual)),
                 data = dt,
                 tie = c("efron"), na.action = na.omit)
saveRDS(cox_raw, paste0(dir_results, "cox_raw.rds"))
cox_raw <- summary(cox_raw)
print(cox_raw)

cox_all <- coxph(Surv(time = followupyr, time2 = followupyr_plusone, event = dead) ~ 
                   pm25 + no2 + ozone + 
                   age + I(age^2) +
                   mean_bmi + smoke_rate + hispanic + pct_blk + 
                   log(medhouseholdincome) + log(medianhousevalue) +  
                   poverty + education + popdensity + pct_owner_occ +
                   summer_tmmx + winter_tmmx + summer_rmax + winter_rmax +
                   as.factor(year) +  
                   strata(as.factor(entry_age_break)) + strata(as.factor(sex)) + 
                   strata(as.factor(race)) + strata(as.factor(dual)),
                 data = dt,
                 tie = c("efron"), na.action = na.omit)
saveRDS(cox_all, paste0(dir_results, "cox_raw.rds"))
cox_all <- summary(cox_all)
print(cox_all)

cox_pm25 <- coxph(Surv(time = followupyr, time2 = followupyr_plusone, event = dead) ~ 
                   pm25 + 
                   # no2 + ozone + 
                   mean_bmi + smoke_rate + hispanic + pct_blk + 
                   medhouseholdincome + medianhousevalue +  
                   poverty + education + popdensity + pct_owner_occ +
                   summer_tmmx + winter_tmmx + summer_rmax + winter_rmax +
                   as.factor(year) +  
                   strata(as.factor(entry_age_break)) + strata(as.factor(sex)) + 
                   strata(as.factor(race)) + strata(as.factor(dual)),
                 data = dt,
                 tie = c("efron"), na.action = na.omit)
saveRDS(cox_pm25, paste0(dir_results, "cox_pm25.rds"))
cox_pm25 <- summary(cox_pm25)
print(cox_pm25)




# temp <- summary(cox_all)
# write.csv(temp$coefficients, paste0(dir_output, "cox_all.csv"))
# 
# cox_pm25 <- coxph(Surv(time = followupyr, time2 = followupyr_plusone, event = dead_peryear) ~ 
#                    pm25 + 
#                    mean_bmi + smoke_rate + hispanic + pct_blk + 
#                    medhouseholdincome + medianhousevalue + poverty + 
#                    education + popdensity + pct_owner_occ +
#                    as.factor(year_admit) +  
#                    strata(as.factor(age_gp)) + strata(as.factor(Sex_gp)) + 
#                    strata(as.factor(race)) + strata(as.factor(Dual_gp)),
#                  data = dt,
#                  tie = c("efron"), na.action = na.omit)
# temp <- summary(cox_pm25)
# write.csv(temp$coefficients, paste0(dir_output, "cox_pm25.csv"))
#  
# cox_no2 <- coxph(Surv(time = followupyr, time2 = followupyr_plusone, event = dead_peryear) ~ 
#                     no2 + 
#                     mean_bmi + smoke_rate + hispanic + pct_blk + 
#                     medhouseholdincome + medianhousevalue + poverty + 
#                     education + popdensity + pct_owner_occ +
#                     as.factor(year_admit) +  
#                     strata(as.factor(age_gp)) + strata(as.factor(Sex_gp)) + 
#                     strata(as.factor(race)) + strata(as.factor(Dual_gp)),
#                   data = dt,
#                   tie = c("efron"), na.action = na.omit)
# temp <- summary(cox_no2)
# write.csv(temp$coefficients, paste0(dir_output, "cox_no2.csv"))
# 
# cox_ozone <- coxph(Surv(time = followupyr, time2 = followupyr_plusone, event = dead_peryear) ~ 
#                    ozone + 
#                    mean_bmi + smoke_rate + hispanic + pct_blk + 
#                    medhouseholdincome + medianhousevalue + poverty + 
#                    education + popdensity + pct_owner_occ +
#                    as.factor(year_admit) +  
#                    strata(as.factor(age_gp)) + strata(as.factor(Sex_gp)) + 
#                    strata(as.factor(race)) + strata(as.factor(Dual_gp)),
#                  data = dt,
#                  tie = c("efron"), na.action = na.omit)
# temp <- summary(cox_ozone)
# write.csv(temp$coefficients, paste0(dir_output, "cox_ozone.csv"))

############################# 2. equivalent Poisson ###########################
## Create aggregated data for Poisson regression
# dt$time_count <- dt$followupyr_plusone - dt$followupyr
# dead_personyear <- dt[, list(dead = sum(dead_peryear), time_count = sum(time_count)), 
#                       by = .(zip, year, sex, race, dual, entry_age_break, followupyr)]
# covariates <- dt[, list(pm25 = min(pm25), no2 = min(no2), ozone = min(ozone),
#                         mean_bmi = min(mean_bmi), smoke_rate = min(smoke_rate), 
#                         hispanic = min(hispanic), pct_blk = min(pct_blk), 
#                         medhouseholdincome = min(medhouseholdincome), 
#                         medianhousevalue = min(medianhousevalue),
#                         poverty = min(poverty), education = min(education), 
#                         popdensity = min(popdensity), 
#                         pct_owner_occ = min(pct_owner_occ)),
#                  by = .(zip, year)]
# aggregate_dt <- merge(dead_personyear, covariates, 
#                       by=c("zip","year"), all.x =T)

## fit models
# library(gnm)
# gnm_all <- gnm(dead ~ pm25 + no2 + ozone +
#                  mean_bmi + smoke_rate + hispanic + pct_blk + 
#                  medhouseholdincome + medianhousevalue + poverty + education + 
#                  popdensity + pct_owner_occ +
#                  as.factor(year) + offset(log(time_count)), 
#                eliminate = (as.factor(sex):as.factor(race):as.factor(dual):as.factor(entry_age_break):as.factor(followup_year)),
#                data = aggregate_dt,
#                family = poisson(link="log"))
# temp <- summary(gnm_all)
