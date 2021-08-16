###############################################################################
# Project: Air Pollution and mortality / readmission in AD/ADRD Medicare      
# Code: Cox PH model with effect modification
# Input: "ADRD_mortality.csv"                                                  
# Output: 
# Author: Shuxin Dong                                                         
# Date: 2021-02-17                                                            
###############################################################################

## 0. setup -------------------------------------------------------------------
rm(list = ls())
gc()

library(data.table)
setDTthreads(threads = 0)
library(survival)

setwd("/nfs/home/S/shd968/shared_space/ci3_shd968/dementia/")

dir_data <- "/nfs/home/S/shd968/shared_space/ci3_shd968/dementia/data/"
dir_results <- "/nfs/home/S/shd968/shared_space/ci3_shd968/dementia/"

dt <- fread(paste0(dir_data, "ADRD_for_mortality.csv"), colClasses = c("zip"="character"))
names(dt)
#  [1] "qid"                "zip"                "year"               "sex"               
#  [5] "race"               "age"                "dual"               "statecode"         
#  [9] "dead"               "mean_bmi"           "smoke_rate"         "hispanic"          
# [13] "pct_blk"            "medhouseholdincome" "medianhousevalue"   "poverty"           
# [17] "education"          "popdensity"         "pct_owner_occ"      "summer_tmmx"       
# [21] "winter_tmmx"        "summer_rmax"        "winter_rmax"        "firstADRDyr"       
# [25] "pm25"               "no2"                "ozone"              "ozone_summer"      
# [29] "entry_age"          "entry_age_break"    "race_collapsed"     "ox"    

dt[, followupyr := (year - firstADRDyr)]
dt[, followupyr_plusone := followupyr +1]
dt[, statecode := as.factor(statecode)]

NORTHEAST <- c("NY", "MA", "PA", "RI", "NH", "ME", "VT", "CT", "NJ")  
SOUTH <- c("DC", "VA", "NC", "WV", "KY", "SC", "GA", "FL", "AL", "TN", "MS", 
           "AR", "MD", "DE", "OK", "TX", "LA")
MIDWEST <- c("OH", "IN", "MI", "IA", "MO", "WI", "MN", "SD", "ND", "IL", "KS", "NE")
WEST <- c("MT", "CO", "WY", "ID", "UT", "NV", "CA", "OR", "WA", "AZ", "NM")

dt$region <- ifelse(dt$statecode %in% NORTHEAST, "NORTHEAST",
                    ifelse(dt$statecode %in% SOUTH, "SOUTH",
                           ifelse(dt$statecode  %in% MIDWEST, "MIDWEST",
                                  ifelse(dt$statecode  %in% WEST, "WEST",
                                         NA))))
dt[, region:=as.factor(region)]
head(dt)
gc()

IQRs <- data.table(IQR(dt$pm25), IQR(dt$no2), IQR(dt$ozone), IQR(dt$ox), IQR(dt$ozone_summer))
colnames(IQRs) <- c("pm25", "no2", "ozone", "ox", "ozone_summer")
print(IQRs)

dt[, sex_female := sex-1]
dt[, entry_age_over85 := entry_age>85]
median_popdensity <- median(dt[, popdensity])
dt[, above_median_popdensity := popdensity>median_popdensity]

## 1. effect modification for PM2.5 -------------------------------------------
IQRunit <- IQRs[,pm25]
### sex 0/1 -----
cox_pm25_sex <- coxph(Surv(time = followupyr, time2 = followupyr_plusone, event = dead) ~ 
                        pm25 + I(pm25*sex_female) +
                        mean_bmi + smoke_rate + hispanic + pct_blk + 
                        medhouseholdincome + medianhousevalue +  
                        poverty + education + popdensity + pct_owner_occ +
                        summer_tmmx + winter_tmmx + summer_rmax + winter_rmax +
                        as.factor(year) +  as.factor(region) +
                        strata(as.factor(entry_age_break)) + strata(as.factor(sex)) + 
                        strata(as.factor(race_collapsed)) + strata(as.factor(dual)),
                      data = dt,
                      tie = c("efron"), 
                      na.action = na.omit)
tb <- summary(cox_pm25_sex)$coefficients
cov_matrix <- vcov(cox_pm25_sex)

HR_reference <- c(tb[1,1], IQRunit, exp(tb[1,1]*IQRunit),exp((tb[1,1]-tb[1,3])*IQRunit), exp((tb[1,1]+tb[1,3])*IQRunit))
se <- sqrt(cov_matrix[1,1] + cov_matrix[2,2] + 2*cov_matrix[1,2])
HR_level1 <- c(tb[1,1]+tb[2,1], IQRunit, exp((tb[1,1]+tb[2,1])*IQRunit), exp((tb[1,1]+tb[2,1]-se)*IQRunit), exp((tb[1,1]+tb[2,1]+se)*IQRunit))

HR <- rbind(HR_reference, HR_level1)
print(HR)
HR <- as.data.frame(HR)
colnames(HR) <- c("coef", "IQR", "HR", "HR_lci", "HR_uci")
write.csv(HR, paste0(dir_results, "cox_mortality_pm25_sex_HR.csv"))
tb <- as.data.frame(tb)
setDT(tb, keep.rownames = TRUE)
fwrite(tb, paste0(dir_results, "cox_mortality_pm25_sex_coef.csv"))

### dual eligibility 0/1 ----
cox_pm25_dual <- coxph(Surv(time = followupyr, time2 = followupyr_plusone, event = dead) ~ 
                         pm25 + I(pm25*dual) +
                         mean_bmi + smoke_rate + hispanic + pct_blk + 
                         medhouseholdincome + medianhousevalue +  
                         poverty + education + popdensity + pct_owner_occ +
                         summer_tmmx + winter_tmmx + summer_rmax + winter_rmax +
                         as.factor(year) +  as.factor(region) +
                         strata(as.factor(entry_age_break)) + strata(as.factor(sex)) + 
                         strata(as.factor(race_collapsed)) + strata(as.factor(dual)),
                       data = dt,
                       tie = c("efron"), 
                       na.action = na.omit)
tb <- summary(cox_pm25_dual)$coefficients
cov_matrix <- vcov(cox_pm25_dual)

HR_reference <- c(tb[1,1], IQRunit, exp(tb[1,1]*IQRunit),exp((tb[1,1]-tb[1,3])*IQRunit), exp((tb[1,1]+tb[1,3])*IQRunit))
se <- sqrt(cov_matrix[1,1] + cov_matrix[2,2] + 2*cov_matrix[1,2])
HR_level1 <- c(tb[1,1]+tb[2,1], IQRunit, exp((tb[1,1]+tb[2,1])*IQRunit), exp((tb[1,1]+tb[2,1]-se)*IQRunit), exp((tb[1,1]+tb[2,1]+se)*IQRunit))

HR <- rbind(HR_reference, HR_level1)
print(HR)
HR <- as.data.frame(HR)
colnames(HR) <- c("coef", "IQR", "HR", "HR_lci", "HR_uci")
write.csv(HR, paste0(dir_results, "cox_mortality_pm25_dual_HR.csv"))
tb <- as.data.frame(tb)
setDT(tb, keep.rownames = TRUE)
fwrite(tb, paste0(dir_results, "cox_mortality_pm25_dual_coef.csv"))

### population density T/F----
cox_pm25_popdensity <- coxph(Surv(time = followupyr, time2 = followupyr_plusone, event = dead) ~ 
                               pm25 + I(pm25*above_median_popdensity) +
                               mean_bmi + smoke_rate + hispanic + pct_blk + 
                               medhouseholdincome + medianhousevalue +  
                               poverty + education + popdensity + pct_owner_occ +
                               summer_tmmx + winter_tmmx + summer_rmax + winter_rmax +
                               as.factor(year) +  as.factor(region) +
                               strata(as.factor(entry_age_break)) + strata(as.factor(sex)) + 
                               strata(as.factor(race_collapsed)) + strata(as.factor(dual)),
                             data = dt,
                             tie = c("efron"), 
                             na.action = na.omit)
tb <- summary(cox_pm25_popdensity)$coefficients
cov_matrix <- vcov(cox_pm25_popdensity)

HR_reference <- c(tb[1,1], IQRunit, exp(tb[1,1]*IQRunit),exp((tb[1,1]-tb[1,3])*IQRunit), exp((tb[1,1]+tb[1,3])*IQRunit))
se <- sqrt(cov_matrix[1,1] + cov_matrix[2,2] + 2*cov_matrix[1,2])
HR_level1 <- c(tb[1,1]+tb[2,1], IQRunit, exp((tb[1,1]+tb[2,1])*IQRunit), exp((tb[1,1]+tb[2,1]-se)*IQRunit), exp((tb[1,1]+tb[2,1]+se)*IQRunit))

HR <- rbind(HR_reference, HR_level1)
print(HR)
HR <- as.data.frame(HR)
colnames(HR) <- c("coef", "IQR", "HR", "HR_lci", "HR_uci")
write.csv(HR, paste0(dir_results, "cox_mortality_pm25_popdensity_HR.csv"))
tb <- as.data.frame(tb)
setDT(tb, keep.rownames = TRUE)
fwrite(tb, paste0(dir_results, "cox_mortality_pm25_popdensity_coef.csv"))

### entry age over85 T/F-----
cox_pm25_entryage <- coxph(Surv(time = followupyr, time2 = followupyr_plusone, event = dead) ~ 
                             pm25 + I(pm25*entry_age_over85) +
                             mean_bmi + smoke_rate + hispanic + pct_blk + 
                             medhouseholdincome + medianhousevalue +  
                             poverty + education + popdensity + pct_owner_occ +
                             summer_tmmx + winter_tmmx + summer_rmax + winter_rmax +
                             as.factor(year) +  as.factor(region) +
                             strata(as.factor(entry_age_break)) + strata(as.factor(sex)) + 
                             strata(as.factor(race_collapsed)) + strata(as.factor(dual)),
                           data = dt,
                           tie = c("efron"), 
                           na.action = na.omit)
tb <- summary(cox_pm25_entryage)$coefficients
cov_matrix <- vcov(cox_pm25_entryage)

HR_reference <- c(tb[1,1], IQRunit, exp(tb[1,1]*IQRunit),exp((tb[1,1]-tb[1,3])*IQRunit), exp((tb[1,1]+tb[1,3])*IQRunit))
se <- sqrt(cov_matrix[1,1] + cov_matrix[2,2] + 2*cov_matrix[1,2])
HR_level1 <- c(tb[1,1]+tb[2,1], IQRunit, exp((tb[1,1]+tb[2,1])*IQRunit), exp((tb[1,1]+tb[2,1]-se)*IQRunit), exp((tb[1,1]+tb[2,1]+se)*IQRunit))

HR <- rbind(HR_reference, HR_level1)
print(HR)
HR <- as.data.frame(HR)
colnames(HR) <- c("coef", "IQR", "HR", "HR_lci", "HR_uci")
write.csv(HR, paste0(dir_results, "cox_mortality_pm25_entryage_HR.csv"))
tb <- as.data.frame(tb)
setDT(tb, keep.rownames = TRUE)
fwrite(tb, paste0(dir_results, "cox_mortality_pm25_entryage_coef.csv"))

### race chr 4 levels ----
cox_pm25_race <- coxph(Surv(time = followupyr, time2 = followupyr_plusone, event = dead) ~ 
                         pm25 + pm25*race_collapsed +
                         mean_bmi + smoke_rate + hispanic + pct_blk + 
                         medhouseholdincome + medianhousevalue +  
                         poverty + education + popdensity + pct_owner_occ +
                         summer_tmmx + winter_tmmx + summer_rmax + winter_rmax +
                         as.factor(year) +  as.factor(region) +
                         strata(as.factor(entry_age_break)) + strata(as.factor(sex)) + 
                         strata(as.factor(race_collapsed)) + strata(as.factor(dual)),
                       data = dt,
                       tie = c("efron"), 
                       na.action = na.omit)
tb <- summary(cox_pm25_race)$coefficients
cov_matrix <- vcov(cox_pm25_race)

IQRunit <- IQRs[,pm25]
HR_reference <- c(tb[1,1], IQRunit, exp(tb[1,1]*IQRunit),exp((tb[1,1]-tb[1,3])*IQRunit), exp((tb[1,1]+tb[1,3])*IQRunit))
se1 <- sqrt(cov_matrix[1,1] + cov_matrix[39,39] + 2*cov_matrix[1,39])
se2 <- sqrt(cov_matrix[1,1] + cov_matrix[38,38] + 2*cov_matrix[1,38])
se3 <- sqrt(cov_matrix[1,1] + cov_matrix[37,37] + 2*cov_matrix[1,37])
HR_level1 <- c(tb[1,1]+tb[39,1], IQRunit, exp((tb[1,1]+tb[39,1])*IQRunit), exp((tb[1,1]+tb[39,1]-se1)*IQRunit), exp((tb[1,1]+tb[39,1]+se1)*IQRunit))
HR_level2 <- c(tb[1,1]+tb[38,1], IQRunit, exp((tb[1,1]+tb[38,1])*IQRunit), exp((tb[1,1]+tb[38,1]-se2)*IQRunit), exp((tb[1,1]+tb[38,1]+se2)*IQRunit))
HR_level3 <- c(tb[1,1]+tb[37,1], IQRunit, exp((tb[1,1]+tb[37,1])*IQRunit), exp((tb[1,1]+tb[37,1]-se3)*IQRunit), exp((tb[1,1]+tb[37,1]+se3)*IQRunit))

HR <- rbind(HR_reference, HR_level1, HR_level2, HR_level3)
print(HR)
HR <- as.data.frame(HR)
colnames(HR) <- c("coef", "IQR", "HR", "HR_lci", "HR_uci")
write.csv(HR, paste0(dir_results, "cox_mortality_pm25_race4_HR.csv"))
tb <- as.data.frame(tb)
setDT(tb, keep.rownames = TRUE)
fwrite(tb, paste0(dir_results, "cox_mortality_pm25_race4_coef.csv"))

## 2. effect modification for NO2 -------------------------------------------
IQRunit <- IQRs$no2
### sex 0/1 -----
cox_no2_sex <- coxph(Surv(time = followupyr, time2 = followupyr_plusone, event = dead) ~ 
                       no2 + I(no2*sex_female) +
                       mean_bmi + smoke_rate + hispanic + pct_blk + 
                       medhouseholdincome + medianhousevalue +  
                       poverty + education + popdensity + pct_owner_occ +
                       summer_tmmx + winter_tmmx + summer_rmax + winter_rmax +
                       as.factor(year) +  as.factor(region) +
                       strata(as.factor(entry_age_break)) + strata(as.factor(sex)) + 
                       strata(as.factor(race_collapsed)) + strata(as.factor(dual)),
                     data = dt,
                     tie = c("efron"), 
                     na.action = na.omit)
tb <- summary(cox_no2_sex)$coefficients
cov_matrix <- vcov(cox_no2_sex)

HR_reference <- c(tb[1,1], IQRunit, exp(tb[1,1]*IQRunit),exp((tb[1,1]-tb[1,3])*IQRunit), exp((tb[1,1]+tb[1,3])*IQRunit))
se <- sqrt(cov_matrix[1,1] + cov_matrix[2,2] + 2*cov_matrix[1,2])
HR_level1 <- c(tb[1,1]+tb[2,1], IQRunit, exp((tb[1,1]+tb[2,1])*IQRunit), exp((tb[1,1]+tb[2,1]-se)*IQRunit), exp((tb[1,1]+tb[2,1]+se)*IQRunit))

HR <- rbind(HR_reference, HR_level1)
print(HR)
HR <- as.data.frame(HR)
colnames(HR) <- c("coef", "IQR", "HR", "HR_lci", "HR_uci")
write.csv(HR, paste0(dir_results, "cox_mortality_no2_sex_HR.csv"))
tb <- as.data.frame(tb)
setDT(tb, keep.rownames = TRUE)
fwrite(tb, paste0(dir_results, "cox_mortality_no2_sex_coef.csv"))

### dual eligibility 0/1 ----
cox_no2_dual <- coxph(Surv(time = followupyr, time2 = followupyr_plusone, event = dead) ~ 
                        no2 + I(no2*dual) +
                        mean_bmi + smoke_rate + hispanic + pct_blk + 
                        medhouseholdincome + medianhousevalue +  
                        poverty + education + popdensity + pct_owner_occ +
                        summer_tmmx + winter_tmmx + summer_rmax + winter_rmax +
                        as.factor(year) +  as.factor(region) +
                        strata(as.factor(entry_age_break)) + strata(as.factor(sex)) + 
                        strata(as.factor(race_collapsed)) + strata(as.factor(dual)),
                      data = dt,
                      tie = c("efron"), 
                      na.action = na.omit)
tb <- summary(cox_no2_dual)$coefficients
cov_matrix <- vcov(cox_no2_dual)

HR_reference <- c(tb[1,1], IQRunit, exp(tb[1,1]*IQRunit),exp((tb[1,1]-tb[1,3])*IQRunit), exp((tb[1,1]+tb[1,3])*IQRunit))
se <- sqrt(cov_matrix[1,1] + cov_matrix[2,2] + 2*cov_matrix[1,2])
HR_level1 <- c(tb[1,1]+tb[2,1], IQRunit, exp((tb[1,1]+tb[2,1])*IQRunit), exp((tb[1,1]+tb[2,1]-se)*IQRunit), exp((tb[1,1]+tb[2,1]+se)*IQRunit))

HR <- rbind(HR_reference, HR_level1)
print(HR)
HR <- as.data.frame(HR)
colnames(HR) <- c("coef", "IQR", "HR", "HR_lci", "HR_uci")
write.csv(HR, paste0(dir_results, "cox_mortality_no2_dual_HR.csv"))
tb <- as.data.frame(tb)
setDT(tb, keep.rownames = TRUE)
fwrite(tb, paste0(dir_results, "cox_mortality_no2_dual_coef.csv"))

### population density T/F----
cox_no2_popdensity <- coxph(Surv(time = followupyr, time2 = followupyr_plusone, event = dead) ~ 
                              no2 + I(no2*above_median_popdensity) +
                              mean_bmi + smoke_rate + hispanic + pct_blk + 
                              medhouseholdincome + medianhousevalue +  
                              poverty + education + popdensity + pct_owner_occ +
                              summer_tmmx + winter_tmmx + summer_rmax + winter_rmax +
                              as.factor(year) +  as.factor(region) +
                              strata(as.factor(entry_age_break)) + strata(as.factor(sex)) + 
                              strata(as.factor(race_collapsed)) + strata(as.factor(dual)),
                            data = dt,
                            tie = c("efron"), 
                            na.action = na.omit)
tb <- summary(cox_no2_popdensity)$coefficients
cov_matrix <- vcov(cox_no2_popdensity)

HR_reference <- c(tb[1,1], IQRunit, exp(tb[1,1]*IQRunit),exp((tb[1,1]-tb[1,3])*IQRunit), exp((tb[1,1]+tb[1,3])*IQRunit))
se <- sqrt(cov_matrix[1,1] + cov_matrix[2,2] + 2*cov_matrix[1,2])
HR_level1 <- c(tb[1,1]+tb[2,1], IQRunit, exp((tb[1,1]+tb[2,1])*IQRunit), exp((tb[1,1]+tb[2,1]-se)*IQRunit), exp((tb[1,1]+tb[2,1]+se)*IQRunit))

HR <- rbind(HR_reference, HR_level1)
print(HR)
HR <- as.data.frame(HR)
colnames(HR) <- c("coef", "IQR", "HR", "HR_lci", "HR_uci")
write.csv(HR, paste0(dir_results, "cox_mortality_no2_popdensity_HR.csv"))
tb <- as.data.frame(tb)
setDT(tb, keep.rownames = TRUE)
fwrite(tb, paste0(dir_results, "cox_mortality_no2_popdensity_coef.csv"))

### entry age over85 T/F-----
cox_no2_entryage <- coxph(Surv(time = followupyr, time2 = followupyr_plusone, event = dead) ~ 
                            no2 + I(no2*entry_age_over85) +
                            mean_bmi + smoke_rate + hispanic + pct_blk + 
                            medhouseholdincome + medianhousevalue +  
                            poverty + education + popdensity + pct_owner_occ +
                            summer_tmmx + winter_tmmx + summer_rmax + winter_rmax +
                            as.factor(year) +  as.factor(region) +
                            strata(as.factor(entry_age_break)) + strata(as.factor(sex)) + 
                            strata(as.factor(race_collapsed)) + strata(as.factor(dual)),
                          data = dt,
                          tie = c("efron"), 
                          na.action = na.omit)
tb <- summary(cox_no2_entryage)$coefficients
cov_matrix <- vcov(cox_no2_entryage)

HR_reference <- c(tb[1,1], IQRunit, exp(tb[1,1]*IQRunit),exp((tb[1,1]-tb[1,3])*IQRunit), exp((tb[1,1]+tb[1,3])*IQRunit))
se <- sqrt(cov_matrix[1,1] + cov_matrix[2,2] + 2*cov_matrix[1,2])
HR_level1 <- c(tb[1,1]+tb[2,1], IQRunit, exp((tb[1,1]+tb[2,1])*IQRunit), exp((tb[1,1]+tb[2,1]-se)*IQRunit), exp((tb[1,1]+tb[2,1]+se)*IQRunit))

HR <- rbind(HR_reference, HR_level1)
print(HR)
HR <- as.data.frame(HR)
colnames(HR) <- c("coef", "IQR", "HR", "HR_lci", "HR_uci")
write.csv(HR, paste0(dir_results, "cox_mortality_no2_entryage_HR.csv"))
tb <- as.data.frame(tb)
setDT(tb, keep.rownames = TRUE)
fwrite(tb, paste0(dir_results, "cox_mortality_no2_entryage_coef.csv"))

### race chr 3 levels ----
cox_no2_race <- coxph(Surv(time = followupyr, time2 = followupyr_plusone, event = dead) ~ 
                        no2 + no2*race_collapsed +
                        mean_bmi + smoke_rate + hispanic + pct_blk + 
                        medhouseholdincome + medianhousevalue +  
                        poverty + education + popdensity + pct_owner_occ +
                        summer_tmmx + winter_tmmx + summer_rmax + winter_rmax +
                        as.factor(year) +  as.factor(region) +
                        strata(as.factor(entry_age_break)) + strata(as.factor(sex)) + 
                        strata(as.factor(race_collapsed)) + strata(as.factor(dual)),
                      data = dt,
                      tie = c("efron"), 
                      na.action = na.omit)
tb <- summary(cox_no2_race)$coefficients
cov_matrix <- vcov(cox_no2_race)

HR_reference <- c(tb[1,1], IQRunit, exp(tb[1,1]*IQRunit),exp((tb[1,1]-tb[1,3])*IQRunit), exp((tb[1,1]+tb[1,3])*IQRunit))
se1 <- sqrt(cov_matrix[1,1] + cov_matrix[37,37] + 2*cov_matrix[1,37])
se2 <- sqrt(cov_matrix[1,1] + cov_matrix[36,36] + 2*cov_matrix[1,36])
HR_level1 <- c(tb[1,1]+tb[37,1], IQRunit, exp((tb[1,1]+tb[37,1])*IQRunit), exp((tb[1,1]+tb[37,1]-se1)*IQRunit), exp((tb[1,1]+tb[37,1]+se1)*IQRunit))
HR_level2 <- c(tb[1,1]+tb[36,1], IQRunit, exp((tb[1,1]+tb[36,1])*IQRunit), exp((tb[1,1]+tb[36,1]-se2)*IQRunit), exp((tb[1,1]+tb[36,1]+se2)*IQRunit))

HR <- rbind(HR_reference, HR_level1, HR_level2)
print(HR)
HR <- as.data.frame(HR)
colnames(HR) <- c("coef", "IQR", "HR", "HR_lci", "HR_uci")
write.csv(HR, paste0(dir_results, "cox_mortality_no2_race_HR.csv"))
tb <- as.data.frame(tb)
setDT(tb, keep.rownames = TRUE)
fwrite(tb, paste0(dir_results, "cox_mortality_no2_race_coef.csv"))

## 3. effect modification for summer ozone ------------------------------------
IQRunit <- IQRs$ozone_summer
### sex 0/1 -----
cox_ozone_summer_sex <- coxph(Surv(time = followupyr, time2 = followupyr_plusone, event = dead) ~ 
                                ozone_summer + I(ozone_summer*sex_female) +
                                mean_bmi + smoke_rate + hispanic + pct_blk + 
                                medhouseholdincome + medianhousevalue +  
                                poverty + education + popdensity + pct_owner_occ +
                                summer_tmmx + winter_tmmx + summer_rmax + winter_rmax +
                                as.factor(year) +  as.factor(region) +
                                strata(as.factor(entry_age_break)) + strata(as.factor(sex)) + 
                                strata(as.factor(race_collapsed)) + strata(as.factor(dual)),
                              data = dt,
                              tie = c("efron"), 
                              na.action = na.omit)
tb <- summary(cox_ozone_summer_sex)$coefficients
cov_matrix <- vcov(cox_ozone_summer_sex)

HR_reference <- c(tb[1,1], IQRunit, exp(tb[1,1]*IQRunit),exp((tb[1,1]-tb[1,3])*IQRunit), exp((tb[1,1]+tb[1,3])*IQRunit))
se <- sqrt(cov_matrix[1,1] + cov_matrix[2,2] + 2*cov_matrix[1,2])
HR_level1 <- c(tb[1,1]+tb[2,1], IQRunit, exp((tb[1,1]+tb[2,1])*IQRunit), exp((tb[1,1]+tb[2,1]-se)*IQRunit), exp((tb[1,1]+tb[2,1]+se)*IQRunit))

HR <- rbind(HR_reference, HR_level1)
print(HR)
HR <- as.data.frame(HR)
colnames(HR) <- c("coef", "IQR", "HR", "HR_lci", "HR_uci")
write.csv(HR, paste0(dir_results, "cox_mortality_ozone_summer_sex_HR.csv"))
tb <- as.data.frame(tb)
setDT(tb, keep.rownames = TRUE)
fwrite(tb, paste0(dir_results, "cox_mortality_ozone_summer_sex_coef.csv"))

### dual eligibility 0/1 ----
cox_ozone_summer_dual <- coxph(Surv(time = followupyr, time2 = followupyr_plusone, event = dead) ~ 
                                 ozone_summer + I(ozone_summer*dual) +
                                 mean_bmi + smoke_rate + hispanic + pct_blk + 
                                 medhouseholdincome + medianhousevalue +  
                                 poverty + education + popdensity + pct_owner_occ +
                                 summer_tmmx + winter_tmmx + summer_rmax + winter_rmax +
                                 as.factor(year) +  as.factor(region) +
                                 strata(as.factor(entry_age_break)) + strata(as.factor(sex)) + 
                                 strata(as.factor(race_collapsed)) + strata(as.factor(dual)),
                               data = dt,
                               tie = c("efron"), 
                               na.action = na.omit)
tb <- summary(cox_ozone_summer_dual)$coefficients
cov_matrix <- vcov(cox_ozone_summer_dual)

HR_reference <- c(tb[1,1], IQRunit, exp(tb[1,1]*IQRunit),exp((tb[1,1]-tb[1,3])*IQRunit), exp((tb[1,1]+tb[1,3])*IQRunit))
se <- sqrt(cov_matrix[1,1] + cov_matrix[2,2] + 2*cov_matrix[1,2])
HR_level1 <- c(tb[1,1]+tb[2,1], IQRunit, exp((tb[1,1]+tb[2,1])*IQRunit), exp((tb[1,1]+tb[2,1]-se)*IQRunit), exp((tb[1,1]+tb[2,1]+se)*IQRunit))

HR <- rbind(HR_reference, HR_level1)
print(HR)
HR <- as.data.frame(HR)
colnames(HR) <- c("coef", "IQR", "HR", "HR_lci", "HR_uci")
write.csv(HR, paste0(dir_results, "cox_mortality_ozone_summer_dual_HR.csv"))
tb <- as.data.frame(tb)
setDT(tb, keep.rownames = TRUE)
fwrite(tb, paste0(dir_results, "cox_mortality_ozone_summer_dual_coef.csv"))

### population density T/F----
cox_ozone_summer_popdensity <- coxph(Surv(time = followupyr, time2 = followupyr_plusone, event = dead) ~ 
                                       ozone_summer + I(ozone_summer*above_median_popdensity) +
                                       mean_bmi + smoke_rate + hispanic + pct_blk + 
                                       medhouseholdincome + medianhousevalue +  
                                       poverty + education + popdensity + pct_owner_occ +
                                       summer_tmmx + winter_tmmx + summer_rmax + winter_rmax +
                                       as.factor(year) +  as.factor(region) +
                                       strata(as.factor(entry_age_break)) + strata(as.factor(sex)) + 
                                       strata(as.factor(race_collapsed)) + strata(as.factor(dual)),
                                     data = dt,
                                     tie = c("efron"), 
                                     na.action = na.omit)
tb <- summary(cox_ozone_summer_popdensity)$coefficients
cov_matrix <- vcov(cox_ozone_summer_popdensity)

HR_reference <- c(tb[1,1], IQRunit, exp(tb[1,1]*IQRunit),exp((tb[1,1]-tb[1,3])*IQRunit), exp((tb[1,1]+tb[1,3])*IQRunit))
se <- sqrt(cov_matrix[1,1] + cov_matrix[2,2] + 2*cov_matrix[1,2])
HR_level1 <- c(tb[1,1]+tb[2,1], IQRunit, exp((tb[1,1]+tb[2,1])*IQRunit), exp((tb[1,1]+tb[2,1]-se)*IQRunit), exp((tb[1,1]+tb[2,1]+se)*IQRunit))

HR <- rbind(HR_reference, HR_level1)
print(HR)
HR <- as.data.frame(HR)
colnames(HR) <- c("coef", "IQR", "HR", "HR_lci", "HR_uci")
write.csv(HR, paste0(dir_results, "cox_mortality_ozone_summer_popdensity_HR.csv"))
tb <- as.data.frame(tb)
setDT(tb, keep.rownames = TRUE)
fwrite(tb, paste0(dir_results, "cox_mortality_ozone_summer_popdensity_coef.csv"))

### entry age over85 T/F-----
cox_ozone_summer_entryage <- coxph(Surv(time = followupyr, time2 = followupyr_plusone, event = dead) ~ 
                                     ozone_summer + I(ozone_summer*entry_age_over85) +
                                     mean_bmi + smoke_rate + hispanic + pct_blk + 
                                     medhouseholdincome + medianhousevalue +  
                                     poverty + education + popdensity + pct_owner_occ +
                                     summer_tmmx + winter_tmmx + summer_rmax + winter_rmax +
                                     as.factor(year) +  as.factor(region) +
                                     strata(as.factor(entry_age_break)) + strata(as.factor(sex)) + 
                                     strata(as.factor(race_collapsed)) + strata(as.factor(dual)),
                                   data = dt,
                                   tie = c("efron"), 
                                   na.action = na.omit)
tb <- summary(cox_ozone_summer_entryage)$coefficients
cov_matrix <- vcov(cox_ozone_summer_entryage)

HR_reference <- c(tb[1,1], IQRunit, exp(tb[1,1]*IQRunit),exp((tb[1,1]-tb[1,3])*IQRunit), exp((tb[1,1]+tb[1,3])*IQRunit))
se <- sqrt(cov_matrix[1,1] + cov_matrix[2,2] + 2*cov_matrix[1,2])
HR_level1 <- c(tb[1,1]+tb[2,1], IQRunit, exp((tb[1,1]+tb[2,1])*IQRunit), exp((tb[1,1]+tb[2,1]-se)*IQRunit), exp((tb[1,1]+tb[2,1]+se)*IQRunit))

HR <- rbind(HR_reference, HR_level1)
print(HR)
HR <- as.data.frame(HR)
colnames(HR) <- c("coef", "IQR", "HR", "HR_lci", "HR_uci")
write.csv(HR, paste0(dir_results, "cox_mortality_ozone_summer_entryage_HR.csv"))
tb <- as.data.frame(tb)
setDT(tb, keep.rownames = TRUE)
fwrite(tb, paste0(dir_results, "cox_mortality_ozone_summer_entryage_coef.csv"))

### race chr 3 levels ----
cox_ozone_summer_race <- coxph(Surv(time = followupyr, time2 = followupyr_plusone, event = dead) ~ 
                                 ozone_summer + ozone_summer*race_collapsed +
                                 mean_bmi + smoke_rate + hispanic + pct_blk + 
                                 medhouseholdincome + medianhousevalue +  
                                 poverty + education + popdensity + pct_owner_occ +
                                 summer_tmmx + winter_tmmx + summer_rmax + winter_rmax +
                                 as.factor(year) +  as.factor(region) +
                                 strata(as.factor(entry_age_break)) + strata(as.factor(sex)) + 
                                 strata(as.factor(race_collapsed)) + strata(as.factor(dual)),
                               data = dt,
                               tie = c("efron"), 
                               na.action = na.omit)
tb <- summary(cox_ozone_summer_race)$coefficients
cov_matrix <- vcov(cox_ozone_summer_race)

HR_reference <- c(tb[1,1], IQRunit, exp(tb[1,1]*IQRunit),exp((tb[1,1]-tb[1,3])*IQRunit), exp((tb[1,1]+tb[1,3])*IQRunit))
se1 <- sqrt(cov_matrix[1,1] + cov_matrix[37,37] + 2*cov_matrix[1,37])
se2 <- sqrt(cov_matrix[1,1] + cov_matrix[36,36] + 2*cov_matrix[1,36])
HR_level1 <- c(tb[1,1]+tb[37,1], IQRunit, exp((tb[1,1]+tb[37,1])*IQRunit), exp((tb[1,1]+tb[37,1]-se1)*IQRunit), exp((tb[1,1]+tb[37,1]+se1)*IQRunit))
HR_level2 <- c(tb[1,1]+tb[36,1], IQRunit, exp((tb[1,1]+tb[36,1])*IQRunit), exp((tb[1,1]+tb[36,1]-se2)*IQRunit), exp((tb[1,1]+tb[36,1]+se2)*IQRunit))

HR <- rbind(HR_reference, HR_level1, HR_level2)
print(HR)
HR <- as.data.frame(HR)
colnames(HR) <- c("coef", "IQR", "HR", "HR_lci", "HR_uci")
write.csv(HR, paste0(dir_results, "cox_mortality_ozone_summer_race_HR.csv"))
tb <- as.data.frame(tb)
setDT(tb, keep.rownames = TRUE)
fwrite(tb, paste0(dir_results, "cox_mortality_ozone_summer_race_coef.csv"))

## 4. effect modification for oxidant ------------------------------------
IQRunit <- IQRs$ox
### sex 0/1 -----
cox_ox_sex <- coxph(Surv(time = followupyr, time2 = followupyr_plusone, event = dead) ~ 
                      ox + I(ox*sex_female) +
                      mean_bmi + smoke_rate + hispanic + pct_blk + 
                      medhouseholdincome + medianhousevalue +  
                      poverty + education + popdensity + pct_owner_occ +
                      summer_tmmx + winter_tmmx + summer_rmax + winter_rmax +
                      as.factor(year) +  as.factor(region) +
                      strata(as.factor(entry_age_break)) + strata(as.factor(sex)) + 
                      strata(as.factor(race_collapsed)) + strata(as.factor(dual)),
                    data = dt,
                    tie = c("efron"), 
                    na.action = na.omit)
tb <- summary(cox_ox_sex)$coefficients
cov_matrix <- vcov(cox_ox_sex)

HR_reference <- c(tb[1,1], IQRunit, exp(tb[1,1]*IQRunit),exp((tb[1,1]-tb[1,3])*IQRunit), exp((tb[1,1]+tb[1,3])*IQRunit))
se <- sqrt(cov_matrix[1,1] + cov_matrix[2,2] + 2*cov_matrix[1,2])
HR_level1 <- c(tb[1,1]+tb[2,1], IQRunit, exp((tb[1,1]+tb[2,1])*IQRunit), exp((tb[1,1]+tb[2,1]-se)*IQRunit), exp((tb[1,1]+tb[2,1]+se)*IQRunit))

HR <- rbind(HR_reference, HR_level1)
print(HR)
HR <- as.data.frame(HR)
colnames(HR) <- c("coef", "IQR", "HR", "HR_lci", "HR_uci")
write.csv(HR, paste0(dir_results, "cox_mortality_ox_sex_HR.csv"))
tb <- as.data.frame(tb)
setDT(tb, keep.rownames = TRUE)
fwrite(tb, paste0(dir_results, "cox_mortality_ox_sex_coef.csv"))

### dual eligibility 0/1 ----
cox_ox_dual <- coxph(Surv(time = followupyr, time2 = followupyr_plusone, event = dead) ~ 
                       ox + I(ox*dual) +
                       mean_bmi + smoke_rate + hispanic + pct_blk + 
                       medhouseholdincome + medianhousevalue +  
                       poverty + education + popdensity + pct_owner_occ +
                       summer_tmmx + winter_tmmx + summer_rmax + winter_rmax +
                       as.factor(year) +  as.factor(region) +
                       strata(as.factor(entry_age_break)) + strata(as.factor(sex)) + 
                       strata(as.factor(race_collapsed)) + strata(as.factor(dual)),
                     data = dt,
                     tie = c("efron"), 
                     na.action = na.omit)
tb <- summary(cox_ox_dual)$coefficients
cov_matrix <- vcov(cox_ox_dual)

HR_reference <- c(tb[1,1], IQRunit, exp(tb[1,1]*IQRunit),exp((tb[1,1]-tb[1,3])*IQRunit), exp((tb[1,1]+tb[1,3])*IQRunit))
se <- sqrt(cov_matrix[1,1] + cov_matrix[2,2] + 2*cov_matrix[1,2])
HR_level1 <- c(tb[1,1]+tb[2,1], IQRunit, exp((tb[1,1]+tb[2,1])*IQRunit), exp((tb[1,1]+tb[2,1]-se)*IQRunit), exp((tb[1,1]+tb[2,1]+se)*IQRunit))

HR <- rbind(HR_reference, HR_level1)
print(HR)
HR <- as.data.frame(HR)
colnames(HR) <- c("coef", "IQR", "HR", "HR_lci", "HR_uci")
write.csv(HR, paste0(dir_results, "cox_mortality_ox_dual_HR.csv"))
tb <- as.data.frame(tb)
setDT(tb, keep.rownames = TRUE)
fwrite(tb, paste0(dir_results, "cox_mortality_ox_dual_coef.csv"))

### population density T/F----
cox_ox_popdensity <- coxph(Surv(time = followupyr, time2 = followupyr_plusone, event = dead) ~ 
                             ox + I(ox*above_median_popdensity) +
                             mean_bmi + smoke_rate + hispanic + pct_blk + 
                             medhouseholdincome + medianhousevalue +  
                             poverty + education + popdensity + pct_owner_occ +
                             summer_tmmx + winter_tmmx + summer_rmax + winter_rmax +
                             as.factor(year) +  as.factor(region) +
                             strata(as.factor(entry_age_break)) + strata(as.factor(sex)) + 
                             strata(as.factor(race_collapsed)) + strata(as.factor(dual)),
                           data = dt,
                           tie = c("efron"), 
                           na.action = na.omit)
tb <- summary(cox_ox_popdensity)$coefficients
cov_matrix <- vcov(cox_ox_popdensity)

HR_reference <- c(tb[1,1], IQRunit, exp(tb[1,1]*IQRunit),exp((tb[1,1]-tb[1,3])*IQRunit), exp((tb[1,1]+tb[1,3])*IQRunit))
se <- sqrt(cov_matrix[1,1] + cov_matrix[2,2] + 2*cov_matrix[1,2])
HR_level1 <- c(tb[1,1]+tb[2,1], IQRunit, exp((tb[1,1]+tb[2,1])*IQRunit), exp((tb[1,1]+tb[2,1]-se)*IQRunit), exp((tb[1,1]+tb[2,1]+se)*IQRunit))

HR <- rbind(HR_reference, HR_level1)
print(HR)
HR <- as.data.frame(HR)
colnames(HR) <- c("coef", "IQR", "HR", "HR_lci", "HR_uci")
write.csv(HR, paste0(dir_results, "cox_mortality_ox_popdensity_HR.csv"))
tb <- as.data.frame(tb)
setDT(tb, keep.rownames = TRUE)
fwrite(tb, paste0(dir_results, "cox_mortality_ox_popdensity_coef.csv"))

### entry age over85 T/F-----
cox_ox_entryage <- coxph(Surv(time = followupyr, time2 = followupyr_plusone, event = dead) ~ 
                           ox + I(ox*entry_age_over85) +
                           mean_bmi + smoke_rate + hispanic + pct_blk + 
                           medhouseholdincome + medianhousevalue +  
                           poverty + education + popdensity + pct_owner_occ +
                           summer_tmmx + winter_tmmx + summer_rmax + winter_rmax +
                           as.factor(year) +  as.factor(region) +
                           strata(as.factor(entry_age_break)) + strata(as.factor(sex)) + 
                           strata(as.factor(race_collapsed)) + strata(as.factor(dual)),
                         data = dt,
                         tie = c("efron"), 
                         na.action = na.omit)
tb <- summary(cox_ox_entryage)$coefficients
cov_matrix <- vcov(cox_ox_entryage)

HR_reference <- c(tb[1,1], IQRunit, exp(tb[1,1]*IQRunit),exp((tb[1,1]-tb[1,3])*IQRunit), exp((tb[1,1]+tb[1,3])*IQRunit))
se <- sqrt(cov_matrix[1,1] + cov_matrix[2,2] + 2*cov_matrix[1,2])
HR_level1 <- c(tb[1,1]+tb[2,1], IQRunit, exp((tb[1,1]+tb[2,1])*IQRunit), exp((tb[1,1]+tb[2,1]-se)*IQRunit), exp((tb[1,1]+tb[2,1]+se)*IQRunit))

HR <- rbind(HR_reference, HR_level1)
print(HR)
HR <- as.data.frame(HR)
colnames(HR) <- c("coef", "IQR", "HR", "HR_lci", "HR_uci")
write.csv(HR, paste0(dir_results, "cox_mortality_ox_entryage_HR.csv"))
tb <- as.data.frame(tb)
setDT(tb, keep.rownames = TRUE)
fwrite(tb, paste0(dir_results, "cox_mortality_ox_entryage_coef.csv"))

### race chr 3 levels ----
cox_ox_race <- coxph(Surv(time = followupyr, time2 = followupyr_plusone, event = dead) ~ 
                       ox + ox*race_collapsed +
                       mean_bmi + smoke_rate + hispanic + pct_blk + 
                       medhouseholdincome + medianhousevalue +  
                       poverty + education + popdensity + pct_owner_occ +
                       summer_tmmx + winter_tmmx + summer_rmax + winter_rmax +
                       as.factor(year) +  as.factor(region) +
                       strata(as.factor(entry_age_break)) + strata(as.factor(sex)) + 
                       strata(as.factor(race_collapsed)) + strata(as.factor(dual)),
                     data = dt,
                     tie = c("efron"), 
                     na.action = na.omit)
tb <- summary(cox_ox_race)$coefficients
cov_matrix <- vcov(cox_ox_race)

HR_reference <- c(tb[1,1], IQRunit, exp(tb[1,1]*IQRunit),exp((tb[1,1]-tb[1,3])*IQRunit), exp((tb[1,1]+tb[1,3])*IQRunit))
se1 <- sqrt(cov_matrix[1,1] + cov_matrix[37,37] + 2*cov_matrix[1,37])
se2 <- sqrt(cov_matrix[1,1] + cov_matrix[36,36] + 2*cov_matrix[1,36])
HR_level1 <- c(tb[1,1]+tb[37,1], IQRunit, exp((tb[1,1]+tb[37,1])*IQRunit), exp((tb[1,1]+tb[37,1]-se1)*IQRunit), exp((tb[1,1]+tb[37,1]+se1)*IQRunit))
HR_level2 <- c(tb[1,1]+tb[36,1], IQRunit, exp((tb[1,1]+tb[36,1])*IQRunit), exp((tb[1,1]+tb[36,1]-se2)*IQRunit), exp((tb[1,1]+tb[36,1]+se2)*IQRunit))

HR <- rbind(HR_reference, HR_level1, HR_level2)
print(HR)
HR <- as.data.frame(HR)
colnames(HR) <- c("coef", "IQR", "HR", "HR_lci", "HR_uci")
write.csv(HR, paste0(dir_results, "cox_mortality_ox_race_HR.csv"))
tb <- as.data.frame(tb)
setDT(tb, keep.rownames = TRUE)
fwrite(tb, paste0(dir_results, "cox_mortality_ox_race_coef.csv"))

## 5. effect modification for PM2.5 in multi pollutant model -----------------
IQRunit <- IQRs$pm25
### sex 0/1 -----
cox_all3_pm25_sex <- coxph(Surv(time = followupyr, time2 = followupyr_plusone, event = dead) ~ 
                             pm25 + I(pm25*sex_female) + no2 + ozone_summer +
                             mean_bmi + smoke_rate + hispanic + pct_blk + 
                             medhouseholdincome + medianhousevalue +  
                             poverty + education + popdensity + pct_owner_occ +
                             summer_tmmx + winter_tmmx + summer_rmax + winter_rmax +
                             as.factor(year) +  as.factor(region) +
                             strata(as.factor(entry_age_break)) + strata(as.factor(sex)) + 
                             strata(as.factor(race_collapsed)) + strata(as.factor(dual)),
                           data = dt,
                           tie = c("efron"), 
                           na.action = na.omit)
tb <- summary(cox_all3_pm25_sex)$coefficients
cov_matrix <- vcov(cox_all3_pm25_sex)

HR_reference <- c(tb[1,1], IQRunit, exp(tb[1,1]*IQRunit),exp((tb[1,1]-tb[1,3])*IQRunit), exp((tb[1,1]+tb[1,3])*IQRunit))
se <- sqrt(cov_matrix[1,1] + cov_matrix[2,2] + 2*cov_matrix[1,2])
HR_level1 <- c(tb[1,1]+tb[2,1], IQRunit, exp((tb[1,1]+tb[2,1])*IQRunit), exp((tb[1,1]+tb[2,1]-se)*IQRunit), exp((tb[1,1]+tb[2,1]+se)*IQRunit))

HR <- rbind(HR_reference, HR_level1)
print(HR)
HR <- as.data.frame(HR)
colnames(HR) <- c("coef", "IQR", "HR", "HR_lci", "HR_uci")
write.csv(HR, paste0(dir_results, "cox_mortality_all3_pm25_sex_HR.csv"))
tb <- as.data.frame(tb)
setDT(tb, keep.rownames = TRUE)
fwrite(tb, paste0(dir_results, "cox_mortality_all3_pm25_sex_coef.csv"))

### dual eligibility 0/1 ----
cox_all3_pm25_dual <- coxph(Surv(time = followupyr, time2 = followupyr_plusone, event = dead) ~ 
                              pm25 + I(pm25*dual) + no2 + ozone_summer +
                              mean_bmi + smoke_rate + hispanic + pct_blk + 
                              medhouseholdincome + medianhousevalue +  
                              poverty + education + popdensity + pct_owner_occ +
                              summer_tmmx + winter_tmmx + summer_rmax + winter_rmax +
                              as.factor(year) +  as.factor(region) +
                              strata(as.factor(entry_age_break)) + strata(as.factor(sex)) + 
                              strata(as.factor(race_collapsed)) + strata(as.factor(dual)),
                            data = dt,
                            tie = c("efron"), 
                            na.action = na.omit)
tb <- summary(cox_all3_pm25_dual)$coefficients
cov_matrix <- vcov(cox_all3_pm25_dual)

HR_reference <- c(tb[1,1], IQRunit, exp(tb[1,1]*IQRunit),exp((tb[1,1]-tb[1,3])*IQRunit), exp((tb[1,1]+tb[1,3])*IQRunit))
se <- sqrt(cov_matrix[1,1] + cov_matrix[2,2] + 2*cov_matrix[1,2])
HR_level1 <- c(tb[1,1]+tb[2,1], IQRunit, exp((tb[1,1]+tb[2,1])*IQRunit), exp((tb[1,1]+tb[2,1]-se)*IQRunit), exp((tb[1,1]+tb[2,1]+se)*IQRunit))

HR <- rbind(HR_reference, HR_level1)
print(HR)
HR <- as.data.frame(HR)
colnames(HR) <- c("coef", "IQR", "HR", "HR_lci", "HR_uci")
write.csv(HR, paste0(dir_results, "cox_mortality_all3_pm25_dual_HR.csv"))
tb <- as.data.frame(tb)
setDT(tb, keep.rownames = TRUE)
fwrite(tb, paste0(dir_results, "cox_all3_pm25_dual_coef.csv"))

### population density T/F----
cox_all3_pm25_popdensity <- coxph(Surv(time = followupyr, time2 = followupyr_plusone, event = dead) ~ 
                               pm25 + I(pm25*above_median_popdensity) + no2 + ozone_summer +
                               mean_bmi + smoke_rate + hispanic + pct_blk + 
                               medhouseholdincome + medianhousevalue +  
                               poverty + education + popdensity + pct_owner_occ +
                               summer_tmmx + winter_tmmx + summer_rmax + winter_rmax +
                               as.factor(year) +  as.factor(region) +
                               strata(as.factor(entry_age_break)) + strata(as.factor(sex)) + 
                               strata(as.factor(race_collapsed)) + strata(as.factor(dual)),
                             data = dt,
                             tie = c("efron"), 
                             na.action = na.omit)
tb <- summary(cox_all3_pm25_popdensity)$coefficients
cov_matrix <- vcov(cox_all3_pm25_popdensity)

HR_reference <- c(tb[1,1], IQRunit, exp(tb[1,1]*IQRunit),exp((tb[1,1]-tb[1,3])*IQRunit), exp((tb[1,1]+tb[1,3])*IQRunit))
se <- sqrt(cov_matrix[1,1] + cov_matrix[2,2] + 2*cov_matrix[1,2])
HR_level1 <- c(tb[1,1]+tb[2,1], IQRunit, exp((tb[1,1]+tb[2,1])*IQRunit), exp((tb[1,1]+tb[2,1]-se)*IQRunit), exp((tb[1,1]+tb[2,1]+se)*IQRunit))

HR <- rbind(HR_reference, HR_level1)
print(HR)
HR <- as.data.frame(HR)
colnames(HR) <- c("coef", "IQR", "HR", "HR_lci", "HR_uci")
write.csv(HR, paste0(dir_results, "cox_mortality_all3_pm25_popdensity_HR.csv"))
tb <- as.data.frame(tb)
setDT(tb, keep.rownames = TRUE)
fwrite(tb, paste0(dir_results, "cox_mortality_all3_pm25_popdensity_coef.csv"))

### entry age over85 T/F-----
cox_all3_pm25_entryage <- coxph(Surv(time = followupyr, time2 = followupyr_plusone, event = dead) ~ 
                             pm25 + I(pm25*entry_age_over85) + no2 + ozone_summer +
                             mean_bmi + smoke_rate + hispanic + pct_blk + 
                             medhouseholdincome + medianhousevalue +  
                             poverty + education + popdensity + pct_owner_occ +
                             summer_tmmx + winter_tmmx + summer_rmax + winter_rmax +
                             as.factor(year) +  as.factor(region) +
                             strata(as.factor(entry_age_break)) + strata(as.factor(sex)) + 
                             strata(as.factor(race_collapsed)) + strata(as.factor(dual)),
                           data = dt,
                           tie = c("efron"), 
                           na.action = na.omit)
tb <- summary(cox_all3_pm25_entryage)$coefficients
cov_matrix <- vcov(cox_all3_pm25_entryage)

HR_reference <- c(tb[1,1], IQRunit, exp(tb[1,1]*IQRunit),exp((tb[1,1]-tb[1,3])*IQRunit), exp((tb[1,1]+tb[1,3])*IQRunit))
se <- sqrt(cov_matrix[1,1] + cov_matrix[2,2] + 2*cov_matrix[1,2])
HR_level1 <- c(tb[1,1]+tb[2,1], IQRunit, exp((tb[1,1]+tb[2,1])*IQRunit), exp((tb[1,1]+tb[2,1]-se)*IQRunit), exp((tb[1,1]+tb[2,1]+se)*IQRunit))

HR <- rbind(HR_reference, HR_level1)
print(HR)
HR <- as.data.frame(HR)
colnames(HR) <- c("coef", "IQR", "HR", "HR_lci", "HR_uci")
write.csv(HR, paste0(dir_results, "cox_mortality_all3_pm25_entryage_HR.csv"))
tb <- as.data.frame(tb)
setDT(tb, keep.rownames = TRUE)
fwrite(tb, paste0(dir_results, "cox_mortality_all3_pm25_entryage_coef.csv"))

### race chr 3 levels ----
cox_all3_pm25_race <- coxph(Surv(time = followupyr, time2 = followupyr_plusone, event = dead) ~ 
                         pm25 + pm25*race_collapsed + no2 + ozone_summer +
                         mean_bmi + smoke_rate + hispanic + pct_blk + 
                         medhouseholdincome + medianhousevalue +  
                         poverty + education + popdensity + pct_owner_occ +
                         summer_tmmx + winter_tmmx + summer_rmax + winter_rmax +
                         as.factor(year) +  as.factor(region) +
                         strata(as.factor(entry_age_break)) + strata(as.factor(sex)) + 
                         strata(as.factor(race_collapsed)) + strata(as.factor(dual)),
                       data = dt,
                       tie = c("efron"), 
                       na.action = na.omit)
tb <- summary(cox_all3_pm25_race)$coefficients
cov_matrix <- vcov(cox_all3_pm25_race)

HR_reference <- c(tb[1,1], IQRunit, exp(tb[1,1]*IQRunit),exp((tb[1,1]-tb[1,3])*IQRunit), exp((tb[1,1]+tb[1,3])*IQRunit))
se1 <- sqrt(cov_matrix[1,1] + cov_matrix[39,39] + 2*cov_matrix[1,39])
se2 <- sqrt(cov_matrix[1,1] + cov_matrix[38,38] + 2*cov_matrix[1,38])
HR_level1 <- c(tb[1,1]+tb[39,1], IQRunit, exp((tb[1,1]+tb[39,1])*IQRunit), exp((tb[1,1]+tb[39,1]-se1)*IQRunit), exp((tb[1,1]+tb[39,1]+se1)*IQRunit))
HR_level2 <- c(tb[1,1]+tb[38,1], IQRunit, exp((tb[1,1]+tb[38,1])*IQRunit), exp((tb[1,1]+tb[38,1]-se2)*IQRunit), exp((tb[1,1]+tb[38,1]+se2)*IQRunit))

HR <- rbind(HR_reference, HR_level1, HR_level2)
print(HR)
HR <- as.data.frame(HR)
colnames(HR) <- c("coef", "IQR", "HR", "HR_lci", "HR_uci")
write.csv(HR, paste0(dir_results, "cox_mortality_all3_pm25_race_HR.csv"))
tb <- as.data.frame(tb)
setDT(tb, keep.rownames = TRUE)
fwrite(tb, paste0(dir_results, "cox_mortality_all3_pm25_race_coef.csv"))

## 6. effect modification for NO2 in multi-pollutant model -----------------
IQRunit <- IQRs$no2
### sex 0/1 -----
cox_all3_no2_sex <- coxph(Surv(time = followupyr, time2 = followupyr_plusone, event = dead) ~ 
                             no2 + I(no2*sex_female) + pm25 + ozone_summer +
                             mean_bmi + smoke_rate + hispanic + pct_blk + 
                             medhouseholdincome + medianhousevalue +  
                             poverty + education + popdensity + pct_owner_occ +
                             summer_tmmx + winter_tmmx + summer_rmax + winter_rmax +
                             as.factor(year) +  as.factor(region) +
                             strata(as.factor(entry_age_break)) + strata(as.factor(sex)) + 
                             strata(as.factor(race_collapsed)) + strata(as.factor(dual)),
                           data = dt,
                           tie = c("efron"), 
                           na.action = na.omit)
tb <- summary(cox_all3_no2_sex)$coefficients
cov_matrix <- vcov(cox_all3_no2_sex)

HR_reference <- c(tb[1,1], IQRunit, exp(tb[1,1]*IQRunit),exp((tb[1,1]-tb[1,3])*IQRunit), exp((tb[1,1]+tb[1,3])*IQRunit))
se <- sqrt(cov_matrix[1,1] + cov_matrix[2,2] + 2*cov_matrix[1,2])
HR_level1 <- c(tb[1,1]+tb[2,1], IQRunit, exp((tb[1,1]+tb[2,1])*IQRunit), exp((tb[1,1]+tb[2,1]-se)*IQRunit), exp((tb[1,1]+tb[2,1]+se)*IQRunit))

HR <- rbind(HR_reference, HR_level1)
print(HR)
HR <- as.data.frame(HR)
colnames(HR) <- c("coef", "IQR", "HR", "HR_lci", "HR_uci")
write.csv(HR, paste0(dir_results, "cox_mortality_all3_no2_sex_HR.csv"))
tb <- as.data.frame(tb)
setDT(tb, keep.rownames = TRUE)
fwrite(tb, paste0(dir_results, "cox_mortality_all3_no2_sex_coef.csv"))

### dual eligibility 0/1 ----
cox_all3_no2_dual <- coxph(Surv(time = followupyr, time2 = followupyr_plusone, event = dead) ~ 
                              no2 + I(no2*dual) + pm25 + ozone_summer +
                              mean_bmi + smoke_rate + hispanic + pct_blk + 
                              medhouseholdincome + medianhousevalue +  
                              poverty + education + popdensity + pct_owner_occ +
                              summer_tmmx + winter_tmmx + summer_rmax + winter_rmax +
                              as.factor(year) +  as.factor(region) +
                              strata(as.factor(entry_age_break)) + strata(as.factor(sex)) + 
                              strata(as.factor(race_collapsed)) + strata(as.factor(dual)),
                            data = dt,
                            tie = c("efron"), 
                            na.action = na.omit)
tb <- summary(cox_all3_no2_dual)$coefficients
cov_matrix <- vcov(cox_all3_no2_dual)

HR_reference <- c(tb[1,1], IQRunit, exp(tb[1,1]*IQRunit),exp((tb[1,1]-tb[1,3])*IQRunit), exp((tb[1,1]+tb[1,3])*IQRunit))
se <- sqrt(cov_matrix[1,1] + cov_matrix[2,2] + 2*cov_matrix[1,2])
HR_level1 <- c(tb[1,1]+tb[2,1], IQRunit, exp((tb[1,1]+tb[2,1])*IQRunit), exp((tb[1,1]+tb[2,1]-se)*IQRunit), exp((tb[1,1]+tb[2,1]+se)*IQRunit))

HR <- rbind(HR_reference, HR_level1)
print(HR)
HR <- as.data.frame(HR)
colnames(HR) <- c("coef", "IQR", "HR", "HR_lci", "HR_uci")
write.csv(HR, paste0(dir_results, "cox_mortality_all3_no2_dual_HR.csv"))
tb <- as.data.frame(tb)
setDT(tb, keep.rownames = TRUE)
fwrite(tb, paste0(dir_results, "cox_all3_no2_dual_coef.csv"))

### population density T/F----
cox_all3_no2_popdensity <- coxph(Surv(time = followupyr, time2 = followupyr_plusone, event = dead) ~ 
                                    no2 + I(no2*above_median_popdensity) + pm25 + ozone_summer +
                                    mean_bmi + smoke_rate + hispanic + pct_blk + 
                                    medhouseholdincome + medianhousevalue +  
                                    poverty + education + popdensity + pct_owner_occ +
                                    summer_tmmx + winter_tmmx + summer_rmax + winter_rmax +
                                    as.factor(year) +  as.factor(region) +
                                    strata(as.factor(entry_age_break)) + strata(as.factor(sex)) + 
                                    strata(as.factor(race_collapsed)) + strata(as.factor(dual)),
                                  data = dt,
                                  tie = c("efron"), 
                                  na.action = na.omit)
tb <- summary(cox_all3_no2_popdensity)$coefficients
cov_matrix <- vcov(cox_all3_no2_popdensity)

HR_reference <- c(tb[1,1], IQRunit, exp(tb[1,1]*IQRunit),exp((tb[1,1]-tb[1,3])*IQRunit), exp((tb[1,1]+tb[1,3])*IQRunit))
se <- sqrt(cov_matrix[1,1] + cov_matrix[2,2] + 2*cov_matrix[1,2])
HR_level1 <- c(tb[1,1]+tb[2,1], IQRunit, exp((tb[1,1]+tb[2,1])*IQRunit), exp((tb[1,1]+tb[2,1]-se)*IQRunit), exp((tb[1,1]+tb[2,1]+se)*IQRunit))

HR <- rbind(HR_reference, HR_level1)
print(HR)
HR <- as.data.frame(HR)
colnames(HR) <- c("coef", "IQR", "HR", "HR_lci", "HR_uci")
write.csv(HR, paste0(dir_results, "cox_mortality_all3_no2_popdensity_HR.csv"))
tb <- as.data.frame(tb)
setDT(tb, keep.rownames = TRUE)
fwrite(tb, paste0(dir_results, "cox_mortality_all3_no2_popdensity_coef.csv"))

### entry age over85 T/F-----
cox_all3_no2_entryage <- coxph(Surv(time = followupyr, time2 = followupyr_plusone, event = dead) ~ 
                                  no2 + I(no2*entry_age_over85) + pm25 + ozone_summer +
                                  mean_bmi + smoke_rate + hispanic + pct_blk + 
                                  medhouseholdincome + medianhousevalue +  
                                  poverty + education + popdensity + pct_owner_occ +
                                  summer_tmmx + winter_tmmx + summer_rmax + winter_rmax +
                                  as.factor(year) +  as.factor(region) +
                                  strata(as.factor(entry_age_break)) + strata(as.factor(sex)) + 
                                  strata(as.factor(race_collapsed)) + strata(as.factor(dual)),
                                data = dt,
                                tie = c("efron"), 
                                na.action = na.omit)
tb <- summary(cox_all3_no2_entryage)$coefficients
cov_matrix <- vcov(cox_all3_no2_entryage)

HR_reference <- c(tb[1,1], IQRunit, exp(tb[1,1]*IQRunit),exp((tb[1,1]-tb[1,3])*IQRunit), exp((tb[1,1]+tb[1,3])*IQRunit))
se <- sqrt(cov_matrix[1,1] + cov_matrix[2,2] + 2*cov_matrix[1,2])
HR_level1 <- c(tb[1,1]+tb[2,1], IQRunit, exp((tb[1,1]+tb[2,1])*IQRunit), exp((tb[1,1]+tb[2,1]-se)*IQRunit), exp((tb[1,1]+tb[2,1]+se)*IQRunit))

HR <- rbind(HR_reference, HR_level1)
print(HR)
HR <- as.data.frame(HR)
colnames(HR) <- c("coef", "IQR", "HR", "HR_lci", "HR_uci")
write.csv(HR, paste0(dir_results, "cox_mortality_all3_no2_entryage_HR.csv"))
tb <- as.data.frame(tb)
setDT(tb, keep.rownames = TRUE)
fwrite(tb, paste0(dir_results, "cox_mortality_all3_no2_entryage_coef.csv"))

### race chr 3 levels ----
cox_all3_no2_race <- coxph(Surv(time = followupyr, time2 = followupyr_plusone, event = dead) ~ 
                              no2 + no2*race_collapsed + pm25 + ozone_summer +
                              mean_bmi + smoke_rate + hispanic + pct_blk + 
                              medhouseholdincome + medianhousevalue +  
                              poverty + education + popdensity + pct_owner_occ +
                              summer_tmmx + winter_tmmx + summer_rmax + winter_rmax +
                              as.factor(year) +  as.factor(region) +
                              strata(as.factor(entry_age_break)) + strata(as.factor(sex)) + 
                              strata(as.factor(race_collapsed)) + strata(as.factor(dual)),
                            data = dt,
                            tie = c("efron"), 
                            na.action = na.omit)
tb <- summary(cox_all3_no2_race)$coefficients
cov_matrix <- vcov(cox_all3_no2_race)

HR_reference <- c(tb[1,1], IQRunit, exp(tb[1,1]*IQRunit),exp((tb[1,1]-tb[1,3])*IQRunit), exp((tb[1,1]+tb[1,3])*IQRunit))
se1 <- sqrt(cov_matrix[1,1] + cov_matrix[39,39] + 2*cov_matrix[1,39])
se2 <- sqrt(cov_matrix[1,1] + cov_matrix[38,38] + 2*cov_matrix[1,38])
HR_level1 <- c(tb[1,1]+tb[39,1], IQRunit, exp((tb[1,1]+tb[39,1])*IQRunit), exp((tb[1,1]+tb[39,1]-se1)*IQRunit), exp((tb[1,1]+tb[39,1]+se1)*IQRunit))
HR_level2 <- c(tb[1,1]+tb[38,1], IQRunit, exp((tb[1,1]+tb[38,1])*IQRunit), exp((tb[1,1]+tb[38,1]-se2)*IQRunit), exp((tb[1,1]+tb[38,1]+se2)*IQRunit))

HR <- rbind(HR_reference, HR_level1, HR_level2)
print(HR)
HR <- as.data.frame(HR)
colnames(HR) <- c("coef", "IQR", "HR", "HR_lci", "HR_uci")
write.csv(HR, paste0(dir_results, "cox_mortality_all3_no2_race_HR.csv"))
tb <- as.data.frame(tb)
setDT(tb, keep.rownames = TRUE)
fwrite(tb, paste0(dir_results, "cox_mortality_all3_no2_race_coef.csv"))

## 7. effect modification for ozone_summer in multi-pollutant model -----------------
IQRunit <- IQRs$ozone_summer
### sex 0/1 -----
cox_all3_ozone_summer_sex <- coxph(Surv(time = followupyr, time2 = followupyr_plusone, event = dead) ~ 
                             ozone_summer + I(ozone_summer*sex_female) + no2 + pm25 +
                             mean_bmi + smoke_rate + hispanic + pct_blk + 
                             medhouseholdincome + medianhousevalue +  
                             poverty + education + popdensity + pct_owner_occ +
                             summer_tmmx + winter_tmmx + summer_rmax + winter_rmax +
                             as.factor(year) +  as.factor(region) +
                             strata(as.factor(entry_age_break)) + strata(as.factor(sex)) + 
                             strata(as.factor(race_collapsed)) + strata(as.factor(dual)),
                           data = dt,
                           tie = c("efron"), 
                           na.action = na.omit)
tb <- summary(cox_all3_ozone_summer_sex)$coefficients
cov_matrix <- vcov(cox_all3_ozone_summer_sex)

HR_reference <- c(tb[1,1], IQRunit, exp(tb[1,1]*IQRunit),exp((tb[1,1]-tb[1,3])*IQRunit), exp((tb[1,1]+tb[1,3])*IQRunit))
se <- sqrt(cov_matrix[1,1] + cov_matrix[2,2] + 2*cov_matrix[1,2])
HR_level1 <- c(tb[1,1]+tb[2,1], IQRunit, exp((tb[1,1]+tb[2,1])*IQRunit), exp((tb[1,1]+tb[2,1]-se)*IQRunit), exp((tb[1,1]+tb[2,1]+se)*IQRunit))

HR <- rbind(HR_reference, HR_level1)
print(HR)
HR <- as.data.frame(HR)
colnames(HR) <- c("coef", "IQR", "HR", "HR_lci", "HR_uci")
write.csv(HR, paste0(dir_results, "cox_mortality_all3_ozone_summer_sex_HR.csv"))
tb <- as.data.frame(tb)
setDT(tb, keep.rownames = TRUE)
fwrite(tb, paste0(dir_results, "cox_mortality_all3_ozone_summer_sex_coef.csv"))

### dual eligibility 0/1 ----
cox_all3_ozone_summer_dual <- coxph(Surv(time = followupyr, time2 = followupyr_plusone, event = dead) ~ 
                              ozone_summer + I(ozone_summer*dual) + no2 + pm25 +
                              mean_bmi + smoke_rate + hispanic + pct_blk + 
                              medhouseholdincome + medianhousevalue +  
                              poverty + education + popdensity + pct_owner_occ +
                              summer_tmmx + winter_tmmx + summer_rmax + winter_rmax +
                              as.factor(year) +  as.factor(region) +
                              strata(as.factor(entry_age_break)) + strata(as.factor(sex)) + 
                              strata(as.factor(race_collapsed)) + strata(as.factor(dual)),
                            data = dt,
                            tie = c("efron"), 
                            na.action = na.omit)
tb <- summary(cox_all3_ozone_summer_dual)$coefficients
cov_matrix <- vcov(cox_all3_ozone_summer_dual)

HR_reference <- c(tb[1,1], IQRunit, exp(tb[1,1]*IQRunit),exp((tb[1,1]-tb[1,3])*IQRunit), exp((tb[1,1]+tb[1,3])*IQRunit))
se <- sqrt(cov_matrix[1,1] + cov_matrix[2,2] + 2*cov_matrix[1,2])
HR_level1 <- c(tb[1,1]+tb[2,1], IQRunit, exp((tb[1,1]+tb[2,1])*IQRunit), exp((tb[1,1]+tb[2,1]-se)*IQRunit), exp((tb[1,1]+tb[2,1]+se)*IQRunit))

HR <- rbind(HR_reference, HR_level1)
print(HR)
HR <- as.data.frame(HR)
colnames(HR) <- c("coef", "IQR", "HR", "HR_lci", "HR_uci")
write.csv(HR, paste0(dir_results, "cox_mortality_all3_ozone_summer_dual_HR.csv"))
tb <- as.data.frame(tb)
setDT(tb, keep.rownames = TRUE)
fwrite(tb, paste0(dir_results, "cox_all3_ozone_summer_dual_coef.csv"))

### population density T/F----
cox_all3_ozone_summer_popdensity <- coxph(Surv(time = followupyr, time2 = followupyr_plusone, event = dead) ~ 
                                    ozone_summer + I(ozone_summer*above_median_popdensity) + no2 + pm25 +
                                    mean_bmi + smoke_rate + hispanic + pct_blk + 
                                    medhouseholdincome + medianhousevalue +  
                                    poverty + education + popdensity + pct_owner_occ +
                                    summer_tmmx + winter_tmmx + summer_rmax + winter_rmax +
                                    as.factor(year) +  as.factor(region) +
                                    strata(as.factor(entry_age_break)) + strata(as.factor(sex)) + 
                                    strata(as.factor(race_collapsed)) + strata(as.factor(dual)),
                                  data = dt,
                                  tie = c("efron"), 
                                  na.action = na.omit)
tb <- summary(cox_all3_ozone_summer_popdensity)$coefficients
cov_matrix <- vcov(cox_all3_ozone_summer_popdensity)

HR_reference <- c(tb[1,1], IQRunit, exp(tb[1,1]*IQRunit),exp((tb[1,1]-tb[1,3])*IQRunit), exp((tb[1,1]+tb[1,3])*IQRunit))
se <- sqrt(cov_matrix[1,1] + cov_matrix[2,2] + 2*cov_matrix[1,2])
HR_level1 <- c(tb[1,1]+tb[2,1], IQRunit, exp((tb[1,1]+tb[2,1])*IQRunit), exp((tb[1,1]+tb[2,1]-se)*IQRunit), exp((tb[1,1]+tb[2,1]+se)*IQRunit))

HR <- rbind(HR_reference, HR_level1)
print(HR)
HR <- as.data.frame(HR)
colnames(HR) <- c("coef", "IQR", "HR", "HR_lci", "HR_uci")
write.csv(HR, paste0(dir_results, "cox_mortality_all3_ozone_summer_popdensity_HR.csv"))
tb <- as.data.frame(tb)
setDT(tb, keep.rownames = TRUE)
fwrite(tb, paste0(dir_results, "cox_mortality_all3_ozone_summer_popdensity_coef.csv"))

### entry age over85 T/F-----
cox_all3_ozone_summer_entryage <- coxph(Surv(time = followupyr, time2 = followupyr_plusone, event = dead) ~ 
                                  ozone_summer + I(ozone_summer*entry_age_over85) + no2 + pm25 +
                                  mean_bmi + smoke_rate + hispanic + pct_blk + 
                                  medhouseholdincome + medianhousevalue +  
                                  poverty + education + popdensity + pct_owner_occ +
                                  summer_tmmx + winter_tmmx + summer_rmax + winter_rmax +
                                  as.factor(year) +  as.factor(region) +
                                  strata(as.factor(entry_age_break)) + strata(as.factor(sex)) + 
                                  strata(as.factor(race_collapsed)) + strata(as.factor(dual)),
                                data = dt,
                                tie = c("efron"), 
                                na.action = na.omit)
tb <- summary(cox_all3_ozone_summer_entryage)$coefficients
cov_matrix <- vcov(cox_all3_ozone_summer_entryage)

HR_reference <- c(tb[1,1], IQRunit, exp(tb[1,1]*IQRunit),exp((tb[1,1]-tb[1,3])*IQRunit), exp((tb[1,1]+tb[1,3])*IQRunit))
se <- sqrt(cov_matrix[1,1] + cov_matrix[2,2] + 2*cov_matrix[1,2])
HR_level1 <- c(tb[1,1]+tb[2,1], IQRunit, exp((tb[1,1]+tb[2,1])*IQRunit), exp((tb[1,1]+tb[2,1]-se)*IQRunit), exp((tb[1,1]+tb[2,1]+se)*IQRunit))

HR <- rbind(HR_reference, HR_level1)
print(HR)
HR <- as.data.frame(HR)
colnames(HR) <- c("coef", "IQR", "HR", "HR_lci", "HR_uci")
write.csv(HR, paste0(dir_results, "cox_mortality_all3_ozone_summer_entryage_HR.csv"))
tb <- as.data.frame(tb)
setDT(tb, keep.rownames = TRUE)
fwrite(tb, paste0(dir_results, "cox_mortality_all3_ozone_summer_entryage_coef.csv"))

### race chr 3 levels ----
cox_all3_ozone_summer_race <- coxph(Surv(time = followupyr, time2 = followupyr_plusone, event = dead) ~ 
                              ozone_summer + ozone_summer*race_collapsed + no2 + pm25 +
                              mean_bmi + smoke_rate + hispanic + pct_blk + 
                              medhouseholdincome + medianhousevalue +  
                              poverty + education + popdensity + pct_owner_occ +
                              summer_tmmx + winter_tmmx + summer_rmax + winter_rmax +
                              as.factor(year) +  as.factor(region) +
                              strata(as.factor(entry_age_break)) + strata(as.factor(sex)) + 
                              strata(as.factor(race_collapsed)) + strata(as.factor(dual)),
                            data = dt,
                            tie = c("efron"), 
                            na.action = na.omit)
tb <- summary(cox_all3_ozone_summer_race)$coefficients
cov_matrix <- vcov(cox_all3_ozone_summer_race)

HR_reference <- c(tb[1,1], IQRunit, exp(tb[1,1]*IQRunit),exp((tb[1,1]-tb[1,3])*IQRunit), exp((tb[1,1]+tb[1,3])*IQRunit))
se1 <- sqrt(cov_matrix[1,1] + cov_matrix[39,39] + 2*cov_matrix[1,39])
se2 <- sqrt(cov_matrix[1,1] + cov_matrix[38,38] + 2*cov_matrix[1,38])
HR_level1 <- c(tb[1,1]+tb[39,1], IQRunit, exp((tb[1,1]+tb[39,1])*IQRunit), exp((tb[1,1]+tb[39,1]-se1)*IQRunit), exp((tb[1,1]+tb[39,1]+se1)*IQRunit))
HR_level2 <- c(tb[1,1]+tb[38,1], IQRunit, exp((tb[1,1]+tb[38,1])*IQRunit), exp((tb[1,1]+tb[38,1]-se2)*IQRunit), exp((tb[1,1]+tb[38,1]+se2)*IQRunit))

HR <- rbind(HR_reference, HR_level1, HR_level2)
print(HR)
HR <- as.data.frame(HR)
colnames(HR) <- c("coef", "IQR", "HR", "HR_lci", "HR_uci")
write.csv(HR, paste0(dir_results, "cox_mortality_all3_ozone_summer_race_HR.csv"))
tb <- as.data.frame(tb)
setDT(tb, keep.rownames = TRUE)
fwrite(tb, paste0(dir_results, "cox_mortality_all3_ozone_summer_race_coef.csv"))
