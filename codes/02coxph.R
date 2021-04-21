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

######################## 1. single-pollutants model ############################
cox_1_pm25 <- coxph(Surv(time = followupyr, time2 = followupyr_plusone, event = dead) ~ 
                      pm25 + 
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
tb <- summary(cox_1_pm25)$coefficients
tb <- as.data.frame(tb)
setDT(tb, keep.rownames = TRUE)[]
fwrite(tb, paste0(dir_results, "cox_1_pm25_coef.csv"))

IQRunit <- IQRs$pm25
HR <- tb[1,]
HR <- cbind(HR, IQRunit)
print(HR)
HR[, `:=`(HR_IQR = exp(coef*IQRunit),
          HR_lci = exp((coef-1.96*`se(coef)`)*IQRunit),
          HR_uci = exp((coef+1.96*`se(coef)`)*IQRunit))][]
fwrite(HR, paste0(dir_results, "cox_1_pm25_HR.csv"))


cox_1_no2 <- coxph(Surv(time = followupyr, time2 = followupyr_plusone, event = dead) ~ 
                     no2 + 
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
tb <- summary(cox_1_no2)$coefficients
tb <- as.data.frame(tb)
setDT(tb, keep.rownames = TRUE)[]
fwrite(tb, paste0(dir_results, "cox_1_no2_coef.csv"))

IQRunit <- IQRs$no2
HR <- tb[1,]
HR <- cbind(HR, IQRunit)
print(HR)
HR[, `:=`(HR_IQR = exp(coef*IQRunit),
          HR_lci = exp((coef-1.96*`se(coef)`)*IQRunit),
          HR_uci = exp((coef+1.96*`se(coef)`)*IQRunit))][]
fwrite(HR, paste0(dir_results, "cox_1_no2_HR.csv"))


cox_1_ozone <- coxph(Surv(time = followupyr, time2 = followupyr_plusone, event = dead) ~ 
                       ozone + 
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
tb <- summary(cox_1_ozone)$coefficients
tb <- as.data.frame(tb)
setDT(tb, keep.rownames = TRUE)[]
fwrite(tb, paste0(dir_results, "cox_1_ozone_coef.csv"))

IQRunit <- IQRs$ozone
HR <- tb[1,]
HR <- cbind(HR, IQRunit)
print(HR)
HR[, `:=`(HR_IQR = exp(coef*IQRunit),
          HR_lci = exp((coef-1.96*`se(coef)`)*IQRunit),
          HR_uci = exp((coef+1.96*`se(coef)`)*IQRunit))][]
fwrite(HR, paste0(dir_results, "cox_1_ozone_HR.csv"))

cox_1_ox <- coxph(Surv(time = followupyr, time2 = followupyr_plusone, event = dead) ~ 
                       ox + 
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
tb <- summary(cox_1_ox)$coefficients
tb <- as.data.frame(tb)
setDT(tb, keep.rownames = TRUE)[]
fwrite(tb, paste0(dir_results, "cox_1_ox_coef.csv"))

IQRunit <- IQRs$ox
HR <- tb[1,]
HR <- cbind(HR, IQRunit)
print(HR)
HR[, `:=`(HR_IQR = exp(coef*IQRunit),
          HR_lci = exp((coef-1.96*`se(coef)`)*IQRunit),
          HR_uci = exp((coef+1.96*`se(coef)`)*IQRunit))][]
fwrite(HR, paste0(dir_results, "cox_1_ox_HR.csv"))

cox_1_ozone_summer <- coxph(Surv(time = followupyr, time2 = followupyr_plusone, event = dead) ~ 
                    ozone_summer + 
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
tb <- summary(cox_1_ozone_summer)$coefficients
tb <- as.data.frame(tb)
setDT(tb, keep.rownames = TRUE)[]
fwrite(tb, paste0(dir_results, "cox_1_ozone_summer_coef.csv"))

IQRunit <- IQRs$ozone_summer
HR <- tb[1,]
HR <- cbind(HR, IQRunit)
print(HR)
HR[, `:=`(HR_IQR = exp(coef*IQRunit),
          HR_lci = exp((coef-1.96*`se(coef)`)*IQRunit),
          HR_uci = exp((coef+1.96*`se(coef)`)*IQRunit))][]
fwrite(HR, paste0(dir_results, "cox_1_ozone_summer_HR.csv"))


######################## 2. multi-pollutants model ############################
cox_1_all3 <- coxph(Surv(time = followupyr, time2 = followupyr_plusone, event = dead) ~ 
                 pm25 + no2 + ozone + 
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
tb <- summary(cox_1_all3)$coefficients
tb <- as.data.frame(tb)
setDT(tb, keep.rownames = TRUE)[]
fwrite(tb, paste0(dir_results, "cox_1_all3_coef.csv"))

IQRunit <- c(IQRs$pm25, IQRs$no2, IQRs$ozone)
HR <- tb[1:3,]
HR <- cbind(HR,IQRunit)
print(HR)
HR[, `:=`(HR_IQR = exp(coef*IQRunit),
          HR_lci = exp((coef-1.96*`se(coef)`)*IQRunit),
          HR_uci = exp((coef+1.96*`se(coef)`)*IQRunit))][]
fwrite(HR, paste0(dir_results, "cox_1_all3_HR.csv"))

cox_1_all3_summer <- coxph(Surv(time = followupyr, time2 = followupyr_plusone, event = dead) ~ 
                      pm25 + no2 + ozone_summer + 
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
tb <- summary(cox_1_all3_summer)$coefficients
tb <- as.data.frame(tb)
setDT(tb, keep.rownames = TRUE)[]
fwrite(tb, paste0(dir_results, "cox_1_all3_summer_coef.csv"))

IQRunit <- c(IQRs$pm25, IQRs$no2, IQRs$ozone_summer)
HR <- tb[1:3,]
HR <- cbind(HR,IQRunit)
print(HR)
HR[, `:=`(HR_IQR = exp(coef*IQRunit),
          HR_lci = exp((coef-1.96*`se(coef)`)*IQRunit),
          HR_uci = exp((coef+1.96*`se(coef)`)*IQRunit))][]
fwrite(HR, paste0(dir_results, "cox_1_all3_summer_HR.csv"))

cox_1_all2 <- coxph(Surv(time = followupyr, time2 = followupyr_plusone, event = dead) ~ 
                 pm25 + ox +
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
tb <- summary(cox_1_all2)$coefficients
tb <- as.data.frame(tb)
setDT(tb, keep.rownames = TRUE)[]
fwrite(tb, paste0(dir_results, "cox_1_all2_coef.csv"))

IQRunit <- c(IQRs$pm25, IQRs$ox)
HR <- tb[1:2,]
HR <- cbind(HR,IQRunit)
print(HR)
HR[, `:=`(HR_IQR = exp(coef*IQRunit),
          HR_lci = exp((coef-1.96*`se(coef)`)*IQRunit),
          HR_uci = exp((coef+1.96*`se(coef)`)*IQRunit))][]
fwrite(HR, paste0(dir_results, "cox_1_all2_HR.csv"))

cox_splines <- coxph(Surv(time = followupyr, time2 = followupyr_plusone, event = dead) ~ 
                             pspline(pm25, df=4) + 
                       pspline(no2, df=4) + 
                       pspline(ozone, df=4) + 
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
print(cox_splines)
termplot(cox_splines, term = 1, se=TRUE, col.term = 1, col.se = 1)


# write.csv(temp$coefficients, paste0(dir_output, "cox_all.csv"))
