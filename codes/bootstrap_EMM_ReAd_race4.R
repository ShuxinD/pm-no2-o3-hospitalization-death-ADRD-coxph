## 0. setup -------------------------------------------------------------------
rm(list = ls())
gc()

library(data.table)
setDTthreads(threads = 0)
library(survival)

setwd("/nfs/home/S/shd968/shared_space/ci3_shd968/dementia/")

dir_data <- "/nfs/home/S/shd968/shared_space/ci3_shd968/dementia/data/"
dir_results <- "/nfs/home/S/shd968/shared_space/ci3_shd968/dementia/"

dt <- fread(paste0(dir_data, "ADRD_for_ReAd.csv"), colClasses = c("zip"="character"))
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

### pm25 race chr 4 levels ----
cox_pm25_race <- coxph(Surv(time = followupyr, time2 = followupyr_plusone, event = ReAd) ~ 
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
colnames(HR) <- c("coef", "IQR", "HR", "HR_lci", "HR_uci")
rownames(HR) <- c("Black", "White", "Others", "Hispanic")
print(HR)
write.table(HR, paste0(dir_results, "cox_ReAd_pm25_race4_HR.csv"))
tb <- as.data.frame(tb)
setDT(tb, keep.rownames = TRUE)
fwrite(tb, paste0(dir_results, "cox_ReAd_pm25_race_coef.csv"))

### no2 race chr 4 levels ----
cox_no2_race <- coxph(Surv(time = followupyr, time2 = followupyr_plusone, event = ReAd) ~ 
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

IQRunit <- IQRs[,no2]
HR_reference <- c(tb[1,1], IQRunit, exp(tb[1,1]*IQRunit),exp((tb[1,1]-tb[1,3])*IQRunit), exp((tb[1,1]+tb[1,3])*IQRunit))
se1 <- sqrt(cov_matrix[1,1] + cov_matrix[39,39] + 2*cov_matrix[1,39])
se2 <- sqrt(cov_matrix[1,1] + cov_matrix[38,38] + 2*cov_matrix[1,38])
se3 <- sqrt(cov_matrix[1,1] + cov_matrix[37,37] + 2*cov_matrix[1,37])
HR_level1 <- c(tb[1,1]+tb[39,1], IQRunit, exp((tb[1,1]+tb[39,1])*IQRunit), exp((tb[1,1]+tb[39,1]-se1)*IQRunit), exp((tb[1,1]+tb[39,1]+se1)*IQRunit))
HR_level2 <- c(tb[1,1]+tb[38,1], IQRunit, exp((tb[1,1]+tb[38,1])*IQRunit), exp((tb[1,1]+tb[38,1]-se2)*IQRunit), exp((tb[1,1]+tb[38,1]+se2)*IQRunit))
HR_level3 <- c(tb[1,1]+tb[37,1], IQRunit, exp((tb[1,1]+tb[37,1])*IQRunit), exp((tb[1,1]+tb[37,1]-se3)*IQRunit), exp((tb[1,1]+tb[37,1]+se3)*IQRunit))

HR <- rbind(HR_reference, HR_level1, HR_level2, HR_level3)
print(HR)
colnames(HR) <- c("coef", "IQR", "HR", "HR_lci", "HR_uci")
rownames(HR) <- c("Black", "White", "Others", "Hispanic")
print(HR)
write.table(HR, paste0(dir_results, "cox_ReAd_no2_race4_HR.csv"))
tb <- as.data.frame(tb)
setDT(tb, keep.rownames = TRUE)
fwrite(tb, paste0(dir_results, "cox_ReAd_no2_race_coef.csv"))

rm(cox_no2_race)
gc()

### ozone_summer race chr 4 levels ----
cox_ozone_summer_race <- coxph(Surv(time = followupyr, time2 = followupyr_plusone, event = ReAd) ~ 
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

IQRunit <- IQRs[,ozone_summer]
HR_reference <- c(tb[1,1], IQRunit, exp(tb[1,1]*IQRunit),exp((tb[1,1]-tb[1,3])*IQRunit), exp((tb[1,1]+tb[1,3])*IQRunit))
se1 <- sqrt(cov_matrix[1,1] + cov_matrix[39,39] + 2*cov_matrix[1,39])
se2 <- sqrt(cov_matrix[1,1] + cov_matrix[38,38] + 2*cov_matrix[1,38])
se3 <- sqrt(cov_matrix[1,1] + cov_matrix[37,37] + 2*cov_matrix[1,37])
HR_level1 <- c(tb[1,1]+tb[39,1], IQRunit, exp((tb[1,1]+tb[39,1])*IQRunit), exp((tb[1,1]+tb[39,1]-se1)*IQRunit), exp((tb[1,1]+tb[39,1]+se1)*IQRunit))
HR_level2 <- c(tb[1,1]+tb[38,1], IQRunit, exp((tb[1,1]+tb[38,1])*IQRunit), exp((tb[1,1]+tb[38,1]-se2)*IQRunit), exp((tb[1,1]+tb[38,1]+se2)*IQRunit))
HR_level3 <- c(tb[1,1]+tb[37,1], IQRunit, exp((tb[1,1]+tb[37,1])*IQRunit), exp((tb[1,1]+tb[37,1]-se3)*IQRunit), exp((tb[1,1]+tb[37,1]+se3)*IQRunit))

HR <- rbind(HR_reference, HR_level1, HR_level2, HR_level3)
print(HR)
colnames(HR) <- c("coef", "IQR", "HR", "HR_lci", "HR_uci")
rownames(HR) <- c("Black", "White", "Others", "Hispanic")
print(HR)
write.table(HR, paste0(dir_results, "cox_ReAd_ozone_summer_race4_HR.csv"))
tb <- as.data.frame(tb)
setDT(tb, keep.rownames = TRUE)
fwrite(tb, paste0(dir_results, "cox_ReAd_ozone_summer_race_coef.csv"))

rm(cox_ozone_summer_race)
gc()

### ox race chr 4 levels ----
cox_ox_race <- coxph(Surv(time = followupyr, time2 = followupyr_plusone, event = ReAd) ~ 
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

IQRunit <- IQRs[,ox]
HR_reference <- c(tb[1,1], IQRunit, exp(tb[1,1]*IQRunit),exp((tb[1,1]-tb[1,3])*IQRunit), exp((tb[1,1]+tb[1,3])*IQRunit))
se1 <- sqrt(cov_matrix[1,1] + cov_matrix[39,39] + 2*cov_matrix[1,39])
se2 <- sqrt(cov_matrix[1,1] + cov_matrix[38,38] + 2*cov_matrix[1,38])
se3 <- sqrt(cov_matrix[1,1] + cov_matrix[37,37] + 2*cov_matrix[1,37])
HR_level1 <- c(tb[1,1]+tb[39,1], IQRunit, exp((tb[1,1]+tb[39,1])*IQRunit), exp((tb[1,1]+tb[39,1]-se1)*IQRunit), exp((tb[1,1]+tb[39,1]+se1)*IQRunit))
HR_level2 <- c(tb[1,1]+tb[38,1], IQRunit, exp((tb[1,1]+tb[38,1])*IQRunit), exp((tb[1,1]+tb[38,1]-se2)*IQRunit), exp((tb[1,1]+tb[38,1]+se2)*IQRunit))
HR_level3 <- c(tb[1,1]+tb[37,1], IQRunit, exp((tb[1,1]+tb[37,1])*IQRunit), exp((tb[1,1]+tb[37,1]-se3)*IQRunit), exp((tb[1,1]+tb[37,1]+se3)*IQRunit))

HR <- rbind(HR_reference, HR_level1, HR_level2, HR_level3)
print(HR)
colnames(HR) <- c("coef", "IQR", "HR", "HR_lci", "HR_uci")
rownames(HR) <- c("Black", "White", "Others", "Hispanic")
print(HR)
write.table(HR, paste0(dir_results, "cox_ReAd_ox_race4_HR.csv"))
tb <- as.data.frame(tb)
setDT(tb, keep.rownames = TRUE)
fwrite(tb, paste0(dir_results, "cox_ReAd_ox_race_coef.csv"))

rm(cox_ox_race)
gc()