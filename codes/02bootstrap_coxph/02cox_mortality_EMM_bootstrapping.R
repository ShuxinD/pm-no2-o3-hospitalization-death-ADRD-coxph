###############################################################################
# Project: Air Pollution and mortality / readmission in AD/ADRD Medicare      
# Code: Cox PH model - bootstraping
# Input: "ADRD_for_mortality.csv"                                                  
# Output: 
# Author: Shuxin Dong                                                         
# Date: 2021-07-07                                                         
###############################################################################

## 0. Setup---------------------
rm(list = ls())
gc()

library(data.table)
setDTthreads(threads = 0)
library(survival)
library(fst)

setwd("/nfs/home/S/shd968/shared_space/ci3_shd968/dementia/")

dir_data <- "/nfs/home/S/shd968/shared_space/ci3_shd968/dementia/data/"
dir_results <- "/nfs/home/S/shd968/shared_space/ci3_shd968/dementia/bootstrapping_results/"

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

## 1. prepare for bootstrap ------------
all_zip <- unique(dt[,zip])
num_uniq_zip <- uniqueN(dt[,zip])

# Save the bootstrapped data to accelerate computing
# dir.create(file.path("/nfs/home/S/shd968/shared_space/ci3_shd968/dementia/data/cox_mortality_bootstrap_temp"), showWarnings = FALSE)

# lapply(1:1000, function(boots_id){
#  set.seed(boots_id)
#  zip_sample <- sample(1:num_uniq_zip, floor(2*sqrt(num_uniq_zip)), replace=T) 
#  dt_boots <- subset(dt, zip %in% all_zip[zip_sample]) 
#  write_fst(dt_boots, paste0("/nfs/home/S/shd968/shared_space/ci3_shd968/dementia/data/cox_mortality_bootstrap_temp/", boots_id,".fst"))
#  cat("finish creating data", boots_id, "of 1000\n")
# })

## 2. bootstrapping ------------
exposure <- c("pm25", "no2", "ozone_summer", "ox")
modifier <- c("sex_female", "dual", "entry_age_over85", "above_median_popdensity", "race_collapsed")
num_uniq_zip <- 33527L

## sex_female ------
cox_mortality_sex_boots <- NULL
pm25_HR <- c(1.009,
             1.007)
no2_HR <- c(1.009,
             1.015)
ozone_summer_HR <- c( 1.002,            
             1.004)
ox_HR <- c(1.003,
             1.009)
for (exposure_i in exposure){
  cox_boots_coefs_reference <- NULL
  cox_boots_coefs_level1 <- NULL
  for (boots_id in 1:500) {
    dt_boots <- read_fst(paste0("/nfs/home/S/shd968/shared_space/ci3_shd968/dementia/data/cox_mortality_bootstrap_temp/", boots_id,".fst"))
    dt_boots$sex_female <- dt_boots$sex-1
#    dt_boots[, entry_age_over85 := entry_age>85]
#    median_popdensity <- median(dt_boots[, popdensity])
#     dt_boots[, above_median_popdensity := popdensity>median_popdensity]
    mod <- coxph(Surv(time = followupyr, time2 = followupyr_plusone, event = dead) ~ 
                   get(exposure_i) + I(get(exposure_i)*sex_female) +
                   mean_bmi + smoke_rate + hispanic + pct_blk + 
                   medhouseholdincome + medianhousevalue +  
                   poverty + education + popdensity + pct_owner_occ +
                   summer_tmmx + winter_tmmx + summer_rmax + winter_rmax +
                   as.factor(year) +  as.factor(region) +
                   strata(as.factor(entry_age_break)) + strata(as.factor(sex)) + 
                   strata(as.factor(race_collapsed)) + strata(as.factor(dual)),
                 data = dt_boots,
                 tie = c("efron"), 
                 na.action = na.omit)
    cox_boots_coefs_reference <- c(cox_boots_coefs_reference,summary(mod)$coefficients[1] )
    cox_boots_coefs_level1 <- c(cox_boots_coefs_level1, summary(mod)$coefficients[1] + summary(mod)$coefficients[2])
    rm(dt_boots)
    gc()
    cat("finish", exposure_i, "sample", boots_id, "of 500\n")
  }
  save(num_uniq_zip, cox_boots_coefs_reference, cox_boots_coefs_level1, file=paste0(dir_results, "bootstrap_cox_mortality_sex_", exposure_i, ".RData"))
}

load(file=paste0(dir_results, "bootstrap_cox_mortality_sex_", "ox", ".RData"))
summary_cox_boots_coefs_reference <- c(exp(log(get(paste0("ox","_HR"))[1])), 
                                       exp(log(get(paste0("ox","_HR"))[1]) - IQRs[,get("ox")]*1.96*sd(cox_boots_coefs_reference)*sqrt(2*sqrt(num_uniq_zip))/sqrt(num_uniq_zip)),
                                       exp(log(get(paste0("ox","_HR"))[1]) + IQRs[,get("ox")]*1.96*sd(cox_boots_coefs_reference)*sqrt(2*sqrt(num_uniq_zip))/sqrt(num_uniq_zip)))
summary_cox_boots_coefs_level1 <- c(exp(log(get(paste0("ox","_HR"))[2])), 
                                    exp(log(get(paste0("ox","_HR"))[2]) - IQRs[,get("ox")]*1.96*sd(cox_boots_coefs_level1)*sqrt(2*sqrt(num_uniq_zip))/sqrt(num_uniq_zip)),
                                    exp(log(get(paste0("ox","_HR"))[2]) + IQRs[,get("ox")]*1.96*sd(cox_boots_coefs_level1)*sqrt(2*sqrt(num_uniq_zip))/sqrt(num_uniq_zip)))
cox_mortality_sex_boots <- rbind(cox_mortality_sex_boots, summary_cox_boots_coefs_reference)
cox_mortality_sex_boots <- rbind(cox_mortality_sex_boots, summary_cox_boots_coefs_level1)
print(cox_mortality_sex_boots)

cox_mortality_sex_boots <- cbind(cox_mortality_sex_boots, c(rep("pm25",2),rep("no2",2),rep("ozone_summer",2),rep("ox",2)))

write.table(cox_mortality_sex_boots, file = "/nfs/home/S/shd968/shared_space/ci3_shd968/dementia/airPollution_ADRD/results/bootstrapping_cox_mortality_EMMsex.csv")

## dual ------
cox_mortality_dual_boots <- NULL
pm25_HR <- c(1.005,
             1.012)
no2_HR <- c(1.019,
            1.001)
ozone_summer_HR <- c( 1.002,            
                      1.005)
ox_HR <- c(1.008,
           1.005)
for (exposure_i in exposure){
  cox_boots_coefs_reference <- NULL
  cox_boots_coefs_level1 <- NULL
  for (boots_id in 1:500) {
    dt_boots <- read_fst(paste0("/nfs/home/S/shd968/shared_space/ci3_shd968/dementia/data/cox_mortality_bootstrap_temp/", boots_id,".fst"))
    #dt_boots$sex_female <- dt_boots$sex-1
    #    dt_boots[, entry_age_over85 := entry_age>85]
    #    median_popdensity <- median(dt_boots[, popdensity])
    #     dt_boots[, above_median_popdensity := popdensity>median_popdensity]
    mod <- coxph(Surv(time = followupyr, time2 = followupyr_plusone, event = dead) ~ 
                   get(exposure_i) + I(get(exposure_i)*dual) +
                   mean_bmi + smoke_rate + hispanic + pct_blk + 
                   medhouseholdincome + medianhousevalue +  
                   poverty + education + popdensity + pct_owner_occ +
                   summer_tmmx + winter_tmmx + summer_rmax + winter_rmax +
                   as.factor(year) +  as.factor(region) +
                   strata(as.factor(entry_age_break)) + strata(as.factor(sex)) + 
                   strata(as.factor(race_collapsed)) + strata(as.factor(dual)),
                 data = dt_boots,
                 tie = c("efron"), 
                 na.action = na.omit)
    cox_boots_coefs_reference <- c(cox_boots_coefs_reference,summary(mod)$coefficients[1] )
    cox_boots_coefs_level1 <- c(cox_boots_coefs_level1, summary(mod)$coefficients[1] + summary(mod)$coefficients[2])
    rm(dt_boots)
    gc()
    cat("finish", exposure_i, "sample", boots_id, "of 500\n")
  }
  save(num_uniq_zip, cox_boots_coefs_reference, cox_boots_coefs_level1, file=paste0(dir_results, "bootstrap_cox_mortality_dual_", exposure_i, ".RData"))
  summary_cox_boots_coefs_reference <- c(exp(log(get(paste0(exposure_i,"_HR"))[1])), 
                                         exp(log(get(paste0(exposure_i,"_HR"))[1]) - IQRs[,get(exposure_i)]*1.96*sd(cox_boots_coefs_reference)*sqrt(2*sqrt(num_uniq_zip))/sqrt(num_uniq_zip)),
                                         exp(log(get(paste0(exposure_i,"_HR"))[1]) + IQRs[,get(exposure_i)]*1.96*sd(cox_boots_coefs_reference)*sqrt(2*sqrt(num_uniq_zip))/sqrt(num_uniq_zip)))
  summary_cox_boots_coefs_level1 <- c(exp(log(get(paste0(exposure_i,"_HR"))[2])), 
                                      exp(log(get(paste0(exposure_i,"_HR"))[2]) - IQRs[,get(exposure_i)]*1.96*sd(cox_boots_coefs_level1)*sqrt(2*sqrt(num_uniq_zip))/sqrt(num_uniq_zip)),
                                      exp(log(get(paste0(exposure_i,"_HR"))[2]) + IQRs[,get(exposure_i)]*1.96*sd(cox_boots_coefs_level1)*sqrt(2*sqrt(num_uniq_zip))/sqrt(num_uniq_zip)))
  cox_mortality_dual_boots <- rbind(cox_mortality_dual_boots, summary_cox_boots_coefs_reference, summary_cox_boots_coefs_level1)
  print(cox_mortality_dual_boots)
}

## race chr 4 levels ------


# exp(log(1.008)+IQRs[, pm25]*1.96*sd(cox_coefs_boots) *sqrt(2*sqrt(num_uniq_zip))/sqrt(num_uniq_zip))
# exp(log(1.066)-10*1.96*sd(Cox_coefs_boots) *sqrt(2*sqrt(num_uniq_zip))/sqrt(num_uniq_zip))
