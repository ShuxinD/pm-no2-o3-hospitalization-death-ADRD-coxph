###############################################################################
# Project: Air Pollution and mortality / readmission in AD/ADRD Medicare      
# Code: Cox PH model - bootstraping
# Input: "ADRD_for_mortality.csv"                                                  
# Output: 
# Author: Shuxin Dong                                                         
# Date: 2021-07-07                                                         
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

####################### 1. prepare for bootstrap ##############################
all_zip <- unique(dt[,zip])
num_uniq_zip <- uniqueN(dt[,zip])

# Save the bootstrapped data to accelerate computing
dir.create(file.path("/nfs/home/S/shd968/shared_space/ci3_shd968/dementia/data/cox_mortality_bootstrap_temp"), showWarnings = FALSE)
lapply(1:500, function(boots_id){
  set.seed(boots_id)
  zip_sample <- sample(1:num_uniq_zip, floor(2*sqrt(num_uniq_zip)), replace=T) 
  ADRD_for_mortality_boots <- subset(ADRD_for_mortality_boots, zip %in% all_zip[zip_sample]) 
  write_fst(ADRD_for_mortality_boots, paste0("/nfs/home/S/shd968/shared_space/ci3_shd968/dementia/data/cox_mortality_bootstrap_temp/", boots_id,".fst"))
})

cox_coefs_boots<-NULL
for (boots_id in 1:500){
  set.seed(boots_id)
  ADRD_for_mortality_boots<- read_fst(paste0("/nfs/home/S/shd968/shared_space/ci3_shd968/dementia/data/cox_mortality_bootstrap_temp/", boots_id,".fst"))
  ## main model
  cox_mortality_pm25 <- coxph(Surv(time = followupyr, time2 = followupyr_plusone, event = dead) ~ 
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
  cox_coefs_boots<-c(cox_coefs_boots, summary(cox_mortality_pm25)$coefficients[1])
  rm(ADRD_for_mortality_boots)
  cat("finish ", boots_id, "of 500\n")
}

save(num_uniq_zip, cox_coefs_boots,file="/nfs/home/S/shd968/shared_space/ci3_shd968/dementia/data/cox_mortality_bootstrap_temp/cox_coefs_boots.RData")

# exp(log(1.066)+10*1.96*sd(Cox_coefs_boots) *sqrt(2*sqrt(num_uniq_zip))/sqrt(num_uniq_zip))
# exp(log(1.066)-10*1.96*sd(Cox_coefs_boots) *sqrt(2*sqrt(num_uniq_zip))/sqrt(num_uniq_zip))
