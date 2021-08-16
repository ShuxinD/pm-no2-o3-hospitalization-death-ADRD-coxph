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
dir.create(file.path("/nfs/home/S/shd968/shared_space/ci3_shd968/dementia/data/cox_mortality_bootstrap_temp"), showWarnings = FALSE)

lapply(1:500, function(boots_id){
  set.seed(boots_id)
  zip_sample <- sample(1:num_uniq_zip, floor(2*sqrt(num_uniq_zip)), replace=T) 
  dt_boots <- subset(dt, zip %in% all_zip[zip_sample]) 
  write_fst(dt_boots, paste0("/nfs/home/S/shd968/shared_space/ci3_shd968/dementia/data/cox_mortality_bootstrap_temp/", boots_id,".fst"))
  cat("finish creating data", boots_id, "of 500\n")
})

## 2. bootstrapping ------------
exposure <- c("pm25", "no2", "ozone_summer", "ox")
num_uniq_zip <- 33527L

cox_mortality_boots <- NULL
for (exposure_i in exposure) {
  cox_coefs_boots<-NULL
  for (boots_id in 1:500) {
    set.seed(boots_id)
    dt_boots<- read_fst(paste0("/nfs/home/S/shd968/shared_space/ci3_shd968/dementia/data/cox_mortality_bootstrap_temp/", boots_id,".fst"))
    ## single pollutant model
    mod <- coxph(Surv(time = followupyr, time2 = followupyr_plusone, event = dead) ~ 
                   get(exposure_i) +          
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
    cox_coefs_boots<-c(cox_coefs_boots, summary(mod)$coefficients[1])
    rm(dt_boots)
    gc()
    cat("finish", exposure_i, "sample", boots_id, "of 500\n")
  }
  save(num_uniq_zip, cox_coefs_boots, file=paste0(dir_results, "bootstrap_cox_mortality_", exposure_i, ".RData"))
#   summary_cox_coefs_boots <- c(exp(mean(cox_coefs_boots)*IQRs[, get(exposure_i)]), 
#                                exp((mean(cox_coefs_boots) - 1.96*sd(cox_coefs_boots)*sqrt(2*sqrt(num_uniq_zip))/sqrt(num_uniq_zip))*IQRs[,get(exposure_i)]),
#                                exp((mean(cox_coefs_boots) + 1.96*sd(cox_coefs_boots)*sqrt(2*sqrt(num_uniq_zip))/sqrt(num_uniq_zip))*IQRs[,get(exposure_i)]))
#   cox_mortality_boots <- rbind(cox_mortality_boots, summary_cox_coefs_boots)
}

load(file = paste0(dir_results, "bootstrap_cox_mortality_pm25.RData"))
summary_cox_coefs_boots_pm25 <- c(exp(log(1.008)),
                                  exp(log(1.008) - IQRs[,pm25]* 1.96*sd(cox_coefs_boots)*sqrt(2*sqrt(num_uniq_zip))/sqrt(num_uniq_zip)),
                                  exp(log(1.008) + IQRs[,pm25]*1.96*sd(cox_coefs_boots)*sqrt(2*sqrt(num_uniq_zip))/sqrt(num_uniq_zip)))
summary_cox_coefs_boots_pm25

load(file = paste0(dir_results, "bootstrap_cox_mortality_no2.RData"))
summary_cox_coefs_boots_no2 <- c(exp(log(1.013)),
                                  exp(log(1.013) - IQRs[,no2]* 1.96*sd(cox_coefs_boots)*sqrt(2*sqrt(num_uniq_zip))/sqrt(num_uniq_zip)),
                                  exp(log(1.013) + IQRs[,no2]*1.96*sd(cox_coefs_boots)*sqrt(2*sqrt(num_uniq_zip))/sqrt(num_uniq_zip)))
summary_cox_coefs_boots_no2

load(file = paste0(dir_results, "bootstrap_cox_mortality_ozone_summer.RData"))
summary_cox_coefs_boots_ozone_summer <- c(exp(log(1.003)),
                                  exp(log(1.003) - IQRs[,ozone_summer]* 1.96*sd(cox_coefs_boots)*sqrt(2*sqrt(num_uniq_zip))/sqrt(num_uniq_zip)),
                                  exp(log(1.003) + IQRs[,ozone_summer]*1.96*sd(cox_coefs_boots)*sqrt(2*sqrt(num_uniq_zip))/sqrt(num_uniq_zip)))
summary_cox_coefs_boots_ozone_summer

load(file = paste0(dir_results, "bootstrap_cox_mortality_ox.RData"))
summary_cox_coefs_boots_ox <- c(exp(log(1.007)),
                                  exp(log(1.007) - IQRs[,ox]* 1.96*sd(cox_coefs_boots)*sqrt(2*sqrt(num_uniq_zip))/sqrt(num_uniq_zip)),
                                  exp(log(1.007) + IQRs[,ox]*1.96*sd(cox_coefs_boots)*sqrt(2*sqrt(num_uniq_zip))/sqrt(num_uniq_zip)))
summary_cox_coefs_boots_ox

cox_mortality_boots <- rbind(summary_cox_coefs_boots_pm25, summary_cox_coefs_boots_no2, summary_cox_coefs_boots_ozone_summer, summary_cox_coefs_boots_ox)
rownames(cox_mortality_boots) <- exposure
colnames(cox_mortality_boots) <- c("HR", "HR_lci", "HR_uci")
write.table(cox_mortality_boots, file = "/nfs/home/S/shd968/shared_space/ci3_shd968/dementia/airPollution_ADRD/results/bootstrapping_cox_mortality_singlePollutant.csv")

cox_coefs_boots_pm25<-NULL
cox_coefs_boots_no2<-NULL
cox_coefs_boots_ozone_summer<-NULL
for (boots_id in 1:500) {
  set.seed(boots_id)
  dt_boots<- read_fst(paste0("/nfs/home/S/shd968/shared_space/ci3_shd968/dementia/data/cox_mortality_bootstrap_temp/", boots_id,".fst"))
  ## single pollutant model
  mod <- coxph(Surv(time = followupyr, time2 = followupyr_plusone, event = dead) ~ 
                 pm25 + no2 + ozone_summer +         
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
  cox_coefs_boots_pm25 <- c(cox_coefs_boots_pm25, summary(mod)$coefficients[1])
  cox_coefs_boots_no2 <- c(cox_coefs_boots_no2, summary(mod)$coefficients[2])
  cox_coefs_boots_ozone_summer <- c(cox_coefs_boots_ozone_summer, summary(mod)$coefficients[3])
  rm(dt_boots)
  gc()
  cat("finish 3multi sample", boots_id, "of 500\n")
}
save(num_uniq_zip, cox_coefs_boots_pm25, cox_coefs_boots_no2, cox_coefs_boots_ozone_summer, file=paste0(dir_results, "bootstrap_cox_mortality_3multi.RData"))

load(file=paste0(dir_results, "bootstrap_cox_mortality_3multi.RData"))
summary_cox_coefs_boots_pm25 <- c(exp(log(1.004)),
                                exp(log(1.004) - IQRs[,pm25]* 1.96*sd(cox_coefs_boots_pm25)*sqrt(2*sqrt(num_uniq_zip))/sqrt(num_uniq_zip)),
                                exp(log(1.004) + IQRs[,pm25]*1.96*sd(cox_coefs_boots_pm25)*sqrt(2*sqrt(num_uniq_zip))/sqrt(num_uniq_zip)))
summary_cox_coefs_boots_pm25

summary_cox_coefs_boots_no2 <- c(exp(log(1.011)),
                                  exp(log(1.011) - IQRs[,no2]* 1.96*sd(cox_coefs_boots_no2)*sqrt(2*sqrt(num_uniq_zip))/sqrt(num_uniq_zip)),
                                  exp(log(1.011) + IQRs[,no2]*1.96*sd(cox_coefs_boots_no2)*sqrt(2*sqrt(num_uniq_zip))/sqrt(num_uniq_zip)))
summary_cox_coefs_boots_no2

summary_cox_coefs_boots_ozone_summer <- c(exp(log(1.001)),
                                  exp(log(1.001) - IQRs[,ozone_summer]* 1.96*sd(cox_coefs_boots_ozone_summer)*sqrt(2*sqrt(num_uniq_zip))/sqrt(num_uniq_zip)),
                                  exp(log(1.001) + IQRs[,ozone_summer]*1.96*sd(cox_coefs_boots_ozone_summer)*sqrt(2*sqrt(num_uniq_zip))/sqrt(num_uniq_zip)))
summary_cox_coefs_boots_ozone_summer

cox_mortality_boots_3multi <- rbind(summary_cox_coefs_boots_pm25, summary_cox_coefs_boots_no2, summary_cox_coefs_boots_ozone_summer)
rownames(cox_mortality_boots_3multi) <- c("pm25" ,"no2", "ozone_summer")
colnames(cox_mortality_boots_3multi) <- c("HR", "HR_lci", "HR_uci")
cox_mortality_boots_3multi
write.table(cox_mortality_boots_3multi, file = "/nfs/home/S/shd968/shared_space/ci3_shd968/dementia/airPollution_ADRD/results/bootstrapping_cox_mortality_3multi.csv")




cox_coefs_boots_pm25<-NULL
cox_coefs_boots_ox<-NULL
for (boots_id in 1:500) {
  set.seed(boots_id)
  dt_boots<- read_fst(paste0("/nfs/home/S/shd968/shared_space/ci3_shd968/dementia/data/cox_mortality_bootstrap_temp/", boots_id,".fst"))
  ## single pollutant model
  mod <- coxph(Surv(time = followupyr, time2 = followupyr_plusone, event = dead) ~ 
                 pm25 + ox +    
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
  cox_coefs_boots_pm25 <- c(cox_coefs_boots_pm25, summary(mod)$coefficients[1])
  cox_coefs_boots_ox <- c(cox_coefs_boots_ox, summary(mod)$coefficients[2])
  rm(dt_boots)
  gc()
  cat("finish 2 multi sample", boots_id, "of 500\n")
}
save(num_uniq_zip, cox_coefs_boots_pm25, cox_coefs_boots_ox, file=paste0(dir_results, "bootstrap_cox_mortality_2multi.RData"))

load(file=paste0(dir_results, "bootstrap_cox_mortality_2multi.RData"))
summary_cox_coefs_boots_pm25 <- c(exp(log(1.006)),
                                  exp(log(1.006) - IQRs[,pm25]* 1.96*sd(cox_coefs_boots_pm25)*sqrt(2*sqrt(num_uniq_zip))/sqrt(num_uniq_zip)),
                                  exp(log(1.006) + IQRs[,pm25]*1.96*sd(cox_coefs_boots_pm25)*sqrt(2*sqrt(num_uniq_zip))/sqrt(num_uniq_zip)))
summary_cox_coefs_boots_pm25

summary_cox_coefs_boots_ox <- c(exp(log(1.005)),
                                 exp(log(1.005) - IQRs[,ox]* 1.96*sd(cox_coefs_boots_ox)*sqrt(2*sqrt(num_uniq_zip))/sqrt(num_uniq_zip)),
                                 exp(log(1.005) + IQRs[,ox]*1.96*sd(cox_coefs_boots_ox)*sqrt(2*sqrt(num_uniq_zip))/sqrt(num_uniq_zip)))
summary_cox_coefs_boots_ox

cox_mortality_boots_2multi <- rbind(summary_cox_coefs_boots_pm25, summary_cox_coefs_boots_ox)
rownames(cox_mortality_boots_2multi) <- c("pm25" ,"ox")
colnames(cox_mortality_boots_2multi) <- c("HR", "HR_lci", "HR_uci")
cox_mortality_boots_2multi
write.table(cox_mortality_boots_2multi, file = "/nfs/home/S/shd968/shared_space/ci3_shd968/dementia/airPollution_ADRD/results/bootstrapping_cox_mortality_2multi.csv")