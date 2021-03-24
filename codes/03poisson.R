###############################################################################
# Project: Air Pollution and mortality / readmission in AD/ADRD Medicare      #
# Code: Poisson survival model
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
dt[, strata := I(as.factor(entry_age_break):as.factor(sex):as.factor(race):as.factor(dual))][]
head(dt)

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
gc()

IQRs <- data.table(IQR(dt$pm25), IQR(dt$no2), IQR(dt$ozone))
colnames(IQRs) <- c("pm25", "no2", "ozone")
print(IQRs)

############################# 1. fit models ###################################
library(parallel)
library(mgcv)
numCores <- detectCores()
cl <- makeCluster(numCores-1)

# cox_raw <- coxph(Surv(time = followupyr, time2 = followupyr_plusone, event = dead) ~
#                    pm25 + no2 + ozone +
#                    mean_bmi + smoke_rate + hispanic + pct_blk +
#                    medhouseholdincome + medianhousevalue +
#                    poverty + education + popdensity + pct_owner_occ +
#                    summer_tmmx + winter_tmmx + summer_rmax + winter_rmax +
#                    as.factor(year) +  as.factor(region) +
#                    strata(as.factor(entry_age_break)) + strata(as.factor(sex)) +
#                    strata(as.factor(race)) + strata(as.factor(dual)),
#                  data = dt,
#                  tie = c("efron"),
#                  na.action = na.omit)

p_raw <- bam(dead ~ s(followupyr, by = "strata") + 
                 pm25 + no2 + ozone + 
                 mean_bmi + smoke_rate + hispanic + pct_blk +
                 medhouseholdincome + medianhousevalue +
                 poverty + education + popdensity + pct_owner_occ +
                 summer_tmmx + winter_tmmx + summer_rmax + winter_rmax +
                 as.factor(year) + as.factor(region),
               family="poisson", data = dt,
               cluster = cl, nthreads = NA)
# saveRDS(p_raw, paste0(dir_results, "p_raw.rds"))
tb <- summary(cox_raw)$coefficients
tb <- as.data.frame(tb)
setDT(tb, keep.rownames = TRUE)[]
fwrite(tb, paste0(dir_results, "poisson_raw.csv"))

IQRunit <- c(IQRs$pm25, IQRs$no2, IQRs$ozone)
HR <- tb[1:3,]
HR <- cbind(HR,IQRunit)
print(HR)
HR[, `:=`(HR_IQR = exp(coef*IQRunit),
          HR_lci = exp((coef-1.96*`se(coef)`)*IQRunit),
          HR_uci = exp((coef+1.96*`se(coef)`)*IQRunit))][]
fwrite(HR, paste0(dir_results, "poisson_raw_HR.csv"))

gc()

p_splines <- bam(dead ~ s(followupyr, by = "strata") + 
               s(pm25) + s(no2) + s(ozone) + 
               mean_bmi + smoke_rate + hispanic + pct_blk +
               medhouseholdincome + medianhousevalue +
               poverty + education + popdensity + pct_owner_occ +
               summer_tmmx + winter_tmmx + summer_rmax + winter_rmax +
               as.factor(year) + as.factor(region),
             family="poisson", data = dt,
             cluster = cl, nthreads = NA)

# s.P_all <- bam(dead ~ s(followupyr, by = "strata") + 
#                  pm25 + no2 + ozone + 
#                  s(age) +
#                  s(mean_bmi) + s(smoke_rate) + s(hispanic) + s(pct_blk) +
#                  s(medhouseholdincome) + s(medianhousevalue) +
#                  s(poverty) + s(education) + s(popdensity) + s(pct_owner_occ) +
#                  s(summer_tmmx) + s(winter_tmmx) + s(summer_rmax) + s(winter_rmax) +
#                  as.factor(year),
#                family="poisson", data = dt,
#                cluster = cl, nthreads = NA)
