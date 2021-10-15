#' Project: Air Pollution and mortality / readmission in AD/ADRD Medicare 
#' Code: Cox PH 
#' Input: "ADRD_for_mortality.csv"                                                  
#' Output: model specific results
#' Author: Shuxin Dong
#' First create date: 2021-02-17

# 0. setup ----
rm(list = ls())
gc()

library(data.table)
setDTthreads(threads = 0)
library(survival)

setwd("/nfs/home/S/shd968/shared_space/ci3_shd968/dementia/")
dir_data <- "/nfs/home/S/shd968/shared_space/ci3_shd968/dementia/data/"

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

# 1. single-pollutants model ----
dir_out <- "/nfs/home/S/shd968/shared_space/ci3_shd968/dementia/airPollution_ADRD/results/main_analyses/coxph_mortality/"
pollutants <- c("pm25", "no2", "ozone_summer", "ox")

for (pollutants_i in pollutants){
  cat("fit coxph model", pollutants_i, "\n")
  cox <- coxph(Surv(time = followupyr, time2 = followupyr_plusone, event = dead) ~ 
                 get(pollutants_i) + 
                 mean_bmi + smoke_rate + 
                 hispanic + pct_blk + medhouseholdincome + medianhousevalue + poverty + education + popdensity + pct_owner_occ +
                 summer_tmmx + winter_tmmx + summer_rmax + winter_rmax +
                 as.factor(year) +  as.factor(region) +
                 strata(as.factor(entry_age_break)) + strata(as.factor(sex)) + strata(as.factor(race_collapsed)) + strata(as.factor(dual)),
               data = dt,
               tie = c("efron"), 
               na.action = na.omit)
  tb <- summary(cox)$coefficients
  tb <- as.data.frame(tb)
  setDT(tb, keep.rownames = TRUE)
  fwrite(tb, paste0(dir_out, "cox_mortality_", pollutants_i, ".csv"))
  cat("output coefs...\n")
  HR <- tb[1,]
  HR <- cbind(HR, IQRs_mortality[, get(pollutants_i)])
  HR[, `:=`(HR_IQR = exp(coef*IQRs_mortality[, get(pollutants_i)]),
            HR_lci = exp((coef-1.96*`se(coef)`)*IQRs_mortality[, get(pollutants_i)]),
            HR_uci = exp((coef+1.96*`se(coef)`)*IQRs_mortality[, get(pollutants_i)]))][]
  fwrite(HR, paste0(dir_out, "cox_mortality_", pollutants_i, "_HR.csv"))
  cat("save HR for cox mortality", pollutants_i, "\n")
}

## 2. multi-pollutants model ----
cat("estimate cox for 3-pollutant model \n")
cox_all3 <- coxph(Surv(time = followupyr, time2 = followupyr_plusone, event = dead) ~ 
                    pm25 + no2 + ozone_summer + 
                    mean_bmi + smoke_rate + 
                    hispanic + pct_blk + medhouseholdincome + medianhousevalue + poverty + education + popdensity + pct_owner_occ +
                    summer_tmmx + winter_tmmx + summer_rmax + winter_rmax +
                    as.factor(year) +  as.factor(region) +
                    strata(as.factor(entry_age_break)) + strata(as.factor(sex)) + strata(as.factor(race_collapsed)) + strata(as.factor(dual)),
                  data = dt,
                  tie = c("efron"), 
                  na.action = na.omit)
tb <- summary(cox_all3)$coefficients
tb <- as.data.frame(tb)
setDT(tb, keep.rownames = TRUE)[]
fwrite(tb, paste0(dir_results, "cox_mortality_all3.csv"))

IQRunit <- c(IQRs$pm25, IQRs$no2, IQRs$ozone_summer)
HR <- tb[1:3,]
HR <- cbind(HR,IQRunit)
cat("output HR for cox 3-pollutant model \n")
HR[, `:=`(HR_IQR = exp(coef*IQRunit),
          HR_lci = exp((coef-1.96*`se(coef)`)*IQRunit),
          HR_uci = exp((coef+1.96*`se(coef)`)*IQRunit))][]
fwrite(HR, paste0(dir_results, "cox_mortality_all3_HR.csv"))

cat("estimate cox for 2-pollutant model \n")
cox_all2 <- coxph(Surv(time = followupyr, time2 = followupyr_plusone, event = dead) ~ 
                    pm25 + ox +
                    mean_bmi + smoke_rate + 
                    hispanic + pct_blk + medhouseholdincome + medianhousevalue + poverty + education + popdensity + pct_owner_occ +
                    summer_tmmx + winter_tmmx + summer_rmax + winter_rmax +
                    as.factor(year) +  as.factor(region) +
                    strata(as.factor(entry_age_break)) + strata(as.factor(sex)) + strata(as.factor(race_collapsed)) + strata(as.factor(dual)),
                  data = dt,
                  tie = c("efron"), 
                  na.action = na.omit)
tb <- summary(cox_1_all2)$coefficients
tb <- as.data.frame(tb)
setDT(tb, keep.rownames = TRUE)[]
fwrite(tb, paste0(dir_results, "cox_mortality_all2.csv"))

IQRunit <- c(IQRs$pm25, IQRs$ox)
HR <- tb[1:2,]
HR <- cbind(HR,IQRunit)
cat("output HR for cox 3-pollutant model \n")
HR[, `:=`(HR_IQR = exp(coef*IQRunit),
          HR_lci = exp((coef-1.96*`se(coef)`)*IQRunit),
          HR_uci = exp((coef+1.96*`se(coef)`)*IQRunit))][]
fwrite(HR, paste0(dir_results, "cox_mortality_all2_HR.csv"))

## 3. splines ----
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
