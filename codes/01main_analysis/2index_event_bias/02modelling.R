###############################################################################
#' Project: Air Pollution on mortality and readmission in Medicare AD/ADRD
#' Code: coxph with ip weights
#' Input: ...
#' Output: "" 
#' Author: Shuxin Dong
#' Date: 2021-09-15
###############################################################################

############################# 0. Setup ########################################
rm(list = ls())
gc()

library(data.table)
library(survival)
library(mgcv)

dir_in <- "/nfs/home/S/shd968/shared_space/ci3_shd968/dementia/data/"
dir_out <- "/nfs/home/S/shd968/shared_space/ci3_shd968/dementia/"

dtipw_mortality <- fread(paste0(dir_in, "index_event_bias_dt_mortality.csv"))
dtipw_mortality[, `:=`(region_NORTHEAST=(region=="NORTHEAST"),
                    region_SOUTH=(region=="SOUTH"),
                    region_MIDWEST=(region=="MIDWEST"),
                    region_WEST=(region=="WEST"))]
dtipw_mortality <- na.omit(dtipw_mortality)
dtipw_ReAd <- fread(paste0(dir_in, "index_event_bias_dt_ReAd.csv"))
dtipw_ReAd[, `:=`(region_NORTHEAST=(region=="NORTHEAST"),
               region_SOUTH=(region=="SOUTH"),
               region_MIDWEST=(region=="MIDWEST"),
               region_WEST=(region=="WEST"))]
dtipw_ReAd <- na.omit(dtipw_ReAd)

dt_mortality <- fread(paste0(dir_in, "ADRD_for_mortality.csv"), colClasses = c("zip"="character"))
dt_mortality[, followupyr := (year - firstADRDyr)]
dt_mortality[, followupyr_plusone := followupyr +1]
dt_mortality[, statecode := as.factor(statecode)]

NORTHEAST <- c("NY", "MA", "PA", "RI", "NH", "ME", "VT", "CT", "NJ")  
SOUTH <- c("DC", "VA", "NC", "WV", "KY", "SC", "GA", "FL", "AL", "TN", "MS", 
           "AR", "MD", "DE", "OK", "TX", "LA")
MIDWEST <- c("OH", "IN", "MI", "IA", "MO", "WI", "MN", "SD", "ND", "IL", "KS", "NE")
WEST <- c("MT", "CO", "WY", "ID", "UT", "NV", "CA", "OR", "WA", "AZ", "NM")

dt_mortality$region <- ifelse(dt_mortality$statecode %in% NORTHEAST, "NORTHEAST",
                    ifelse(dt_mortality$statecode %in% SOUTH, "SOUTH",
                           ifelse(dt_mortality$statecode  %in% MIDWEST, "MIDWEST",
                                  ifelse(dt_mortality$statecode  %in% WEST, "WEST",
                                         NA))))
dt_mortality[, region:=as.factor(region)]


dt_ReAd <- fread(paste0(dir_in, "ADRD_for_ReAd.csv"), colClasses = c("zip"="character"))
dt_ReAd[, followupyr := (year - firstADRDyr)]
dt_ReAd[, followupyr_plusone := followupyr +1]
dt_ReAd[, statecode := as.factor(statecode)]
dt_ReAd$region <- ifelse(dt_ReAd$statecode %in% NORTHEAST, "NORTHEAST",
                              ifelse(dt_ReAd$statecode %in% SOUTH, "SOUTH",
                                     ifelse(dt_ReAd$statecode  %in% MIDWEST, "MIDWEST",
                                            ifelse(dt_ReAd$statecode  %in% WEST, "WEST",
                                                   NA))))
dt_ReAd[, region:=as.factor(region)]

IQRs_mortality <- data.table(
  IQR(dt_mortality[,pm25]), IQR(dt_mortality[,no2]), IQR(dt_mortality[,ozone_summer]), IQR(dt_mortality[,ox]))
colnames(IQRs_mortality) <- c("pm25", "no2", "ozone_summer", "ox")
print(IQRs_mortality)

IQRs_ReAd <- data.table(
  IQR(dt_ReAd[,pm25]), IQR(dt_ReAd[,no2]), IQR(dt_ReAd[,ozone_summer]), IQR(dt_ReAd[,ox]))
colnames(IQRs_ReAd) <- c("pm25", "no2", "ozone_summer", "ox")
print(IQRs_ReAd)

## 1. models ------------------------------------------------------------------
pollutants <- c("pm25", "no2", "ozone_summer", "ox")

## 1.1 mortality  -------------------------------------------------------------
for (pollutants_i in pollutants) {
  cat("FITTING ", pollutants_i, "\n")
  mod <- lm(get(paste0(pollutants_i,"_e2")) ~ get(pollutants_i) + 
              mean_bmi + smoke_rate + hispanic + pct_blk + 
              medhouseholdincome + medianhousevalue +  
              poverty + education + popdensity + pct_owner_occ +
              summer_tmmx + winter_tmmx + summer_rmax + winter_rmax +
              as.factor(year) + 
              region_NORTHEAST + region_SOUTH + region_MIDWEST + region_WEST,
            data = dtipw_mortality)
  cat("fit PS model\n")
  IEBweights <- 
    dnorm(dtipw_mortality[,get(pollutants_i)], mean(dtipw_mortality[,get(pollutants_i)]), sd(dtipw_mortality[,get(pollutants_i)])) /
    dnorm(dtipw_mortality[,get(pollutants_i)], predict(mod, dtipw_mortality), summary(mod)$sigma)
  temp_ipw <- data.table(qid = dtipw_mortality[,qid],
                     IEBweights = IEBweights)
  dt <- merge(dt_mortality, temp_ipw, by = "qid")
  cat("merge ipw into .. \n")
  rm(temp_ipw)
  cat("fit coxph model\n")
  cox <- coxph(Surv(time = followupyr, time2 = followupyr_plusone, event = dead) ~ 
          get(pollutants_i) + 
          mean_bmi + smoke_rate + hispanic + pct_blk + 
          medhouseholdincome + medianhousevalue +  
          poverty + education + popdensity + pct_owner_occ +
          summer_tmmx + winter_tmmx + summer_rmax + winter_rmax +
          as.factor(year) +  as.factor(region) +
          strata(as.factor(entry_age_break)) + strata(as.factor(sex)) + 
          strata(as.factor(race_collapsed)) + strata(as.factor(dual)),
        data = dt,
        weights = dt[,IEBweights],
        tie = c("efron"), 
        na.action = na.omit)
  tb <- summary(cox)$coefficients
  tb <- as.data.frame(tb)
  setDT(tb, keep.rownames = TRUE)
  fwrite(tb, paste0(dir_out, "mortality_", pollutants_i, ".csv"))
  cat("output coefs...\n")
  HR <- tb[1,]
  HR <- cbind(HR, IQRs_mortality[, get(pollutants_i)])
  HR[, `:=`(HR_IQR = exp(coef*IQRs_mortality[, get(pollutants_i)]),
            HR_lci = exp((coef-1.96*`se(coef)`)*IQRs_mortality[, get(pollutants_i)]),
            HR_uci = exp((coef+1.96*`se(coef)`)*IQRs_mortality[, get(pollutants_i)]))][]
  fwrite(HR, paste0(dir_out, "mortality_", pollutants_i, "_HR.csv"))
  cat("output HR...\n")
}
## 1.2 ReAd  -------------------------------------------------------------
for (pollutants_i in pollutants) {
  cat("FITTING ", pollutants_i, "\n")
  mod <- lm(get(paste0(pollutants_i,"_e2")) ~ get(pollutants_i) + 
              mean_bmi + smoke_rate + hispanic + pct_blk + 
              medhouseholdincome + medianhousevalue +  
              poverty + education + popdensity + pct_owner_occ +
              summer_tmmx + winter_tmmx + summer_rmax + winter_rmax +
              as.factor(year) + 
              region_NORTHEAST + region_SOUTH + region_MIDWEST + region_WEST,
            data = dtipw_ReAd)
  cat("fit PS model\n")
  IEBweights <- 
    dnorm(dtipw_ReAd[,get(pollutants_i)], mean(dtipw_ReAd[,get(pollutants_i)]), sd(dtipw_ReAd[,get(pollutants_i)])) /
    dnorm(dtipw_ReAd[,get(pollutants_i)], predict(mod, dtipw_ReAd), summary(mod)$sigma)
  temp_ipw <- data.table(qid = dtipw_ReAd[,qid],
                         IEBweights = IEBweights)
  dt <- merge(dt_ReAd, temp_ipw, by = "qid")
  cat("merge ipw into .. \n")
  rm(temp_ipw)
  cat("fit coxph model\n")
  cox <- coxph(Surv(time = followupyr, time2 = followupyr_plusone, event = ReAd) ~ 
                 get(pollutants_i) + 
                 mean_bmi + smoke_rate + hispanic + pct_blk + 
                 medhouseholdincome + medianhousevalue +  
                 poverty + education + popdensity + pct_owner_occ +
                 summer_tmmx + winter_tmmx + summer_rmax + winter_rmax +
                 as.factor(year) +  as.factor(region) +
                 strata(as.factor(entry_age_break)) + strata(as.factor(sex)) + 
                 strata(as.factor(race_collapsed)) + strata(as.factor(dual)),
               data = dt,
               weights = dt[,IEBweights],
               tie = c("efron"), 
               na.action = na.omit)
  tb <- summary(cox)$coefficients
  tb <- as.data.frame(tb)
  setDT(tb, keep.rownames = TRUE)
  fwrite(tb, paste0(dir_out, "ReAd_", pollutants_i, ".csv"))
  cat("output coefs...\n")
  HR <- tb[1,]
  HR <- cbind(HR, IQRs_ReAd[, get(pollutants_i)])
  HR[, `:=`(HR_IQR = exp(coef*IQRs_ReAd[, get(pollutants_i)]),
            HR_lci = exp((coef-1.96*`se(coef)`)*IQRs_ReAd[, get(pollutants_i)]),
            HR_uci = exp((coef+1.96*`se(coef)`)*IQRs_ReAd[, get(pollutants_i)]))][]
  fwrite(HR, paste0(dir_out, "ReAd_", pollutants_i, "_HR.csv"))
  cat("output HR...\n")
}
