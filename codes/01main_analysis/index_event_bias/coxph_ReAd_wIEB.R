#' Project: airPollution_ADRD
#' Code: Cox PH 
#' Input: "ADRDcohort_ReAd.fst"                                                  
#' Output: model specific results
#' Author: Shuxin Dong
#' First create date: 2021-02-17

# setup ----
rm(list = ls())
gc()

library(data.table)
library(fst)
setDTthreads(threads = 0)
library(survival)

setwd("/nfs/home/S/shd968/shared_space/ci3_shd968/dementia/")
dir_data <- "/nfs/home/S/shd968/shared_space/ci3_shd968/dementia/data/"

## load data ----
dt <- read_fst(paste0(dir_data, "ADRDcohort_ReAd.fst"), as.data.table = T)
IPWs <- read_fst(paste0(dir_data, "ADRDcohort_ReAd_ipw_ieb.fst"), as.data.table = T)
deadIPWs <- read_fst(paste0(dir_data, "ADRDcohort_ReAd_deadipw.fst"), as.data.table = T)

names(dt)
names(IPWs)
names(deadIPWs)
dt <- merge(dt, IPWs, by = c("qid", "year"))
dt <- merge(dt, deadIPWs, by = c("qid", "year"))
gc()
names(dt)

dt[, dual := as.numeric(dual)]
dt[, followupyr_start := (year - firstADRDyr-1)]
dt[, followupyr_end := (year - firstADRDyr)]


IQRs <- data.table(IQR(dt$pm25), IQR(dt$no2), IQR(dt$ozone), IQR(dt$ox), IQR(dt$ozone_summer))
colnames(IQRs) <- c("pm25", "no2", "ozone", "ox", "ozone_summer")
print(IQRs)

## single-pollutants model ----
dir_out <- "/nfs/home/S/shd968/shared_space/ci3_shd968/dementia/airPollution_ADRD/results/main_analyses/index_event_bias/coxph_ReAd/"
pollutants <- c("pm25", "no2", "ozone_summer", "ox")

for (pollutants_i in pollutants){
  cat("fit coxph model", pollutants_i, "\n")
  temp <- dt[, get(paste0("deadipw_",pollutants_i))]*dt[,get(paste0("ipw_ieb_",pollutants_i))]
  cat(summary(temp))
}

for (pollutants_i in pollutants){
  cat("fit coxph model", pollutants_i, "\n")
  cox <- coxph(Surv(time = followupyr_start, time2 = followupyr_end, event = ReAd) ~ 
                 get(pollutants_i) + 
                 mean_bmi + smoke_rate + 
                 hispanic + pct_blk + medhouseholdincome + medianhousevalue + poverty + education + popdensity + pct_owner_occ +
                 summer_tmmx + winter_tmmx + summer_rmax + winter_rmax +
                 as.factor(year) +  as.factor(region) +
                 strata(as.factor(entry_age_break), as.factor(sex), as.factor(race_collapsed), as.factor(dual)) + cluster(qid),
               weights = get(paste0("deadipw_",pollutants_i)),
               data = dt,
               tie = "efron", 
               na.action = na.omit)
  tb <- summary(cox)$coefficients
  tb <- as.data.frame(tb)
  setDT(tb, keep.rownames = TRUE)
  fwrite(tb, paste0(dir_out, "cox_ReAd_", pollutants_i, ".csv"))
  cat("output coefs...\n")
  HR <- tb[1,]
  HR <- cbind(HR, IQRs[, get(pollutants_i)])
  HR[, `:=`(HR_IQR = exp(coef*IQRs[, get(pollutants_i)]),
            HR_lci = exp((coef-1.96*`robust se`)*IQRs[, get(pollutants_i)]),
            HR_uci = exp((coef+1.96*`robust se`)*IQRs[, get(pollutants_i)]))]
  print(HR)
  fwrite(HR, paste0(dir_out, "cox_ReAd_", pollutants_i, "_HR.csv"))
  cat("save HR for cox ReAd", pollutants_i, "\n")
}
