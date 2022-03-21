#' Project: airPollution_ADRD
#' Code: Cox PH 
#' Input: "ADRDcohort_dead.fst"                                                  
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

setwd("/nfs/home/S/shd968/shared_space/ci3_shd968/medicareADRD/")
dir_data <- "/nfs/home/S/shd968/shared_space/ci3_shd968/medicareADRD/data/"

## load data ----
dt <- read_fst(paste0(dir_data, "ADRDcohort_dead.fst"), as.data.table = T)
IPWs <- read_fst(paste0(dir_data, "ADRDcohort_dead_ipw_ieb.fst"), as.data.table = T)
names(dt)
names(IPWs)
dt <- merge(dt, IPWs, by = c("qid", "year"))
names(dt)

# dt[, dual := as.numeric(dual)]
dt[, followupyr_start := (year - firstADRDyr-1)]
dt[, followupyr_end := (year - firstADRDyr)]


IQRs <- data.table(IQR(dt$pm25), IQR(dt$no2), IQR(dt$ozone), IQR(dt$ox), IQR(dt$ozone_summer))
colnames(IQRs) <- c("pm25", "no2", "ozone", "ox", "ozone_summer")
print(IQRs)

## single-pollutants model ----
dir_out <- "/nfs/home/S/shd968/shared_space/ci3_shd968/medicareADRD/github_repo/results/main_analysis/2index_event_bias/coxph_mortality/"
pollutants <- c("pm25", "no2", "ozone_summer", "ox")

for (pollutants_i in pollutants){
  cat("fit coxph model", pollutants_i, "\n")
  cox <- coxph(Surv(time = followupyr_start, time2 = followupyr_end, event = dead) ~ 
                 get(pollutants_i) + 
                 mean_bmi + smoke_rate + 
                 hispanic + pct_blk + medhouseholdincome + medianhousevalue + poverty + education + popdensity + pct_owner_occ +
                 summer_tmmx + winter_tmmx + summer_rmax + winter_rmax +
                 as.factor(year) +  as.factor(region) +
                 strata(as.factor(entry_age_break), as.factor(sex), as.factor(race_collapsed), as.factor(dual)) + cluster(qid),
               weights = get(paste0("ipw_ieb_", pollutants_i)),
               data = dt,
               tie = "efron",
               na.action = na.omit)
  tb <- summary(cox)$coefficients
  tb <- as.data.frame(tb)
  setDT(tb, keep.rownames = TRUE)
  fwrite(tb, paste0(dir_out, "cox_mortality_", pollutants_i, ".csv"))
  cat("output coefs...\n")
  HR <- tb[1,]
  HR <- cbind(HR, IQRs[, get(pollutants_i)])
  HR[, `:=`(HR_IQR = exp(coef*IQRs[, get(pollutants_i)]),
            HR_lci = exp((coef-qnorm(0.975)*`robust se`)*IQRs[, get(pollutants_i)]),
            HR_uci = exp((coef+qnorm(0.975)*`robust se`)*IQRs[, get(pollutants_i)]))]
  fwrite(HR, paste0(dir_out, "cox_mortality_", pollutants_i, "_HR.csv"))
  print(HR)
  cat("save HR for cox mortality", pollutants_i, "\n")
}

#' ## multi-pollutants model ----
truncate_ipw <- function(ipw_raw, upper_bound_percentile, lower_bound_percentile){
  #' ipw_raw: raw stabilized ipw
  #' upper_bound_percentile: truncation upper bound limit usually 0.99
  up_bound <- quantile(ipw_raw, upper_bound_percentile)
  low_bound <- quantile(ipw_raw, lower_bound_percentile)
  ipw <- ifelse(ipw_raw>up_bound, up_bound, ifelse(ipw_raw<low_bound, low_bound, ipw_raw))
  return(ipw) # output truncated ipw
}
dir_out <- "/nfs/home/S/shd968/shared_space/ci3_shd968/medicareADRD/github_repo/results/main_analysis/2index_event_bias/coxph_mortality/"

cat("estimate cox for 3-pollutant model \n")
ipw_ieb_all3_raw <- dt[,ipw_ieb_pm25_raw]*dt[,ipw_ieb_no2_raw]*dt[,ipw_ieb_ozone_summer_raw]
summary(ipw_ieb_all3_raw)
summary(truncate_ipw(ipw_ieb_all3_raw, 0.97, 0.03))
ipw_ieb_all3 <- truncate_ipw(ipw_ieb_all3_raw, 0.97, 0.03)
cox_all3 <- coxph(Surv(time = followupyr_start, time2 = followupyr_end, event = dead) ~
                    pm25 + no2 + ozone_summer +
                    mean_bmi + smoke_rate +
                    hispanic + pct_blk + medhouseholdincome + medianhousevalue + poverty + education + popdensity + pct_owner_occ +
                    summer_tmmx + winter_tmmx + summer_rmax + winter_rmax +
                    as.factor(year) +  as.factor(region) +
                    strata(as.factor(entry_age_break), as.factor(sex), as.factor(race_collapsed), as.factor(dual)) + cluster(qid),
                  weights = ipw_ieb_all3,
                  data = dt,
                  tie = c("efron"),
                  na.action = na.omit)
tb <- summary(cox_all3)$coefficients
tb <- as.data.frame(tb)
setDT(tb, keep.rownames = TRUE)[]
fwrite(tb, paste0(dir_out, "cox_mortality_all3.csv"))

IQRunit <- c(IQRs$pm25, IQRs$no2, IQRs$ozone_summer)
HR <- tb[1:3,]
HR <- cbind(HR,IQRunit)
cat("output HR for cox 3-pollutant model \n")
HR[, `:=`(HR_IQR = exp(coef*IQRunit),
          HR_lci = exp((coef-1.96*`robust se`)*IQRunit),
          HR_uci = exp((coef+1.96*`robust se`)*IQRunit))][]
fwrite(HR, paste0(dir_out, "cox_mortality_all3_HR.csv"))

cat("estimate cox for 2-pollutant model \n")
ipw_ieb_all2_raw <- dt[,ipw_ieb_pm25_raw]*dt[,ipw_ieb_ox_raw]
summary(ipw_ieb_all2_raw)
summary(truncate_ipw(ipw_ieb_all2_raw, 0.985, 0.015))
ipw_ieb_all2 <- truncate_ipw(ipw_ieb_all2_raw, 0.985, 0.015)
cox_all2 <- coxph(Surv(time = followupyr_start, time2 = followupyr_end, event = dead) ~
                    pm25 + ox +
                    mean_bmi + smoke_rate +
                    hispanic + pct_blk + medhouseholdincome + medianhousevalue + poverty + education + popdensity + pct_owner_occ +
                    summer_tmmx + winter_tmmx + summer_rmax + winter_rmax +
                    as.factor(year) +  as.factor(region) +
                    strata(as.factor(entry_age_break), as.factor(sex), as.factor(race_collapsed), as.factor(dual)) + cluster(qid),
                  weights = ipw_ieb_all2,
                  data = dt,
                  tie = c("efron"),
                  na.action = na.omit)
tb <- summary(cox_all2)$coefficients
tb <- as.data.frame(tb)
setDT(tb, keep.rownames = TRUE)[]
fwrite(tb, paste0(dir_out, "cox_mortality_all2.csv"))

IQRunit <- c(IQRs$pm25, IQRs$ox)
HR <- tb[1:2,]
HR <- cbind(HR,IQRunit)
cat("output HR for cox 2-pollutant model \n")
HR[, `:=`(HR_IQR = exp(coef*IQRunit),
          HR_lci = exp((coef-1.96*`robust se`)*IQRunit),
          HR_uci = exp((coef+1.96*`robust se`)*IQRunit))][]
fwrite(HR, paste0(dir_out, "cox_mortality_all2_HR.csv"))


