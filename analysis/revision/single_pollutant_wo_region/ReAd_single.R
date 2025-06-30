# setup ----
rm(list = ls())
gc()

library(data.table)
library(fst)
setDTthreads(threads = 0)
library(survival)

wkdir <- "/n/dominici_nsaph_l3/Lab/projects/pm-no2-o3-hospitalization-death-ADRD-coxph"
getwd()
cr_data_path <- file.path(wkdir, "data", "ADRDcohort_ReAd_deadipw.fst")
data_path <- file.path(wkdir, "data", "ADRDcohort_ReAd.fst")
run_path <- file.path(wkdir, "single_pollutant_wo_region")

## load data ----
cr_dt <- read_fst(cr_data_path, as.data.table = T)
names(cr_dt)
ReAd_dt <- read_fst(data_path, as.data.table = T)
names(ReAd_dt)
dt <- merge(ReAd_dt, cr_dt, by = c("qid", "year"))
rm(cr_dt);rm(ReAd_dt); gc()

dt[, followupyr_start := (year - firstADRDyr-1)]
dt[, followupyr_end := (year - firstADRDyr)]

IQRs <- data.table(IQR(dt$pm25), IQR(dt$no2), IQR(dt$ozone), IQR(dt$ox), IQR(dt$ozone_summer))
colnames(IQRs) <- c("pm25", "no2", "ozone", "ox", "ozone_summer")
print(IQRs)

## single-pollutant models ----
run_path
pollutants <- c("pm25", "no2", "ozone_summer", "ox")

for (pollutants_i in pollutants){
  cat("fit coxph model", pollutants_i, "\n")
  cox <- coxph(Surv(time = followupyr_start, time2 = followupyr_end, event = ReAd) ~ 
                 get(pollutants_i) + 
                 mean_bmi + smoke_rate + 
                 hispanic + pct_blk + medhouseholdincome + medianhousevalue + poverty + education + popdensity + pct_owner_occ +
                 summer_tmmx + winter_tmmx + summer_rmax + winter_rmax +
                 as.factor(year) +  
                 # as.factor(region) +
                 strata(as.factor(entry_age_break), as.factor(sex), as.factor(race_collapsed), as.factor(dual)) + cluster(qid),
               weights = get(paste0("deadipw_",pollutants_i)),
               data = dt,
               tie = "efron", 
               na.action = na.omit)
  tb <- summary(cox)$coefficients
  tb <- as.data.frame(tb)
  setDT(tb, keep.rownames = TRUE)
  fwrite(tb, file.path(run_path, paste0("ReAd_", pollutants_i, ".csv")))
  cat("output coefs...\n")
  HR <- tb[1,]
  HR <- cbind(HR, IQRs[, get(pollutants_i)])
  HR[, `:=`(HR_IQR = exp(coef*IQRs[, get(pollutants_i)]),
            HR_lci = exp((coef-1.96*`robust se`)*IQRs[, get(pollutants_i)]),
            HR_uci = exp((coef+1.96*`robust se`)*IQRs[, get(pollutants_i)]))]
  print(HR)
  fwrite(HR, file.path(run_path, paste0("ReAd_", pollutants_i, "_HR.csv")))
  cat("save HR for cox ReAd", pollutants_i, "\n")
}