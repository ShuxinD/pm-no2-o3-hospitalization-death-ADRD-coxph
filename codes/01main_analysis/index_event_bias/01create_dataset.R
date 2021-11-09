#' Project: airPollution_ADRD
#' Code: create datasets to solve index event bias
#' Input: ...
#' Output: "ADRDcohort_mortality_IEB.csv" 
#' Output: "ADRDcohort_ReAd_IEB.csv"
#' Author: Shuxin Dong
#' FIrst create ate: 2021-09-15

## setup ----
rm(list = ls())
gc()

library(data.table)
setDTthreads(threads = 0)
library(fst)

dir_data <- "/nfs/home/S/shd968/shared_space/ci3_shd968/dementia/data/"

truncate_ipw <- function(ipw_raw, upper_bound_percentile, lower_bound_percentile){
  #' ipw_raw: raw stabilized ipw
  #' upper_bound_percentile: truncation upper bound limit usually 0.99
  up_bound <- quantile(ipw_raw, upper_bound_percentile)
  low_bound <- quantile(ipw_raw, lower_bound_percentile)
  ipw <- ifelse(ipw_raw>up_bound, up_bound, ifelse(ipw_raw<low_bound, low_bound, ipw_raw))
  return(ipw) # output truncated ipw
}
# load full cohort data
dt_full <- read_fst(paste0(dir_data, "ADRDcohort.fst"), as.data.table = T)
dt_full <- dt_full[,.(qid, year, pm25, no2, ozone, ozone_summer, ox)]
colnames(dt_full)
colnames(dt_full)[3:7] <- paste0(c("pm25", "no2", "ozone", "ozone_summer", "ox"), "_E0")

## generate mortality ipw dataset ----
#' load data
dt_mortality <- read_fst(paste0(dir_data, "ADRDcohort_clean.fst"), as.data.table = T)
#' merge by qid and firstADRDyr
dt_mortality <- merge(dt_mortality, dt_full, by.x = c("qid", "firstADRDyr"), by.y = c("qid", "year"))
#' remove full cohort
rm(dt_full)
gc()
#' calculate IPW
pollutants <- c("pm25", "no2", "ozone","ozone_summer", "ox")
for (pollutants_i in pollutants) {
  ipw_ieb <- NULL
  num <- NULL
  denom <- NULL
  gc()
  cat("FITTING ", pollutants_i, "\n")
  mod <- lm(get(pollutants_i) ~ get(paste0(pollutants_i, "_E0")), data = dt_mortality)
  cat("FINISHING PS model\n")
  # marginal density
  num <- dnorm(dt_mortality[,get(pollutants_i)], mean(dt_mortality[,get(pollutants_i)]), sd(dt_mortality[,get(pollutants_i)]))
  gc()
  # conditional density (GPS)
  denom <- dnorm(dt_mortality[,get(pollutants_i)], mod$fitted.values, summary(mod)$sigma)
  gc()
  ipw_ieb <- num/denom
  summary(ipw_ieb)
  assign(paste0("ipw_ieb_", pollutants_i), ipw_ieb)
}
IPWs <- data.table(qid = dt_mortality[,qid],
                   ipw_ieb_pm25,
                   ipw_ieb_no2,
                   ipw_ieb_ozone,
                   ipw_ieb_ozone_summer,
                   ipw_ieb_ox)
summary(IPWs)
# qid             ipw_ieb_pm25        ipw_ieb_no2        ipw_ieb_ozone       ipw_ieb_ozone_summer   ipw_ieb_ox       
# Length:22609292    Min.   :0.000e+00   Min.   : 0.00e+00   Min.   :0.000e+00   Min.   :0.000e+00    Min.   :0.000e+00  
# Class :character   1st Qu.:0.000e+00   1st Qu.: 0.00e+00   1st Qu.:0.000e+00   1st Qu.:0.000e+00    1st Qu.:0.000e+00  
# Mode  :character   Median :1.000e+00   Median : 0.00e+00   Median :1.000e+00   Median :1.000e+00    Median :1.000e+00  
# Mean   :1.444e+37   Mean   : 5.63e+99   Mean   :7.028e+23   Mean   :2.475e+20    Mean   :4.236e+47  
# 3rd Qu.:1.000e+00   3rd Qu.: 0.00e+00   3rd Qu.:1.000e+00   3rd Qu.:1.000e+00    3rd Qu.:1.000e+00  
# Max.   :3.263e+44   Max.   :1.25e+107   Max.   :1.386e+31   Max.   :5.057e+27    Max.   :9.507e+54 
summary(truncate_ipw(IPWs[,ipw_ieb_pm25], 0.99, 0.01))
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# 0.03404  0.38285  0.53331  1.18336  0.63130 35.60944 
summary(truncate_ipw(IPWs[,ipw_ieb_no2], 0.985, 0.015))
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# 0.04784  0.29364  0.41662  1.30741  0.49078 41.19423 
summary(truncate_ipw(IPWs[,ipw_ieb_ozone], 0.99, 0.01))
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# 0.02174  0.47572  0.59888  1.15008  0.75242 29.26037
summary(truncate_ipw(IPWs[,ipw_ieb_ozone_summer], 0.99, 0.01))
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.0229  0.4724  0.6090  1.0733  0.7821 21.6568 
summary(truncate_ipw(IPWs[,ipw_ieb_ox], 0.99, 0.01))
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# 0.01799  0.43933  0.57219  1.11011  0.70151 29.13202 
IPWs <- data.table(qid = dt_mortality[,qid],
                   year = dt_mortality[,year],
                   ipw_ieb_pm25 = truncate_ipw(ipw_ieb_pm25, 0.99, 0.01),
                   ipw_ieb_no2 = truncate_ipw(ipw_ieb_no2, 0.985, 0.015),
                   ipw_ieb_ozone = truncate_ipw(ipw_ieb_ozone, 0.99, 0.01),
                   ipw_ieb_ozone_summer = truncate_ipw(ipw_ieb_ozone_summer, 0.99, 0.01),
                   ipw_ieb_ox = truncate_ipw(ipw_ieb_ox, 0.99, 0.01))
write_fst(IPWs, paste0(dir_data, "ADRDcohort_mortality_ipw_ieb.fst"))

gc()

## generate ReAd ipw dataset ----
#' load data
dt_ReAd <- read_fst(paste0(dir_data, "ADRDcohort_ReAd.fst"), as.data.table = T)
#' merge by qid and firstADRDyr
dt_ReAd <- merge(dt_ReAd, dt_full, by.x = c("qid", "firstADRDyr"), by.y = c("qid", "year"))
#' remove full cohort
rm(dt_full)
gc()
#' calculate IPW
pollutants <- c("pm25", "no2", "ozone","ozone_summer", "ox")
for (pollutants_i in pollutants) {
  ipw_ieb <- NULL
  num <- NULL
  denom <- NULL
  gc()
  cat("FITTING ", pollutants_i, "\n")
  mod <- lm(get(pollutants_i) ~ get(paste0(pollutants_i, "_E0")), data = dt_ReAd)
  cat("FINISHING PS model\n")
  # marginal density
  num <- dnorm(dt_ReAd[,get(pollutants_i)], mean(dt_ReAd[,get(pollutants_i)]), sd(dt_ReAd[,get(pollutants_i)]))
  gc()
  # conditional density (GPS)
  denom <- dnorm(dt_ReAd[,get(pollutants_i)], mod$fitted.values, summary(mod)$sigma)
  gc()
  ipw_ieb <- num/denom
  assign(paste0("ipw_ieb_", pollutants_i), ipw_ieb)
}
IPWs <- data.table(qid = dt_ReAd[,qid],
                   ipw_ieb_pm25,
                   ipw_ieb_no2,
                   ipw_ieb_ozone,
                   ipw_ieb_ozone_summer,
                   ipw_ieb_ox)
summary(IPWs)
summary(truncate_ipw(IPWs[,ipw_ieb_pm25], 0.99, 0.01))
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# 0.02352  0.29079  0.41787  1.51392  0.49283 69.52411 
summary(truncate_ipw(IPWs[,ipw_ieb_no2], 0.985, 0.015))
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# 0.03332  0.23536  0.33635  1.27012  0.39288 45.85371 
summary(truncate_ipw(IPWs[,ipw_ieb_ozone], 0.99, 0.01))
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# 0.01523  0.39595  0.51452  1.31258  0.64493 44.93728 
summary(truncate_ipw(IPWs[,ipw_ieb_ozone_summer], 0.99, 0.01))
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# 0.01731  0.39586  0.52813  1.12485  0.67939 28.01821 
summary(truncate_ipw(IPWs[,ipw_ieb_ox], 0.99, 0.01))
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# 0.01231  0.36411  0.48369  1.19142  0.59259 39.99381        

IPWs <- data.table(qid = dt_ReAd[,qid],
                   year = dt_ReAd[,year],
                   ipw_ieb_pm25 = truncate_ipw(ipw_ieb_pm25, 0.99, 0.01),
                   ipw_ieb_no2 = truncate_ipw(ipw_ieb_no2, 0.985, 0.015),
                   ipw_ieb_ozone = truncate_ipw(ipw_ieb_ozone, 0.99, 0.01),
                   ipw_ieb_ozone_summer = truncate_ipw(ipw_ieb_ozone_summer, 0.99, 0.01),
                   ipw_ieb_ox = truncate_ipw(ipw_ieb_ox, 0.99, 0.01))
write_fst(IPWs, paste0(dir_data, "ADRDcohort_ReAd_ipw_ieb.fst"))


