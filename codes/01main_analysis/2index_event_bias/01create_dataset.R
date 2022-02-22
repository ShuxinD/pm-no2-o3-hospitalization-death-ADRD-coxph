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

dir_data <- "/nfs/home/S/shd968/shared_space/ci3_shd968/medicareADRD/data/"

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
dt_mortality <- read_fst(paste0(dir_data, "ADRDcohort_dead.fst"), as.data.table = T)
#' merge by qid and firstADRDyr
dt_mortality <- merge(dt_mortality, dt_full, by.x = c("qid", "firstADRDyr"), by.y = c("qid", "year"))
#' remove full cohort
rm(dt_full)
gc()
#' calculate IPW
pollutants <- c("pm25", "no2","ozone_summer", "ox")
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
                   ipw_ieb_ozone_summer,
                   ipw_ieb_ox)
summary(IPWs)
# qid             ipw_ieb_pm25        ipw_ieb_no2        ipw_ieb_ozone_summer   ipw_ieb_ox       
# Length:18514503    Min.   :0.000e+00   Min.   :0.000e+00   Min.   :0.000e+00    Min.   :0.000e+00  
# Class :character   1st Qu.:0.000e+00   1st Qu.:0.000e+00   1st Qu.:1.000e+00    1st Qu.:1.000e+00  
# Mode  :character   Median :1.000e+00   Median :0.000e+00   Median :1.000e+00    Median :1.000e+00  
#                    Mean   :2.699e+31   Mean   :2.626e+80   Mean   :1.171e+13    Mean   :1.537e+34  
#                    3rd Qu.:1.000e+00   3rd Qu.:1.000e+00   3rd Qu.:1.000e+00    3rd Qu.:1.000e+00  
#                    Max.   :4.997e+38   Max.   :4.630e+87   Max.   :1.770e+20    Max.   :2.803e+41 

summary(truncate_ipw(IPWs[,ipw_ieb_pm25], 0.99, 0.01))
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# 0.04448  0.42077  0.57313  1.02113  0.72032 22.76960
summary(truncate_ipw(IPWs[,ipw_ieb_no2], 0.985, 0.015))
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# 0.05576  0.33479  0.46857  1.16453  0.56409 30.80177
summary(truncate_ipw(IPWs[,ipw_ieb_ozone_summer], 0.99, 0.01))
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# 0.03066  0.55686  0.68551  0.98241  0.87410 13.26209  
summary(truncate_ipw(IPWs[,ipw_ieb_ox], 0.99, 0.01))
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# 0.02521  0.50538  0.64426  0.98882  0.79203 17.86753 
IPWs <- data.table(qid = dt_mortality[,qid],
                   year = dt_mortality[,year],
                   ipw_ieb_pm25 = truncate_ipw(ipw_ieb_pm25, 0.99, 0.01),
                   ipw_ieb_no2 = truncate_ipw(ipw_ieb_no2, 0.985, 0.015),
                   ipw_ieb_ozone_summer = truncate_ipw(ipw_ieb_ozone_summer, 0.99, 0.01),
                   ipw_ieb_ox = truncate_ipw(ipw_ieb_ox, 0.99, 0.01),
                   ipw_ieb_pm25_raw = ipw_ieb_pm25,
                   ipw_ieb_no2_raw = ipw_ieb_no2,
                   ipw_ieb_ozone_summer_raw = ipw_ieb_ozone_summer,
                   ipw_ieb_ox_raw = ipw_ieb_ox)
write_fst(IPWs, paste0(dir_data, "ADRDcohort_dead_ipw_ieb.fst"))

gc()

## generate ReAd ipw dataset ----
#' load data
dt_ReAd <- read_fst(paste0(dir_data, "ADRDcohort_ReAd.fst"), as.data.table = T)
#' load full cohort data
dt_full <- read_fst(paste0(dir_data, "ADRDcohort.fst"), as.data.table = T)
dt_full <- dt_full[,.(qid, year, pm25, no2, ozone, ozone_summer, ox)]
colnames(dt_full)
colnames(dt_full)[3:7] <- paste0(c("pm25", "no2", "ozone", "ozone_summer", "ox"), "_E0")
#' merge by qid and firstADRDyr
dt_ReAd <- merge(dt_ReAd, dt_full, by.x = c("qid", "firstADRDyr"), by.y = c("qid", "year"))
#' remove full cohort
rm(dt_full)
gc()
#' calculate IPW
pollutants <- c("pm25", "no2","ozone_summer", "ox")
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
                   ipw_ieb_ozone_summer,
                   ipw_ieb_ox)
summary(IPWs)
# qid             ipw_ieb_pm25        ipw_ieb_no2         ipw_ieb_ozone_summer   ipw_ieb_ox       
# Length:10962248    Min.   :0.000e+00   Min.   : 0.000e+00   Min.   :0.000e+00    Min.   :0.000e+00  
# Class :character   1st Qu.:0.000e+00   1st Qu.: 0.000e+00   1st Qu.:1.000e+00    1st Qu.:0.000e+00  
# Mode  :character   Median :0.000e+00   Median : 0.000e+00   Median :1.000e+00    Median :1.000e+00  
#                    Mean   :5.506e+45   Mean   :5.498e+113   Mean   :3.827e+14    Mean   :2.866e+41  
#                    3rd Qu.:1.000e+00   3rd Qu.: 0.000e+00   3rd Qu.:1.000e+00    3rd Qu.:1.000e+00  
#                    Max.   :6.026e+52   Max.   :6.027e+120   Max.   :1.304e+21    Max.   :3.063e+48
summary(truncate_ipw(IPWs[,ipw_ieb_pm25], 0.99, 0.01))
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# 0.03571  0.35330  0.49232  1.04013  0.62014 31.00900 
summary(truncate_ipw(IPWs[,ipw_ieb_no2], 0.985, 0.015))
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# 0.04418  0.29730  0.41710  1.09602  0.49872 31.55332
summary(truncate_ipw(IPWs[,ipw_ieb_ozone_summer], 0.99, 0.01))
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# 0.02664  0.51631  0.64651  0.93853  0.84672 12.22823 
summary(truncate_ipw(IPWs[,ipw_ieb_ox], 0.99, 0.01))
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# 0.01958  0.45979  0.59442  0.94524  0.73882 18.31202       

IPWs <- data.table(qid = dt_ReAd[,qid],
                   year = dt_ReAd[,year],
                   ipw_ieb_pm25 = truncate_ipw(ipw_ieb_pm25, 0.99, 0.01),
                   ipw_ieb_no2 = truncate_ipw(ipw_ieb_no2, 0.985, 0.015),
                   ipw_ieb_ozone_summer = truncate_ipw(ipw_ieb_ozone_summer, 0.99, 0.01),
                   ipw_ieb_ox = truncate_ipw(ipw_ieb_ox, 0.99, 0.01),
                   ipw_ieb_pm25_raw = ipw_ieb_pm25,
                   ipw_ieb_no2_raw = ipw_ieb_no2,
                   ipw_ieb_ozone_summer_raw = ipw_ieb_ozone_summer,
                   ipw_ieb_ox_raw = ipw_ieb_ox)
write_fst(IPWs, paste0(dir_data, "ADRDcohort_ReAd_ipw_ieb.fst"))


