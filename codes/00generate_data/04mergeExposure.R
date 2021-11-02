#' Project: airPollution_ADRD
#' Code: merge air pollution into denominator files
#' Input: "ADRDpeople_denom.csv"
#' Input: pm25 NO2 and ozone data                             
#' Output: "ADRDcohort.fst"
#' Author: Shuxin Dong
#' First create date: 2021-02-05

## setup----
rm(list = ls())
gc()

library(NSAPHutils)
set_threads()
library(data.table)
setDTthreads(threads = 0)
library(fst)

dir_pm25 <- "/nfs/nsaph_ci3/ci3_exposure/pm25/whole_us/annual/zipcode/qd_predictions_ensemble/ywei_aggregation/"
dir_no2 <- "/nfs/nsaph_ci3/ci3_exposure/no2/whole_us/annual/zipcode/qd_predictions_ensemble/ywei_aggregations/"
dir_ozone <- "/nfs/nsaph_ci3/ci3_exposure/ozone/whole_us/annual/zipcode/requaia_predictions/ywei_aggregation/"
dir_summer_ozone <- "/nfs/nsaph_ci3/ci3_shd968/dementia/data/"
dir_denom <- "/nfs/home/S/shd968/shared_space/ci3_shd968/dementia/data/"

dir_out <- "/nfs/home/S/shd968/shared_space/ci3_shd968/dementia/data/"

## merge ----
ADRDpeople <- read_fst(paste0(dir_denom, "ADRDpeople_denom.fst"), as.data.table = T)
names(ADRDpeople)
# [1] "qid"                "year"               "zip"                "sex"                "race"              
# [6] "age"                "dual"               "statecode"          "dead"               "mean_bmi"          
# [11] "smoke_rate"         "hispanic"           "pct_blk"            "medhouseholdincome" "medianhousevalue"  
# [16] "poverty"            "education"          "popdensity"         "pct_owner_occ"      "summer_tmmx"       
# [21] "winter_tmmx"        "summer_rmax"        "winter_rmax"        "firstADRDyr"  
class(ADRDpeople[,zip])

pm25_data <- fread(paste0(dir_pm25, "all_years.csv"))
head(pm25_data)
pm25_data[, ZIP := int_to_zip_str(ZIP)]
head(pm25_data)
no2_data <- fread(paste0(dir_no2, "all_years.csv"))
head(no2_data)
no2_data[, ZIP := int_to_zip_str(ZIP)]
head(no2_data)
ozone_data <- fread(paste0(dir_ozone, "all_years.csv"))
head(ozone_data)
ozone_data[, ZIP := int_to_zip_str(ZIP)]
head(ozone_data)
summer_ozone_data <- readRDS(paste0(dir_summer_ozone, "all_ozone_summer.rds"))
setDT(summer_ozone_data)
head(summer_ozone_data)

exposure <- merge(pm25_data, no2_data, by = c("ZIP", "year"))
exposure <- merge(exposure, ozone_data, by = c("ZIP", "year"))
exposure <- merge(exposure, summer_ozone_data, by.x = c("ZIP", "year"), by.y = c("zip", "year"))
head(exposure)
rm(pm25_data)
rm(no2_data)
rm(ozone_data)
rm(summer_ozone_data)
gc()

dim(ADRDpeople) # 25787050
#' remove the denominator info of the firstADRD yr, followed them from the next year of
ADRDcohort <- ADRDpeople[year!=firstADRDyr,]
ADRDcohort[,year_prev:=year-1]

combined <- merge(ADRDcohort, exposure, by.x = c("zip", "year_prev"), by.y = c("ZIP", "year"), all.x = TRUE) # merge on the previous year of calendar follow-up year
head(combined,100)
# > summary(combined[,pm25])
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#    0.28    8.67   10.40   10.53   12.40   30.92   51422 
# > summary(combined[,no2])
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 0.01   11.89   18.00   19.90   26.42  127.63   51422 
# > summary(combined[,ozone])
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#   13.38   36.44   38.64   38.60   40.80   65.09   51422 
# > summary(combined[,ozone_summer])
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 8.81   41.90   45.02   45.02   48.54   80.76   51422 
write_fst(combined, paste0(dir_out, "ADRDcohort.fst"))
