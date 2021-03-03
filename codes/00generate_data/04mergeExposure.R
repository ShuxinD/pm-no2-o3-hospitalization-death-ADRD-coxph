###############################################################################
# Project: Air Pollution on mortality and readmission in Medicare AD/ADRD     #
# Code: extract ADRD population based on enrolled info from denominator files #
# Input: "ADRDpeople_denom.csv"
# Input: pm25 NO2 and ozone data                             
# Output: "ADRDpeople.csv" 
# Author: Shuxin Dong                                                         #
# Date: 2021-02-05                                                            #
###############################################################################

############################# 0. Setup ########################################
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
dir_denom <- "/nfs/home/S/shd968/shared_space/ci3_shd968/dementia/"

dir_out <- "/nfs/home/S/shd968/shared_space/ci3_shd968/dementia/"

############################# 1. merge ########################################
ADRDpeople <- fread(paste0(dir_denom, "ADRDpeople_denom.csv"))
names(ADRDpeople)

pm25_data <- fread(paste0(dir_pm25, "all_years.csv"))
no2_data <- fread(paste0(dir_no2, "all_years.csv"))
ozone_data <- fread(paste0(dir_ozone, "all_years.csv"))

exposure <- merge(pm25_data, no2_data, by = c("ZIP", "year"))
exposure <- merge(exposure, ozone_data, by = c("ZIP", "year"))
head(exposure)

rm(pm25_data)
rm(no2_data)
rm(ozone_data)
gc()
head(exposure[, ZIP])

names(ADRDpeople)
# [1] "qid"                "year"               "summer_tmmx"        "winter_tmmx"        "summer_rmax"        "winter_rmax"       
# [7] "zip"                "dead"               "sex"                "race"               "age"                "dual"              
# [13] "statecode"          "entry_age_break"    "mean_bmi"           "smoke_rate"         "hispanic"           "pct_blk"           
# [19] "medhouseholdincome" "medianhousevalue"   "poverty"            "education"          "popdensity"         "pct_owner_occ"     
# [25] "firstADRDyr"    

head(ADRDpeople)
setorder(ADRDpeople, zip, year)
ADRDpeople[1000:2000,]

# ADRDpeople[exposure, on = .(zip = ZIP, year = year)]
combined <- merge(ADRDpeople, exposure, by.x = c("zip", "year"), by.y = c("ZIP", "year"), all.x = TRUE)
combined
head(combined,100)
tail(combined)

fwrite(combined, paste0(dir_out, "ADRDpeople.csv"))
