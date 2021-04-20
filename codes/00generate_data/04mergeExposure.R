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
dir_summer_ozone <- "/nfs/nsaph_ci3/ci3_shd968/dementia/data/"
dir_denom <- "/nfs/home/S/shd968/shared_space/ci3_shd968/dementia/data/"

dir_out <- "/nfs/home/S/shd968/shared_space/ci3_shd968/dementia/data/"

############################# 1. merge ########################################
ADRDpeople <- fread(paste0(dir_denom, "ADRDpeople_denom.csv"), colClasses = c("zip" = "character"))
names(ADRDpeople)

pm25_data <- fread(paste0(dir_pm25, "all_years.csv"))
head(pm25_data)
pm25_data[, ZIP := int_to_zip_str(ZIP)]
no2_data <- fread(paste0(dir_no2, "all_years.csv"))
head(no2_data)
no2_data[, ZIP := int_to_zip_str(ZIP)]
ozone_data <- fread(paste0(dir_ozone, "all_years.csv"))
head(ozone_data)
ozone_data[, ZIP := int_to_zip_str(ZIP)]
summer_ozone_data <- readRDS(paste0(dir_summer_ozone, "all_ozone_summer.rds"))

exposure <- merge(pm25_data, no2_data, by = c("ZIP", "year"))
exposure <- merge(exposure, ozone_data, by = c("ZIP", "year"))
exposure <- merge(exposure, summer_ozone_data, by.x = c("ZIP", "year"), by.y = c("zip", "year"))
head(exposure)

rm(pm25_data)
rm(no2_data)
rm(ozone_data)
rm(summer_ozone_data)
gc()

head(ADRDpeople)

# ADRDpeople[exposure, on = .(zip = ZIP, year = year)]
combined <- merge(ADRDpeople, exposure, by.x = c("zip", "year"), by.y = c("ZIP", "year"), all.x = TRUE)
head(combined,100)
tail(combined)
fwrite(combined, paste0(dir_out, "ADRDpeople.csv"))
