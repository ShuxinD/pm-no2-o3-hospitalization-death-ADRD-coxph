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

## load ADRD denominator files ----
ADRDpeople <- read_fst(paste0(dir_denom, "ADRDpeople_denom.fst"), as.data.table = T)
names(ADRDpeople)
# [1] "qid"                "year"               "zip"                "sex"                "race"              
# [6] "age"                "dual"               "statecode"          "dead"               "mean_bmi"          
# [11] "smoke_rate"         "hispanic"           "pct_blk"            "medhouseholdincome" "medianhousevalue"  
# [16] "poverty"            "education"          "popdensity"         "pct_owner_occ"      "summer_tmmx"       
# [21] "winter_tmmx"        "summer_rmax"        "winter_rmax"        "firstADRDyr"  
class(ADRDpeople[,zip])

## load exposure data ----
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

## merge ----
dim(ADRDpeople) # 25787050 
#' though followup starting from the next year of firstADRDyr, we need the exposure info to correct index event bias

combined <- merge(ADRDpeople, exposure, by.x = c("zip", "year"), by.y = c("ZIP", "year"), all.x = T) # merge on the previous year of calendar follow-up year
head(combined,100)

#' generate Ox based on no2 and ozone
combined[, ox := (1.07*no2 + 2.075*ozone)/3.14]
summary(combined[,.(pm25,no2,ozone,ozone_summer,ox)])
# pm25             no2             ozone         ozone_summer          ox        
# Min.   : 0.3     Min.   :  0.0    Min.   :13.4     Min.   : 8.8     Min.   : 9.8    
# 1st Qu.: 8.7     1st Qu.: 12.0    1st Qu.:36.4     1st Qu.:41.9     1st Qu.:29.8    
# Median :10.5     Median : 18.3    Median :38.6     Median :45.1     Median :32.1    
# Mean   :10.6     Mean   : 20.1    Mean   :38.6     Mean   :45.1     Mean   :32.4    
# 3rd Qu.:12.6     3rd Qu.: 26.8    3rd Qu.:40.8     3rd Qu.:48.6     3rd Qu.:34.6    
# Max.   :30.9     Max.   :127.6    Max.   :65.1     Max.   :80.8     Max.   :70.8    
# NA's   :765980   NA's   :765980   NA's   :765980   NA's   :765980   NA's   :765980  
write_fst(combined, paste0(dir_out, "ADRDcohort.fst"))
