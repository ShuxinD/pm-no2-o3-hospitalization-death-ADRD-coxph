#' Project: airPollution_ADRD
#' Code: merge air pollution into ADRD denom file
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
dir_summer_ozone <- paste0(getwd(),"/data/")
dir_denom <- paste0(getwd(),"/data/")

dir_out <- paste0(getwd(),"/data/")

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
dim(ADRDpeople) # 27297657 
#' though followup starting from the next year of firstADRDyr, we need the exposure info to correct index event bias

combined <- merge(ADRDpeople, exposure, by.x = c("zip", "year"), by.y = c("ZIP", "year"), all.x = T) # merge on the previous year of calendar follow-up year
head(combined,100)

#' generate Ox based on no2 and ozone
combined[, ox := (1.07*no2 + 2.075*ozone)/3.14]
summary(combined[,.(pm25,no2,ozone,ozone_summer,ox)])
# pm25            no2             ozone        ozone_summer         ox       
# Min.   : 0.01   Min.   :  0.01   Min.   :13.38   Min.   : 8.81   Min.   : 9.81  
# 1st Qu.: 8.26   1st Qu.: 11.60   1st Qu.:36.54   1st Qu.:41.75   1st Qu.:29.57  
# Median :10.05   Median : 17.54   Median :38.67   Median :44.77   Median :31.88  
# Mean   :10.22   Mean   : 19.43   Mean   :38.64   Mean   :44.80   Mean   :32.15  
# 3rd Qu.:12.12   3rd Qu.: 25.74   3rd Qu.:40.72   3rd Qu.:48.22   3rd Qu.:34.39  
# Max.   :30.92   Max.   :127.63   Max.   :65.09   Max.   :80.76   Max.   :70.83  
# NA's   :58522   NA's   :58522    NA's   :58522   NA's   :58522   NA's   :58522  
write_fst(combined, paste0(dir_out, "ADRDcohort.fst"))
