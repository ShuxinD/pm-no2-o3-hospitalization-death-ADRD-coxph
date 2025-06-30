## setup----
rm(list = ls())
gc()

library(data.table)
setDTthreads(threads = 0)
library(fst)

wkdir <- "/n/dominici_nsaph_l3/Lab/projects/pm-no2-o3-hospitalization-death-ADRD-coxph"
rundir <- file.path(wkdir, "code", "revise_all_hospitalization")

dir_pm25 <- "/n/dominici_nsaph_l3/Lab/exposure/pm25/whole_us/annual/zipcode/qd_predictions_ensemble/ywei_aggregation"
dir_no2 <- "/n/dominici_nsaph_l3/Lab/exposure/no2/whole_us/annual/zipcode/qd_predictions_ensemble/ywei_aggregations"
dir_ozone <- "/n/dominici_nsaph_l3/Lab/exposure/ozone/whole_us/annual/zipcode/requaia_predictions/ywei_aggregation"
dir_summer_ozone <- file.path(wkdir, "data")

## load ALL denominator files ----
ALLpeople <- read_fst(file.path(rundir, "ALLpeople_denom.fst"), as.data.table = T)
names(ALLpeople)

## load exposure data ----
pm25_data <- fread(file.path(dir_pm25, "all_years.csv"))
head(pm25_data)
no2_data <- fread(file.path(dir_no2, "all_years.csv"))
head(no2_data)
ozone_data <- fread(file.path(dir_ozone, "all_years.csv"))
head(ozone_data)
summer_ozone_data <- readRDS(file.path(dir_summer_ozone, "all_ozone_summer.rds"))
summer_ozone_data$zip <- as.integer(summer_ozone_data$zip)
gc()
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
dim(ALLpeople)
#' though followup starting from the next year of firstHOSPyr, we need the exposure info to correct index event bias

combined <- merge(ALLpeople, exposure, by.x = c("zip", "year"), by.y = c("ZIP", "year"), all.x = T) # merge on the previous year of calendar follow-up year
# head(combined,100)

#' generate Ox based on no2 and ozone
combined[, ox := (1.07*no2 + 2.075*ozone)/3.14]
summary(combined[,.(pm25,no2,ozone,ozone_summer,ox)])
write_fst(combined, file.path(rundir, "ALLcohort.fst"))