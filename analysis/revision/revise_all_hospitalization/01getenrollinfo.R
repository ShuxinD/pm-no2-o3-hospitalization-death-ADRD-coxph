## setup ----
rm(list = ls())
gc()

library(data.table)
library(fst)
library(lubridate)

setDTthreads(threads = 0)

dir_hosp <- file.path("/n/dominici_nsaph_l3/Lab/projects/analytic", "admissions_by_year")
dir_xwalk <- file.path("/n/dominici_nsaph_l3/Lab/data/ci3_health_data/medicare/id_crosswalk")
wkdir <- "/n/dominici_nsaph_l3/Lab/projects/pm-no2-o3-hospitalization-death-ADRD-coxph"

## combine hospitalization files together ----
hosp <- NULL
for (i in 2000:2016) {
  adm_ <- read_fst(file.path(dir_hosp, paste0("admissions_", i, ".fst")), 
                   columns = c("QID", "ADATE"),
                   as.data.table = T)
  adm_[, ADATE := dmy(ADATE)][, year := year(ADATE)]
  hosp <- rbind(hosp, adm_[year %in% 2000:2016, .(QID, year)])
  cat("finish loading file:", "admissions_", i,".fst", "\n")
  rm(adm_)
}
gc()

setDT(hosp)
names(hosp)

dim(hosp)

any(duplicated(hosp))
sum(duplicated(hosp))
hosp <- unique(hosp) # remove all the duplicates
dim(hosp)

# hosp_time <- hosp[,.(QID, YEAR)] # drop all diagnosis codes, only save QID, admission year
# gc()
# head(hosp_time)
# hosp_time <- unique(hosp_time) # remove duplicate of Admission year (some may be admitted several times in one year)
# dim(hosp_time)
# gc()

## get enroll info ----
#' first admission year for each QID
setorder(hosp, QID, year)
enrolledInfo <- hosp[, .(firstHOSPyr = min(year)), by = QID]
dim(enrolledInfo)

table(enrolledInfo[,firstHOSPyr])

#' readmission year for each QID
temp <- merge(hosp, enrolledInfo, by = "QID", all.x = T)
ReAdInfo <- temp[year>firstHOSPyr,][, .(firstReAdyr = min(year)), by = QID]

## exclude problematic IDs ----
probIDs <- read_fst(file.path(dir_xwalk, "no_crosswalk_no_death_ids.fst"), as.data.table = T) # IDs without cross
names(probIDs)
sum(enrolledInfo[,QID] %in% probIDs[,old_id]) # number of problemetic IDs in enrolledInfo
sum(ReAdInfo[,QID] %in% probIDs[,old_id])

enrolledInfo <- enrolledInfo[!(QID %in% probIDs[,old_id]),] # exclude problematic IDs
ReAdInfo <- ReAdInfo[!(QID %in% probIDs[,old_id]),] 

dim(enrolledInfo)
dim(ReAdInfo)

## save enrolled INFO ----
write_fst(enrolledInfo, file.path(wkdir,"code", "revise_all_hospitalization", "All_EnrolledInfo.fst"))
write_fst(ReAdInfo, file.path(wkdir,"code", "revise_all_hospitalization", "All_ReAdInfo.fst"))