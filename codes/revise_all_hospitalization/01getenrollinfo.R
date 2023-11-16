## setup ----
rm(list = ls())
gc()

library(data.table)
library(fst)

setDTthreads(threads = 0)

dir_hosp <- file.path("/n/dominici_nsaph_l3/Lab/projects/analytic", "admissions_by_year")
dir_xwalk <- file.path("/n/dominici_nsaph_l3/Lab/data/ci3_health_data/medicare/id_crosswalk")

## combine hospitalization files together ----
hosp <- NULL
for (i in 2000:2016) {
  adm_ <- read_fst(file.path(dir_hosp, paste0("admissions_", i, ".fst")))
  hosp <- rbind(hosp, adm_)
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

hosp_time <- hosp[,.(QID, year)] # drop all diagnosis codes, only save QID, admission year
gc()
head(hosp_time)
hosp_time <- unique(hosp_time) # remove duplicate of Admission year (some may be admitted several times in one year)
dim(hosp_time)
gc()

## get enroll info ----
#' first admission year for each QID
setorder(hosp_time, QID, year)
enrolledInfo <- hosp_time[, .(firstHOSPyr = min(year)), by = QID]
dim(enrolledInfo)

table(enrolledInfo[,firstHOSPyr])

## exclude problematic IDs ----
probIDs <- read_fst(paste0(dir_xwalk, "no_crosswalk_no_death_ids.fst"), as.data.table = T) # IDs without cross
head(probIDs[, old_id])
sum(enrolledInfo[,QID] %in% probIDs[,old_id]) # number of problemetic IDs in enrolledInfo # 2108
enrolledInfo <- enrolledInfo[!(QID %in% probIDs[,old_id]),] # exclude problematic IDs
dim(enrolledInfo)

## save enrolled INFO ----
fwrite(enrolledInfo, file.path(getwd(),"codes", "revise_all_hospitalization", "All_EnrolledInfo.csv"))