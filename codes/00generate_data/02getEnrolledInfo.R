#' Project: airPollution_ADRD
#' Code: get ADRD study population, exclude problematic ID
#' Input: "no_crosswalk_no_death_ids.fst"                
#' Input: "ADRD`type`_`year`.fst"
#' Output: "EnrolledInfo.csv" - IDs and first year of ADRD admission   
#' Author: Shuxin Dong    
#' First create date: 2021-02-04                                                      

## setup ----
rm(list = ls())
gc()

library(data.table)
library(fst)
library(NSAPHutils)

setDTthreads(threads = 31)
setwd("/nfs/home/S/shd968/shared_space/ci3_shd968/dementia")

dir_input_hospital <- "/nfs/home/S/shd968/shared_space/ci3_shd968/dementia/data/ADRDhospitalization/"
dir_input_crosswalk <- "/nfs/home/S/shd968/shared_space/ci3_health_data/medicare/id_crosswalk/"
dir_output <- "/nfs/home/S/shd968/shared_space/ci3_shd968/dementia/data/"

## combine hospitalization files together ----
ADRDhosp <- NULL
for (i in 2000:2016) {
  adm_ <- read_fst(paste0(dir_input_hospital, "ADRDsecondary_", i,".fst"))
  ADRDhosp <- rbind(ADRDhosp, adm_)
  cat("finish loading file:", "ADRDsecondary_", i,".fst", "\n")
}
rm(adm_)
gc()

setDT(ADRDhosp)
names(ADRDhosp)
# [1] "QID"            "ADATE"          "DDATE"          "DIAG1"          "DIAG2"          "DIAG3"          "DIAG4"         
# [8] "DIAG5"          "DIAG6"          "DIAG7"          "DIAG8"          "DIAG9"          "DIAG10"         "diag11"        
# [15] "diag12"         "diag13"         "diag14"         "diag15"         "diag16"         "diag17"         "diag18"        
# [22] "diag19"         "diag20"         "diag21"         "diag22"         "diag23"         "diag24"         "diag25"        
# [29] "year"           "ADRD_primary"   "ADRD_secondary"
#' year as Admission year

head(ADRDhosp)
dim(ADRDhosp) # [1] 16190443       31

any(duplicated(ADRDhosp))  # TRUE
sum(duplicated(ADRDhosp)) # 1441 number of duplicates
ADRDhosp <- unique(ADRDhosp) # remove all the duplicates
dim(ADRDhosp) # [1] 16189002       31 # dimention after removing duplicates
gc()

ADRDhosp_time <- ADRDhosp[,.(QID, year)] # drop all diagnosis codes, only save QID, admission year
gc()
head(ADRDhosp_time)
ADRDhosp_time <- unique(ADRDhosp_time) # remove duplicate of Admission year (some may be admitted several times in one year)
dim(ADRDhosp_time) # [1] 11843820        2
gc()

## get enroll info ----
#' first admission year for each QID
setorder(ADRDhosp_time, QID, year)
enrolledInfo <- ADRDhosp_time[, .(firstADRDyr = min(year)), by = QID]
dim(enrolledInfo)
# [1] 7649705       2

table(enrolledInfo[,firstADRDyr])
# 2000   2001   2002   2003   2004   2005   2006   2007   2008   2009   2010   2011   2012   2013   2014   2015   2016 
# 697399 582075 543621 522912 497055 482124 458507 454411 442956 422106 443356 497491 258273 232146 225325 347709 542239 

## exclude problematic IDs ----
probIDs <- read_fst(paste0(dir_input_crosswalk, "no_crosswalk_no_death_ids.fst"), as.data.table = T) # IDs without cross
head(probIDs[, old_id])
sum(enrolledInfo[,QID] %in% probIDs[,old_id]) # number of problemetic IDs in enrolledInfo
enrolledInfo <- enrolledInfo[!(QID %in% probIDs[,old_id]),] # exclude problematic IDs
dim(enrolledInfo)
# [1] 7647589       2

## save enrolled INFO ----
fwrite(enrolledInfo, paste0(dir_output, "EnrolledInfo.csv"))

## save readmission info ----
ADRDhosp_time <- ADRDhosp_time[!(QID %in% probIDs[,old_id]),]
setorder(ADRDhosp_time, QID, year)
head(ADRDhosp_time, 10)
ADRDhosp_time[, count:=.N, by = QID][]
dim(ADRDhosp_time) # 11841340
re_ADRDhosp <- ADRDhosp_time[count>1, ]
dim(re_ADRDhosp) # 6902721
dt <- re_ADRDhosp[, .SD[2], by = QID]
head(dt)
dt[, first_ReAdyr := year][]
dt[, `:=` (count = NULL,
           year = NULL)][]
fwrite(dt, paste0(dir_output, "ReAdmissionInfo.csv"))
