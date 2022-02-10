#' Project: airPollution_ADRD
#' Code: get ADRD cohort enrollment info
#' Input: "no_crosswalk_no_death_ids.fst"                
#' Input: "ADRD_`year`.fst"
#' Output: "EnrolledInfo.csv" - IDs and first year of ADRD admission   
#' Author: Shuxin Dong    
#' First create date: 2021-02-04                                                      

## setup ----
rm(list = ls())
gc()

library(data.table)
library(fst)
library(NSAPHutils)

setDTthreads(threads = 0)
setwd("/nfs/home/S/shd968/shared_space/ci3_shd968/medicareADRD")

dir_input_hospital <- "/nfs/home/S/shd968/shared_space/ci3_analysis/data_ADRDhospitalization/ADRDhospitalization_CCWlist/"
dir_input_crosswalk <- "/nfs/home/S/shd968/shared_space/ci3_health_data/medicare/id_crosswalk/"
dir_output <- "/nfs/home/S/shd968/shared_space/ci3_shd968/medicareADRD/data/"

## combine hospitalization files together ----
ADRDhosp <- NULL
for (i in 2000:2016) {
  adm_ <- read_fst(paste0(dir_input_hospital, "ADRD_", i,".fst"))
  ADRDhosp <- rbind(ADRDhosp, adm_)
  cat("finish loading file:", "ADRD_", i,".fst", "\n")
}
rm(adm_)
gc()

setDT(ADRDhosp)
names(ADRDhosp)
# [1] "QID"          "ADATE"        "DDATE"        "zipcode_R"    "DIAG1"        "DIAG2"        "DIAG3"       
# [8] "DIAG4"        "DIAG5"        "DIAG6"        "DIAG7"        "DIAG8"        "DIAG9"        "DIAG10"      
# [15] "AGE"          "Sex_gp"       "Race_gp"      "SSA_STATE_CD" "SSA_CNTY_CD"  "PROV_NUM"     "ADM_SOURCE"  
# [22] "ADM_TYPE"     "Dual"         "year"         "AD_primary"   "AD_any"       "ADRD_primary" "ADRD_any" 
#' year as Admission year

head(ADRDhosp)
dim(ADRDhosp) # [1]17600463       28

any(duplicated(ADRDhosp))  # TRUE
sum(duplicated(ADRDhosp)) # 1271 number of duplicates
ADRDhosp <- unique(ADRDhosp) # remove all the duplicates
dim(ADRDhosp) # [1]17599192       28 # dimention after removing duplicates
gc()

ADRDhosp_time <- ADRDhosp[,.(QID, year)] # drop all diagnosis codes, only save QID, admission year
gc()
head(ADRDhosp_time)
ADRDhosp_time <- unique(ADRDhosp_time) # remove duplicate of Admission year (some may be admitted several times in one year)
dim(ADRDhosp_time) # [1] 12868871        2
gc()

## get enroll info ----
#' first admission year for each QID
setorder(ADRDhosp_time, QID, year)
enrolledInfo <- ADRDhosp_time[, .(firstADRDyr = min(year)), by = QID]
dim(enrolledInfo)
# [1] 8153355       2

table(enrolledInfo[,firstADRDyr])
# 2000   2001   2002   2003   2004   2005   2006   2007   2008   2009   2010   2011   2012   2013   2014   2015 
# 687372 583704 544365 523328 497407 482350 458650 454507 443028 422160 439483 450657 428407 415478 414892 453065 
# 2016 
# 454502 

## exclude problematic IDs ----
probIDs <- read_fst(paste0(dir_input_crosswalk, "no_crosswalk_no_death_ids.fst"), as.data.table = T) # IDs without cross
head(probIDs[, old_id])
sum(enrolledInfo[,QID] %in% probIDs[,old_id]) # number of problemetic IDs in enrolledInfo # 2108
enrolledInfo <- enrolledInfo[!(QID %in% probIDs[,old_id]),] # exclude problematic IDs
dim(enrolledInfo)
# [1] 8151247       2

## save enrolled INFO ----
fwrite(enrolledInfo, paste0(dir_output, "EnrolledInfo.csv"))

## save readmission info: not save admision here
# ADRDhosp_time <- ADRDhosp_time[!(QID %in% probIDs[,old_id]),]
# setorder(ADRDhosp_time, QID, year)
# head(ADRDhosp_time, 10)
# ADRDhosp_time[, count:=.N, by = QID][]
# dim(ADRDhosp_time) # 11841340
# re_ADRDhosp <- ADRDhosp_time[count>1, ]
# dim(re_ADRDhosp) # 6902721
# dt <- re_ADRDhosp[, .SD[2], by = QID]
# head(dt)
# dt[, first_ReAdyr := year][]
# dt[, `:=` (count = NULL,
#            year = NULL)][]
# fwrite(dt, paste0(dir_output, "ReAdmissionInfo.csv"))
