###############################################################################
# Project: Air Pollution on mortality and readmission in Medicare AD/ADRD     #
# Code: get ADRD study population, exclude problematic ID                     #
# Input: "no_crosswalk_no_death_ids.fst"                
# Input: "ADRD`type`_`year`.fst"
# Output: "EnrolledInfo.csv" - IDs and first year of ADRD admission           #
# Author: Shuxin Dong                                                         #
# Date: 2021-02-04                                                            #
###############################################################################

############################# 0. Setup ########################################
rm(list = ls())
gc()

library(data.table)
library(fst)
library(NSAPHutils)

setDTthreads(threads = 0)
setwd("/nfs/home/S/shd968/shared_space/ci3_shd968/dementia")

dir_input_hospital <- "/nfs/home/S/shd968/shared_space/ci3_shd968/dementia/ADRDhospitalization/"
dir_input_crosswalk <- "/nfs/home/S/shd968/shared_space/ci3_health_data/medicare/id_crosswalk/"
dir_output <- "/nfs/home/S/shd968/shared_space/ci3_shd968/dementia/"

################ 1. combine hospitalization files together ####################
ADRDhosp <- NULL
for (i in 2000:2016) {
  for (type in c("primary", "secondary")) {
    adm_ <- read_fst(paste0(dir_input_hospital, "ADRD", type,"_", i,".fst"))
    ADRDhosp <- rbind(ADRDhosp, adm_)
    cat("finish loading file:", "ADRD", type, "_", i,".fst", "\n")
  }
}
rm(adm_)
gc()
setDT(ADRDhosp)
names(ADRDhosp)

any(duplicated(ADRDhosp))  # TRUE
ADRDhosp <- unique(ADRDhosp)
gc()

ADRDhosp <- ADRDhosp[,.(QID, ADATE)][, year_admit := as.numeric(format(ADATE, "%Y"))][]
gc()

enrolledInfo <- ADRDhosp[, list(firstADRDyr = min(year_admit)), by = .(QID)]
setDT(enrolledInfo)

table(enrolledInfo$firstADRDyr)



######################### 2. exclude problematic IDs ##########################
probIDs <- read_fst(paste0(dir_input_crosswalk, "no_crosswalk_no_death_ids.fst"), 
                    as.data.table = T)
probIDs[, old_id]
enrolledInfo <- enrolledInfo[!(QID %in% probIDs[,old_id]),] # exclude problematic IDs

######################### 3. save enrolled INFO ###############################
fwrite(enrolledInfo, paste0(dir_output, "enrolledInfo.csv"))
