###############################################################################
# Project: Air Pollution on mortality and readmission in Medicare AD/ADRD     #
# Code: get ADRD study population, exclude problematic ID                     #
# Input: "hospital_total.rds", "no_crosswalk_no_death_ids.fst"                #
# Output: "enrolledINFO.csv" - IDs and first year of ADRD admission           #
# Author: Shuxin Dong                                                         #
# Date: 2021-01-18                                                            #
###############################################################################

############################# 0. Setup ########################################
rm(list = ls())
gc()

library(data.table)
library(fst)

setDTthreads(threads = 0)
setwd("/nfs/home/S/shd968/shared_space/ci3_shd968/dementia")

dir_input_hospital <- "/nfs/home/S/shd968/shared_space/ci3_myitshak/dementia/"
dir_input_crosswalk <- "/nfs/home/S/shd968/shared_space/ci3_health_data/medicare/id_crosswalk/"
dir_output <- "/nfs/home/S/shd968/shared_space/ci3_shd968/dementia/"
########## 1. get IDs and enroll info for ADRD cohort from hosp. ##############
med <- readRDS(paste0(dir_input_hospital, "hospital_total.rds"))
setDT(med)

ADRDmed <- subset(med, Alzheimer_pdx==1|Alzheimer_pdx2dx_10==1|Alzheimer_pdx2dx_25==1|
                    Dementia_pdx==1|Dementia_pdx2dx_10==1|Dementia_pdx2dx_25 ==1,
                  select = c("QID", "ADATE"))
ADRDmed[, year_admit := as.numeric(format(ADATE, "%Y"))][]

enrolledInfo <- ADRDmed[, list(firstADRDyr = min(year_admit)), by = .(QID)]
setDT(enrolledInfo)
######################### 2. exclude problematic IDs ##########################
probIDs <- read_fst(paste0(dir_input_crosswalk, "no_crosswalk_no_death_ids.fst"), 
                    as.data.table = T)
probIDs[, old_id]
enrolledInfo <- enrolledInfo[!(QID %in% probIDs[,old_id]),] # exclude problematic IDs
######################### 3. save enrolled INFO ###############################
fwrite(enrolledInfo, paste0(dir_output, "enrolledInfo.csv"))
