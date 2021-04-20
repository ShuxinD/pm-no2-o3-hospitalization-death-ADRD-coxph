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

dir_input_hospital <- "/nfs/home/S/shd968/shared_space/ci3_shd968/dementia/data/ADRDhospitalization/"
dir_input_crosswalk <- "/nfs/home/S/shd968/shared_space/ci3_health_data/medicare/id_crosswalk/"
dir_output <- "/nfs/home/S/shd968/shared_space/ci3_shd968/dementia/data/"

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
dim(ADRDhosp) # [1] 18823188       16

any(duplicated(ADRDhosp))  # TRUE
sum(duplicated(ADRDhosp)) # 1363406
ADRDhosp <- unique(ADRDhosp)
gc()

ADRDhosp <- ADRDhosp[,.(QID, ADATE, year)]
head(ADRDhosp)
gc()

enrolledInfo <- ADRDhosp[, list(firstADRDyr = min(year)), by = .(QID)]
setDT(enrolledInfo)
dim(enrolledInfo)
# [1] 8048515       2

table(enrolledInfo[,firstADRDyr])
# 2000   2001   2002   2003   2004   2005   2006   2007   2008   2009   2010   2011   2012 
# 693287 578738 540839 520162 494557 479842 456069 451934 440574 419484 436534 447404 424715 
# 2013   2014   2015   2016 
# 410670 408980 434485 410241

######################### 2. exclude problematic IDs ##########################
probIDs <- read_fst(paste0(dir_input_crosswalk, "no_crosswalk_no_death_ids.fst"), 
                    as.data.table = T)
probIDs[, old_id]
enrolledInfo <- enrolledInfo[!(QID %in% probIDs[,old_id]),] # exclude problematic IDs
dim(enrolledInfo)
# [1] 8046408       2

######################### 3. save enrolled INFO ###############################
fwrite(enrolledInfo, paste0(dir_output, "EnrolledInfo.csv"))
