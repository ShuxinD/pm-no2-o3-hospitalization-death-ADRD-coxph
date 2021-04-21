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
# [1] "QID"            "ADATE"          "DDATE"          "DIAG1"          "DIAG2"         
# [6] "DIAG3"          "DIAG4"          "DIAG5"          "DIAG6"          "DIAG7"         
# [11] "DIAG8"          "DIAG9"          "DIAG10"         "year"           "ADRD_primary"  
# [16] "ADRD_secondary"
head(ADRDhosp)
dim(ADRDhosp) # [1] 16482471       16

any(duplicated(ADRDhosp))  # TRUE
sum(duplicated(ADRDhosp)) # 1244244
ADRDhosp <- unique(ADRDhosp)
dim(ADRDhosp) # [1] 15238227       16
gc()

ADRDhosp <- ADRDhosp[,.(QID, ADATE, year)]
head(ADRDhosp)
gc()

enrolledInfo <- ADRDhosp[, list(firstADRDyr = min(year)), by = .(QID)]
setDT(enrolledInfo)
dim(enrolledInfo)
# [1] 7317211       2

table(enrolledInfo[,firstADRDyr])
# 2000   2001   2002   2003   2004   2005   2006   2007   2008   2009   2010   2011   2012 
# 697399 582075 543621 522912 497055 482124 458507 454411 442956 422106 439424 404663 215713 
# 2013   2014   2015   2016 
# 194621 187898 295714 476012 

######################### 2. exclude problematic IDs ##########################
probIDs <- read_fst(paste0(dir_input_crosswalk, "no_crosswalk_no_death_ids.fst"), 
                    as.data.table = T)
probIDs[, old_id]
enrolledInfo <- enrolledInfo[!(QID %in% probIDs[,old_id]),] # exclude problematic IDs
dim(enrolledInfo)
# [1] 7315095       2

######################### 3. save enrolled INFO ###############################
fwrite(enrolledInfo, paste0(dir_output, "EnrolledInfo.csv"))

setorder(ADRDhosp, QID, year)
ADRDhosp[, ADATE:=NULL]
ADRDhosp <- unique(ADRDhosp)
ADRDhosp <- ADRDhosp[!(QID %in% probIDs[,old_id]),]

setorder(ADRDhosp, QID, year)
head(ADRDhosp, 10)
ADRDhosp[,count:=.N, by = .(QID)]
re_ADRDhosp <- ADRDhosp[count>1, ]
re_ADRDhosp[, .SD[2], by = QID]
re_ADRDhosp[, first_readmisson_yr := year][]
re_ADRDhosp[, count := NULL][]
fwrite(re_ADRDhosp, paste0(dir_output, "ReAdmissonInfo.csv"))
