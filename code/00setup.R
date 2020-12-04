###############################################################################
# Project: Medicare Mortality and Air Pollution in AD/ADRD                    #
# Code: clean/merge datasets                                                  #
# Input: "national_exp.fst", "hospital_total.rds"                             #
# Output: "enrolledINFO.csv" - IDs and  the year of enrollment                #
# Output: "ADRDnational_exp.csv" as retricted "national_exp.fst"              #
# Output: "ADRDmort_all.csv" as data added death info but not checking dups   #
# Author: Shuxin Dong                                                         #
# Date: Nov 30, 2020                                                          #
###############################################################################

############################# 0. Setup ########################################
rm(list = ls())
gc()

library(data.table)
library(dplyr)
library(fst)

setwd("/nfs/home/S/shd968/shared_space/ci3_shd968/dementia")

dir_data <- "/nfs/home/S/shd968/shared_space/ci3_myitshak/dementia/"
dir_output <- "/nfs/home/S/shd968/shared_space/ci3_shd968/dementia/"

setDTthreads(threads = 0)

########## 1. get IDs and enroll info for ADRD cohort from hosp. ##############
med <- readRDS(paste0(dir_data, "hospital_total.rds"))
setDT(med)

ADRDmed <- subset(med, Alzheimer_pdx==1|Alzheimer_pdx2dx_10==1|Alzheimer_pdx2dx_25==1|
                    Dementia_pdx==1|Dementia_pdx2dx_10==1|Dementia_pdx2dx_25 ==1,
                  select = c("QID", "ADATE"))
ADRDmed$year_admit <- as.numeric(format(ADRDmed$ADATE, "%Y"))

# ## save all the IDs with ADRD diagnoses
# id <- ADRDmed[!duplicated(QID),]
# id$include <- 1 # add a column to mark whether to include
# id[, year := NULL]
# head(id)
# fwrite(id, paste0(dir_output, "populationID.csv"))
# 
# ## save enroll info for ADRD cohort
# enrollyr <- aggregate(year_admit ~ QID, ADRDmed, min)
# setnames(enrollyr, "year_admit", "firstADRDyear")
# fwrite(enrollyr, paste0(dir_output, "enrollyrINFO.csv"))

## drop subjects whose firstADRDyr <= 2006
enrollINFO <- aggregate(year_admit ~ QID, ADRDmed, min)
setDT(enrollINFO)
setnames(enrollINFO, "year_admit", "firstADRDyr")
enrolledINFO <- enrollINFO[firstADRDyr>2006]
fwrite(enrolledINFO, paste0(dir_output, "enrolledINFO.csv"))

##################### 2. load denominator file ################################
# denom_file <- read_fst(paste0(dir_data,"national_exp.fst"))
# >  dim(denom_file)
# [1] 538173801        37
n.denom <- 538173801
enrolledINFO  <- fread(paste0(dir_output, "enrolledINFO.csv"))

## split the national denominator file to save memory
## subset the dataset to those in ADRD cohort
denom_file.1 <- read_fst(paste0(dir_data,"national_exp.fst"),
                         from = 1, to = floor(n.denom/2), as.data.table = TRUE)
ADRDdenom.1 <- denom_file.1[QID %in% enrolledINFO[,QID]]
rm(denom_file.1)
gc()
denom_file.2 <- read_fst(paste0(dir_data,"national_exp.fst"),
                         from = ceiling(n.denom/2), as.data.table = TRUE)
ADRDdenom.2 <- denom_file.2[QID %in% enrolledINFO[,QID]]
rm(denom_file.2)
gc()
ADRDdenom <- rbind(ADRDdenom.1, ADRDdenom.2) # combine splits
rm(ADRDdenom.1, ADRDdenom.2)
gc()

ADRDdenom <- ADRDdenom[order(QID, year),] # order
fwrite(ADRDdenom, paste0(dir_output, "ADRDnational_exp.csv"))
gc()

ADRDdenom <- fread(paste0(dir_output, "ADRDnational_exp.csv"))

## add "firstADRDyr"
ADRDdenom <- merge(ADRDdenom, enrolledINFO, by="QID")
table(ADRDdenom$firstADRDyr)

## start to follow-up after firstADRDyr
ADRDmort <- ADRDdenom[year_admit>=firstADRDyr]

## get death info as mortINFO
ADRDmort$bene_dod <- as.Date(ADRDmort$bene_dod, format = "%Y-%m-%d") # convert format
mortINFO <- ADRDmort[,.(QID, bene_dod)][!is.na(bene_dod)]
mortINFO[, mort_yr := as.numeric(format(bene_dod, "%Y"))] #get death year for each ID
mortINFO[!duplicated(mortINFO)][duplicated(QID)]
mortINFO[!duplicated(mortINFO)][, ':=' (bene_dod = NULL, death = 1)][]

## add "mort_year" and "death" status into ADRDmort
ADRDmort <- merge(ADRDmort, mortINFO, by = "QID", all.x = TRUE)
ADRDmort$death[is.na(ADRDmort$death)] <- 0 # not dead mark as 0
summary(ADRDmort$death)

## Drop years after death
ADRDmort <- subset(ADRDmort, death==0|(death==1 & year_admit <= mort_yr))
fwrite(ADRDmort, paste0(dir_output, "ADRDmort_all.csv"))
##################### 3. check completeness of follow-up ######################
ADRDmort <- fread(paste0(dir_output, "ADRDmort_all.csv"))

## drop info of admission dates, diagnoses, diabetes
check <- ADRDmort[, !c("bene_dod", "ADATE", "DDATE", "year", "DIAG1", "DIAG2"), with=FALSE]

## remove duplication
check <- check[!duplicated(ADRDmort)]

## detect follow-up info problems
temp <- check[,.(QID, year_admit, firstADRDyr, mort_yr, death)]
temp[,followyr := year_admit-firstADRDyr]
temp[,.(min.followyr = min(followyr)), by=QID][, min.followyr] %>% table() #detect problems

## for those alive 
alive <- temp[death==0]
ideal_alive <- merge(alive[,.(max.year_admit = max(year_admit)), by = QID], enrollyr, by = "QID")[,.(idealN = max.year_admit-firstADRDyr + 1)]

temp[deaht==0][,.(max.year_admit = max(year_admit)), by = QID]

merge(temp[,.(max.year_admit = max(year_admit)), by = QID], enrollyr, by = "QID")[max.year_admit == mort_yr]
