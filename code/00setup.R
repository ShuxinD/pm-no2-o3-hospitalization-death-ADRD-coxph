###############################################################################
# Project: Medicare Mortality and Air Pollution in AD/ADRD                    #
# Code: clean/merge datasets                                                  #
# Input: "national_exp.fst", "hospital_total.rds"                             #
# Output: "populationID.csv" as study population's IDs                        #
# Output: "enrollyrINFO.csv" as the year of enrollment for each ID            #
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
                  select = c("QID", "year"))

## save all the IDs with ADRD diagnoses
id <- ADRDmed[!duplicated(QID),]
id$include <- 1 # add a column to mark whether to include
id[, year := NULL]
head(id)
fwrite(id, paste0(dir_output, "populationID.csv"))

## save enroll info for ADRD cohort
enrollyr <- aggregate(year ~ QID, ADRDmed, min)
setnames(enrollyr, "year", "firstADRDyear")
fwrite(enrollyr, paste0(dir_output, "enrollyrINFO.csv"))

##################### 2. load denominator file ################################
# denom_file <- read_fst(paste0(dir_data,"national_exp.fst"))
# >  dim(denom_file)
# [1] 538173801        37
# n.denom <- 538173801
# id  <- fread(paste0(dir_output, "populationID.csv"))

## split the national denominator file to save memory and subset
denom_file.1 <- read_fst(paste0(dir_data,"national_exp.fst"),
                         from = 1, to = floor(n.denom/2))
setDT(denom_file.1)
ADRDdenom.1 <- denom_file.1[QID %in% id[,QID]]
rm(denom_file.1)
gc()
denom_file.2 <- read_fst(paste0(dir_data,"national_exp.fst"),
                         from = ceiling(n.denom/2))
setDT(denom_file.2)
ADRDdenom.2 <- denom_file.2[QID %in% id[,QID]]
rm(denom_file.2)
gc()

ADRDdenom <- rbind(ADRDdenom.1, ADRDdenom.2) # combine splits
rm(ADRDdenom.1, ADRDdenom.2)
gc()

ADRDdenom <- ADRDdenom[order(QID, year),] # order
fwrite(ADRDdenom, paste0(dir_output, "ADRDnational_exp.csv"))
gc()

ADRDdenom <- fread(paste0(dir_output, "ADRDnational_exp.csv"))
enrollyr <- fread(paste0(dir_output, "enrollyrINFO.csv"))

## add firstADRDyear
ADRDdenom <- merge(ADRDdenom, enrollyr, by="QID")
summary(ADRDdenom$firstADRDyear)

## start to follow-up after firstADRDyear
ADRDmort <- ADRDdenom[year>firstADRDyear]
rm(ADRDdenom)
temp <- ADRDmort[,.(followyr = year-firstADRDyear), by=QID]
temp[,.SD[1], by=QID] %>% filter(followyr!=1) %>% select(followyr) %>% count()

# ## add the year of death
# ADRDdenom$mort_year<-as.numeric(format(ADRDdenom$bene_dod, "%Y"))
# table(ADRDdenom$mort_year)
# 
# #Add to all QID rows
# data$dyear_no_miss<-data$mort_year
# data$dyear_no_miss[is.na(data$dyear_no_miss)] <- 0
# setDT(data)[, Max_dyear:= max(dyear_no_miss), QID]
# 
# #Drop years after death
# data1=subset(data,Max_dyear==0|(Max_dyear!=0 & year<=Max_dyear))
# rm(data)
