###############################################################################
# Project: Medicare Mortality and Air Pollution in AD/ADRD                    #
# Code: clean/merge datasets                                                  #
# Input: "national_exp.fst", "hospital_total.rds"                             #
# Output: "final_.."                                                          #
# Author: Shuxin Dong                                                         #
# Date: Nov 30, 2020                                                          #
###############################################################################

############################# 0. Setup ########################################
library(data.table)
library(dplyr)
library(fst)

setwd("/nfs/home/S/shd968/shared_space/ci3_shd968/dementia")

dir_data <- "/nfs/home/S/shd968/shared_space/ci3_myitshak/dementia/"
dir_output <- "/nfs/home/S/shd968/shared_space/ci3_shd968/dementia/"

#### 1. get study population's IDs and firstADRDyear from hospital_total.rds ##
med <- readRDS(paste0(dir_data, "hospital_total.rds"))
med <- as.data.table(med)
## store all the IDs with ADRD diagnoses
d <- subset(med, Alzheimer_pdx==1|Alzheimer_pdx2dx_10==1|Alzheimer_pdx2dx_25==1|
              Dementia_pdx==1|Dementia_pdx2dx_10==1|Dementia_pdx2dx_25 ==1,
            select = c("QID", "year"))
id <- d[!duplicated(QID),] # remove duplicated IDs
id$include <- 1 # add a column to mark whether to include
fwrite(id, paste0(dir_output, "population.csv")) # save study population's ID

## add the time when first diagnosed with ADRD 
enrollyr <- aggregate(year~QID, d, min)
# setnames(enrollyr,"year","firstADRDyear")

############################# 2. load denominator-admissions ##################
denom_file <- read_fst(paste0(dir_data,"national_exp.fst"))
names(denom_file)


## subset the national dataset to those with ADRD diagnoses based on IDs
denom_file <- left_join(denom_file, id, by = "QID")
ADRDdenom <- subset(denom_file, include == 1)
fwrite(ADRDdenom, paste0(dir_output, "ADRDnational_exp.csv"))

rm(denom_file)
gc()

ADRDdenom <- ADRDdenom[order(QID, year),]
# ADRDdenom <- left_join(ADRDdenom, enrollyr, by="QID")
# summary(ADRDdenom$firstARDRyear)
# data=subset(ADRDdenom,year>=firstARDRyear)

## add the year of death
ADRDdenom$mort_year<-as.numeric(format(ADRDdenom$bene_dod, "%Y"))
table(ADRDdenom$mort_year)

#Add to all QID rows
data$dyear_no_miss<-data$mort_year
data$dyear_no_miss[is.na(data$dyear_no_miss)] <- 0
setDT(data)[, Max_dyear:= max(dyear_no_miss), QID]

#Drop years after death
data1=subset(data,Max_dyear==0|(Max_dyear!=0 & year<=Max_dyear))
rm(data)
