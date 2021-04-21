###############################################################################
# Project: Air Pollution on mortality and readmission in Medicare AD/ADRD     #
# Code: clean dataset for mortality analyses
# Input: "ADRDpeople.csv"
# Output: "ADRD_for_mortality.csv" 
# Author: Shuxin Dong                                                         #
# Date: 2021-02-08                                                            #
###############################################################################

############################# 0. Setup ########################################
rm(list = ls())
gc()

library(data.table)
setDTthreads(threads = 15)

dir_in <- "/nfs/home/S/shd968/shared_space/ci3_shd968/dementia/data/"
dir_out <- "/nfs/home/S/shd968/shared_space/ci3_shd968/dementia/data/"

########################### 1. Load data ######################################
ADRDpeople <- fread(paste0(dir_in, "ADRDpeople.csv"), colClasses = c("zip"="character"))
names(ADRDpeople)
# [1] "zip"                "year"               "qid"                "sex"               
# [5] "race"               "age"                "dual"               "statecode"         
# [9] "dead"               "mean_bmi"           "smoke_rate"         "hispanic"          
# [13] "pct_blk"            "medhouseholdincome" "medianhousevalue"   "poverty"           
# [17] "education"          "popdensity"         "pct_owner_occ"      "summer_tmmx"       
# [21] "winter_tmmx"        "summer_rmax"        "winter_rmax"        "firstADRDyr"       
# [25] "pm25"               "no2"                "ozone"              "ozone_summer"  
dim(ADRDpeople)
# [1] 25112131       28
ADRDpeople <- ADRDpeople[year!=firstADRDyr, ] # remove firstADRDyr
dim(ADRDpeople) # study population personyrs
# [1] 17807821       28
uniqueN(ADRDpeople, by = "qid") # study population individuals
# [1] 5069279
dt <- na.omit(ADRDpeople) # remove NAs
dim(dt)
# [1] 17376870       28
uniqueN(dt, by = "qid")
# [1] 4962098
dim(ADRDpeople)[1] - dim(dt)[1] # number of person-years to be removed due to NAs
# [1] 430951
uniqueN(ADRDpeople, by = "qid") - uniqueN(dt, by = "qid") # number of indivuals removed due to NAs
# [1] 107181
dt <- dt[race!=0,] # remove those with race==0 (unknown)
dim(dt) # final
# [1] 17338260       27
uniqueN(dt, by = "qid") #final # of subjects
# [1] 4949616

########################### 2. Clean data #####################################
temp <- dt[, .(start_yr = min(year),
               end_yr = max(year),
               count = uniqueN(year)), by = .(qid)]
dim(temp)
head(temp)
temp <- merge(temp, unique(dt[,.(qid,firstADRDyr)]), by = "qid", all.x = TRUE)
dim(temp)
head(temp)
gc()

## remove those not followed-up from the year following firstADRDyr
dim(temp[start_yr > (firstADRDyr+1)]) # number of subjects to remove
# [1] 15378     5
# dt[!(qid %in% temp[start_yr != (firstADRDyr+1)][, qid]), ]
dt <- dt[!(qid %in% temp[start_yr != (firstADRDyr+1)][, qid]), ]
gc()

dim(ADRDpeople)[1] - dim(dt)[1] # number of person-years to removed
# [1] 524314

## remove those not having each year’s info during follow-up
dim(temp[(end_yr-start_yr+1) != count,]) # number of subjects to remove
# [1] 5399     5
dt <- dt[qid %in% temp[(end_yr-start_yr+1) == count, qid],]
gc()

dim(ADRDpeople)[1] - dim(dt)[1]
# [1] 555521

head(dt)

## create entry_age variable
tempAge <- dt[, .(entry_age = min(age)), by = .(qid)]
head(tempAge)
min(tempAge[,entry_age])
max(tempAge[,entry_age])
seq(65, 115, 2)
tempAge[, entry_age_break := cut(entry_age, breaks = seq(65, 115, 2), right = FALSE)][]
summary(tempAge[,entry_age_break])
head(tempAge)
dt <- merge(dt, tempAge, by = "qid") ## add entry_age and entry_age_break

## merge different race categories and create race_collapsed
dt[,race:=as.factor(race)]
table(dt[,race])
dt$race_collapsed <- "Others"
dt$race_collapsed[dt$race==1] <- "White"
dt$race_collapsed[dt$race==2] <- "Black"
dt[, race_collapsed:=as.factor(race_collapsed)]

## generate Ox based on no2 and ozone
dt[, ox := (1.07*no2 + 2.075*ozone)/3.14]
head(dt)

fwrite(dt, paste0(dir_out, "ADRD_for_mortality.csv"))
