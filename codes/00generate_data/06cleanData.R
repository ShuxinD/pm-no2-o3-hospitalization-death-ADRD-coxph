###############################################################################
# Project: Air Pollution on mortality and readmission in Medicare AD/ADRD     #
# Code: clean dataset for mortality analyses
# Input: "ADRDpeople.csv"
# Output: "ADRD_mortality.csv" 
# Author: Shuxin Dong                                                         #
# Date: 2021-02-08                                                            #
###############################################################################

############################# 0. Setup ########################################
rm(list = ls())
gc()

library(data.table)
setDTthreads(threads = 0)

dir_in <- "/nfs/home/S/shd968/shared_space/ci3_shd968/dementia/"
dir_out <- "/nfs/home/S/shd968/shared_space/ci3_shd968/dementia/"

########################### 1. Load data ######################################
ADRDpeople <- fread(paste0(dir_in, "ADRDpeople.csv"))
names(ADRDpeople)
# [1] "zip"                "year"               "qid"                "summer_tmmx"        "winter_tmmx"        "summer_rmax"       
# [7] "winter_rmax"        "dead"               "sex"                "race"               "age"                "dual"              
# [13] "statecode"          "entry_age_break"    "mean_bmi"           "smoke_rate"         "hispanic"           "pct_blk"           
# [19] "medhouseholdincome" "medianhousevalue"   "poverty"            "education"          "popdensity"         "pct_owner_occ"     
# [25] "firstADRDyr"        "pm25"               "no2"                "ozone"  
dim(ADRDpeople)
# [1] 27049496       28
ADRDpeople <- ADRDpeople[year!=firstADRDyr, ] # remove firstADRDyr
dim(ADRDpeople) # study population personyrs
# [1] 19014337       28
uniqueN(ADRDpeople, by = "qid") # study population individuals
# [1] 5664786
dt <- na.omit(ADRDpeople) # remove NAs
dim(dt)
# [1] 17081631       28
uniqueN(dt, by = "qid")
# [1] 5154447
dim(ADRDpeople)[1] - dim(dt)[1] # number of person-years to be removed due to NAs
# [1] 1932706
uniqueN(ADRDpeople, by = "qid") - uniqueN(dt, by = "qid") # number of indivuals removed due to NAs
# [1] 510339
dt[, entry_age_break:=NULL] # remove entry_age_break from the denominator file
dt <- dt[race!=0,] # remove those with race==0 (unknown)
dim(dt) # final
# [1] 17043989       27
uniqueN(dt, by = "qid") #final # of subjects
# [1] 5141820

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
# [1] 64461     5
dt[!(qid %in% temp[start_yr != (firstADRDyr+1)][, qid]), ]
dt <- dt[!(qid %in% temp[start_yr != (firstADRDyr+1)][, qid]), ]
gc()

dim(ADRDpeople)[1] - dim(dt)[1] # number of person-years to removed
# [1] 2185897

## remove those not having each year’s info during follow-up
dim(temp[(end_yr-start_yr+1) != count,]) # number of subjects to remove
# [1] 13646     5
dt <- dt[qid %in% temp[(end_yr-start_yr+1) == count, qid],]
gc()

dim(ADRDpeople)[1] - dim(dt)[1]
# [1] 2260689

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

fwrite(dt, paste0(dir_out, "ADRD_mortality.csv"))
