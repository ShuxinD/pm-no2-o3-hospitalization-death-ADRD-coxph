###############################################################################
# Project: Air Pollution and mortality / readmission in AD/ADRD Medicare      #
# Code: covariates correlations, create table one                             #
# Input: "ADRD_mortality.csv"                                                  
# Output: "corr.csv" as correlations between covariates                       #
# Output: "table1.doc"                                                        #
# Author: Shuxin Dong                                                         #
# Date: 2021-02-08                                                            #
###############################################################################

############################# 0. Setup ########################################
rm(list = ls())
gc()

library(data.table)
setDTthreads(threads = 0)

setwd("/nfs/home/S/shd968/shared_space/ci3_shd968/dementia")

dir_in <- "/nfs/home/S/shd968/shared_space/ci3_shd968/dementia/"
dir_out <- "/nfs/home/S/shd968/shared_space/ci3_shd968/dementia/"

dt <- fread(paste0(dir_in, "ADRD_mortality.csv"))
names(dt)
# > names(dt)
# [1] "zip"                "year"               "qid"                "dead"               "sex"               
# [6] "race"               "age"                "dual"               "statecode"          "entry_age_break"   
# [11] "mean_bmi"           "smoke_rate"         "hispanic"           "pct_blk"            "medhouseholdincome"
# [16] "medianhousevalue"   "poverty"            "education"          "popdensity"         "pct_owner_occ"     
# [21] "firstADRDyr"        "pm25"               "no2"                "ozone"   

########################## 1. calculate corr ##################################
corr_data <- dt[,.(race, dual, mean_bmi, smoke_rate, hispanic,
                   pct_blk, medhouseholdincome, medianhousevalue, poverty,
                   education, popdensity, pct_owner_occ, pm25, no2, ozone)]
corr <- cor(corr_data)
write.csv(corr, file = paste0(dir_out, "corr.csv"))

####################### 2. create death end var ###############################
event <- dt[(dead),.(qid,dead)]
names(event)[2] <- "dead_end"
names(event)

dt <- merge(dt, event, by = "qid", all.x = TRUE)
dt$dead_end[is.na(dt$dead_end)] <- FALSE
summary(dt$dead_end)
gc()

############################# 3. create table one #############################
listVars <- c("pm25", "no2", "ozone", 
              "mean_bmi", "smoke_rate", "hispanic", "pct_blk",
              "medhouseholdincome", "medianhousevalue", "poverty",
              "education", "popdensity", "pct_owner_occ")
table1.personyr <- tableone::CreateTableOne(vars = listVars, 
                                            data = dt)
table1.personyr <- print(table1.personyr)

## for individual
head(dt)
setorder(dt, qid, year)
dt_ind <- dt[,.SD[1], by=qid]
head(dt_ind)
dt_ind <- dt_ind[,.(qid, sex, race, age, entry_age_break, dual, year, dead_end)]

listVars <- c("sex", "race", "age", "entry_age_break", "dual", "year", "dead_end")
catVars <- c("sex", "race", "entry_age_break", "dual", "year")
table1.individual <- tableone::CreateTableOne(vars = listVars, 
                                              data = dt_ind, 
                                              factorVars = catVars)
table1.individual <- print(table1.individual)

library(rtf)
rtffile <- RTF(paste0(dir_out, "table1.doc"))
addParagraph(rtffile, "Table1_person year")
addTable(rtffile, cbind(rownames(table1.personyr), table1.personyr))
addParagraph(rtffile, "\n \n Table1_individual")
addTable(rtffile, cbind(rownames(table1.individual), table1.individual))
done(rtffile)
