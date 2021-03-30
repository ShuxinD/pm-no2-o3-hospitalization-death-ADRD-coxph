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

setwd("/nfs/home/S/shd968/shared_space/ci3_shd968/dementia/")

dir_in <- "/nfs/home/S/shd968/shared_space/ci3_shd968/dementia/"
dir_out <- "/nfs/home/S/shd968/shared_space/ci3_shd968/dementia/results/"

dt <- fread(paste0(dir_in, "ADRD_mortality.csv"))
names(dt)
# > names(dt)
# [1] "qid"                "zip"                "year"               "summer_tmmx"       
# [5] "winter_tmmx"        "summer_rmax"        "winter_rmax"        "dead"              
# [9] "sex"                "race"               "age"                "dual"              
# [13] "statecode"          "mean_bmi"           "smoke_rate"         "hispanic"          
# [17] "pct_blk"            "medhouseholdincome" "medianhousevalue"   "poverty"           
# [21] "education"          "popdensity"         "pct_owner_occ"      "firstADRDyr"       
# [25] "pm25"               "no2"                "ozone"              "entry_age"         
# [29] "entry_age_break"    "race_collapsed"     "ox" 
dt[, race_collapsed:=as.factor(race_collapsed)]
dt[, entry_age_break := as.factor(entry_age_break)]


########################## 1. calculate corr ##################################
corr_data <- dt[,.(dual, mean_bmi, smoke_rate, hispanic,
                   pct_blk, medhouseholdincome, medianhousevalue, poverty,
                   education, popdensity, pct_owner_occ, pm25, no2, ozone, ox)]
corr <- cor(corr_data)
write.csv(corr, file = paste0(dir_out, "corr.csv"))

################### 2. create death end var and followup-time #################
event <- dt[(dead),.(qid,dead)]
names(event)[2] <- "dead_end"
names(event)

dt <- merge(dt, event, by = "qid", all.x = TRUE)
dt$dead_end[is.na(dt$dead_end)] <- FALSE
summary(dt$dead_end)
gc()

setorder(dt, qid, year)
head(dt)
duration <- dt[,.SD[.N], by = qid]
duration <- duration[,.(qid, year, firstADRDyr)]
head(duration)
duration[, followup_duration := year-firstADRDyr+1][]
dt <- merge(dt, duration[,.(qid,followup_duration)], by = "qid", all.x = TRUE)
head(dt)

############################# 3. create table one #############################
listVars <- c("pm25", "no2", "ozone", "ox",
              "mean_bmi", "smoke_rate", "hispanic", "pct_blk",
              "medhouseholdincome", "medianhousevalue", "poverty",
              "education", "popdensity", "pct_owner_occ",
              "summer_tmmx", "winter_tmmx", "summer_rmax", "winter_rmax")
table1.personyr <- tableone::CreateTableOne(vars = listVars, 
                                            data = dt)
table1.personyr <- print(table1.personyr)

## for individual
head(dt)
setorder(dt, qid, year)
dt_ind <- dt[,.SD[1], by=qid]
head(dt_ind)
dt_ind <- dt_ind[,.(qid, sex, race_collapsed, age, entry_age_break, dual, year, dead_end)]

listVars <- c("sex", "race_collapsed", "age", "entry_age_break", "dual", 
              "year", "followup_duration","dead_end")
catVars <- c("sex", "race_collapsed", "entry_age_break", "dual", "year")
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
