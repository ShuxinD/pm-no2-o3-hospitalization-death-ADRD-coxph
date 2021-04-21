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

dir_in <- "/nfs/home/S/shd968/shared_space/ci3_shd968/dementia/data/"
dir_out <- "/nfs/home/S/shd968/shared_space/ci3_shd968/dementia/results/"

dt <- fread(paste0(dir_in, "ADRD_for_mortality.csv"), colClasses = c("zip"="character"))
names(dt)
# [1] "qid"                "zip"                "year"               "sex"               
# [5] "race"               "age"                "dual"               "statecode"         
# [9] "dead"               "mean_bmi"           "smoke_rate"         "hispanic"          
# [13] "pct_blk"            "medhouseholdincome" "medianhousevalue"   "poverty"           
# [17] "education"          "popdensity"         "pct_owner_occ"      "summer_tmmx"       
# [21] "winter_tmmx"        "summer_rmax"        "winter_rmax"        "firstADRDyr"       
# [25] "pm25"               "no2"                "ozone"              "ozone_summer"      
# [29] "entry_age"          "entry_age_break"    "race_collapsed"     "ox"
dt[, race_collapsed:=as.factor(race_collapsed)]
dt[, entry_age_break := as.factor(entry_age_break)]
NORTHEAST <- c("NY", "MA", "PA", "RI", "NH", "ME", "VT", "CT", "NJ")  
SOUTH <- c("DC", "VA", "NC", "WV", "KY", "SC", "GA", "FL", "AL", "TN", "MS", 
           "AR", "MD", "DE", "OK", "TX", "LA")
MIDWEST <- c("OH", "IN", "MI", "IA", "MO", "WI", "MN", "SD", "ND", "IL", "KS", "NE")
WEST <- c("MT", "CO", "WY", "ID", "UT", "NV", "CA", "OR", "WA", "AZ", "NM")
dt$region <- ifelse(dt$statecode %in% NORTHEAST, "NORTHEAST",
                    ifelse(dt$statecode %in% SOUTH, "SOUTH",
                           ifelse(dt$statecode  %in% MIDWEST, "MIDWEST",
                                  ifelse(dt$statecode  %in% WEST, "WEST",
                                         NA))))
dt[, region := as.factor(region)]
summary(dt$region)

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
listVars <- c("pm25", "no2", "ozone", "ox", "ozone_summer",
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
dt_ind <- dt_ind[,.(qid, dead_end, sex, race_collapsed, age, entry_age_break, 
                    region, dual, year, followup_duration)]

listVars <- c("sex", "race_collapsed", "age", "entry_age_break", "dual", "region",
              "year", "followup_duration","dead_end")
catVars <- c("sex", "race_collapsed", "entry_age_break", "dual", "region", "year")
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

############################# 0. Setup ########################################
rm(list = ls())
gc()

library(data.table)
setDTthreads(threads = 0)

setwd("/nfs/home/S/shd968/shared_space/ci3_shd968/dementia/")

dir_in <- "/nfs/home/S/shd968/shared_space/ci3_shd968/dementia/data/"
dir_out <- "/nfs/home/S/shd968/shared_space/ci3_shd968/dementia/results/"

dt <- fread(paste0(dir_in, "ADRD_for_ReAd.csv"), colClasses = c("zip"="character"))
names(dt)
# [1] "qid"                "zip"                "year"               "sex"               
# [5] "race"               "age"                "dual"               "statecode"         
# [9] "dead"               "mean_bmi"           "smoke_rate"         "hispanic"          
# [13] "pct_blk"            "medhouseholdincome" "medianhousevalue"   "poverty"           
# [17] "education"          "popdensity"         "pct_owner_occ"      "summer_tmmx"       
# [21] "winter_tmmx"        "summer_rmax"        "winter_rmax"        "firstADRDyr"       
# [25] "pm25"               "no2"                "ozone"              "ozone_summer"      
# [29] "entry_age"          "entry_age_break"    "race_collapsed"     "ox"                
# [33] "first_ReAdyr"       "ReAd" 
dt[, race_collapsed:=as.factor(race_collapsed)]
dt[, entry_age_break := as.factor(entry_age_break)]
NORTHEAST <- c("NY", "MA", "PA", "RI", "NH", "ME", "VT", "CT", "NJ")  
SOUTH <- c("DC", "VA", "NC", "WV", "KY", "SC", "GA", "FL", "AL", "TN", "MS", 
           "AR", "MD", "DE", "OK", "TX", "LA")
MIDWEST <- c("OH", "IN", "MI", "IA", "MO", "WI", "MN", "SD", "ND", "IL", "KS", "NE")
WEST <- c("MT", "CO", "WY", "ID", "UT", "NV", "CA", "OR", "WA", "AZ", "NM")
dt$region <- ifelse(dt$statecode %in% NORTHEAST, "NORTHEAST",
                    ifelse(dt$statecode %in% SOUTH, "SOUTH",
                           ifelse(dt$statecode  %in% MIDWEST, "MIDWEST",
                                  ifelse(dt$statecode  %in% WEST, "WEST",
                                         NA))))
dt[, region := as.factor(region)]
summary(dt$region)

########################## 1. calculate corr ##################################
corr_data <- dt[,.(dual, mean_bmi, smoke_rate, hispanic,
                   pct_blk, medhouseholdincome, medianhousevalue, poverty,
                   education, popdensity, pct_owner_occ, pm25, no2, ozone, ozone_summer, ox)]
corr <- cor(corr_data)
write.csv(corr, file = paste0(dir_out, "corr.csv"))

################### 2. create death end var and followup-time #################
event <- dt[(ReAd),.(qid,ReAd)]
names(event)[2] <- "ReAd_end"
names(event)

dt <- merge(dt, event, by = "qid", all.x = TRUE)
dt$ReAd_end[is.na(dt$ReAd_end)] <- FALSE
summary(dt$ReAd_end)
gc()

setorder(dt, qid, year)
head(dt)
duration <- dt[,.SD[.N], by = qid]
duration <- duration[,.(qid, year, first_ReAdyr, firstADRDyr)]
head(duration)
duration[, followup_duration := year-firstADRDyr+1][]
dt <- merge(dt, duration[,.(qid,followup_duration)], by = "qid", all.x = TRUE)
head(dt)

############################# 3. create table one #############################
listVars <- c("pm25", "no2", "ozone", "ox", "ozone_summer",
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
dt_ind <- dt_ind[,.(qid, dead_end, ReAd_end, sex, race_collapsed, age, entry_age_break, 
                    region, dual, year, followup_duration)]

listVars <- c("sex", "race_collapsed", "age", "entry_age_break", "dual", "region",
              "year", "followup_duration","dead_end", "ReAd_end")
catVars <- c("sex", "race_collapsed", "entry_age_break", "dual", "region", "year")
table1.individual <- tableone::CreateTableOne(vars = listVars, 
                                              data = dt_ind, 
                                              factorVars = catVars)
table1.individual <- print(table1.individual)

library(rtf)
rtffile <- RTF(paste0(dir_out, "table1_ReAd.doc"))
addParagraph(rtffile, "Table1_person year_ReAd")
addTable(rtffile, cbind(rownames(table1.personyr), table1.personyr))
addParagraph(rtffile, "\n \n Table1_individual_ReAd")
addTable(rtffile, cbind(rownames(table1.individual), table1.individual))
done(rtffile)
