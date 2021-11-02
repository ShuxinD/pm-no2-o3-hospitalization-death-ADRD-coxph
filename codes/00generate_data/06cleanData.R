#' Project: airPollution_ADRD
#' Code: clean dataset for mortality analyses
#' Input: "ADRDcohort.fst"
#' Output: "ADRDcohort_clean.fst" 
#' Author: Shuxin Dong 
#' First create cate: 2021-02-08

## setup----
rm(list = ls())
gc()

library(data.table)
library(fst)
setDTthreads(threads = 15)

dir_in <- "/nfs/home/S/shd968/shared_space/ci3_shd968/dementia/data/"
dir_out <- "/nfs/home/S/shd968/shared_space/ci3_shd968/dementia/data/"

## load data ----
ADRDcohort <- read_fst(paste0(dir_in, "ADRDcohort.fst"), as.data.table = T)
names(ADRDcohort)
# [1] "zip"                "year_prev"          "qid"                "year"              
# [5] "sex"                "race"               "age"                "dual"              
# [9] "statecode"          "dead"               "mean_bmi"           "smoke_rate"        
# [13] "hispanic"           "pct_blk"            "medhouseholdincome" "medianhousevalue"  
# [17] "poverty"            "education"          "popdensity"         "pct_owner_occ"     
# [21] "summer_tmmx"        "winter_tmmx"        "summer_rmax"        "winter_rmax"       
# [25] "firstADRDyr"        "pm25"               "no2"                "ozone"             
# [29] "ozone_summer" 
dim(ADRDcohort)
# [1] 18150478       29
uniqueN(ADRDcohort[,qid]) # study population individuals
# [1] 5226549

## omit ----
#' first: remove NAs
dt <- na.omit(ADRDcohort) # remove NAs
dim(dt) # person-year
# [1] 17702025       29
uniqueN(dt[,qid]) # number of subjects
# [1] 5114720

#' second: remove those without complete follow-ups
omitInfo <- read.csv(paste0(dir_in,"omitInfo.csv"))
dt <- dt[!(qid %in% omitInfo$qid),]
dim(dt) # person-year
# [1] 17603959       29
uniqueN(dt[,qid]) # number of subjects
# [1] 5090939

#' third: race==unknow
uniqueN(dt[race==0,qid]) # number subjects with unknown race info
# [1] 12795
dt <- dt[!(qid %in% dt[race==0,qid]),]
dim(dt) # person-year
# [1] 17564617       29
uniqueN(dt[,qid]) # number of subjects
# [1] 5078144


dim(dt) # final
# [1] 17564617       29
uniqueN(dt, by = "qid") #final # of subjects
# [1] 5078144

## add necessary variables ----
#' create entry_age variable, 5 years as a break
tempAge <- dt[, .(entry_age = min(age)), by = qid]
head(tempAge)
min(tempAge[,entry_age])
max(tempAge[,entry_age])
seq(65, 115, 5)
tempAge[, entry_age_break := cut(entry_age, breaks = seq(65, 115, 5), right = FALSE)][]
summary(tempAge[,entry_age_break])
head(tempAge)
dt <- merge(dt, tempAge, by = "qid") ## add entry_age and entry_age_break

#' merge different race categories and create race_collapsed
dt[,race:=as.factor(race)]
table(dt[,race])
dt$race_collapsed <- "Others"
dt$race_collapsed[dt$race==1] <- "White"
dt$race_collapsed[dt$race==2] <- "Black"
dt$race_collapsed[dt$race==5] <- "Hispanic"
dt[, race_collapsed:=as.factor(race_collapsed)]

#' generate Ox based on no2 and ozone
dt[, ox := (1.07*no2 + 2.075*ozone)/3.14]
head(dt)

#' generate region based on statecode
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

write_fst(dt, paste0(dir_out, "ADRDcohort_clean.fst"))
