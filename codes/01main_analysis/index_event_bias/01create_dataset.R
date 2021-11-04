#' Project: airPollution_ADRD
#' Code: create datasets to solve index event bias
#' Input: ...
#' Output: "index_event_bias_dt_mortality.csv" 
#' Output: "index_event_bias_dt_ReAd.csv
#' Author: Shuxin Dong
#' Date: 2021-09-15
###############################################################################

############################# 0. Setup ########################################
rm(list = ls())
gc()

library(data.table)

dir_in <- "/nfs/home/S/shd968/shared_space/ci3_shd968/dementia/data/"
dir_out <- "/nfs/home/S/shd968/shared_space/ci3_shd968/dementia/data/"

dt_mortality <- fread(paste0(dir_in, "ADRD_for_mortality.csv"), colClasses = c("zip"="character"))
dt_ReAd <- fread(paste0(dir_in, "ADRD_for_ReAd.csv"), colClasses = c("zip"="character"))
############################# 1. event info ####################################
dt_mortality[,.N,by=qid]
head(dt_mortality)
mortality_event <- dt_mortality[,.SD[.N], by=qid][,.(qid, firstADRDyr, pm25, no2, ozone_summer, ox)]
rm(dt_mortality)

dt_ReAd[,.N,by=qid]
head(dt_ReAd)
dt_ReAd[,.SD[.N], by=qid][,]
ReAd_event <- dt_ReAd[,.SD[.N], by=qid][, .(qid, firstADRDyr, pm25, no2, ozone_summer, ox)]
rm(dt_ReAd)
############################# 2. Includsion info ##############################
denom <- fread(paste0(dir_in, "ADRDpeople_denom.csv"), colClasses = c("zip"="character"))
names(denom)
#' create region variable based on statecode
NORTHEAST <- c("NY", "MA", "PA", "RI", "NH", "ME", "VT", "CT", "NJ")  
SOUTH <- c("DC", "VA", "NC", "WV", "KY", "SC", "GA", "FL", "AL", "TN", "MS", 
           "AR", "MD", "DE", "OK", "TX", "LA")
MIDWEST <- c("OH", "IN", "MI", "IA", "MO", "WI", "MN", "SD", "ND", "IL", "KS", "NE")
WEST <- c("MT", "CO", "WY", "ID", "UT", "NV", "CA", "OR", "WA", "AZ", "NM")
denom$region <- ifelse(denom$statecode %in% NORTHEAST, "NORTHEAST",
                    ifelse(denom$statecode %in% SOUTH, "SOUTH",
                           ifelse(denom$statecode  %in% MIDWEST, "MIDWEST",
                                  ifelse(denom$statecode  %in% WEST, "WEST",
                                         NA))))
denom[, region := as.factor(region)]
#' create inclusion info datasets
inclusion_mortality <- denom[(qid %in% mortality_event[,qid])&(year==firstADRDyr),.(year, zip, qid, mean_bmi, smoke_rate, hispanic, pct_blk ,medhouseholdincome, medianhousevalue, poverty, education, popdensity, pct_owner_occ, summer_tmmx, winter_tmmx, summer_rmax, winter_rmax, region)]
inclusion_ReAd <- denom[(qid %in% ReAd_event[,qid])&(year==firstADRDyr),.(year, zip, qid, mean_bmi, smoke_rate, hispanic, pct_blk ,medhouseholdincome, medianhousevalue, poverty, education, popdensity, pct_owner_occ, summer_tmmx, winter_tmmx, summer_rmax, winter_rmax,region)]
rm(denom)

exposure <-  fread(paste0(dir_in, "exposure.csv"), colClasses = c("ZIP"="character"))
inclusion_mortality <- merge(inclusion_mortality, exposure, by.x=c("year", "zip"), by.y=c("year", "ZIP"))
inclusion_ReAd <- merge(inclusion_ReAd, exposure, by.x=c("year", "zip"), by.y=c("year", "ZIP"))
length(unique(inclusion_mortality[,qid]))
length(unique(inclusion_ReAd[,qid]))
inclusion_mortality[, ox := (1.07*no2 + 2.075*ozone)/3.14]
inclusion_ReAd[, ox := (1.07*no2 + 2.075*ozone)/3.14]

## combine
colnames(mortality_event)[3:6] <- paste0(c("pm25", "no2", "ozone_summer", "ox"), "_e2")
colnames(ReAd_event)[3:6] <- paste0(c("pm25", "no2", "ozone_summer", "ox"), "_e2")
names(ReAd_event)
final_ReAd <- merge(ReAd_event, inclusion_ReAd, by= "qid")
final_mortality <- merge(mortality_event, inclusion_mortality, by="qid")

fwrite(final_mortality, paste0(dir_out, "index_event_bias_dt_mortality.csv"))
fwrite(final_ReAd, paste0(dir_out, "index_event_bias_dt_ReAd.csv"))
