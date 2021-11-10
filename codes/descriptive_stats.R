#' Project: airPollution_ADRD
#' Code: covariates correlations, create table one
#' Input: "ADRDcohort_clean.fst"
#' Output: "corr.csv" as correlations between covariates
#' Output: "table1.doc"
#' Author: Shuxin Dong
#' Date: 2021-02-08

## setup ----
rm(list = ls())
gc()

library(data.table)
library(fst)
setDTthreads(threads = 0)

setwd("/nfs/home/S/shd968/shared_space/ci3_shd968/dementia/")

dir_in <- "/nfs/home/S/shd968/shared_space/ci3_shd968/dementia/data/"
dir_out <- "/nfs/home/S/shd968/shared_space/ci3_shd968/dementia/airPollution_ADRD/results/descriptive_stats/"

## load mortality data----
dt <- read_fst(paste0(dir_in, "ADRDcohort_clean.fst"), as.data.table = T)
names(dt)
# [1] "qid"                "zip"                "year_prev"          "year"              
# [5] "sex"                "race"               "age"                "dual"              
# [9] "statecode"          "dead"               "mean_bmi"           "smoke_rate"        
# [13] "hispanic"           "pct_blk"            "medhouseholdincome" "medianhousevalue"  
# [17] "poverty"            "education"          "popdensity"         "pct_owner_occ"     
# [21] "summer_tmmx"        "winter_tmmx"        "summer_rmax"        "winter_rmax"       
# [25] "firstADRDyr"        "pm25"               "no2"                "ozone"             
# [29] "ozone_summer"       "entry_age"          "entry_age_break"    "race_collapsed"    
# [33] "ox"                 "region"  
dt$dual <- as.numeric(dt$dual)

## calculate corr ----
corr_data <- dt[,.(mean_bmi, smoke_rate, hispanic,
                   pct_blk, medhouseholdincome, medianhousevalue, poverty,
                   education, popdensity, pct_owner_occ, pm25, no2, ozone, ozone_summer, ox)]
library(corrplot)
M <- cor(corr_data)
cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}
p.mat <- cor.mtest(corr_data)

pdf(file = paste0(dir_out, "corr.pdf"), width = 12, height = 12)
corrplot(M, method="number", type = "lower", p.mat = p.mat, sig.level = 0.05)
dev.off()

## create event_end var and followup-time ----
event <- dt[(dead),.(qid,dead)]
names(event)[2] <- "dead_end"
names(event)

dt <- merge(dt, event, by = "qid", all.x = TRUE)
dt$dead_end[is.na(dt$dead_end)] <- FALSE
summary(dt$dead_end)
# Mode    FALSE     TRUE 
# logical  3345350 14219267 
gc()

setorder(dt, qid, year)
head(dt)
duration <- dt[,.SD[.N], by = qid]
duration <- duration[,.(qid, year, firstADRDyr)]
head(duration)
duration[, followup_duration := year-firstADRDyr][]
dt <- merge(dt, duration[,.(qid,followup_duration)], by = "qid", all.x = TRUE)
head(dt)

## table one ----
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

gc()

## distribution of all variables ----
Vars <- c("pm25", "no2", "ozone", "ozone_summer", "ox",
          "summer_tmmx", "winter_tmmx", "summer_rmax", "winter_rmax",
          "mean_bmi", "smoke_rate", 
          "medhouseholdincome", "medianhousevalue", "poverty",
          "education","pct_owner_occ", "hispanic", "pct_blk","popdensity")
summary <- rbind(dt[,..Vars][, lapply(.SD, mean)], dt[,..Vars][, lapply(.SD, sd)],
                 dt[,..Vars][, lapply(.SD, quantile)])
rownames(summary) <- c("mean", "sd", "0percentile", "25percentile", "50percentile","75percentile", "100percentile")
print(summary)
write.csv(summary, paste0(dir_out, "tableone_mortality_cohort_spatial.csv"))

## load read data----
dt <- read_fst(paste0(dir_in, "ADRDcohort_ReAd.fst"), as.data.table = T)
names(dt)
dt$dual <- as.numeric(dt$dual)

## calculate corr ----
corr_data <- dt[,.(mean_bmi, smoke_rate, hispanic,
                   pct_blk, medhouseholdincome, medianhousevalue, poverty,
                   education, popdensity, pct_owner_occ, pm25, no2, ozone, ozone_summer, ox)]
library(corrplot)
M <- cor(corr_data)
cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}
p.mat <- cor.mtest(corr_data)

pdf(file = paste0(dir_out, "corr_ReAd.pdf"), width = 12, height = 12)
corrplot(M, method="number", type = "lower", p.mat = p.mat, sig.level = 0.05)
dev.off()

## create event_end var and followup-time ----
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
duration[, followup_duration := year-firstADRDyr][]
dt <- merge(dt, duration[,.(qid,followup_duration)], by = "qid", all.x = TRUE)
head(dt)

## table one
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

## distribution of all variables ----
Vars <- c("pm25", "no2", "ozone", "ozone_summer", "ox",
          "summer_tmmx", "winter_tmmx", "summer_rmax", "winter_rmax",
          "mean_bmi", "smoke_rate", 
          "medhouseholdincome", "medianhousevalue", "poverty",
          "education","pct_owner_occ", "hispanic", "pct_blk","popdensity")
summary <- rbind(dt[,..Vars][, lapply(.SD, mean)], dt[,..Vars][, lapply(.SD, sd)],
                 dt[,..Vars][, lapply(.SD, quantile)])
rownames(summary) <- c("mean", "sd", "0percentile", "25percentile", "50percentile","75percentile", "100percentile")
print(summary)
write.csv(summary, paste0(dir_out, "tableone_ReAd_cohort_spatial.csv"))
