###############################################################################
# Project: Air Pollution and mortality / readmission in AD/ADRD Medicare      #
# Code: Poisson survival model
# Input: "ADRD_mortality.csv"                                                  
# Output: 
# Author: Shuxin Dong                                                         #
# Date: 2021-02-17                                                            #
###############################################################################
## set core as 24

############################# 0. Setup ########################################
rm(list = ls())
gc()

library(data.table)
setDTthreads(threads = 0)

library(parallel)
library(mgcv)
# numCores <- detectCores()

setwd("/nfs/home/S/shd968/shared_space/ci3_shd968/dementia/")

dir_data <- "/nfs/home/S/shd968/shared_space/ci3_shd968/dementia/"
dir_results <- "/nfs/home/S/shd968/shared_space/ci3_shd968/dementia/"

dt <- fread(paste0(dir_data, "ADRD_mortality.csv"))
names(dt)

dt[, followupyr := (year - firstADRDyr)]
dt[, statecode := as.factor(statecode)]

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
dt[, region:=as.factor(region)]
head(dt)
gc()

IQRs <- data.table(IQR(dt$pm25), IQR(dt$no2), IQR(dt$ozone), IQR(dt$ox))
colnames(IQRs) <- c("pm25", "no2", "ozone", "ox")
print(IQRs)

#################### 1. single-pollutant models ###############################
cl <- makeCluster(23)

p_1_pm25 <- bam(dead ~ s(followupyr, by = as.factor(entry_age_break)) + 
                  s(followupyr, by = as.factor(sex)) +
                  s(followupyr, by = as.factor(race_collapsed)) + 
                  s(followupyr, by = as.factor(dual)) +
                  pm25 + 
                  mean_bmi + smoke_rate + hispanic + pct_blk +
                  medhouseholdincome + medianhousevalue +
                  poverty + education + popdensity + pct_owner_occ +
                  summer_tmmx + winter_tmmx + summer_rmax + winter_rmax +
                  as.factor(year) + as.factor(region) +
                  s(region, bs = "re"),
                family = poisson(), data = dt,
                discrete = TRUE,
                nthreads = length(cl))
tb <- summary(p_1_pm25)$p.table
tb <- as.data.frame(tb)
setDT(tb, keep.rownames = TRUE)[]
fwrite(tb, paste0(dir_results, "poisson_1_pm25.csv"))

IQRunit <- c(IQRs$pm25)
HR <- tb[2,]
HR <- cbind(HR,IQRunit)
print(HR)
HR[, `:=`(HR_IQR = exp(Estimate*IQRunit),
          HR_lci = exp((Estimate-1.96*`Std. Error`)*IQRunit),
          HR_uci = exp((Estimate+1.96*`Std. Error`)*IQRunit))][]
fwrite(HR, paste0(dir_results, "poisson_1_pm25_HR.csv"))
gc()

p_1_no2 <- bam(dead ~ s(followupyr, by = as.factor(entry_age_break)) + 
                 s(followupyr, by = as.factor(sex)) +
                 s(followupyr, by = as.factor(race_collapsed)) + 
                 s(followupyr, by = as.factor(dual)) +
                 no2 + 
                 mean_bmi + smoke_rate + hispanic + pct_blk +
                 medhouseholdincome + medianhousevalue +
                 poverty + education + popdensity + pct_owner_occ +
                 summer_tmmx + winter_tmmx + summer_rmax + winter_rmax +
                 as.factor(year) + as.factor(region) +
                 s(region, bs = "re"),
               family = poisson(), data = dt,
               discrete = TRUE,
               nthreads = length(cl))
tb <- summary(p_1_no2)$p.table
tb <- as.data.frame(tb)
setDT(tb, keep.rownames = TRUE)[]
fwrite(tb, paste0(dir_results, "poisson_1_no2.csv"))

IQRunit <- c(IQRs$no2)
HR <- tb[2,]
HR <- cbind(HR,IQRunit)
print(HR)
HR[, `:=`(HR_IQR = exp(Estimate*IQRunit),
          HR_lci = exp((Estimate-1.96*`Std. Error`)*IQRunit),
          HR_uci = exp((Estimate+1.96*`Std. Error`)*IQRunit))][]
fwrite(HR, paste0(dir_results, "poisson_1_no2_HR.csv"))


p_1_ozone <- bam(dead ~ s(followupyr, by = as.factor(entry_age_break)) + 
                   s(followupyr, by = as.factor(sex)) +
                   s(followupyr, by = as.factor(race_collapsed)) + 
                   s(followupyr, by = as.factor(dual)) +
                   ozone + 
                   mean_bmi + smoke_rate + hispanic + pct_blk +
                   medhouseholdincome + medianhousevalue +
                   poverty + education + popdensity + pct_owner_occ +
                   summer_tmmx + winter_tmmx + summer_rmax + winter_rmax +
                   as.factor(year) + as.factor(region) +
                   s(region, bs = "re"),
                 family = poisson(), data = dt,
                 discrete = TRUE,
                 nthreads = length(cl))
tb <- summary(p_1_ozone)$p.table
tb <- as.data.frame(tb)
setDT(tb, keep.rownames = TRUE)[]
fwrite(tb, paste0(dir_results, "poisson_1_ozone.csv"))

IQRunit <- c(IQRs$ozone)
HR <- tb[2,]
HR <- cbind(HR,IQRunit)
print(HR)
HR[, `:=`(HR_IQR = exp(Estimate*IQRunit),
          HR_lci = exp((Estimate-1.96*`Std. Error`)*IQRunit),
          HR_uci = exp((Estimate+1.96*`Std. Error`)*IQRunit))][]
fwrite(HR, paste0(dir_results, "poisson_1_ozone_HR.csv"))

p_1_ox <- bam(dead ~ s(followupyr, by = as.factor(entry_age_break)) + 
                   s(followupyr, by = as.factor(sex)) +
                   s(followupyr, by = as.factor(race_collapsed)) + 
                   s(followupyr, by = as.factor(dual)) +
                   ox + 
                   mean_bmi + smoke_rate + hispanic + pct_blk +
                   medhouseholdincome + medianhousevalue +
                   poverty + education + popdensity + pct_owner_occ +
                   summer_tmmx + winter_tmmx + summer_rmax + winter_rmax +
                   as.factor(year) + as.factor(region) +
                   s(region, bs = "re"),
                 family = poisson(), data = dt,
                 discrete = TRUE,
                 nthreads = length(cl))
tb <- summary(p_1_ox)$p.table
tb <- as.data.frame(tb)
setDT(tb, keep.rownames = TRUE)[]
fwrite(tb, paste0(dir_results, "poisson_1_ox.csv"))

IQRunit <- c(IQRs$ox)
HR <- tb[2,]
HR <- cbind(HR,IQRunit)
print(HR)
HR[, `:=`(HR_IQR = exp(Estimate*IQRunit),
          HR_lci = exp((Estimate-1.96*`Std. Error`)*IQRunit),
          HR_uci = exp((Estimate+1.96*`Std. Error`)*IQRunit))][]
fwrite(HR, paste0(dir_results, "poisson_1_ox_HR.csv"))

#################### 2. multi-pollutants models ###############################
p_1_all3 <- bam(dead ~ s(followupyr, by = as.factor(entry_age_break)) + 
             s(followupyr, by = as.factor(sex)) +
             s(followupyr, by = as.factor(race_collapsed)) + 
             s(followupyr, by = as.factor(dual)) +
             pm25 + no2 + ozone + 
             mean_bmi + smoke_rate + hispanic + pct_blk +
             medhouseholdincome + medianhousevalue +
             poverty + education + popdensity + pct_owner_occ +
             summer_tmmx + winter_tmmx + summer_rmax + winter_rmax +
             as.factor(year) + as.factor(region) +
             s(region, bs = "re"),
           family = poisson(), data = dt,
           discrete = TRUE,
           nthreads = length(cl))
tb <- summary(p_1_all3)$p.table
tb <- as.data.frame(tb)
setDT(tb, keep.rownames = TRUE)[]
fwrite(tb, paste0(dir_results, "poisson_1_all3_coef.csv"))

IQRunit <- c(IQRs$pm25, IQRs$no2, IQRs$ozone)
HR <- tb[2:4,]
HR <- cbind(HR,IQRunit)
print(HR)
HR[, `:=`(HR_IQR = exp(Estimate*IQRunit),
          HR_lci = exp((Estimate-1.96*`Std. Error`)*IQRunit),
          HR_uci = exp((Estimate+1.96*`Std. Error`)*IQRunit))][]
fwrite(HR, paste0(dir_results, "poisson_1_all3_HR.csv"))
gc()

p_1_all2 <- bam(dead ~ s(followupyr, by = as.factor(entry_age_break)) + 
                  s(followupyr, by = as.factor(sex)) +
                  s(followupyr, by = as.factor(race_collapsed)) + 
                  s(followupyr, by = as.factor(dual)) +
                  pm25 + ox +
                  mean_bmi + smoke_rate + hispanic + pct_blk +
                  medhouseholdincome + medianhousevalue +
                  poverty + education + popdensity + pct_owner_occ +
                  summer_tmmx + winter_tmmx + summer_rmax + winter_rmax +
                  as.factor(year) + as.factor(region) +
                  s(region, bs = "re"),
                family = poisson(), data = dt,
                discrete = TRUE,
                nthreads = length(cl))
tb <- summary(p_1_all2)$p.table
tb <- as.data.frame(tb)
setDT(tb, keep.rownames = TRUE)[]
fwrite(tb, paste0(dir_results, "poisson_1_all2_coef.csv"))

IQRunit <- c(IQRs$pm25, IQRs$ozone)
HR <- tb[2:3,]
HR <- cbind(HR,IQRunit)
print(HR)
HR[, `:=`(HR_IQR = exp(Estimate*IQRunit),
          HR_lci = exp((Estimate-1.96*`Std. Error`)*IQRunit),
          HR_uci = exp((Estimate+1.96*`Std. Error`)*IQRunit))][]
fwrite(HR, paste0(dir_results, "poisson_1_all3_HR.csv"))
gc()

stop(cl)



# vcov_mod <- vcovHC(p_1)
# fwrite




# p_splines <- bam(dead ~ s(followupyr, by = as.factor(entry_age_break)) + 
#                    s(followupyr, by = as.factor(sex)) +
#                    s(followupyr, by = as.factor(race_collapsed)) + 
#                    s(followupyr, by = as.factor(dual)) +
#                    s(pm25, bs="ps") + s(no2, bs="ps") + s(ozone, bs="ps") + 
#                    mean_bmi + smoke_rate + hispanic + pct_blk +
#                    medhouseholdincome + medianhousevalue +
#                    poverty + education + popdensity + pct_owner_occ +
#                    summer_tmmx + winter_tmmx + summer_rmax + winter_rmax +
#                    as.factor(year) + as.factor(statecode) +
#                    s(as.factor(statecode), bs = "re"),
#                  family = poisson(), data = dt,
#                  discrete = TRUE,
#                  nthreads = length(cl))

