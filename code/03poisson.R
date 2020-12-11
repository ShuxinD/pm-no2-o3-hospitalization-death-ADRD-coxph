###############################################################################
# Project: Medicare Mortality and Air Pollution in AD/ADRD                    #
# Code: covariates correlations, create table one                             #
# Input: "ADRDmort_cplt.csv"                                                  #
# Output: 
# Author: Shuxin Dong                                                         #
# Date: Dec 9, 2020                                                           #
###############################################################################

############################# 0. Setup ########################################
rm(list = ls())
gc()

library(data.table)
library(dplyr)

setwd("/nfs/home/S/shd968/shared_space/ci3_shd968/dementia")

dir_data <- "/nfs/home/S/shd968/shared_space/ci3_shd968/dementia/"
dir_output <- "/nfs/home/S/shd968/shared_space/ci3_shd968/dementia/modelResult/"

setDTthreads(threads = 0)

dt <- fread(paste0(dir_data, "ADRDmort_cplt.csv"))
# > names(dt)
# [1] "QID"                 "year"                "zip"                 "AGE"                 "STATECODE"          
# [6] "Sex_gp"              "age_gp"              "Dual_gp"             "race"                "bene_dod"           
# [11] "ADATE"               "DDATE"               "DIAG1"               "DIAG2"               "diabetes"           
# [16] "year_admit"          "pm25"                "no2"                 "PctEye"              "PctLDL"             
# [21] "Pctmam"              "LungCancerRate"      "poverty"             "popdensity"          "medianhousevalue"   
# [26] "pct_blk"             "medhouseholdincome"  "pct_owner_occ"       "hispanic"            "education"          
# [31] "smoke_rate"          "mean_bmi"            "amb_visit_pct"       "a1c_exm_pct"         "nearest_hospital_km"
# [36] "ozone"               "firstADRDyr"         "mort_yr"             "death" 
dt$dead_peryear <- 0
dt$dead_peryear[dt$death==1 & (dt$year_admit==dt$mort_yr_admit)] <- 1
dt[, followupyr := year_admit - firstADRDyr]
dt[, followupyr_plusone := followupyr +1][]
dt <- dt[!followupyr==0] # drop the first year

dt$zip_num <- as.numeric(dt$zip)
############################# 1. aggregate dataset ############################
## Create aggregated data for Poisson regression
dt$time_count <- dt$followupyr_plusone - dt$followupyr
dead_personyear <- dt[, list(dead = sum(dead_peryear), time_count = sum(time_count)), 
           by = .(zip, year_admit, Sex_gp, race, Dual_gp, age_gp, followupyr)]
covariates <- dt[, list(pm25 = min(pm25), no2 = min(no2), ozone = min(ozone),
                        mean_bmi = min(mean_bmi), smoke_rate = min(smoke_rate), 
                        hispanic = min(hispanic), pct_blk = min(pct_blk), 
                        medhouseholdincome = min(medhouseholdincome), 
                        medianhousevalue = min(medianhousevalue),
                        poverty = min(poverty), education = min(education), 
                        popdensity = min(popdensity), 
                        pct_owner_occ = min(pct_owner_occ)),
                 by = .(zip, year_admit)]
aggregate_dt <- merge(dead_personyear, covariates, 
                      by=c("zip","year_admit"), all.x =T)
                              
############################# 2. spline Poisson model #########################
library(parallel)
library(mgcv)
numCores <- detectCores()
cl <- makeCluster(numCores-1)

s.P_all <- bam(dead_peryear ~ s(pm25) + s(ozone) + s(no2) + 
                 as.factor(year_admit) + AGE + Sex_gp + as.factor(race) + Dual_gp +
                 s(poverty) + s(popdensity) + s(medhouseholdincome) + 
                 s(education) + s(pct_blk) + s(zip_num,bs="re"),
               family="poisson", data = dt,
               cluster = cl, nthreads = NA)

pdf(paste0(dir_output, "splines_pollutants.pdf"))
par(mfrow=c(2,2))
plot(s.P_all,select=1,shade=T,rug=TRUE,ylim=c(-0.2,0.2),xlab=expression(PM[2.5]))
plot(s.P_all,select=2,shade=T,rug=TRUE,ylim=c(-0.2,0.2),xlab=expression(NO[2]))
plot(s.P_all,select=3,shade=T,rug=TRUE,ylim=c(-0.2,0.2),xlab="Ozone")
dev.off()

pdf(paste0(dir_output, "splines_covariates.pdf"))
par(mfrow=c(2,3))
plot(s.P_all,select=4,shade=T,rug=F,ylim=c(-0.2,0.2),xlab="poverty")
plot(s.P_all,select=5,shade=T,rug=F,ylim=c(-0.2,0.2),xlab="popdensity")
plot(s.P_all,select=6,shade=T,rug=F,ylim=c(-0.2,0.2),xlab="medhouseholdincome")
plot(s.P_all,select=7,shade=T,rug=F,ylim=c(-0.2,0.2),xlab="education")
plot(s.P_all,select=8,shade=T,rug=F,ylim=c(-0.2,0.2),xlab="pct_blk")
dev.off()

stopCluster(cl)

##################### 3. cox-equivalent Poisson model #########################
library(gnm)
gnm_all <- gnm(dead~ pm25 + no2 + ozone +
                 mean_bmi + smoke_rate + hispanic + pct_blk + 
                 medhouseholdincome + medianhousevalue + poverty + education + 
                 popdensity + pct_owner_occ +
                 as.factor(year_admit) + offset(log(time_count)), 
               eliminate = (as.factor(Sex_gp):as.factor(race):as.factor(Dual_gp):
                              as.factor(age_gp):as.factor(followupyr)),
               data=aggregate_dt,
               family=poisson(link="log"))
temp <- summary(gnm_all)
write.csv(temp$coefficients, paste0(dir_output, "gnm_all.csv"))

gnm_pm25 <- gnm(dead~ pm25 + 
                  mean_bmi + smoke_rate + hispanic + pct_blk + 
                  medhouseholdincome + medianhousevalue + poverty + education + 
                  popdensity + pct_owner_occ +
                  as.factor(year_admit) + offset(log(time_count)), 
                eliminate = (as.factor(Sex_gp):as.factor(race):as.factor(Dual_gp):
                               as.factor(age_gp):as.factor(followupyr)),
                data=aggregate_dt,
                family=poisson(link="log"))
temp <- summary(gnm_pm25)
write.csv(temp$coefficients, paste0(dir_output, "gnm_pm25.csv"))

gnm_no2 <- gnm(dead~ no2 + 
                  mean_bmi + smoke_rate + hispanic + pct_blk + 
                  medhouseholdincome + medianhousevalue + poverty + education + 
                  popdensity + pct_owner_occ +
                  as.factor(year_admit) + offset(log(time_count)), 
                eliminate = (as.factor(Sex_gp):as.factor(race):as.factor(Dual_gp):
                               as.factor(age_gp):as.factor(followupyr)),
                data=aggregate_dt,
                family=poisson(link="log"))
temp <- summary(gnm_no2)
write.csv(temp$coefficients, paste0(dir_output, "gnm_no2.csv"))

gnm_ozone <- gnm(dead~ ozone + 
                  mean_bmi + smoke_rate + hispanic + pct_blk + 
                  medhouseholdincome + medianhousevalue + poverty + education + 
                  popdensity + pct_owner_occ +
                  as.factor(year_admit) + offset(log(time_count)), 
                eliminate = (as.factor(Sex_gp):as.factor(race):as.factor(Dual_gp):
                               as.factor(age_gp):as.factor(followupyr)),
                data=aggregate_dt,
                family=poisson(link="log"))
temp <- summary(gnm_ozone)
write.csv(temp$coefficients, paste0(dir_output, "gnm_ozone.csv"))
