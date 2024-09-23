#' Project: airPollution_ADRD
#' Code: cox ph model for readmission
#' Input: "ADRDcohort_ReAd.fst"                                                  
#' Output: model specific results
#' Author: Shuxin Dong
#' First create date: 2021-02-17

# setup ----
rm(list = ls())
gc()

library(data.table)
library(fst)
setDTthreads(threads = 0)
library(survival)

# setwd("/nfs/home/S/shd968/shared_space/ci3_shd968/medicareADRD/")
dir_data <- paste0(getwd(),"/data/")

## load data ----
dt <- read_fst(paste0(dir_data, "ADRDcohort_ReAd.fst"), as.data.table = T)
names(dt)

dt[, followupyr_start := (year - firstADRDyr - 1)]
dt[, followupyr_end := (year - firstADRDyr)]

IQRs <- data.table(IQR(dt$pm25), IQR(dt$no2), IQR(dt$ozone), IQR(dt$ox), IQR(dt$ozone_summer))
colnames(IQRs) <- c("pm25", "no2", "ozone", "ox", "ozone_summer")
print(IQRs)

## calculate IP weights to account for competing risks of mortality ----
#' create "dead_cr" to fix death when occuring at the same year as ReAd
dt[, dead_cr := ifelse((dead)&(ReAd),F,dead)]
#' construct PS model and calculate IP weights
## single-pollutant
pollutants <- c("pm25", "no2", "ozone", "ozone_summer", "ox")
for (pollutants_i in pollutants){
  cat("fit ps model for mortality with", pollutants_i, "\n")
  # P(D=1|A,L)
  denom.death <- glm(dead_cr ~ get(pollutants_i) + 
                       mean_bmi + smoke_rate + 
                       hispanic + pct_blk + medhouseholdincome + medianhousevalue + poverty + education + popdensity + pct_owner_occ +
                       summer_tmmx + winter_tmmx + summer_rmax + winter_rmax +
                       as.factor(year) +  as.factor(region) +
                       as.factor(entry_age_break) + as.factor(sex) + as.factor(race_collapsed) + as.factor(dual),
                     data = dt, family = binomial(link = "logit"))
  p_denom.death <- 1 - predict(denom.death, type = "response")
  # P(D=1|A)
  num.death <- glm(dead_cr ~ get(pollutants_i),
                   data = dt, family = binomial(link = "logit"))
  p_num.death <- 1 - predict(num.death, type = "response")
  assign(paste0("deadipw_", pollutants_i), p_num.death/p_denom.death)
}
rm(p_denom.death)
rm(p_num.death)
gc()
## multi-pollutant pm25 no2 and ozone
cat("fit ps model for mortality with pm25 no2 and ozone \n")
# P(D=1|A,L)
denom.death <- glm(dead_cr ~ pm25 + no2 + ozone_summer +  
                       mean_bmi + smoke_rate + 
                       hispanic + pct_blk + medhouseholdincome + medianhousevalue + poverty + education + popdensity + pct_owner_occ +
                       summer_tmmx + winter_tmmx + summer_rmax + winter_rmax +
                       as.factor(year) +  as.factor(region) +
                       as.factor(entry_age_break) + as.factor(sex) + as.factor(race_collapsed) + as.factor(dual),
                   data = dt, family = binomial(link = "logit"))
p_denom.death <- 1 - predict(denom.death, type = "response")
# P(D=1|A)
num.death <- glm(dead_cr ~ pm25 + no2 + ozone_summer,
                   data = dt, family = binomial(link = "logit"))
p_num.death <- 1 - predict(num.death, type = "response")
assign(paste0("deadipw_all3"), p_num.death/p_denom.death)
## multi-pollutant pm25 and ox
cat("fit ps model for mortality with pm25 and ox \n")
# P(D=1|A,L)
denom.death <- glm(dead_cr ~ pm25 + ox +  
                     mean_bmi + smoke_rate + 
                     hispanic + pct_blk + medhouseholdincome + medianhousevalue + poverty + education + popdensity + pct_owner_occ +
                     summer_tmmx + winter_tmmx + summer_rmax + winter_rmax +
                     as.factor(year) +  as.factor(region) +
                     as.factor(entry_age_break) + as.factor(sex) + as.factor(race_collapsed) + as.factor(dual),
                   data = dt, family = binomial(link = "logit"))
p_denom.death <- 1 - predict(denom.death, type = "response")
# P(D=1|A)
num.death <- glm(dead_cr ~ pm25 + ox,
                 data = dt, family = binomial(link = "logit"))
p_num.death <- 1 - predict(num.death, type = "response")
assign(paste0("deadipw_all2"), p_num.death/p_denom.death)
#' merged into the dataset
dt[, `:=` (deadipw_pm25 = deadipw_pm25,
           deadipw_no2 = deadipw_no2,
           deadipw_ozone = deadipw_ozone,
           deadipw_ozone_summer = deadipw_ozone_summer,
           deadipw_ox = deadipw_ox,
           deadipw_all3 = deadipw_all3,
           deadipw_all2 = deadipw_all2)]
# summary(dt[,.(deadipw_pm25, deadipw_no2, deadipw_ozone, deadipw_ozone_summer, deadipw_ox, deadipw_all3, deadipw_all2)])
# deadipw_pm25     deadipw_no2     deadipw_ozone    deadipw_ozone_summer   deadipw_ox      deadipw_all3   
# Min.   :0.8703   Min.   :0.8980   Min.   :0.8979   Min.   :0.9014       Min.   :0.9005   Min.   :0.8670  
# 1st Qu.:0.9643   1st Qu.:0.9633   1st Qu.:0.9632   1st Qu.:0.9632       1st Qu.:0.9632   1st Qu.:0.9643  
# Median :0.9903   Median :0.9893   Median :0.9895   Median :0.9895       Median :0.9893   Median :0.9900  
# Mean   :1.0030   Mean   :1.0031   Mean   :1.0031   Mean   :1.0031       Mean   :1.0031   Mean   :1.0029  
# 3rd Qu.:1.0267   3rd Qu.:1.0269   3rd Qu.:1.0273   3rd Qu.:1.0273       3rd Qu.:1.0271   3rd Qu.:1.0264  
# Max.   :1.7501   Max.   :1.8379   Max.   :1.8359   Max.   :1.8417       Max.   :1.8407   Max.   :1.7706  
# deadipw_all2   
# Min.   :0.8710  
# 1st Qu.:0.9643  
# Median :0.9903  
# Mean   :1.0030  
# 3rd Qu.:1.0267  
# Max.   :1.7593 
#' save ipws just in case
write_fst(dt[,.(qid, year, deadipw_pm25, deadipw_no2, deadipw_ozone, deadipw_ozone_summer, deadipw_ox, deadipw_all3, deadipw_all2)], paste0(dir_data,"ADRDcohort_ReAd_deadipw.fst"))

## single-pollutants model ----
dir_out <- paste0(getwd(),"/results/main_analysis/1plain/coxph_ReAd_cr/")
pollutants <- c("pm25", "no2", "ozone_summer", "ox")

for (pollutants_i in pollutants){
  cat("fit coxph model", pollutants_i, "\n")
  cox <- coxph(Surv(time = followupyr_start, time2 = followupyr_end, event = ReAd) ~ 
                 get(pollutants_i) + 
                 mean_bmi + smoke_rate + 
                 hispanic + pct_blk + medhouseholdincome + medianhousevalue + poverty + education + popdensity + pct_owner_occ +
                 summer_tmmx + winter_tmmx + summer_rmax + winter_rmax +
                 as.factor(year) +  as.factor(region) +
                 strata(as.factor(entry_age_break), as.factor(sex), as.factor(race_collapsed), as.factor(dual)) + cluster(qid),
               weights = get(paste0("deadipw_",pollutants_i)),
               data = dt,
               tie = "efron", 
               na.action = na.omit)
  tb <- summary(cox)$coefficients
  tb <- as.data.frame(tb)
  setDT(tb, keep.rownames = TRUE)
  fwrite(tb, paste0(dir_out, "cox_ReAd_", pollutants_i, ".csv"))
  cat("output coefs...\n")
  HR <- tb[1,]
  HR <- cbind(HR, IQRs[, get(pollutants_i)])
  HR[, `:=`(HR_IQR = exp(coef*IQRs[, get(pollutants_i)]),
            HR_lci = exp((coef-1.96*`robust se`)*IQRs[, get(pollutants_i)]),
            HR_uci = exp((coef+1.96*`robust se`)*IQRs[, get(pollutants_i)]))]
  print(HR)
  fwrite(HR, paste0(dir_out, "cox_ReAd_", pollutants_i, "_HR.csv"))
  cat("save HR for cox ReAd", pollutants_i, "\n")
}

## multi-pollutants model ----
dir_out <- paste0(getwd(),"/results/main_analysis/1plain/coxph_ReAd_cr/")

cat("estimate cox for 3-pollutant model \n")
cox_all3 <- coxph(Surv(time = followupyr_start, time2 = followupyr_end, event = ReAd) ~ 
                    pm25 + no2 + ozone_summer + 
                    mean_bmi + smoke_rate + 
                    hispanic + pct_blk + medhouseholdincome + medianhousevalue + poverty + education + popdensity + pct_owner_occ +
                    summer_tmmx + winter_tmmx + summer_rmax + winter_rmax +
                    as.factor(year) +  as.factor(region) +
                    strata(as.factor(entry_age_break), as.factor(sex), as.factor(race_collapsed), as.factor(dual)) + cluster(qid),
                  weights = deadipw_all3,
                  data = dt,
                  tie = c("efron"), 
                  na.action = na.omit)
tb <- summary(cox_all3)$coefficients
tb <- as.data.frame(tb)
setDT(tb, keep.rownames = TRUE)[]
fwrite(tb, paste0(dir_out, "cox_ReAd_all3.csv"))

IQRunit <- c(IQRs$pm25, IQRs$no2, IQRs$ozone_summer)
HR <- tb[1:3,]
HR <- cbind(HR,IQRunit)
cat("output HR for cox 3-pollutant model \n")
HR[, `:=`(HR_IQR = exp(coef*IQRunit),
          HR_lci = exp((coef-1.96*`robust se`)*IQRunit),
          HR_uci = exp((coef+1.96*`robust se`)*IQRunit))][]
fwrite(HR, paste0(dir_out, "cox_ReAd_all3_HR.csv"))

cat("estimate cox for 2-pollutant model \n")
cox_all2 <- coxph(Surv(time = followupyr_start, time2 = followupyr_end, event = ReAd) ~ 
                    pm25 + ox +
                    mean_bmi + smoke_rate + 
                    hispanic + pct_blk + medhouseholdincome + medianhousevalue + poverty + education + popdensity + pct_owner_occ +
                    summer_tmmx + winter_tmmx + summer_rmax + winter_rmax +
                    as.factor(year) +  as.factor(region) +
                    strata(as.factor(entry_age_break), as.factor(sex), as.factor(race_collapsed), as.factor(dual)) + cluster(qid),
                  weights = deadipw_all2,
                  data = dt,
                  tie = "efron", 
                  na.action = na.omit)
tb <- summary(cox_all2)$coefficients
tb <- as.data.frame(tb)
setDT(tb, keep.rownames = TRUE)[]
fwrite(tb, paste0(dir_out, "cox_ReAd_all2.csv"))

IQRunit <- c(IQRs$pm25, IQRs$ox)
HR <- tb[1:2,]
HR <- cbind(HR,IQRunit)
cat("output HR for cox 2-pollutant model \n")
HR[, `:=`(HR_IQR = exp(coef*IQRunit),
          HR_lci = exp((coef-1.96*`robust se`)*IQRunit),
          HR_uci = exp((coef+1.96*`robust se`)*IQRunit))][]
fwrite(HR, paste0(dir_out, "cox_ReAd_all2_HR.csv"))

# splines ----
# cox_splines <- coxph(Surv(time = followupyr, time2 = followupyr_plusone, event = ReAd) ~ 
#                        pspline(pm25, df=4) + 
#                        pspline(no2, df=4) + 
#                        pspline(ozone, df=4) + 
#                        mean_bmi + smoke_rate + hispanic + pct_blk + 
#                        medhouseholdincome + medianhousevalue +  
#                        poverty + education + popdensity + pct_owner_occ +
#                        summer_tmmx + winter_tmmx + summer_rmax + winter_rmax +
#                        as.factor(year) +  as.factor(region) +
#                        strata(as.factor(entry_age_break), as.factor(sex), as.factor(race_collapsed), as.factor(dual)) + cluster(qid),
#                      data = dt,
#                      tie = c("efron"), 
#                      na.action = na.omit)
# print(cox_splines)
# termplot(cox_splines, term = 1, se=TRUE, col.term = 1, col.se = 1)
