# setup ----
rm(list = ls())
gc()

library(data.table)
library(fst)
setDTthreads(threads = 0)
library(survival)

wkdir <- "/n/dominici_nsaph_l3/Lab/projects/pm-no2-o3-hospitalization-death-ADRD-coxph"
rundir <- file.path(wkdir, "code", "revise_all_hospitalization")

## load data ----
dt <- read_fst(file.path(rundir, "ALLcohort_dead.fst"), as.data.table = T)
names(dt)

dt[, followupyr_start := (year - firstHOSPyr-1)]
dt[, followupyr_end := (year - firstHOSPyr)]

IQRs <- data.table(IQR(dt$pm25), IQR(dt$no2), IQR(dt$ozone), IQR(dt$ox), IQR(dt$ozone_summer))
colnames(IQRs) <- c("pm25", "no2", "ozone", "ox", "ozone_summer")
print(IQRs)

## single-pollutants model ----
pollutants <- c("pm25", "no2", "ozone_summer", "ox")

# for (pollutants_i in pollutants){
#   cat("fit coxph model", pollutants_i, "\n")
#   cox <- coxph(Surv(time = followupyr_start, time2 = followupyr_end, event = dead) ~ 
#                  pspline(get(pollutants_i), df=3) + 
#                  mean_bmi + smoke_rate + 
#                  hispanic + pct_blk + medhouseholdincome + medianhousevalue + poverty + education + popdensity + pct_owner_occ +
#                  summer_tmmx + winter_tmmx + summer_rmax + winter_rmax +
#                  as.factor(year) +  as.factor(region) +
#                  strata(as.factor(entry_age_break), as.factor(sex), as.factor(race_collapsed), as.factor(dual)) + cluster(qid),
#                data = dt,
#                tie = "efron",
#                na.action = na.omit)
#   termplot(cox, term=1, se=TRUE, col.term=1, col.se=1)
# }
# for (pollutants_i in pollutants){
#   cat("fit coxph model", pollutants_i, "\n")
#   cox <- coxph(Surv(time = followupyr_start, time2 = followupyr_end, event = dead) ~ 
#                  get(pollutants_i) + 
#                  mean_bmi + smoke_rate + 
#                  hispanic + pct_blk + medhouseholdincome + medianhousevalue + poverty + education + popdensity + pct_owner_occ +
#                  summer_tmmx + winter_tmmx + summer_rmax + winter_rmax +
#                  as.factor(year) +  as.factor(region) +
#                  strata(as.factor(entry_age_break), as.factor(sex), as.factor(race_collapsed), as.factor(dual)) + cluster(qid),
#                data = dt,
#                tie = c("efron"),
#                na.action = na.omit)
#   tb <- summary(cox)$coefficients
#   tb <- as.data.frame(tb)
#   setDT(tb, keep.rownames = TRUE)
#   fwrite(tb, file.path(rundir, "coxph_mortality", paste0("cox_mortality_", pollutants_i, ".csv")))
#   cat("output coefs...\n")
#   HR <- tb[1,]
#   HR <- cbind(HR, IQRs[, get(pollutants_i)])
#   HR[, `:=`(HR_IQR = exp(coef*IQRs[, get(pollutants_i)]),
#             HR_lci = exp((coef-qnorm(0.975)*`robust se`)*IQRs[, get(pollutants_i)]),
#             HR_uci = exp((coef+qnorm(0.975)*`robust se`)*IQRs[, get(pollutants_i)]))]
#   print(HR)
#   fwrite(HR, file.path(rundir, "coxph_mortality", paste0("cox_mortality_", pollutants_i, "_HR.csv")))
#   cat("save HR for cox mortality", pollutants_i, "\n")
# }
# 
# ## multi-pollutants model ----
# cat("estimate cox for 3-pollutant model \n")
# cox_all3 <- coxph(Surv(time = followupyr_start, time2 = followupyr_end, event = dead) ~ 
#                     pm25 + no2 + ozone_summer + 
#                     mean_bmi + smoke_rate + 
#                     hispanic + pct_blk + medhouseholdincome + medianhousevalue + poverty + education + popdensity + pct_owner_occ +
#                     summer_tmmx + winter_tmmx + summer_rmax + winter_rmax +
#                     as.factor(year) +  as.factor(region) +
#                     strata(as.factor(entry_age_break), as.factor(sex), as.factor(race_collapsed), as.factor(dual)) + cluster(qid),
#                   data = dt,
#                   tie = c("efron"), 
#                   na.action = na.omit)
# tb <- summary(cox_all3)$coefficients
# tb <- as.data.frame(tb)
# setDT(tb, keep.rownames = TRUE)[]
# fwrite(tb, file.path(rundir, "coxph_mortality", "cox_mortality_all3.csv"))
# 
# IQRunit <- c(IQRs$pm25, IQRs$no2, IQRs$ozone_summer)
# HR <- tb[1:3,]
# HR <- cbind(HR,IQRunit)
# cat("output HR for cox 3-pollutant model \n")
# HR[, `:=`(HR_IQR = exp(coef*IQRunit),
#           HR_lci = exp((coef-1.96*`robust se`)*IQRunit),
#           HR_uci = exp((coef+1.96*`robust se`)*IQRunit))][]
# fwrite(HR, file.path(rundir, "coxph_mortality", "cox_mortality_all3_HR.csv"))

cat("estimate cox for 2-pollutant model \n")
cox_all2 <- coxph(Surv(time = followupyr_start, time2 = followupyr_end, event = dead) ~ 
                    pm25 + ox +
                    mean_bmi + smoke_rate + 
                    hispanic + pct_blk + medhouseholdincome + medianhousevalue + poverty + education + popdensity + pct_owner_occ +
                    summer_tmmx + winter_tmmx + summer_rmax + winter_rmax +
                    as.factor(year) +  as.factor(region) +
                    strata(as.factor(entry_age_break), as.factor(sex), as.factor(race_collapsed), as.factor(dual)) + cluster(qid),
                  data = dt,
                  tie = c("efron"), 
                  na.action = na.omit)
tb <- summary(cox_all2)$coefficients
tb <- as.data.frame(tb)
setDT(tb, keep.rownames = TRUE)[]
fwrite(tb, file.path(rundir, "coxph_mortality", "cox_mortality_all2.csv"))

IQRunit <- c(IQRs$pm25, IQRs$ox)
HR <- tb[1:2,]
HR <- cbind(HR,IQRunit)
cat("output HR for cox 2-pollutant model \n")
HR[, `:=`(HR_IQR = exp(coef*IQRunit),
          HR_lci = exp((coef-1.96*`robust se`)*IQRunit),
          HR_uci = exp((coef+1.96*`robust se`)*IQRunit))][]
fwrite(HR, file.path(rundir, "coxph_mortality", "cox_mortality_all2_HR.csv"))

## splines ----
# cox_splines <- coxph(Surv(time = followupyr_start, time2 = followupyr_end, event = dead) ~ 
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
# termplot(cox_splines, term = 1, se=TRUE, col.term = 1, col.se = 1)
