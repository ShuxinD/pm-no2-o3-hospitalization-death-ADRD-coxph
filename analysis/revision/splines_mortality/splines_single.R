# setup ----
rm(list = ls())
gc()

library(data.table)
library(fst)
setDTthreads(threads = 0)
library(survival)

basedir <- "/n/dominici_nsaph_l3/Lab/projects/pm-no2-o3-hospitalization-death-ADRD-coxph"
getwd()
data_path <- file.path(basedir, "data", "ADRDcohort_dead.fst")

## load data ----
dt <- read_fst(data_path, as.data.table = T)
names(dt)

dt[, followupyr_start := (year - firstADRDyr-1)]
dt[, followupyr_end := (year - firstADRDyr)]

IQRs <- data.table(IQR(dt$pm25), IQR(dt$no2), IQR(dt$ozone), IQR(dt$ox), IQR(dt$ozone_summer))
colnames(IQRs) <- c("pm25", "no2", "ozone", "ox", "ozone_summer")
print(IQRs)

## fit single-pollutant models ----
pollutants <- c("pm25", "no2", "ozone_summer", "ox")
pollutants_i <- "pm25"
for (pollutants_i in pollutants){
  cat("fitting coxph model ", pollutants_i, "...\n")
  cox <- coxph(Surv(time = followupyr_start, time2 = followupyr_end, event = dead) ~
                 pspline(get(pollutants_i), df = 3) +
                 mean_bmi + smoke_rate +
                 hispanic + pct_blk + medhouseholdincome + medianhousevalue + poverty + education + popdensity + pct_owner_occ +
                 summer_tmmx + winter_tmmx + summer_rmax + winter_rmax +
                 as.factor(year) +  as.factor(region) +
                 strata(as.factor(entry_age_break), as.factor(sex), as.factor(race_collapsed), as.factor(dual)) + cluster(qid),
               data = dt,
               tie = "efron",
               na.action = na.omit)
  cat("drawing splines....\n")
  # Open PDF device
  pdf(paste0("splines_single_", pollutants_i, ".pdf"), width = 7, height = 5)
  # Generate the plot
  # termplot(cox, term = 1, se = TRUE)
  termplot(cox, term = 1, se=TRUE, col.term = 1, col.se = 1)
  # Close the file
  dev.off()
  # Open png device
  png(paste0("splines_single_", pollutants_i, ".pdf"), units = "in", width = 7, height = 5, res = 300)
  # Generate the plot
  termplot(cox, term = 1, se=TRUE, col.term = 1, col.se = 1)
  # Close the file
  dev.off()
}

cox <- coxph(Surv(time = followupyr_start, time2 = followupyr_end, event = dead) ~
               pspline(get(pollutants_i), df = 3) +
               mean_bmi + smoke_rate +
               hispanic + pct_blk + medhouseholdincome + medianhousevalue + poverty + education + popdensity + pct_owner_occ +
               summer_tmmx + winter_tmmx + summer_rmax + winter_rmax +
               as.factor(year) +  as.factor(region) +
               strata(as.factor(entry_age_break), as.factor(sex), as.factor(race_collapsed), as.factor(dual)) + cluster(qid),
             data = dt,
             tie = "efron",
             na.action = na.omit)

cox1 <- coxph(Surv(time = followupyr_start, time2 = followupyr_end, event = dead) ~
               pspline(get(pollutants_i), df = 3) +
               mean_bmi + smoke_rate +
               hispanic + pct_blk + medhouseholdincome + medianhousevalue + poverty + education + popdensity + pct_owner_occ +
               summer_tmmx + winter_tmmx + summer_rmax + winter_rmax +
               as.factor(year) +  as.factor(region) +
               strata(as.factor(entry_age_break), as.factor(sex), as.factor(race_collapsed), as.factor(dual)) + cluster(qid),
             data = dt,
             tie = "efron",
             na.action = na.omit)

cox2 <- coxph(Surv(time = followupyr_start, time2 = followupyr_end, event = dead) ~
                pspline(get(pollutants_i), df = 4) +
                mean_bmi + smoke_rate +
                hispanic + pct_blk + medhouseholdincome + medianhousevalue + poverty + education + popdensity + pct_owner_occ +
                summer_tmmx + winter_tmmx + summer_rmax + winter_rmax +
                as.factor(year) +  as.factor(region) +
                strata(as.factor(entry_age_break), as.factor(sex), as.factor(race_collapsed), as.factor(dual)) + cluster(qid),
              data = dt,
              tie = "efron",
              na.action = na.omit)

assign("get(pollutants_i)", seq(min(dt[,get(pollutants_i)]), max(dt[,get(pollutants_i)]), length.out = 1000))

new_data <- data.frame(get(pollutants_i),
                       mean_bmi = mean(dt[,mean_bmi]),
                       smoke_rate = mean(dt[,smoke_rate]),
                       hispanic = mean(dt[,hispanic]),
                       pct_blk = mean(dt[,pct_blk]),
                       medhouseholdincome = mean(dt[,medhouseholdincome]),
                       medianhousevalue = mean(dt[,medianhousevalue]),
                       poverty = mean(dt[,poverty]),
                       education = mean(dt[,education]),
                       popdensity = mean(dt[,popdensity]),
                       pct_owner_occ = mean(dt[,pct_owner_occ]),
                       summer_tmmx = mean(dt[,summer_tmmx]),
                       winter_tmmx = mean(dt[,winter_tmmx]),
                       summer_rmax = mean(dt[,summer_rmax]),
                       winter_rmax = mean(dt[,winter_rmax]),
                       year = Mode(),
                       as.factor(region))
setDT(new_data)
predicted_terms <- predict(cox, newdata = new_data, type = "terms", se.fit = TRUE)
