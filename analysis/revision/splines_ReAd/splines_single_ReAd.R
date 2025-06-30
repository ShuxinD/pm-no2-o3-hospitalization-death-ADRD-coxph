# setup ----
rm(list = ls())
gc()

library(data.table)
library(fst)
setDTthreads(threads = 0)
library(survival)

wkdir <- "/n/dominici_nsaph_l3/Lab/projects/pm-no2-o3-hospitalization-death-ADRD-coxph"
getwd()
cr_data_path <- file.path(wkdir, "data", "ADRDcohort_ReAd_deadipw.fst")
data_path <- file.path(wkdir, "data", "ADRDcohort_ReAd.fst")

## load data ----
cr_dt <- read_fst(cr_data_path, as.data.table = T)
names(cr_dt)
ReAd_dt <- read_fst(data_path, as.data.table = T)
names(ReAd_dt)
dt <- merge(ReAd_dt, cr_dt, by = c("qid", "year"))
rm(cr_dt);rm(ReAd_dt); gc()

dt[, followupyr_start := (year - firstADRDyr-1)]
dt[, followupyr_end := (year - firstADRDyr)]

## single-pollutant model ----
## fit single-pollutant models ----
pollutants <- c("pm25", "no2", "ozone_summer", "ox")
for (pollutants_i in pollutants){
  cat("fitting coxph model ", pollutants_i, "...\n")
  cox <- coxph(Surv(time = followupyr_start, time2 = followupyr_end, event = ReAd) ~ 
                 pspline(get(pollutants_i)) +
                 mean_bmi + smoke_rate + 
                 hispanic + pct_blk + medhouseholdincome + medianhousevalue + poverty + education + popdensity + pct_owner_occ +
                 summer_tmmx + winter_tmmx + summer_rmax + winter_rmax +
                 as.factor(year) +  as.factor(region) +
                 strata(as.factor(entry_age_break), as.factor(sex), as.factor(race_collapsed), as.factor(dual)) + cluster(qid),
               weights = deadipw_all3,
               data = dt,
               tie = c("efron"), 
               na.action = na.omit)
  cat("drawing splines....\n")
  # Open PDF device
  pdf(paste0("splines_single_", pollutants_i, ".pdf"), width = 7, height = 5)
  # Generate the plot
  # termplot(cox, term = 1, se = TRUE)
  termplot(cox, term = 1, se = TRUE, col.term = "blue", lwd.term = 2, rug = TRUE)
  # Close the file
  dev.off()
  # Open png device
  png(paste0("splines_single_", pollutants_i, ".pdf"), units = "in", width = 7, height = 5, res = 300)
  # Generate the plot
  termplot(cox, term = 1, se = TRUE, col.term = "blue", lwd.term = 2, rug = TRUE)
  # Close the file
  dev.off()
}