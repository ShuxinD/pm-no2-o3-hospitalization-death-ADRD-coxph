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


cat("estimate cox for 3-pollutant model \n")
cox_all3 <- coxph(Surv(time = followupyr_start, time2 = followupyr_end, event = ReAd) ~ 
                    pspline(pm25, df=4) +
                    pspline(no2, df=4) +
                    pspline(ozone_summer, df=4) +
                    mean_bmi + smoke_rate + 
                    hispanic + pct_blk + medhouseholdincome + medianhousevalue + poverty + education + popdensity + pct_owner_occ +
                    summer_tmmx + winter_tmmx + summer_rmax + winter_rmax +
                    as.factor(year) +  as.factor(region) +
                    strata(as.factor(entry_age_break), as.factor(sex), as.factor(race_collapsed), as.factor(dual)) + cluster(qid),
                  weights = deadipw_all3,
                  data = dt,
                  tie = c("efron"), 
                  na.action = na.omit)

# Open PDF device
pdf("cox_smooth_base_plot_pm25.pdf", width = 7, height = 5)
# Generate the plot
termplot(cox_all3, term = 1, se = TRUE, col.term = "blue", lwd.term = 2, rug = TRUE)
# Close the file
dev.off()

# Open PDF device
pdf("cox_smooth_base_plot_no2.pdf", width = 7, height = 5)
# Generate the plot
termplot(cox_all3, term = 2, se = TRUE, col.term = "blue", lwd.term = 2, rug = TRUE)
# Close the file
dev.off()

pdf("cox_smooth_base_plot_ozone.pdf", width = 7, height = 5)
# Generate the plot
termplot(cox_all3, term = 3, se = TRUE, col.term = "blue", lwd.term = 2, rug = TRUE)
# Close the file
dev.off()


png("cox_smooth_base_plot_pm25.png", units = "in", width = 7, height = 5, res = 300)
# Generate the plot
termplot(cox_all3, term = 1, se = TRUE, col.term = "blue", lwd.term = 2, rug = TRUE)
# Close the file
dev.off()

png("cox_smooth_base_plot_no2.png", units = "in", width = 7, height = 5, res = 300)
# Generate the plot
termplot(cox_all3, term = 2, se = TRUE, col.term = "blue", lwd.term = 2, rug = TRUE)
# Close the file
dev.off()

png("cox_smooth_base_plot_ozone.png", units = "in", width = 7, height = 5, res = 300)
# Generate the plot
termplot(cox_all3, term = 3, se = TRUE, col.term = "blue", lwd.term = 2, rug = TRUE)
# Close the file
dev.off()

