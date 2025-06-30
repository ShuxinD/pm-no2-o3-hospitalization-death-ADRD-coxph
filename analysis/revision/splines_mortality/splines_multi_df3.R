# setup ----
rm(list = ls())
gc()

library(data.table)
library(fst)
setDTthreads(threads = 0)
library(survival)

wkdir <- "/n/dominici_nsaph_l3/Lab/projects/pm-no2-o3-hospitalization-death-ADRD-coxph"
getwd()
data_path <- file.path(wkdir, "data", "ADRDcohort_dead.fst")

## load data ----
dt <- read_fst(data_path, as.data.table = T)
names(dt)

dt[, followupyr_start := (year - firstADRDyr-1)]
dt[, followupyr_end := (year - firstADRDyr)]

IQRs <- data.table(IQR(dt$pm25), IQR(dt$no2), IQR(dt$ozone), IQR(dt$ox), IQR(dt$ozone_summer))
colnames(IQRs) <- c("pm25", "no2", "ozone", "ox", "ozone_summer")
print(IQRs)

## splines ----
cox_splines <- coxph(Surv(time = followupyr_start, time2 = followupyr_end, event = dead) ~
                       pspline(pm25, df=3) +
                       pspline(no2, df=3) +
                       pspline(ozone_summer, df=3) +
                       mean_bmi + smoke_rate + hispanic + pct_blk +
                       medhouseholdincome + medianhousevalue +
                       poverty + education + popdensity + pct_owner_occ +
                       summer_tmmx + winter_tmmx + summer_rmax + winter_rmax +
                       as.factor(year) +  as.factor(region) +
                       strata(as.factor(entry_age_break), as.factor(sex), as.factor(race_collapsed), as.factor(dual)) + cluster(qid),
                     data = dt,
                     tie = c("efron"),
                     na.action = na.omit)
# Open PDF device
pdf("cox_multi_pm25_df3.png", width = 7, height = 5)
# Generate the plot
termplot(cox_splines, term = 1, se = TRUE, col.term = "blue", lwd.term = 2, rug = TRUE,
         xlabs = expression('PM'[2.5]))
# Close the file
dev.off()

# Open PDF device
pdf("cox_multi_no2_df3.png", width = 7, height = 5)
# Generate the plot
termplot(cox_splines, term = 2, se = TRUE, col.term = "blue", lwd.term = 2, rug = TRUE,
         xlabs = expression('NO'[2]))
# Close the file
dev.off()

pdf("cox_multi_ozone_df3.png", width = 7, height = 5)
# Generate the plot
termplot(cox_splines, term = 3, se = TRUE, col.term = "blue", lwd.term = 2, rug = TRUE,
         xlabs = "Summer Ozone")
# Close the file
dev.off()


png("cox_multi_pm25_df3.png", units = "in", width = 7, height = 5, res = 300)
# Generate the plot
termplot(cox_splines, term = 1, se = TRUE, col.term = "blue", lwd.term = 2, rug = TRUE,
         xlabs = expression('PM'[2.5]))
# Close the file
dev.off()

png("cox_multi_no2_df3.png", units = "in", width = 7, height = 5, res = 300)
# Generate the plot
termplot(cox_splines, term = 2, se = TRUE, col.term = "blue", lwd.term = 2, rug = TRUE,
         xlabs = expression('NO'[2]))
# Close the file
dev.off()

png("cox_multi_ozone_df3.png", units = "in", width = 7, height = 5, res = 300)
# Generate the plot
termplot(cox_splines, term = 3, se = TRUE, col.term = "blue", lwd.term = 2, rug = TRUE,
         xlabs = "Summer Ozone")
# Close the file
dev.off()