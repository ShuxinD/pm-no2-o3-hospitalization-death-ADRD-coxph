library(ggplot2)
library(stringr) 
library(data.table)
setwd("~/Desktop/airpollution_adrd_medicare/")

dir_plot <- paste0(getwd(), "/github_repo/results/main_analysis/EMM/")

# dir_plot <- "/nfs/home/S/shd968/shared_space/ci3_shd968/medicareADRD/github_repo/results/main_analysis/EMM/"

############################
# outcome <- "mortality"
outcome <- "ReAd"
############################
dir_EMM_data <- paste0(dir_plot,"coxph_", outcome, "/")

modifiers <- c("sex", "dual", "above_median_popdensity", "entry_age_over85", "race_collapsed")
pollutants <- c("pm25", "no2", "ozone_summer", "ox")

dt <- NULL
for (modifiers_i in modifiers) {
  for (pollutants_i in pollutants) {
    dt_raw <- read.csv(paste0(dir_EMM_data, "cox_", outcome,"_EMM_",modifiers_i, "_", pollutants_i, "_HR.csv"))
    dt_i <- data.frame(modifier = modifiers_i,
                       pollutant = pollutants_i,
                       level = dt_raw$rn,
                       HR = dt_raw$HR_IQR,
                       HR_lci = dt_raw$HR_lci,
                       HR_uci = dt_raw$HR_uci)
    print(dt_i)
    dt <- rbind(dt,dt_i)
  }
}
print(dt)
setDT(dt)
dt[, level := substring(level, first = 6)]
dt[modifier=="sex", level := ifelse(level==1, "Male", "Female")]
dt[modifier=="dual", level := ifelse(level==1, "FALSE", "TRUE")]
dt$modifier <- factor(dt$modifier, levels = c("sex", "dual", "above_median_popdensity", "entry_age_over85", "race_collapsed"), 
                      labels = c("Sex", "Dual \n elegibility", "Above median \n population \n density", "Entry age \n over 85", "Race"))
dt$pollutant <- factor(dt$pollutant, levels = c("pm25", "no2", "ozone_summer", "ox"),
                       labels = c("PM[2.5]", "NO[2]", "Summer\nozone", "Ox"))
fwrite(dt, paste0(dir_EMM_data, "EMMplot_table.csv"))

plot <- ggplot(data = dt, aes(x=level, y=HR, ymin=HR_lci, ymax=HR_uci)) + 
  geom_pointrange(aes(ymin=HR_lci, ymax=HR_uci), fatten = 1) +
  #geom_hline(aes(fill=level), yintercept = 1, linetype=2) +
  #geom_pointrange(aes(col=level_num,ymin=HR_lci, ymax=HR_uci), width=0.2,cex=1) + 
  facet_grid(pollutant~modifier,scales = "free",  space = "free") + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 5))
# theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
print(plot)
pdf(paste0(dir_EMM_data, "EMMplot.pdf"))
plot
dev.off()
