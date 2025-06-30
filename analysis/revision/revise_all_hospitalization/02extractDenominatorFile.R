## setup ----
rm(list = ls())
gc()

library(data.table)
library(fst)
# library(dplyr)

wkdir <- "/n/dominici_nsaph_l3/Lab/projects/pm-no2-o3-hospitalization-death-ADRD-coxph"

dir_enrolledInfo <- file.path(wkdir, "code", "revise_all_hospitalization", "All_EnrolledInfo.fst")
setDTthreads(threads = 0)
dir_denom <- file.path("/n/dominici_nsaph_l3/Lab/projects/analytic", "denom_by_year")
# dir_denominator <- "/nfs/home/S/shd968/shared_space/ci3_health_data/medicare/mortality/1999_2016/wu/cache_data/merged_by_year_v2/"
dir_output <- file.path(wkdir, "code", "revise_all_hospitalization")

f <- list.files(dir_denom,pattern = "\\.fst", full.names = TRUE)

myvars <- c("qid", "year", "zip", "sex", "race", "age", "dual", "statecode", "dead", "mean_bmi", "smoke_rate", "hispanic", "pct_blk", "medhouseholdincome", "medianhousevalue", "poverty", "education", "popdensity", "pct_owner_occ", "summer_tmmx", "winter_tmmx", "summer_rmax", "winter_rmax")

enrolledInfo <- read_fst(file.path(dir_enrolledInfo), as.data.table = T)
dim(enrolledInfo)[1]

## subset denominator files ----
#' select denominator with ADRD based on qid
#' read first 9 files
dt1_9 <- rbindlist(lapply(f[1:9],
                          read_fst,
                          columns = myvars,
                          as.data.table = TRUE))
subset1_9 <- dt1_9[qid %in% enrolledInfo[,QID], ]
rm(dt1_9)
gc()
#' read 10-18 files
dt10_18 <- rbindlist(lapply(f[10:18],
                            read_fst,
                            columns = myvars,
                            as.data.table = TRUE))
subset10_18 <- dt10_18[qid %in% enrolledInfo[,QID], ]
rm(dt10_18)
gc()

#' combine two subset together
ALLpeople <- rbind(subset1_9, subset10_18)
rm(subset1_9)
rm(subset10_18)
gc()

uniqueN(ALLpeople[,qid]) # number of people in denominator file

dim(ALLpeople) # person-year

## merge enrollInfo into ADRD denom ----
ALLpeople <- merge(ALLpeople, enrolledInfo, by.x = "qid", by.y = "QID", all = FALSE)
setorder(ALLpeople, qid, year)
ALLpeople <- ALLpeople[year >= firstHOSPyr, ] # subset from firstHOSPyr
uniqueN(ALLpeople[,qid])
dim(ALLpeople)

# ADRDpeople[, zip := int_to_zip_str(zip)]

# zip <- c(8974, 8974, 4350, 4350, 7623, 55111, 87969)
# zip <- as.character(zip)
# for(i in 1:length(zip)){
#   if(as.numeric(zip[i]) < 10000){
#     zip[i] <- paste0("0", zip[i])
#   }
# }
# zip

write_fst(ALLpeople, file.path(dir_output, "ALLpeople_denom.fst"))