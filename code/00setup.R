###############################################################################
# Project: Medicare Mortality and Air Pollution in AD/ADRD                    #
# Code: clean/merge datasets                                                  #
# Input: "national_exp.fst", "hospital_total.rds"                             #
# Output: "enrolledINFO.csv" - IDs and  the year of enrollment                #
# Output: "ADRDnational_exp.csv" as retricted "national_exp.fst"              #
# Output: "mortINFO.csv" as death year and IDs                                #
# Output: "ADRDmort_all.csv" as data added death info but not checking dup    #
# Output: "ADRDmort_cplt.csv" as the clean, final dataset                     #
# Author: Shuxin Dong                                                         #
# Date: Nov 30, 2020                                                          #
###############################################################################

############################# 0. Setup ########################################
rm(list = ls())
gc()

library(data.table)
library(dplyr)
library(fst)

setwd("/nfs/home/S/shd968/shared_space/ci3_shd968/dementia")

dir_data <- "/nfs/home/S/shd968/shared_space/ci3_myitshak/dementia/"
dir_output <- "/nfs/home/S/shd968/shared_space/ci3_shd968/dementia/"

setDTthreads(threads = 0)

########## 1. get IDs and enroll info for ADRD cohort from hosp. ##############
med <- readRDS(paste0(dir_data, "hospital_total.rds"))
setDT(med)

ADRDmed <- subset(med, Alzheimer_pdx==1|Alzheimer_pdx2dx_10==1|Alzheimer_pdx2dx_25==1|
                    Dementia_pdx==1|Dementia_pdx2dx_10==1|Dementia_pdx2dx_25 ==1,
                  select = c("QID", "ADATE"))
ADRDmed$year_admit <- as.numeric(format(ADRDmed$ADATE, "%Y"))

# ## save all the IDs with ADRD diagnoses
# id <- ADRDmed[!duplicated(QID),]
# id$include <- 1 # add a column to mark whether to include
# id[, year := NULL]
# head(id)
# fwrite(id, paste0(dir_output, "populationID.csv"))
# 
# ## save enroll info for ADRD cohort
# enrollyr <- aggregate(year_admit ~ QID, ADRDmed, min)
# setnames(enrollyr, "year_admit", "firstADRDyr")
# fwrite(enrollyr, paste0(dir_output, "enrollyrINFO.csv"))

## drop subjects whose firstADRDyr <= 2006
enrollINFO <- aggregate(year_admit ~ QID, ADRDmed, min)
setDT(enrollINFO)
setnames(enrollINFO, "year_admit", "firstADRDyr")
enrolledINFO <- enrollINFO[firstADRDyr>2006]
fwrite(enrolledINFO, paste0(dir_output, "enrolledINFO.csv"))

##################### 2. load denominator file ################################
# denom_file <- read_fst(paste0(dir_data,"national_exp.fst"))
# >  dim(denom_file)
# [1] 538173801        37
n.denom <- 538173801
enrolledINFO  <- fread(paste0(dir_output, "enrolledINFO.csv"))

## split the national denominator file to save memory
## subset the dataset to those in ADRD cohort
denom_file.1 <- read_fst(paste0(dir_data,"national_exp.fst"),
                         from = 1, to = floor(n.denom/2), as.data.table = TRUE)
ADRDdenom.1 <- denom_file.1[QID %in% enrolledINFO[,QID]]
rm(denom_file.1)
gc()
denom_file.2 <- read_fst(paste0(dir_data,"national_exp.fst"),
                         from = ceiling(n.denom/2), as.data.table = TRUE)
ADRDdenom.2 <- denom_file.2[QID %in% enrolledINFO[,QID]]
rm(denom_file.2)
gc()
ADRDdenom <- rbind(ADRDdenom.1, ADRDdenom.2) # combine splits
rm(ADRDdenom.1, ADRDdenom.2)
gc()

ADRDdenom <- ADRDdenom[order(QID, year),] # order
fwrite(ADRDdenom, paste0(dir_output, "ADRDnational_exp.csv"))
gc()

ADRDdenom <- fread(paste0(dir_output, "ADRDnational_exp.csv"))
enrolledINFO <- fread(paste0(dir_output, "enrolledINFO.csv"))

## add "firstADRDyr"
ADRDdenom <- merge(ADRDdenom, enrolledINFO, by="QID")
table(ADRDdenom$firstADRDyr)

## start to follow-up after firstADRDyr
ADRDmort <- ADRDdenom[year_admit>=firstADRDyr]
ADRDmort <- ADRDmort[order(QID, year_admit)]

## get death info as mortINFO
ADRDmort$bene_dod <- as.Date(ADRDmort$bene_dod, format = "%Y-%m-%d") # convert format
mortINFO <- ADRDmort[,.(QID, bene_dod, year, year_admit)][!is.na(bene_dod)]
mortINFO[, mort_yr := as.numeric(format(bene_dod, "%Y"))][] #get death year for each ID
mortINFO <- mortINFO[,.SD[.N], by= QID]
mortINFO$mort_yr_admit <- ifelse(mortINFO$year_admit==mortINFO$year, mortINFO$mort_yr,
                                 mortINFO$year_admit)
mortINFO <- mortINFO[!duplicated(mortINFO)][, ':=' (bene_dod = NULL, year = NULL,
                                                    year_admit = NULL, death = 1)]
mortINFO[duplicated(mortINFO)]
fwrite(mortINFO, paste0(dir_output, "mortINFO.csv"))

## add "mort_year" and "death" status into ADRDmort
ADRDmort <- merge(ADRDmort, mortINFO, by = "QID", all.x = TRUE)
ADRDmort$death[is.na(ADRDmort$death)] <- 0 # not dead mark as 0
summary(ADRDmort$death)

## Drop years after death
ADRDmort <- subset(ADRDmort, death==0|(death==1 & year_admit <= mort_yr_admit))
fwrite(ADRDmort, paste0(dir_output, "ADRDmort_all.csv"))

##################### 3. check completeness of follow-up ######################
ADRDmort <- fread(paste0(dir_output, "ADRDmort_all.csv"))
enrolledINFO <- fread(paste0(dir_output, "enrolledINFO.csv"))
mortINFO <- fread(paste0(dir_output, "mortINFO.csv"))
dim(ADRDmort)
# > dim(ADRDmort)
# [1] 9534576      40

## remove duplication for several admissions in one year
## get "ADRDmort_ndup" dataset
check <- ADRDmort[,.(QID, year_admit)]
check$remove <- duplicated(check) # check duplication
summary(check$remove)
ADRDmort <- cbind(ADRDmort, check$remove)
names(ADRDmort)
ADRDmort_ndup <- ADRDmort[V2==FALSE]
ADRDmort_ndup[, V2 := NULL]
dim(ADRDmort_ndup)
# > dim(ADRDmort_ndup)
# [1] 6093887      40
remove(check)

## detect follow-up info problems
temp <- ADRDmort_ndup[,.(QID, year_admit, firstADRDyr)]
temp[, followyr := year_admit - firstADRDyr]
temp[,.(min.followyr = min(followyr)), by=QID][, min.followyr] %>% table() #detect problems
# > temp[,.(min.followyr = min(followyr)), by=QID][, min.followyr] %>% table() #detect problems
# .
# 0       1       2       3       4       5       6       7       8       9 
# 2193529   26057    3064     921     462     192     101      63      59      21 

## omit those without firstADRDyr details info
## get "ADRDmort_ndup.c" dataset
omit.QID <- temp[,.(min.followyr = min(followyr)), by=QID][min.followyr>0][,QID]
ADRDmort_ndup.c <- ADRDmort_ndup[!(QID %in% omit.QID)]
length(omit.QID)
# > length(omit.QID)
# [1] 30940
dim(ADRDmort_ndup.c)
# > dim(ADRDmort_ndup.c)
# [1] 6014801      40
remove(omit.QID)

## omit those alive max.year_admit!=2016, those dead max.year_admit != mort_year_admit
## get "ADRDmort_ndup.cc" dataset
checkEND <- ADRDmort_ndup.c[,.(max.year_admit = max(year_admit)), by = QID]
checkEND <- merge(checkEND, mortINFO, by = "QID", all.x = TRUE) # add death year
checkEND <- rbind(checkEND[is.na(death)][max.year_admit==2016], 
                  checkEND[death==1][max.year_admit==mort_yr_admit])
keep.QID <- checkEND[,QID]
ADRDmort_ndup.cc <- ADRDmort_ndup.c[QID %in% keep.QID]
length(keep.QID)
# > length(keep.QID)
# [1] 2096862
dim(ADRDmort_ndup.cc)
# > dim(ADRDmort_ndup.cc)
# [1] 5766810      40
remove(keep.QID)
remove(checkEND)

## omit those do not have follow-up for each year
checkEACH <- ADRDmort_ndup.cc[,.(max.year_admit = max(year_admit)), by = QID]
checkEACH <- merge(checkEACH, enrolledINFO, by = "QID", all.x = TRUE)[, idealN := max.year_admit - firstADRDyr + 1]
checkEACH <- merge(checkEACH, ADRDmort_ndup.cc[,.N, by = QID], by = "QID", all.x = TRUE)
omit.QID <- checkEACH[idealN!=N][,QID]
ADRDmort_cplt <- ADRDmort_ndup.cc[!(QID %in% omit.QID)]
length(omit.QID)
# > length(omit.QID)
# [1] 36726
dim(ADRDmort_cplt)
# > dim(ADRDmort_cplt)
# [1] 5608558      40
remove(omit.QID)

head(ADRDmort_cplt)
fwrite(ADRDmort_cplt, paste0(dir_output, "ADRDmort_cplt.csv"))
