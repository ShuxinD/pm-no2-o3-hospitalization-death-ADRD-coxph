## setup----
rm(list = ls())
gc()

library(data.table)
library(fst)
setDTthreads(threads = 0)

wkdir <- "/n/dominici_nsaph_l3/Lab/projects/pm-no2-o3-hospitalization-death-ADRD-coxph"
rundir <- file.path(wkdir, "code", "revise_all_hospitalization")

## load data ----
ALLcohort <- read_fst(file.path(rundir, "ALLcohort.fst"), as.data.table = T)
names(ALLcohort)
dim(ALLcohort)
uniqueN(ALLcohort[,qid]) # study population individuals

## omit ----
#' first: remove NAs
dt <- na.omit(ALLcohort) # remove NAs
dim(dt) # person-year
uniqueN(dt[,qid]) # number of subjects

#' second: remove those without complete follow-ups/no-contribution
omitInfo <- read_fst(file.path(rundir,"omitInfo.fst"))
dt <- dt[!(qid %in% omitInfo$qid),]
dim(dt) # person-year
uniqueN(dt[,qid]) # number of subjects

#' third: race==unknow
uniqueN(dt[race==0,qid]) # number subjects with unknown race info
dt <- dt[!(qid %in% dt[race==0,qid]),]
dim(dt) # person-year
uniqueN(dt[,qid]) # number of subjects

dim(dt) # final
uniqueN(dt, by = "qid") #final # of subjects

## add necessary variables ----
#' create entry_age variable, 5 years as a break
tempAge <- dt[, .(entry_age = min(age)), by = qid]
head(tempAge)
min(tempAge[,entry_age])
max(tempAge[,entry_age])
seq(65, 115, 5)
tempAge[, entry_age_break := cut(entry_age, breaks = seq(65, 115, 5), right = FALSE)][]
summary(tempAge[,entry_age_break])
head(tempAge)

dt <- merge(dt, tempAge, by = "qid") ## add entry_age and entry_age_break

#' merge different race categories and create race_collapsed
dt[,race:=as.factor(race)]
table(dt[,race])
dt$race_collapsed <- "Others"
dt$race_collapsed[dt$race==1] <- "White"
dt$race_collapsed[dt$race==2] <- "Black"
dt$race_collapsed[dt$race==5] <- "Hispanic"
dt[, race_collapsed:=as.factor(race_collapsed)]

#' generate region based on statecode
NORTHEAST <- c("NY", "MA", "PA", "RI", "NH", "ME", "VT", "CT", "NJ")  
SOUTH <- c("DC", "VA", "NC", "WV", "KY", "SC", "GA", "FL", "AL", "TN", "MS", 
           "AR", "MD", "DE", "OK", "TX", "LA")
MIDWEST <- c("OH", "IN", "MI", "IA", "MO", "WI", "MN", "SD", "ND", "IL", "KS", "NE")
WEST <- c("MT", "CO", "WY", "ID", "UT", "NV", "CA", "OR", "WA", "AZ", "NM")
dt$region <- ifelse(dt$statecode %in% NORTHEAST, "NORTHEAST",
                    ifelse(dt$statecode %in% SOUTH, "SOUTH",
                           ifelse(dt$statecode  %in% MIDWEST, "MIDWEST",
                                  ifelse(dt$statecode  %in% WEST, "WEST",
                                         NA))))
dt[, region := as.factor(region)]

#' check variable class ----
# summary(dt)
dt[, `:=`(dual = as.factor(dual))]

dim(dt)[1] # 24066724
uniqueN(dt[,qid])
write_fst(dt, file.path(rundir, "ALLcohort_clean.fst"))