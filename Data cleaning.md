# Clean the 2000-2016 Medicare dataset

for the project of mortality and air pollution in Medicare ADRD population

## Availabel datasets overview and general idea of cleaning

Two datasets available to construct the final dataset:

- "hospital_total.rds" contains info of who should be enrolled in our cohort (who were hospitalized with ADRD diagnosis)

- "national_exp.fst" contains info of all the Medicare enrollees, including their basic socioeconomics status, air pollution merged by year of their admission, census data by zip code merged by zip code provided in the hospitalization record. ***Note***: this dataset is a expanded one, the records for each subjects are **expected** to last till 2016 or their death

Our general idea is to:

- get the **QID** and **first ADRD year (firstADRDyr)** of those who should be enrolled in our cohort from "hospital_total.rds"
- subset the "national_exp.fst" based on **QID** we get from the previous step, merge the **firstADRDyr** by QID and drop the year before firstADRDyr.

Our final dataset should:

- be one row per person year
- have follow-up info for every year after the year they enrolled in the cohort (firstADRDyr) till the end of study period or death, whichever comes first.

Final dataset example:

| QID   | AGE  | Sex_gp | year | year_admit | ...  | firstADRDyr | mort_yr | Death |
| ----- | ---- | ------ | ---- | ---------- | ---- | ----------- | ------- | ----- |
| ...   | ...  | ...    | ...  | ...        | ...  | ...         | ...     | ...   |
| xxxx1 | 77   | male   | 2008 | 2008       | ...  | 2008        | 2010    | 1     |
| xxxx1 | 78   | female | 2009 | 2009       | ...  | 2008        | 2010    | 1     |
| xxxx1 | 79   | male   | 2010 | 2010       | ...  | 2008        | 2010    | 1     |
| ...   | ...  | ...    | ...  | ...        | ...  | ...         | ...     | ...   |
| xxxx3 | 73   | female | 2013 | 2013       | ...  | 2013        | NA      | 0     |
| xxxx3 | 74   | female | 2014 | 2014       | ...  | 2013        | NA      | 0     |
| xxxx3 | 75   | female | 2015 | 2015       | ...  | 2013        | NA      | 0     |
| xxxx3 | 76   | female | 2016 | 2016       | ...  | 2013        | NA      | 0     |

## Steps:

### Get QID and firstADRDyr from "hospital_total.rds"

```R
med <- readRDS(paste0(dir_data, "hospital_total.rds"))
setDT(med)
ADRDmed <- subset(med, Alzheimer_pdx==1|Alzheimer_pdx2dx_10==1|Alzheimer_pdx2dx_25==1|
                    Dementia_pdx==1|Dementia_pdx2dx_10==1|Dementia_pdx2dx_25 ==1,
                  select = c("QID", "ADATE"))
ADRDmed$year_admit <- as.numeric(format(ADRDmed$ADATE, "%Y"))
enrollINFO <- aggregate(year_admit ~ QID, ADRDmed, min)
setDT(enrollINFO)
setnames(enrollINFO, "year_admit", "firstADRDyr")
```

### ID PROBLEMS: cohort start from 2007

As described in [a markdown file wrote by Ben](https://github.com/NSAPH/data_requests/blob/master/request_projects/nov2019_check_cms_crosswalk/reports/check_id_crosswalks.md) and [another one](https://github.com/NSAPH/data_requests/blob/master/request_projects/nov2019_check_cms_crosswalk/reports/check_medicare_crosswalk.md), our Medicare dataset actually consist of two parts with two different QID coding system, though the cross-walk file successfully matched some QIDs from two parts, but not all of them - in the "national_exp.fst" dataset, some subjects only follow-up till 2006, and some start in 2007.

Ideally, we could assume all the people, who ended follow-up in 2006, continues their follow-up from 2007 with different QID. In this case, we could leave them in the dataset. But the problem is, we subset the "national_exp.fst" dataset based on QID generated from "hospital_total.rds", thus the people, whose firstADRDyr is before 2006 and who have a different QID after 2007, will be dropped.

Considering our large dataset, the most straightforward way is to drop subjects whose firstADRDyr<=2006, and continue the following steps.

```R
## drop subjects whose firstADRDyr <= 2006
enrolledINFO <- enrollINFO[firstADRDyr>2006]
fwrite(enrolledINFO, paste0(dir_output, "enrolledINFO.csv"))
```

### Subset "national_exp.fst" 

#### subset it based on QID, add enroll information

```R
## subset the dataset to those in ADRD cohort
denom_file <- read_fst(paste0(dir_data,"national_exp.fst"), as.data.table = TRUE)
ADRDdenom <- denom_file.1[QID %in% enrolledINFO[,QID]]
ADRDdenom <- ADRDdenom[order(QID, year),] # order
fwrite(ADRDdenom, paste0(dir_output, "ADRDnational_exp.csv"))
## add "firstADRDyr"
ADRDdenom <- merge(ADRDdenom, enrolledINFO, by="QID")
```

#### subset it to "firstADRDyr ~ mort_yr_admit/2016"

Since `year` stands for year of discharge and `year_admit` stands for year of admission, `year` may not equal to `yea_admit` in one row when someone was discharged in the next year. Considering if someone died in hospital and then was recorded as discharge in the next year of admission, we could not have another row for the death year since "national_exp.fst" does not have it. Thus, we generated another variable to mark the end of follow-up year as `mort_yr_admit`- for those who are the same case as described before, `mort_yr_admit` equals to the last admission year, otherwise, equals to `mort_yr`.

All the pollution are merged by `year_admit` according to Mahdieh,

```R
## get death info as mortINFO
ADRDmort$bene_dod <- as.Date(ADRDmort$bene_dod, format = "%Y-%m-%d") #convert format
mortINFO <- ADRDmort[,.(QID, bene_dod, year, year_admit)][!is.na(bene_dod)]
mortINFO[, mort_yr := as.numeric(format(bene_dod, "%Y"))][] #get death year for each ID
mortINFO <- mortINFO[,.SD[.N], by= QID] # delete duplication
mortINFO$mort_yr_admit <- ifelse(mortINFO$year_admit==mortINFO$year, mortINFO$mort_yr,
                                 mortINFO$year_admit) # create mort_yr_admit
mortINFO <- mortINFO[!duplicated(mortINFO)][, ':=' (bene_dod = NULL, year = NULL,
                                                    year_admit = NULL, death = 1)]
fwrite(mortINFO, paste0(dir_output, "mortINFO.csv"))
## merge mortINFO into ADRDmort
ADRDmort <- merge(ADRDmort, mortINFO, by = "QID", all.x = TRUE)
ADRDmort$death[is.na(ADRDmort$death)] <- 0 # not dead mark as 0
## Drop years after death based on 'mort_yr_admit'
ADRDmort <- subset(ADRDmort, death==0|(death==1 & year_admit <= mort_yr_admit))
fwrite(ADRDmort, paste0(dir_output, "ADRDmort_all.csv"))
```

### Check the completeness of follow-up

#### [1] change one row per admission to one row per p-y

```R
## remove duplication for several admissions in one year, get "ADRDmort_ndup" dataset
check <- ADRDmort[,.(QID, year_admit)]
check$remove <- duplicated(check) # check duplication
summary(check$remove)
ADRDmort <- cbind(ADRDmort, check$remove)
names(ADRDmort)
ADRDmort_ndup <- ADRDmort[V2==FALSE]
ADRDmort_ndup[, V2 := NULL]
```

#### [2] detect problems

```R
temp <- ADRDmort_ndup[,.(QID, year_admit, firstADRDyr)]
temp[, followyr := year_admit - firstADRDyr]
temp[,.(min.followyr = min(followyr)), by=QID][, min.followyr] %>% table() #detect problems
# 0       1       2       3       4       5       6       7       8       9 
# 2193529   26057    3064     921     462     192     101      63      59      21 
```

Ideally, the results should be all zeros, which means every one has the follow-up info starting from first ADRD year. Though we used the dataset omit the firstADRDyr to ensure a complete exposure history, we want to have firstADRDyr info to get the principal diagnosis for future analyses.

#### [3] omit those without firstADRDyr info

```R
## get "ADRDmort_ndup.c" dataset
omit.QID <- temp[,.(min.followyr = min(followyr)), by=QID][min.followyr>0][,QID]
ADRDmort_ndup.c <- ADRDmort_ndup[!(QID %in% omit.QID)]
```

#### [4] omit those not followed-up till the end

```R
## omit those alive max.year_admit!=2016, those dead max.year_admit!=mort_year_admit, get "ADRDmort_ndup.cc" dataset
checkEND <- ADRDmort_ndup.c[,.(max.year_admit = max(year_admit)), by = QID]
checkEND <- merge(checkEND, mortINFO, by = "QID", all.x = TRUE) # add death year
checkEND <- rbind(checkEND[is.na(death)][max.year_admit==2016], 
                  checkEND[death==1][max.year_admit==mort_yr_admit])
keep.QID <- checkEND[,QID]
ADRDmort_ndup.cc <- ADRDmort_ndup.c[QID %in% keep.QID]
```

#### [5] omit those missing years during follow-up

```R
## omit those do not have follow-up for each year
checkEACH <- ADRDmort_ndup.cc[,.(max.year_admit = max(year_admit)), by = QID]
checkEACH <- merge(checkEACH, enrolledINFO, by = "QID", all.x = TRUE)[, idealN := max.year_admit - firstADRDyr + 1]
checkEACH <- merge(checkEACH, ADRDmort_ndup.cc[,.N, by = QID], by = "QID", all.x = TRUE)
omit.QID <- checkEACH[idealN!=N][,QID]
ADRDmort_cplt <- ADRDmort_ndup.cc[!(QID %in% omit.QID)]
```



|                               | Number of person-yrs                               | Number of subjects |
| ----------------------------- | -------------------------------------------------- | ------------------ |
| Initial `ADRDmort`            | 9,534,576 (rows not person-year, have duplication) | 2,224,469          |
| After [1]: `ADRDmort_ndup`    | 6,093,887                                          | same as above      |
| After [3]: `ADRDmort_ndup.c`  | 6,014,801                                          | 2,193,529          |
| After [4]: `ADRDmort_ndup.cc` | 5,766,810                                          | 2,096,862          |
| After [5]: `ADRDmort_cplt`    | 5,608,558                                          | 2,060,136          |

