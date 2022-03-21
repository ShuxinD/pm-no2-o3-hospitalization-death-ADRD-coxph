Check the completeness of follow-up in the ADRD population
================

The dataset `ADRDcohort.fst` was directly generated from the denominator
files, which follow the ADRD people till their death or the end of 2016.

## Setup and load data

    ##          used (Mb) gc trigger (Mb) max used (Mb)
    ## Ncells 443825 23.8     946752 50.6   642637 34.4
    ## Vcells 839661  6.5    8388608 64.0  1825869 14.0

    ## Warning: replacing previous import 'lifecycle::last_warnings' by
    ## 'rlang::last_warnings' when loading 'pillar'

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:data.table':
    ## 
    ##     between, first, last

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

Our final dataset should:

  - be one row per person-year (long format)
  - have follow-up info for every year starting from `firstADRDyr` till
    the end of study period (2016) or death or leaving the US, whichever
    comes first (though we do not need the info for `year==firstADRDyr`
    in survival analysis, we do need it for correcting index event bias)

## Basic info of original ADRD dataset

    ## number of subjects in original ADRD cohort dataset 8142113

    ## number of subjects in cohorts after removing NAs 7974481

Those who admitted for the first time with ADRD code and died in the
same year don’t contribute to the risk set. **Consider deleting those
subjects**

The number of person-years/subjects considered to be deleted due to
no-contribution is
2427706.

``` r
temp <- na.omit(ADRDcohort[!(qid %in% no_contribution[,qid])]) # remove all the NAs and those without contribution to risk set
sum_temp <- temp[, .(start_yr = min(year),
                     end_yr = max(year),
                     count = uniqueN(year)), by = qid]
sum_temp <- merge(sum_temp, enrolledInfo, by.x = "qid", by.y = "QID", all.x = TRUE)
```

We constructed a temporary dataset named `temp` which is a subset of
`ADRDcohort` after removing NA (not remove rows with
`year==firstADRDyr`, but remove those making no contribution to the risk
set), and summarize each person as one row in total 5602854 subjects:

  - generated `start_yr` as the minimum of calendar year (should equal
    to `firstADRDyr`)
  - `end_yr` as the maximum of calendar year
  - `count` as the count number of unique calendar year for each
    subject.

We also merged the enroll information (`firstADRDyear`) to `sum_temp`.
`firstADRDyr` indicates the year that subjects should start to be
followed-up.

`sum_temp` is a one-row-per-person
    dataset.

    ## the number of subjects in ADRD cohort (after removing NAs, and non-contribution subjects) is 5602854

    ## the number of person-years in ADRD cohort (after removing NAs, and non-contribution subjects) is 24274301

    ## is there any duplication of the combination of `qid` and calendar year:  FALSE

    ## is there any duplication of the combination of `qid` and age:  TRUE

## Check the completeness of follow-up after removing no-contribution subjects

### 1\. how many subjects were not followed-up from `firstADRDyr`

The number of those not followed-up from `firstADRDyr`: 27769

Their person-years in denominator files are:
98596

    ## the number of subjects not followed from firstADRDyr is 27769

    ## the number of person-years of related subjects is 98596

<!-- ```{r summarize} # dead event summary(temp[qid %in% sum_temp[start_yr != -->

<!-- firstADRDyr, qid],.SD[.N],by=qid][,dead]) # almost the same as the general -->

<!-- population ``` -->

<!-- <!-- #### fix -->

–\>

<!-- ```{r operature} -->

<!-- # create row for each missing year -->

<!-- op1 <- sum_temp[start_yr != (firstADRDyr+1),.(qid, firstADRDyr, year=start_yr-1)] -->

<!-- op2 <- op1[year!=firstADRDyr+1,.(qid, firstADRDyr, year=year-1)] -->

<!-- op3 <- op2[year!=firstADRDyr+1,.(qid, firstADRDyr, year=year-1)] -->

<!-- op4 <- op3[year!=firstADRDyr+1,.(qid, firstADRDyr, year=year-1)] -->

<!-- op5 <- op4[year!=firstADRDyr+1,.(qid, firstADRDyr, year=year-1)] -->

<!-- op6 <- op5[year!=firstADRDyr+1,.(qid, firstADRDyr, year=year-1)] -->

<!-- op7 <- op6[year!=firstADRDyr+1,.(qid, firstADRDyr, year=year-1)] -->

<!-- op8 <- op7[year!=firstADRDyr+1,.(qid, firstADRDyr, year=year-1)] -->

<!-- op9 <- op8[year!=firstADRDyr+1,.(qid, firstADRDyr, year=year-1)] -->

<!-- op10 <- op9[year!=firstADRDyr+1,.(qid, firstADRDyr, year=year-1)] -->

<!-- op11 <- op10[year!=firstADRDyr+1,.(qid, firstADRDyr, year=year-1)] -->

<!-- op12 <- op11[year!=firstADRDyr+1,.(qid, firstADRDyr, year=year-1)] -->

<!-- op13 <- op12[year!=firstADRDyr+1,.(qid, firstADRDyr, year=year-1)] -->

<!-- op14 <- op13[year!=firstADRDyr+1,.(qid, firstADRDyr, year=year-1)] -->

<!-- op15 <- op14[year!=firstADRDyr+1,.(qid, firstADRDyr, year=year-1)] -->

<!-- op16 <- op15[year!=firstADRDyr+1,.(qid, firstADRDyr, year=year-1)] -->

<!-- insert <- rbind(op1,op2,op3,op4,op5,op6,op7,op8,op9,op10,op11,op12,op13,op14,op15,op16) -->

<!-- setorder(insert,qid, year) -->

<!-- insert # dataset to be inserted -->

<!-- ``` -->

<!-- ```{r covariates} -->

<!-- # covariates info by zip and year -->

<!-- covariates <- unique(temp[,.(zip, year, mean_bmi, smoke_rate, hispanic, pct_blk, medhouseholdincome, medianhousevalue, poverty, education, popdensity, pct_owner_occ, summer_tmmx, winter_tmmx, summer_rmax, winter_rmax, pm25, no2, ozone, ozone_summer)]) -->

<!-- ``` -->

<!-- ```{r demographic} -->

<!-- setorder(temp, qid, year) -->

<!-- demographic <- temp[,.SD[1],by=qid][,diff_ageyear:=year-age][,.(qid, sex, race, dual, statecode,diff_ageyear)] -->

<!-- demographic -->

<!-- ``` -->

<!-- ```{r} -->

<!-- # get zip info to "insert"" datset -->

<!-- temp[qid %in% insert[,qid],.(qid,zip)][,uniqueN(zip),by=qid][V1!=1,] # check if zip change during follow-up -->

<!-- zipinfo <- temp[qid %in% insert[,qid],.(qid,zip)][,.SD[1],by=qid] # take one of zip -->

<!-- insert <- merge(insert, zipinfo, by="qid", all.x = T) -->

<!-- ``` -->

<!-- ```{r} -->

<!-- insert <- merge(insert, covariates, by=c("year", "zip"), all.x = T) -->

<!-- anyNA(insert) -->

<!-- insert <- merge(insert, demographic, by="qid", all.x=T) -->

<!-- insert[,age:=year-diff_ageyear] -->

<!-- insert[,diff_ageyear:=NULL] -->

<!-- ``` -->

<!-- ```{r} -->

<!-- insert[,`:=`(year_prev=year-1,dead=F)] -->

<!-- ADRDcohort_added <- rbind(insert,temp) -->

<!-- ``` -->

<!-- ```{r} -->

<!-- temp_added <- na.omit(ADRDcohort_added) # remove all the NAs -->

<!-- sum_temp_added <- temp_added[, .(start_yr = min(year), -->

<!--                      end_yr = max(year), -->

<!--                      count = uniqueN(year)), by = qid] -->

<!-- sum_temp_added <- merge(sum_temp_added, enrolledInfo, by.x = "qid", by.y = "QID", all.x = TRUE) -->

<!-- ``` -->

### 2\. how many people don’t have each year’s info

    ## the number of subjects not having each year's info (contain those from 1.) is 36843

    ## the number of person-years (contain those from 1.) is 152417

## Omit those without complete follow-up and contribution to the risk set

    ## the number of subjects without complete follow-up is 36843

    ## the number of person-years of related subjects is 152417

    ## the number of person-years/subjects making no-contribution to the risk set is 2427706

``` r
## save qid without complete follow-up or no contribution to risk set
omitInfo <- rbind(sum_temp[(end_yr-firstADRDyr+1) != count, .(qid,end_yr, firstADRDyr)], no_contribution)
```

The above subject (2464549 subjects in total) do not have each year’s
info during follow-up or (mostly) make no contribution to the risk set.
**Deleting them**

## Check right-censoring of the processed data

Check whether alive people were followed-up till the end of study period
(2016) after excluding those without complete
follow-up

``` r
sum_temp[!(qid %in% temp[(dead),qid]), ][!(qid %in% omitInfo[,qid]),end_yr] %>% table()
```

    ## .
    ##   2000   2001   2002   2003   2004   2005   2006   2007   2008   2009   2010 
    ##    890    878   1043   1911   2583   2564   2743   1818   1617   1521   1522 
    ##   2011   2012   2013   2014   2015   2016 
    ##   1527   1794   1701   2656   2127 981936

We could see some alive subjects weren’t followed-up till 2016. This
should be considered as right-censored subjects in the analyses.
