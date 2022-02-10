Check the completeness of follow-up in the ADRD population
================

The dataset `ADRDcohort.fst` was directly generated from the denominator
files, which follow the ADRD people till their death or the end of 2016.

## Setup and load data

    ##          used (Mb) gc trigger (Mb) max used (Mb)
    ## Ncells 443911 23.8     946998 50.6   642637 34.4
    ## Vcells 839798  6.5    8388608 64.0  1825873 14.0

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

    ## Warning: replacing previous import 'lifecycle::last_warnings' by
    ## 'rlang::last_warnings' when loading 'hms'

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

`sum_temp` is a one-row-per-person dataset, looks like:

    ##          qid start_yr end_yr count firstADRDyr
    ## 1: 003050048     2000   2004     5        2000
    ## 2: 003628694     2002   2005     4        2002
    ## 3: 003749417     2005   2006     2        2005
    ## 4: 004402139     2004   2005     2        2004
    ## 5: 012472193     2000   2004     5        2000
    ## 6: 014929014     2005   2006     2        2005

    ## the number of subjects in ADRD cohort (after removing NAs, and non-contribution subjects) is 5602854

    ## the number of person-years in ADRD cohort (after removing NAs, and non-contribution subjects) is 24274301

    ## is there any duplication of the combination of `qid` and calendar year:  FALSE

    ## is there any duplication of the combination of `qid` and age:  TRUE

## Check the completeness of follow-up after removing no-contribution subjects

### 1\. how many subjects were not followed-up from `firstADRDyr`

The indivudual-level info of those not followed-up from `firstADRDyr`:

    ##                    qid start_yr end_yr count firstADRDyr
    ##     1:       A00003963     2002   2002     1        2000
    ##     2:       A00005533     2001   2002     2        2000
    ##     3:       A00016302     2002   2002     1        2001
    ##     4:       A00024753     2001   2002     2        2000
    ##     5:       A00041247     2002   2002     1        2001
    ##    ---                                                  
    ## 27765: lllllllooooXoS7     2003   2004     2        2001
    ## 27766: lllllllooool4OO     2006   2006     1        2002
    ## 27767: llllllloooolllX     2004   2005     2        2001
    ## 27768: lllllllooooo70l     2009   2012     4        2008
    ## 27769: lllllllooooolU8     2006   2006     1        2000

Their info in denominator
    files:

    ##          zip year             qid sex race age dual statecode  dead mean_bmi
    ##     1: 01001 2012 llllllloX8S8U40   1    1  76    1        MA FALSE 28.30406
    ##     2: 01001 2013 llllllloX8S8U40   1    1  76    1        MA  TRUE 28.18981
    ##     3: 01002 2013 llllllXl7Xl7lS0   1    1  65    0        MA FALSE 26.58850
    ##     4: 01002 2014 llllllXl7Xl7lS0   1    1  66    0        MA FALSE 26.59922
    ##     5: 01002 2015 llllllXl7Xl7lS0   1    1  67    0        MA FALSE 26.57164
    ##    ---                                                                      
    ## 98592: 99403 2010 lllllll4S88S0XX   2    1  82    0        WA  TRUE 27.45471
    ## 98593: 99403 2010 llllllloXlSUlSS   1    6  87    0        WA FALSE 27.45471
    ## 98594: 99403 2011 llllllloXlSUlSS   1    6  88    0        WA FALSE 27.76113
    ## 98595: 99403 2012 llllllloXlSUlSS   1    6  89    0        WA FALSE 28.49232
    ## 98596: 99403 2013 llllllloXlSUlSS   1    6  89    0        WA  TRUE 28.15183
    ##        smoke_rate   hispanic     pct_blk medhouseholdincome medianhousevalue
    ##     1:  0.4815401 0.04666283 0.019965478            63682.0         216300.0
    ##     2:  0.4889693 0.05329081 0.011771528            58733.0         213000.0
    ##     3:  0.4938542 0.07585594 0.044727670            54422.0         338900.0
    ##     4:  0.4947380 0.07545905 0.046743064            55082.0         341600.0
    ##     5:  0.4893738 0.06443922 0.046977837            50540.0         342600.0
    ##    ---                                                                      
    ## 98592:  0.5045455 0.03188513 0.005108372            42003.4         168373.3
    ## 98593:  0.5045455 0.03188513 0.005108372            42003.4         168373.3
    ## 98594:  0.4623116 0.03088451 0.004657992            41832.0         167400.0
    ## 98595:  0.5142857 0.03189135 0.005231388            41944.0         169200.0
    ## 98596:  0.5010794 0.03394085 0.006125420            42813.0         171100.0
    ##           poverty  education popdensity pct_owner_occ summer_tmmx winter_tmmx
    ##     1: 0.05391239 0.15499682  1518.9230     0.7467940    301.9466    279.6620
    ##     2: 0.05792350 0.14565826  1507.1250     0.7172619    301.7673    277.7625
    ##     3: 0.02638329 0.05393021   531.7096     0.4931877    300.5434    276.6102
    ##     4: 0.03756201 0.03510566   542.2108     0.4678744    299.2179    273.8323
    ##     5: 0.04808317 0.04571071   541.0480     0.4653600    299.4404    272.2597
    ##    ---                                                                       
    ## 98592: 0.08562909 0.16409869   129.6158     0.6894861    297.7093    277.9926
    ## 98593: 0.08562909 0.16409869   129.6158     0.6894861    297.7093    277.9926
    ## 98594: 0.08572216 0.16851852   129.1871     0.7085169    298.0834    276.8914
    ## 98595: 0.08380187 0.16446858   130.0309     0.6706572    300.1234    278.0944
    ## 98596: 0.08106721 0.15386534   130.2729     0.6658240    300.9082    276.8044
    ##        summer_rmax winter_rmax firstADRDyr     pm25       no2    ozone
    ##     1:    88.36022    76.76339        2005 7.785247 21.251995 38.56446
    ##     2:    89.34300    79.80806        2005 8.117705 20.953636 39.29929
    ##     3:    92.34675    82.94412        2012 5.491770  8.813608 37.94450
    ##     4:    89.27372    89.09430        2012 5.359485  8.800052 38.20112
    ##     5:    91.46494    91.47405        2012 4.516610  9.261477 39.83256
    ##    ---                                                                
    ## 98592:    60.52847    82.76904        2004 4.240930 10.042199 38.62864
    ## 98593:    60.52847    82.76904        2006 4.240930 10.042199 38.62864
    ## 98594:    70.07012    87.93301        2006 5.313538  8.325685 39.74654
    ## 98595:    68.33219    86.07601        2006 6.048468  6.782057 40.82305
    ## 98596:    60.85726    88.92360        2006 4.135061  7.509630 40.00931
    ##        ozone_summer       ox
    ##     1:     43.72098 32.72640
    ##     2:     43.79662 33.11032
    ##     3:     40.41729 28.07815
    ##     4:     40.73180 28.24311
    ##     5:     43.05488 29.47845
    ##    ---                      
    ## 98592:     42.04486 28.94891
    ## 98593:     42.04486 28.94891
    ## 98594:     43.70882 29.10272
    ## 98595:     46.27007 29.28810
    ## 98596:     44.29974 28.99829

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

The indivudual-level info of those not having each year’s info
(containing those from 1.):

    ##                    qid start_yr end_yr count firstADRDyr
    ##     1:       037641248     2002   2004     2        2002
    ##     2:       A00003963     2002   2002     1        2000
    ##     3:       A00005533     2001   2002     2        2000
    ##     4:       A00016302     2002   2002     1        2001
    ##     5:       A00024753     2001   2002     2        2000
    ##    ---                                                  
    ## 36839: lllllllooooXoS7     2003   2004     2        2001
    ## 36840: lllllllooool4OO     2006   2006     1        2002
    ## 36841: llllllloooolllX     2004   2005     2        2001
    ## 36842: lllllllooooo70l     2009   2012     4        2008
    ## 36843: lllllllooooolU8     2006   2006     1        2000

    ## the number of subjects not having each year's info (contain those from 1.) is 36843

    ## the number of person-years (contain those from 1.) is 152417

## Omit those without complete follow-up and contribution to the risk set

    ## the number of subjects without complete follow-up is 36843

    ## the number of person-years of related subjects is 152417

``` r
cat("the number of person-years/subjects making no-contribution to the risk set is", dim(no_contribution)[1], "\n")
```

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
