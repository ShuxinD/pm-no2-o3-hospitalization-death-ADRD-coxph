Check the completeness of follow-up in the ADRD population
================

The dataset `ADRDcohort.fst` was directly generated from the denominator
files, which follow the ADRD people till their death or the end of 2016.

## Setup and load data

    ##          used (Mb) gc trigger (Mb) max used (Mb)
    ## Ncells 403488 21.6     831504 44.5   642637 34.4
    ## Vcells 774276  6.0    8388608 64.0  1825873 14.0

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

## Check the completeness of follow-up

Our final dataset should:

  - be one row per person-year (long format)
  - have follow-up info for every year starting from `firstADRDyr` till
    the end of study period (2016) or death or leaving the US, whichever
    comes first (though we do not need the info for `year==firstADRDyr`
    in survival analysis, we do need it for correcting index event bias)

### 1\. basic info of original ADRD dataset

``` r
# number of subjects in raw cohort
uniqueN(ADRDcohort[,qid])
```

    ## [1] 7638546

``` r
# number of subjects in cohorts after removing NAs
uniqueN(na.omit(ADRDcohort)[,qid])
```

    ## [1] 7480702

Those who admitted for the first time with ADRD code and died in the
same year don’t contribute to the risk set. Delete those

``` r
A <- ADRDcohort[, .(end_yr = max(year)), by = qid]
A <- merge(A, enrolledInfo, by.x = "qid", by.y = "QID")
no_contribution <- A[end_yr==firstADRDyr,]
dim(no_contribution)
```

    ## [1] 2411997       3

``` r
temp <- na.omit(ADRDcohort[!(qid %in% no_contribution[,qid])]) # remove all the NAs and those without contribution to risk set
sum_temp <- temp[, .(start_yr = min(year),
                     end_yr = max(year),
                     count = uniqueN(year)), by = qid]
sum_temp <- merge(sum_temp, enrolledInfo, by.x = "qid", by.y = "QID", all.x = TRUE)
```

We constructed a temporary dataset named `temp` which is a subset of
`ADRDcohort` after removing NA (not remove rows with
`year==firstADRDyr`), and summarize each person as one row in total
5123866 subjects:

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

    ## the number of subjects in ADRD cohort (removed NAs, and non-contribution subjects) is 5123866

    ## the number of person-years in ADRD cohort (removed NAs, and non-contribution subjects) is 22810558

    ## is there any duplication of the combination of `qid` and calendar year:  FALSE

    ## is there any duplication of the combination of `qid` and age:  TRUE

### 2\. check whether all people were followed-up from the year following firstADRDyr

``` r
sum_temp[start_yr != (firstADRDyr)]
```

    ##                    qid start_yr end_yr count firstADRDyr
    ##     1:       A00003963     2002   2002     1        2000
    ##     2:       A00005533     2001   2002     2        2000
    ##     3:       A00016302     2002   2002     1        2001
    ##     4:       A00024753     2001   2002     2        2000
    ##     5:       A00041247     2002   2002     1        2001
    ##    ---                                                  
    ## 26257: lllllllooooXoS7     2003   2004     2        2001
    ## 26258: lllllllooool4OO     2006   2006     1        2002
    ## 26259: llllllloooolllX     2004   2005     2        2001
    ## 26260: lllllllooooo70l     2009   2012     4        2008
    ## 26261: lllllllooooolU8     2006   2006     1        2000

The above subject (5109426 subjects in total), were not followed-up from
the year following firstADRDyr.

Their info in denominator
    files:

    ##          zip year             qid sex race age dual statecode  dead mean_bmi
    ##     1: 01001 2012 llllllloX8S8U40   1    1  76    1        MA FALSE 28.30406
    ##     2: 01001 2013 llllllloX8S8U40   1    1  76    1        MA  TRUE 28.18981
    ##     3: 01001 2014 llllllU800X4O7O   2    2  66    1        MA  TRUE 28.19716
    ##     4: 01002 2013 llllllXl7Xl7lS0   1    1  65    0        MA FALSE 26.58850
    ##     5: 01002 2014 llllllXl7Xl7lS0   1    1  66    0        MA FALSE 26.59922
    ##    ---                                                                      
    ## 95579: 99403 2010 llllllloXlSUlSS   1    6  87    0        WA FALSE 27.45471
    ## 95580: 99403 2011 llllllloXlSUlSS   1    6  88    0        WA FALSE 27.76113
    ## 95581: 99403 2012 llllllloXlSUlSS   1    6  89    0        WA FALSE 28.49232
    ## 95582: 99403 2013 llllllloXlSUlSS   1    6  89    0        WA  TRUE 28.15183
    ## 95583: 99403 2016 llllllloX0llXX4   1    1  75    0        WA  TRUE 28.24859
    ##        smoke_rate   hispanic     pct_blk medhouseholdincome medianhousevalue
    ##     1:  0.4815401 0.04666283 0.019965478            63682.0         216300.0
    ##     2:  0.4889693 0.05329081 0.011771528            58733.0         213000.0
    ##     3:  0.4875532 0.05909807 0.011259553            60775.0         212900.0
    ##     4:  0.4938542 0.07585594 0.044727670            54422.0         338900.0
    ##     5:  0.4947380 0.07545905 0.046743064            55082.0         341600.0
    ##    ---                                                                      
    ## 95579:  0.5045455 0.03188513 0.005108372            42003.4         168373.3
    ## 95580:  0.4623116 0.03088451 0.004657992            41832.0         167400.0
    ## 95581:  0.5142857 0.03189135 0.005231388            41944.0         169200.0
    ## 95582:  0.5010794 0.03394085 0.006125420            42813.0         171100.0
    ## 95583:  0.4969610 0.03814741 0.003137450            44386.0         174300.0
    ##           poverty  education popdensity pct_owner_occ summer_tmmx winter_tmmx
    ##     1: 0.05391239 0.15499682  1518.9230     0.7467940    301.9466    279.6620
    ##     2: 0.05792350 0.14565826  1507.1250     0.7172619    301.7673    277.7625
    ##     3: 0.07678883 0.14491449  1498.0350     0.7125714    300.7621    275.2047
    ##     4: 0.02638329 0.05393021   531.7096     0.4931877    300.5434    276.6102
    ##     5: 0.03756201 0.03510566   542.2108     0.4678744    299.2179    273.8323
    ##    ---                                                                       
    ## 95579: 0.08562909 0.16409869   129.6158     0.6894861    297.7093    277.9926
    ## 95580: 0.08572216 0.16851852   129.1871     0.7085169    298.0834    276.8914
    ## 95581: 0.08380187 0.16446858   130.0309     0.6706572    300.1234    278.0944
    ## 95582: 0.08106721 0.15386534   130.2729     0.6658240    300.9082    276.8044
    ## 95583: 0.07679012 0.10903804   131.3391     0.6577767    299.1663    279.0391
    ##        summer_rmax winter_rmax firstADRDyr     pm25       no2    ozone
    ##     1:    88.36022    76.76339        2005 7.785247 21.251995 38.56446
    ##     2:    89.34300    79.80806        2005 8.117705 20.953636 39.29929
    ##     3:    85.19388    85.68319        2012 6.792893 20.091299 38.57935
    ##     4:    92.34675    82.94412        2012 5.491770  8.813608 37.94450
    ##     5:    89.27372    89.09430        2012 5.359485  8.800052 38.20112
    ##    ---                                                                
    ## 95579:    60.52847    82.76904        2006 4.240930 10.042199 38.62864
    ## 95580:    70.07012    87.93301        2006 5.313538  8.325685 39.74654
    ## 95581:    68.33219    86.07601        2006 6.048468  6.782057 40.82305
    ## 95582:    60.85726    88.92360        2006 4.135061  7.509630 40.00931
    ## 95583:    63.34435    83.86771        2015 3.073741  6.444270 39.77924
    ##        ozone_summer       ox
    ##     1:     43.72098 32.72640
    ##     2:     43.79662 33.11032
    ##     3:     42.47194 32.34071
    ##     4:     40.41729 28.07815
    ##     5:     40.73180 28.24311
    ##    ---                      
    ## 95579:     42.04486 28.94891
    ## 95580:     43.70882 29.10272
    ## 95581:     46.27007 29.28810
    ## 95582:     44.29974 28.99829
    ## 95583:     43.94474 28.48321

    ## the number of person-years of related subjects is 95583

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

### 3\. check whether all people have each year’s info during follow-up

``` r
sum_temp[(end_yr-firstADRDyr+1) != count,]
```

    ##                    qid start_yr end_yr count firstADRDyr
    ##     1:       037641248     2002   2004     2        2002
    ##     2:       A00003963     2002   2002     1        2000
    ##     3:       A00005533     2001   2002     2        2000
    ##     4:       A00016302     2002   2002     1        2001
    ##     5:       A00024753     2001   2002     2        2000
    ##    ---                                                  
    ## 35163: lllllllooooXoS7     2003   2004     2        2001
    ## 35164: lllllllooool4OO     2006   2006     1        2002
    ## 35165: llllllloooolllX     2004   2005     2        2001
    ## 35166: lllllllooooo70l     2009   2012     4        2008
    ## 35167: lllllllooooolU8     2006   2006     1        2000

``` r
setorder(temp,qid,year)
# summary(temp[qid%in%sum_temp[(end_yr-firstADRDyr+1) != count,qid],][,.SD[.N],by=qid][,dead])
```

``` r
## save qid without complete follow-up or no contribution to risk set
omitInfo <- rbind(sum_temp[(end_yr-firstADRDyr+1) != count, .(qid,end_yr, firstADRDyr)], no_contribution)
fwrite(omitInfo, "/nfs/home/S/shd968/shared_space/ci3_shd968/dementia/data/omitInfo.csv")
```

The above subject (2447164 subjects in total) do not have each year’s
info during follow-up. **Consider deleting them**

Their info in denominator
    files:

    ##           zip year             qid sex race age dual statecode  dead mean_bmi
    ##      1: 43701 2002       037641248   2    1  86    0        OH FALSE 28.78469
    ##      2: 43701 2004       037641248   2    1  88    1        OH  TRUE 28.78469
    ##      3: 34604 2002       A00003963   1    1  80    0        FL  TRUE 26.68151
    ##      4: 03301 2001       A00005533   1    1  76    0        NH FALSE 25.88389
    ##      5: 03301 2002       A00005533   1    1  77    0        NH  TRUE 26.24185
    ##     ---                                                                      
    ## 148936: 42086 2009 lllllllooooo70l   2    1  79    0        KY FALSE 28.66079
    ## 148937: 42002 2010 lllllllooooo70l   2    1  80    0        KY FALSE 27.68462
    ## 148938: 42002 2011 lllllllooooo70l   2    1  81    0        KY FALSE 27.11329
    ## 148939: 42002 2012 lllllllooooo70l   2    1  82    0        KY  TRUE 28.50269
    ## 148940: 40422 2006 lllllllooooolU8   2    1  81    0        KY  TRUE 26.78969
    ##         smoke_rate    hispanic    pct_blk medhouseholdincome medianhousevalue
    ##      1:  0.5166249 0.005524899 0.05695084           32531.84         78819.38
    ##      2:  0.5166249 0.005930144 0.05544395           33036.33         81744.44
    ##      3:  0.5818182 0.135892804 0.05058142           57035.00        155033.33
    ##      4:  0.5364078 0.014848201 0.01170791           41502.41        112147.17
    ##      5:  0.5485612 0.014880203 0.01176940           41566.15        112882.95
    ##     ---                                                                      
    ## 148936:  0.5071770 0.011186195 0.05193199           61628.71        127742.86
    ## 148937:  0.4360465 0.022525678 0.09608502           36515.80         97180.00
    ## 148938:  0.4406780 0.023859601 0.09392132           35999.00         95000.00
    ## 148939:  0.4454976 0.021765169 0.09352863           36688.00         98800.00
    ## 148940:  0.4309484 0.030342730 0.08892803           40559.25        123200.00
    ##            poverty education popdensity pct_owner_occ summer_tmmx winter_tmmx
    ##      1: 0.10821406 0.3684625   310.4379     0.6988227    302.3375    280.1457
    ##      2: 0.10808094 0.3557562   310.4324     0.6938975    299.2050    276.1900
    ##      3: 0.08448096 0.2044711   282.4742     0.8329174    304.8413    295.8388
    ##      4: 0.05586883 0.1927171   642.7494     0.4888090    300.2352    273.4485
    ##      5: 0.05602187 0.1923004   642.9005     0.4889875    300.1612    277.1927
    ##     ---                                                                      
    ## 148936: 0.10470198 0.1736261   111.8252     0.8802034    302.8817    280.6499
    ## 148937: 0.09784028 0.2758157   417.4595     0.6485279    306.1439    277.7010
    ## 148938: 0.10286104 0.2887309   416.6713     0.6493965    306.4575    279.0194
    ## 148939: 0.09847495 0.2658936   419.8032     0.6499919    308.0365    283.6296
    ## 148940: 0.08051955 0.2768526   211.9207     0.6788284    302.4201    281.1118
    ##         summer_rmax winter_rmax firstADRDyr      pm25       no2    ozone
    ##      1:    93.81768    90.19248        2002 13.815444 16.131259 42.50094
    ##      2:    95.57566    88.99893        2002 12.698842 16.832488 38.66580
    ##      3:    99.98691    98.51110        2000  8.935241 15.748415 39.82680
    ##      4:    91.64285    90.94008        2000 10.145499 18.591306 33.53720
    ##      5:    90.42228    94.02909        2000  9.783997 20.571240 35.10930
    ##     ---                                                                 
    ## 148936:    91.27092    81.29837        2008 10.740379  9.210874 38.07277
    ## 148937:    94.62798    88.58525        2008 12.492827 20.976752 41.00387
    ## 148938:    97.15064    87.11779        2008 10.918692 16.381240 41.32823
    ## 148939:    82.26487    89.11086        2008 10.401345 18.152665 38.79238
    ## 148940:    93.93636    91.13973        2000 13.447658 17.115598 41.44548
    ##         ozone_summer       ox
    ##      1:     54.64130 33.58277
    ##      2:     47.58352 31.28736
    ##      3:     41.14792 31.68516
    ##      4:     37.70789 28.49758
    ##      5:     40.74352 30.21116
    ##     ---                      
    ## 148936:     44.10608 28.29829
    ## 148937:     43.37243 34.24463
    ## 148938:     45.65680 32.89299
    ## 148939:     41.79859 31.82087
    ## 148940:     48.34699 33.22072

### 4\. check whether alive people were followed-up till the end of study period (2016) after excluding those without complete follow-up

``` r
sum_temp[!(qid %in% temp[(dead),qid]), ][!(qid %in% omitInfo[,qid]),end_yr] %>% table()
```

    ## .
    ##   2000   2001   2002   2003   2004   2005   2006   2007   2008   2009   2010 
    ##    910    883   1046   1913   2585   2564   2744   1820   1617   1520   1526 
    ##   2011   2012   2013   2014   2015   2016 
    ##   1510   1635   1436   2018   1603 715462

We could see loads of alive subjects weren’t followed-up till 2016. This
should be considered as right-censored subjects in the analyses.
