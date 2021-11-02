Check the completeness of follow-up in the ADRD population
================

The dataset `ADRDpeople.csv` was directly generated from the denominator
files, which follow the ADRD people till their death or the end of 2016.

## Setup and load data

    ##          used (Mb) gc trigger (Mb) max used (Mb)
    ## Ncells 403454 21.6     831407 44.5   642637 34.4
    ## Vcells 774057  6.0    8388608 64.0  1825873 14.0

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
  - have follow-up info for every year after the year they were
    hospitalized with ADRD for the first time (`firstADRDyr`) till the
    end of study period (2016) or death or leaving the US, whichever
    comes first.

### 1\. basic info of original ADRD dataset

``` r
# number of subjects in raw cohort
uniqueN(ADRDcohort[,qid])
```

    ## [1] 5226549

``` r
# number of subjects in cohorts after removing NAs
uniqueN(na.omit(ADRDcohort)[,qid])
```

    ## [1] 5114720

``` r
temp <- na.omit(ADRDcohort) # remove all the NAs
sum_temp <- temp[, .(start_yr = min(year),
                     end_yr = max(year),
                     count = uniqueN(year)), by = qid]
sum_temp <- merge(sum_temp, enrolledInfo, by.x = "qid", by.y = "QID", all.x = TRUE)
```

We constructed a temporary dataset named `temp` which is a subset of
`ADRDcohort` after removing NA (natually had removed rows whose
`year==firstADRDyr` when merging with exposures), and summarize each
person as one row with 5114720 subjects: - generated `start_yr` as the
minimum of calendar year - `end_yr` as the maximum of calendar year -
`count` as the count number of unique calendar year for each subject.

We also merged the enroll information (`firstADRDyear`) to `temp`.
(`firstADRDyr`+1) indicates the year that subjects should start to be
followed-up.

`sum_temp` is a one-row-per-person dataset, looks like:

    ##          qid start_yr end_yr count firstADRDyr
    ## 1: 003050048     2001   2004     4        2000
    ## 2: 003628694     2003   2005     3        2002
    ## 3: 003749417     2006   2006     1        2005
    ## 4: 004402139     2005   2005     1        2004
    ## 5: 012472193     2001   2004     4        2000
    ## 6: 014929014     2006   2006     1        2005

    ## the number of subjects in ADRDpeople(removed NAs) is 5114720

    ## the number of person-years in ADRDpeople(removed NAs) is 17702025

    ## is there any duplication of the combination of `qid` and calendar year:  FALSE

    ## is there any duplication of the combination of `qid` and age:  TRUE

### 2\. check whether all people were followed-up from the year following firstADRDyr

``` r
sum_temp[start_yr != (firstADRDyr+1)]
```

    ##                    qid start_yr end_yr count firstADRDyr
    ##     1:       037641248     2004   2004     1        2002
    ##     2:       A00109981     2002   2002     1        2000
    ##     3:       A00536277     2002   2002     1        2000
    ##     4:       A00539663     2002   2002     1        2000
    ##     5:       A00977147     2002   2002     1        2000
    ##    ---                                                  
    ## 17993: lllllllooooO7Xo     2007   2007     1        2005
    ## 17994: lllllllooooXoS7     2003   2004     2        2001
    ## 17995: lllllllooool4OO     2006   2006     1        2002
    ## 17996: llllllloooolllX     2004   2005     2        2001
    ## 17997: lllllllooooolU8     2006   2006     1        2000

The above subject (17997 subjects in total), were not followed-up from
the year following firstADRDyr.

Their info in denominator
    files:

    ##          zip year_prev             qid year sex race age dual statecode  dead
    ##     1: 01002      2015 llllllll874So4o 2016   2    2  87    0        MA FALSE
    ##     2: 01010      2009 lllllll0l4784Sl 2010   1    1  89    0        MA  TRUE
    ##     3: 01013      2008 llllllloX8oS77U 2009   1    1  74    1        MA FALSE
    ##     4: 01013      2010 llllllloX8U8S0U 2011   2    5  86    1        MA FALSE
    ##     5: 01013      2011 llllllloX8oS77U 2012   1    1  77    1        MA FALSE
    ##    ---                                                                       
    ## 64208: 99403      2008 llllllloXlSUlSS 2009   1    6  86    0        WA FALSE
    ## 64209: 99403      2009 llllllloXlSUlSS 2010   1    6  87    0        WA FALSE
    ## 64210: 99403      2010 llllllloXlSUlSS 2011   1    6  88    0        WA FALSE
    ## 64211: 99403      2011 llllllloXlSUlSS 2012   1    6  89    0        WA FALSE
    ## 64212: 99403      2012 llllllloXlSUlSS 2013   1    6  89    0        WA  TRUE
    ##        mean_bmi smoke_rate    hispanic     pct_blk medhouseholdincome
    ##     1: 26.57164  0.4893738 0.062395729 0.057090424           48923.00
    ##     2: 27.96209  0.5042735 0.004006434 0.000000000           79424.07
    ##     3: 28.08690  0.5087948 0.222230494 0.040043412           40357.86
    ##     4: 28.10089  0.4912193 0.218793265 0.037917020           39502.00
    ##     5: 28.30406  0.4815401 0.231234534 0.043854825           41612.00
    ##    ---                                                               
    ## 64208: 28.38485  0.5435685 0.031608800 0.005031452           42004.14
    ## 64209: 27.45471  0.5045455 0.031885133 0.005108372           42003.40
    ## 64210: 27.76113  0.4623116 0.030884512 0.004657992           41832.00
    ## 64211: 28.49232  0.5142857 0.031891348 0.005231388           41944.00
    ## 64212: 28.15183  0.5010794 0.033940855 0.006125420           42813.00
    ##        medianhousevalue    poverty  education popdensity pct_owner_occ
    ##     1:         344000.0 0.06653809 0.05760297   544.5000     0.4671060
    ##     2:         261040.0 0.01491774 0.17235426   103.6826     0.8239247
    ##     3:         181742.9 0.13227870 0.39036310  3945.1100     0.4755766
    ##     4:         182600.0 0.13054341 0.39090093  3981.0760     0.4860470
    ##     5:         181000.0 0.13465347 0.40379147  3879.9090     0.4689500
    ##    ---                                                                
    ## 64208:         168442.9 0.08450852 0.16526808   129.5833     0.6916009
    ## 64209:         168373.3 0.08562909 0.16409869   129.6158     0.6894861
    ## 64210:         167400.0 0.08572216 0.16851852   129.1871     0.7085169
    ## 64211:         169200.0 0.08380187 0.16446858   130.0309     0.6706572
    ## 64212:         171100.0 0.08106721 0.15386534   130.2729     0.6658240
    ##        summer_tmmx winter_tmmx summer_rmax winter_rmax firstADRDyr     pm25
    ##     1:    301.2432    278.2013    91.27892    83.43145        2014 4.516610
    ##     2:    299.9531    274.1636    86.12584    80.28111        2008 6.147212
    ##     3:    299.1525    275.3920    91.21321    88.06722        2002 9.780730
    ##     4:    301.5910    274.7779    91.83898    86.23466        2007 7.819747
    ##     5:    301.7805    279.3821    88.93819    77.39214        2002 9.456769
    ##    ---                                                                     
    ## 64208:    299.2287    277.0668    63.42863    88.93414        2006 3.714551
    ## 64209:    297.7093    277.9926    60.52847    82.76904        2006 4.199544
    ## 64210:    298.0834    276.8914    70.07012    87.93301        2006 4.240930
    ## 64211:    300.1234    278.0944    68.33219    86.07601        2006 5.313538
    ## 64212:    300.9082    276.8044    60.85726    88.92360        2006 6.048468
    ##              no2    ozone ozone_summer
    ##     1:  9.261477 39.83256     43.05488
    ##     2: 10.216422 31.94758     41.27915
    ##     3: 23.913036 36.71614     44.38763
    ##     4: 20.711534 39.27092     44.49197
    ##     5: 24.299117 38.36417     42.16886
    ##    ---                                
    ## 64208: 12.935805 39.60611     43.98285
    ## 64209: 10.943100 40.35688     45.34238
    ## 64210: 10.042199 38.62864     42.04486
    ## 64211:  8.325685 39.74654     43.70882
    ## 64212:  6.782057 40.82305     46.27007

    ## the number of person-years of related subjects is 64212

``` r
# dead event
summary(temp[qid %in% sum_temp[start_yr != (firstADRDyr+1),qid],.SD[.N],by=qid][,dead]) # almost the same as the general population
```

    ##    Mode   FALSE    TRUE 
    ## logical    4285   13712

<!-- #### fix -->

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

### 3\. check whether alive people were followed-up till the end of study period (2016)

``` r
sum_temp[!(qid %in% temp[(dead),qid]), end_yr] %>% table()
```

    ## .
    ##   2001   2002   2003   2004   2005   2006   2007   2008   2009   2010   2011 
    ##    514    608   1524   2251   2352   2466   1621   1447   1141   1224   1237 
    ##   2012   2013   2014   2015   2016 
    ##   1432   1311   1868   1252 720779

We could see loads of alive subjects weren’t followed-up till 2016. This
should be considered as right-censored subjects in the analyses.

### 4\. check whether all people have each year’s info during follow-up

``` r
sum_temp[(end_yr-firstADRDyr) != count,]
```

    ##                    qid start_yr end_yr count firstADRDyr
    ##     1:       037641248     2004   2004     1        2002
    ##     2:       A00109981     2002   2002     1        2000
    ##     3:       A00536277     2002   2002     1        2000
    ##     4:       A00539663     2002   2002     1        2000
    ##     5:       A00977147     2002   2002     1        2000
    ##    ---                                                  
    ## 23777: lllllllooooO7Xo     2007   2007     1        2005
    ## 23778: lllllllooooXoS7     2003   2004     2        2001
    ## 23779: lllllllooool4OO     2006   2006     1        2002
    ## 23780: llllllloooolllX     2004   2005     2        2001
    ## 23781: lllllllooooolU8     2006   2006     1        2000

``` r
setorder(temp,qid,year)
summary(temp[qid%in%sum_temp[(end_yr-firstADRDyr) != count,qid],][,.SD[.N],by=qid][,dead])
```

    ##    Mode   FALSE    TRUE 
    ## logical    4347   19434

``` r
## save qid without complete follow-up
omitInfo <- sum_temp[(end_yr-firstADRDyr) != count,]
write.csv(omitInfo, "/nfs/home/S/shd968/shared_space/ci3_shd968/dementia/data/omitInfo.csv")
```

The above subject (23781 subjects in total) do not have each year’s info
during follow-up. **Consider deleting them**

Their info in denominator
    files:

    ##          zip year_prev             qid year sex race age dual statecode  dead
    ##     1: 14051      2011 llllllU78SUO7oX 2012   2    1  66    0        NY FALSE
    ##     2: 14113      2015 llllllU78SUO7oX 2016   2    1  70    0        NY FALSE
    ##     3: 38666      2011 llllllU7S700ol0 2012   1    2  67    1        MS FALSE
    ##     4: 38666      2012 llllllU7S700ol0 2013   1    2  68    1        MS FALSE
    ##     5: 38666      2013 llllllU7S700ol0 2014   1    2  69    1        MS FALSE
    ##    ---                                                                       
    ## 34556: 48192      2004 llllllloooXUU7O 2005   1    1  75    0        MI FALSE
    ## 34557: 48193      2006 llllllloooXUU7O 2007   1    1  77    0        MI  TRUE
    ## 34558: 54703      2000 llllllloooXo774 2001   2    1  83    0        WI FALSE
    ## 34559: 54703      2001 llllllloooXo774 2002   2    1  84    0        WI FALSE
    ## 34560: 54703      2003 llllllloooXo774 2004   2    1  86    0        WI  TRUE
    ##        mean_bmi smoke_rate    hispanic     pct_blk medhouseholdincome
    ##     1: 27.59694  0.5248619 0.017493147 0.015449788          103592.00
    ##     2: 27.86503  0.5166667 0.017857143 0.000000000           45714.00
    ##     3: 27.95404  0.4774775 0.054616385 0.599869961           33229.00
    ##     4: 28.26844  0.4647242 0.006181925 0.620841919           29316.00
    ##     5: 28.27037  0.4663399 0.030132641 0.586505190           30998.00
    ##    ---                                                               
    ## 34556: 27.90004  0.4943486 0.035408238 0.016631583           47019.67
    ## 34557: 28.88342  0.4657702 0.051970007 0.038893404           50651.00
    ## 34558: 26.67950  0.5925926 0.009397801 0.006132204           38079.05
    ## 34559: 26.67208  0.5263158 0.009433991 0.006211056           38111.95
    ## 34560: 26.95571  0.4000000 0.010078616 0.007615602           38697.89
    ##        medianhousevalue    poverty  education popdensity pct_owner_occ
    ##     1:        239500.00 0.03320000 0.10440000 1228.24900     0.8907777
    ##     2:        140500.00 0.09375000 0.05208333   32.90134     0.7824427
    ##     3:         67000.00 0.14145031 0.36199484   56.48962     0.6877076
    ##     4:         66900.00 0.17355372 0.34400000   49.90773     0.6809141
    ##     5:         67800.00 0.17015926 0.34331984   50.95084     0.6731485
    ##    ---                                                                
    ## 34556:        112000.00 0.06255710 0.37755102 3989.21867     0.7213161
    ## 34557:        134666.67 0.05834700 0.27882257 2237.05633     0.6668248
    ## 34558:         88583.24 0.07989014 0.26487195  515.72245     0.6356412
    ## 34559:         88831.01 0.07980558 0.26435552  515.79792     0.6356812
    ## 34560:         93244.44 0.07829918 0.25515653  517.14213     0.6363927
    ##        summer_tmmx winter_tmmx summer_rmax winter_rmax firstADRDyr      pm25
    ##     1:    300.1124    277.6702    89.59876    88.30940        2011  8.177497
    ##     2:    299.2894    276.0806    85.67926    88.31711        2011  9.124506
    ##     3:    309.9128    288.1695    80.80557    89.99661        2011 10.740772
    ##     4:    307.0157    287.3972    95.32682    88.48691        2011  9.802320
    ##     5:    303.3466    283.1995    83.60630    82.37091        2011  8.357090
    ##    ---                                                                      
    ## 34556:    302.1786    274.1335    87.55107    86.89602        2001 13.787623
    ## 34557:    300.2819    275.1169    81.82950    86.33153        2001 13.217335
    ## 34558:    300.3420    267.3696    87.24791    90.47155        2000 10.718763
    ## 34559:    300.4331    274.2678    89.17513    92.70893        2000 10.499985
    ## 34560:    297.5410    270.2341    88.19340    86.28922        2000 10.101603
    ##              no2    ozone ozone_summer
    ##     1: 17.419759 38.76164     42.90785
    ##     2: 13.927334 40.32836     44.46669
    ##     3:  8.314814 41.24729     45.61554
    ##     4:  9.004085 40.82845     45.62806
    ##     5:  5.590701 36.85643     40.60377
    ##    ---                                
    ## 34556: 31.638605 32.50471     38.14903
    ## 34557: 28.787421 31.01116     39.54351
    ## 34558: 20.423165 36.99470     41.55280
    ## 34559: 20.765969 37.71103     43.50158
    ## 34560: 16.136598 37.54296     44.87236
