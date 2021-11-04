Check the completeness of follow-up in the ADRD population
================

The dataset `ADRDcohort.fst` was directly generated from the denominator
files, which follow the ADRD people till their death or the end of 2016.

## Setup and load data

    ##          used (Mb) gc trigger (Mb) max used (Mb)
    ## Ncells 403487 21.6     831501 44.5   642637 34.4
    ## Vcells 774270  6.0    8388608 64.0  1825873 14.0

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

    ## [1] 7276514

Those who admitted for the first time with ADRD code and died in the
same year don’t contribute to the risk set. Delete those

``` r
A <- ADRDcohort[, .(end_yr = max(year)), by = qid]
A <- A[enrolledInfo, on = .(qid=QID)]
no_contribution <- subset(A, end_yr==firstADRDyr)
```

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
5121692 subjects:

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
    ## 1: 003050048     2001   2004     4        2000
    ## 2: 003628694     2002   2005     4        2002
    ## 3: 003749417     2005   2006     2        2005
    ## 4: 004402139     2004   2005     2        2004
    ## 5: 012472193     2001   2004     4        2000
    ## 6: 014929014     2005   2006     2        2005

    ## the number of subjects in ADRD cohort (removed NAs, and non-contribution subjects) is 5121692

    ## the number of person-years in ADRD cohort (removed NAs, and non-contribution subjects) is 22323901

    ## is there any duplication of the combination of `qid` and calendar year:  FALSE

    ## is there any duplication of the combination of `qid` and age:  TRUE

### 2\. check whether all people were followed-up from the year following firstADRDyr

``` r
sum_temp[start_yr != (firstADRDyr)]
```

    ##                     qid start_yr end_yr count firstADRDyr
    ##      1:       003050048     2001   2004     4        2000
    ##      2:       012472193     2001   2004     4        2000
    ##      3:       015128651     2001   2006     6        2000
    ##      4:       029979975     2001   2003     3        2000
    ##      5:       036300873     2001   2005     5        2000
    ##     ---                                                  
    ## 499812: lllllllooooo70l     2009   2012     4        2008
    ## 499813: lllllllooooo70o     2001   2005     5        2000
    ## 499814: lllllllooooolSX     2001   2004     4        2000
    ## 499815: lllllllooooolU8     2006   2006     1        2000
    ## 499816: lllllllooooolXS     2001   2012    12        2000

The above subject (4636381 subjects in total), were not followed-up from
the year following firstADRDyr.

Their info in denominator
    files:

    ##            zip year_prev             qid year sex race age dual statecode  dead
    ##       1: 01001      2000       A00110515 2001   2    1  91    0        MA  TRUE
    ##       2: 01001      2000       A00127897 2001   2    1  91    1        MA  TRUE
    ##       3: 01001      2000       A00137887 2001   2    1  85    0        MA  TRUE
    ##       4: 01001      2000       A00137987 2001   2    1  94    0        MA FALSE
    ##       5: 01001      2000       A00149890 2001   2    1  89    0        MA  TRUE
    ##      ---                                                                       
    ## 1802112: 99403      2009 llllllloXlSUlSS 2010   1    6  87    0        WA FALSE
    ## 1802113: 99403      2010 llllllloXlSUlSS 2011   1    6  88    0        WA FALSE
    ## 1802114: 99403      2011 llllllloXlSUlSS 2012   1    6  89    0        WA FALSE
    ## 1802115: 99403      2012 llllllloXlSUlSS 2013   1    6  89    0        WA  TRUE
    ## 1802116: 99403      2015 llllllloX0llXX4 2016   1    1  75    0        WA  TRUE
    ##          mean_bmi smoke_rate   hispanic     pct_blk medhouseholdincome
    ##       1: 26.89790  0.5155502 0.01687498 0.010627160           45761.74
    ##       2: 26.89790  0.5155502 0.01687498 0.010627160           45761.74
    ##       3: 26.89790  0.5155502 0.01687498 0.010627160           45761.74
    ##       4: 26.89790  0.5155502 0.01687498 0.010627160           45761.74
    ##       5: 26.89790  0.5155502 0.01687498 0.010627160           45761.74
    ##      ---                                                              
    ## 1802112: 27.45471  0.5045455 0.03188513 0.005108372           42003.40
    ## 1802113: 27.76113  0.4623116 0.03088451 0.004657992           41832.00
    ## 1802114: 28.49232  0.5142857 0.03189135 0.005231388           41944.00
    ## 1802115: 28.15183  0.5010794 0.03394085 0.006125420           42813.00
    ## 1802116: 28.24859  0.4969610 0.03814741 0.003137450           44386.00
    ##          medianhousevalue    poverty education popdensity pct_owner_occ
    ##       1:         122090.3 0.05763368 0.2783539  1351.4259     0.7149814
    ##       2:         122090.3 0.05763368 0.2783539  1351.4259     0.7149814
    ##       3:         122090.3 0.05763368 0.2783539  1351.4259     0.7149814
    ##       4:         122090.3 0.05763368 0.2783539  1351.4259     0.7149814
    ##       5:         122090.3 0.05763368 0.2783539  1351.4259     0.7149814
    ##      ---                                                               
    ## 1802112:         168373.3 0.08562909 0.1640987   129.6158     0.6894861
    ## 1802113:         167400.0 0.08572216 0.1685185   129.1871     0.7085169
    ## 1802114:         169200.0 0.08380187 0.1644686   130.0309     0.6706572
    ## 1802115:         171100.0 0.08106721 0.1538653   130.2729     0.6658240
    ## 1802116:         174300.0 0.07679012 0.1090380   131.3391     0.6577767
    ##          summer_tmmx winter_tmmx summer_rmax winter_rmax firstADRDyr      pm25
    ##       1:    301.3939    275.2712    87.79336    88.40830        2000 11.669156
    ##       2:    301.3939    275.2712    87.79336    88.40830        2000 11.669156
    ##       3:    301.3939    275.2712    87.79336    88.40830        2000 11.669156
    ##       4:    301.3939    275.2712    87.79336    88.40830        2000 11.669156
    ##       5:    301.3939    275.2712    87.79336    88.40830        2000 11.669156
    ##      ---                                                                      
    ## 1802112:    297.7093    277.9926    60.52847    82.76904        2006  4.199544
    ## 1802113:    298.0834    276.8914    70.07012    87.93301        2006  4.240930
    ## 1802114:    300.1234    278.0944    68.33219    86.07601        2006  5.313538
    ## 1802115:    300.9082    276.8044    60.85726    88.92360        2006  6.048468
    ## 1802116:    299.1663    279.0391    63.34435    83.86771        2015  6.059223
    ##                no2    ozone ozone_summer       ox
    ##       1: 26.183961 34.73918     39.39532 31.87918
    ##       2: 26.183961 34.73918     39.39532 31.87918
    ##       3: 26.183961 34.73918     39.39532 31.87918
    ##       4: 26.183961 34.73918     39.39532 31.87918
    ##       5: 26.183961 34.73918     39.39532 31.87918
    ##      ---                                         
    ## 1802112: 10.943100 40.35688     45.34238 30.39797
    ## 1802113: 10.042199 38.62864     42.04486 28.94891
    ## 1802114:  8.325685 39.74654     43.70882 29.10272
    ## 1802115:  6.782057 40.82305     46.27007 29.28810
    ## 1802116: 10.299380 42.24531     48.01473 31.42654

    ## the number of person-years of related subjects is 1802116

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

    ##                     qid start_yr end_yr count firstADRDyr
    ##      1:       003050048     2001   2004     4        2000
    ##      2:       012472193     2001   2004     4        2000
    ##      3:       015128651     2001   2006     6        2000
    ##      4:       029979975     2001   2003     3        2000
    ##      5:       036300873     2001   2005     5        2000
    ##     ---                                                  
    ## 508020: lllllllooooo70l     2009   2012     4        2008
    ## 508021: lllllllooooo70o     2001   2005     5        2000
    ## 508022: lllllllooooolSX     2001   2004     4        2000
    ## 508023: lllllllooooolU8     2006   2006     1        2000
    ## 508024: lllllllooooolXS     2001   2012    12        2000

``` r
setorder(temp,qid,year)
# summary(temp[qid%in%sum_temp[(end_yr-firstADRDyr+1) != count,qid],][,.SD[.N],by=qid][,dead])
```

``` r
## save qid without complete follow-up or no contribution to risk set
omitInfo <- rbind(sum_temp[(end_yr-firstADRDyr+1) != count, .(qid,end_yr, firstADRDyr)], no_contribution)
fwrite(omitInfo, "/nfs/home/S/shd968/shared_space/ci3_shd968/dementia/data/omitInfo.csv")
```

The above subject (2920021 subjects in total) do not have each year’s
info during follow-up. **Consider deleting them**

Their info in denominator
    files:

    ##            zip year_prev             qid year sex race age dual statecode  dead
    ##       1: 07017      2000       003050048 2001   2    2  79    1        NJ FALSE
    ##       2: 07017      2001       003050048 2002   2    2  80    1        NJ FALSE
    ##       3: 07017      2002       003050048 2003   2    2  81    1        NJ FALSE
    ##       4: 07017      2003       003050048 2004   2    2  82    1        NJ  TRUE
    ##       5: 77327      2000       012472193 2001   2    1  85    1        TX FALSE
    ##      ---                                                                       
    ## 1850756: 44907      2007 lllllllooooolXS 2008   1    1  80    1        OH FALSE
    ## 1850757: 44907      2008 lllllllooooolXS 2009   1    1  81    1        OH FALSE
    ## 1850758: 44907      2009 lllllllooooolXS 2010   1    1  82    1        OH FALSE
    ## 1850759: 43031      2010 lllllllooooolXS 2011   1    1  83    1        OH FALSE
    ## 1850760: 43031      2011 lllllllooooolXS 2012   1    1  84    1        OH  TRUE
    ##          mean_bmi smoke_rate    hispanic     pct_blk medhouseholdincome
    ##       1: 27.24894  0.4459930 0.051962709 0.883559343           33433.42
    ##       2: 26.19451  0.4215457 0.052246328 0.883450206           33479.32
    ##       3: 26.78496  0.4287440 0.053348578 0.883026058           33657.70
    ##       4: 27.42488  0.4233410 0.057298306 0.881506193           34296.89
    ##       5: 28.07143  0.5107527 0.120779107 0.087200304           35894.06
    ##      ---                                                               
    ## 1850756: 27.31835  0.5625000 0.007510488 0.098069094           35706.67
    ## 1850757: 27.32350  0.5000000 0.007290469 0.099607633           35880.57
    ## 1850758: 29.26256  0.5058824 0.007291936 0.099485684           35970.27
    ## 1850759: 27.69381  0.4619048 0.022058207 0.006458106           71121.00
    ## 1850760: 27.57949  0.4816754 0.028479445 0.006603929           72119.00
    ##          medianhousevalue    poverty education  popdensity pct_owner_occ
    ##       1:        127453.41 0.12171030 0.4739187 16736.98094     0.2839382
    ##       2:        128207.75 0.12188140 0.4731641 16730.34558     0.2839315
    ##       3:        131139.39 0.12254639 0.4702314 16704.55818     0.2839051
    ##       4:        141644.44 0.12492925 0.4597227 16612.15333     0.2838107
    ##       5:         53851.46 0.15785047 0.4623461    58.51964     0.7958126
    ##      ---                                                                
    ## 1850756:         94066.67 0.06803827 0.2584333  1901.53067     0.6062020
    ## 1850757:         93742.86 0.06991257 0.2553666  1897.21343     0.6073896
    ## 1850758:         93426.67 0.07140519 0.2511322  1893.66300     0.6080344
    ## 1850759:        192000.00 0.03815261 0.1926606   133.82100     0.8445727
    ## 1850760:        185200.00 0.02503129 0.1546961   135.96480     0.8536642
    ##          summer_tmmx winter_tmmx summer_rmax winter_rmax firstADRDyr     pm25
    ##       1:    302.3111    277.3330    87.94717    81.19625        2000 15.28591
    ##       2:    303.0856    282.2298    84.72578    79.19652        2000 14.22669
    ##       3:    301.1065    275.7024    91.76800    77.11076        2000 13.49739
    ##       4:    300.9977    276.9084    90.33487    72.49658        2000 15.52122
    ##       5:    306.2512    288.8942    99.55182    92.00179        2000 11.50066
    ##      ---                                                                     
    ## 1850756:    299.9805    274.8892    85.53203    90.50627        2000 13.67321
    ## 1850757:    298.7652    273.8026    90.26439    94.50513        2000 11.23732
    ## 1850758:    301.0449    272.6778    87.79951    90.86286        2000 11.31354
    ## 1850759:    302.1135    273.5917    96.21930    92.53259        2000 12.39964
    ## 1850760:    303.7150    278.7491    87.26368    90.17001        2000 12.25752
    ##               no2    ozone ozone_summer       ox
    ##       1: 44.39516 31.67699     40.99779 36.06133
    ##       2: 45.83957 34.36022     46.48215 38.32669
    ##       3: 44.00760 33.02408     44.15771 36.81946
    ##       4: 45.49518 30.68789     40.23328 35.78255
    ##       5: 16.53659 39.78638     46.79837 31.92703
    ##      ---                                        
    ## 1850756: 17.80304 40.49826     50.25581 32.82903
    ## 1850757: 15.71246 39.37103     48.60567 31.37173
    ## 1850758: 15.23600 37.96943     45.41263 30.28315
    ## 1850759: 13.20786 42.58013     50.88687 32.63891
    ## 1850760: 12.44414 40.70178     48.20580 31.13739

### 4\. check whether alive people were followed-up till the end of study period (2016) after excluding those without complete follow-up

``` r
sum_temp[!(qid %in% temp[(dead),qid]), ][!(qid %in% omitInfo[,qid]),end_yr] %>% table()
```

    ## .
    ##   2001   2002   2003   2004   2005   2006   2007   2008   2009   2010   2011 
    ##    653    861   1572   2440   2588   2778   1891   1589   1453   1543   1493 
    ##   2012   2013   2014   2015   2016 
    ##   1549   1400   1966   1631 712312

We could see loads of alive subjects weren’t followed-up till 2016. This
should be considered as right-censored subjects in the analyses.
