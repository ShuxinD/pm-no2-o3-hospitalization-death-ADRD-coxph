Check the completeness of follow-up in the ADRD population
==========================================================

The dataset `ADRDpeople.csv` was directly generated from the denominator
files.

Setup and read in data
----------------------

    ##          used (Mb) gc trigger (Mb) max used (Mb)
    ## Ncells 454688 24.3     977790 52.3   642637 34.4
    ## Vcells 885264  6.8    8388608 64.0  1825873 14.0

    ## Warning: As of rlang 0.4.0, dplyr must be at least version 0.8.0.
    ## * dplyr 0.7.6 is too old for rlang 0.4.6.
    ## * Please update dplyr with `install.packages("dplyr")` and restart R.

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

Check the completeness of follow-up
-----------------------------------

Our final dataset should:

-   be one row per person-year
-   have follow-up info for every year after the year they were
    hospitalized with ADRD for the first time (`firstADRDyr`) till the
    end of study period (2016) or death, whichever comes first.

### 1. basic info of original ADRD dataset

``` r
temp <- na.omit(ADRDpeople)
temp <- temp[year!=firstADRDyr, ][, .(start_yr = min(year),
                                           end_yr = max(year),
                                           count = uniqueN(year)), by = .(qid)]
temp <- merge(temp, enrolledInfo, by.x = "qid", by.y = "QID", all.x = TRUE)
```

We constructed a temporary dataset named `temp` which is a subset of
`ADRDpeople` after removing NA and removing rows whose
`year==firstADRDyr`, and summarize each person as one row (the number of
unique subjects) with 5154447 subjects: - generated `start_yr` as the
minimum of calendar year - `end_yr` as the maximum of calendar year -
`count` as the count number of unique calendar year for each subject.

We also merged the enroll information (`firstADRDyear`) to `temp`.
(`firstADRDyr`+1) indicates the year that subjects should start to be
followed-up.

`temp` is a one-row-per-person dataset, looks like:

    ##          qid start_yr end_yr count firstADRDyr
    ## 1: 003050048     2001   2004     4        2000
    ## 2: 003628694     2003   2005     3        2002
    ## 3: 003749417     2006   2006     1        2005
    ## 4: 004402139     2005   2005     1        2004
    ## 5: 012472193     2001   2004     4        2000
    ## 6: 014929014     2006   2006     1        2005

    ## the number of subjects in ADRDpeople(removed NAs) is 5154447

    ## the number of person-years in ADRDpeople(removed NAs) is 27049496

    ## is there any duplication of the combination of `qid` and calendar year:  FALSE

### 2. check whether all people were followed-up from the year following firstADRDyr

``` r
temp[start_yr != (firstADRDyr+1)]
```

    ##                    qid start_yr end_yr count firstADRDyr
    ##     1:       037641248     2004   2004     1        2002
    ##     2:       A00000760     2002   2002     1        2000
    ##     3:       A00003963     2002   2002     1        2000
    ##     4:       A00054041     2002   2002     1        2000
    ##     5:       A00054463     2002   2002     1        2000
    ##    ---                                                  
    ## 64526: llllllloooolU7l     2011   2012     2        2009
    ## 64527: llllllloooolXl4     2007   2011     5        2005
    ## 64528: llllllloooolllX     2004   2005     2        2001
    ## 64529: lllllllooooolO0     2015   2016     2        2007
    ## 64530: lllllllooooolU8     2006   2006     1        2000

The above subject (64530 subjects in total), were not followed-up from
the year following firstADRDyr. **Consider deleting them**

Their info in denominator files:

    ##           zip year             qid summer_tmmx winter_tmmx summer_rmax
    ##      1:     0 2000       A07897374          NA          NA          NA
    ##      2:     0 2000       A07997964          NA          NA          NA
    ##      3:     0 2000       A08004102          NA          NA          NA
    ##      4:     0 2000 llllllloOll0llO          NA          NA          NA
    ##      5:     0 2000 llllllloo4lXU4X          NA          NA          NA
    ##     ---                                                               
    ## 447506: 99999 2016 llllllll877S4l4          NA          NA          NA
    ## 447507: 99999 2016 llllllllOUl8o4O          NA          NA          NA
    ## 447508: 99999 2016 lllllllo44OXUUl          NA          NA          NA
    ## 447509: 99999 2016 lllllllo788o7O8          NA          NA          NA
    ## 447510: 99999 2016 lllllllol47484O          NA          NA          NA
    ##         winter_rmax  dead sex race age dual statecode entry_age_break
    ##      1:          NA FALSE   1    2  79    1                         3
    ##      2:          NA FALSE   2    0  96    0                         7
    ##      3:          NA FALSE   2    1  85    0                         4
    ##      4:          NA FALSE   1    1  76    0                         3
    ##      5:          NA FALSE   2    1  85    0                         4
    ##     ---                                                              
    ## 447506:          NA FALSE   1    5  78    0                         1
    ## 447507:          NA FALSE   2    5  94    0                         3
    ## 447508:          NA FALSE   2    1  94    0                         3
    ## 447509:          NA FALSE   1    1  74    0                         1
    ## 447510:          NA FALSE   2    1  98    0                         4
    ##         mean_bmi smoke_rate hispanic pct_blk medhouseholdincome
    ##      1:       NA         NA       NA      NA                 NA
    ##      2:       NA         NA       NA      NA                 NA
    ##      3:       NA         NA       NA      NA                 NA
    ##      4:       NA         NA       NA      NA                 NA
    ##      5:       NA         NA       NA      NA                 NA
    ##     ---                                                        
    ## 447506:       NA         NA       NA      NA                 NA
    ## 447507:       NA         NA       NA      NA                 NA
    ## 447508:       NA         NA       NA      NA                 NA
    ## 447509:       NA         NA       NA      NA                 NA
    ## 447510:       NA         NA       NA      NA                 NA
    ##         medianhousevalue poverty education popdensity pct_owner_occ
    ##      1:               NA      NA        NA         NA            NA
    ##      2:               NA      NA        NA         NA            NA
    ##      3:               NA      NA        NA         NA            NA
    ##      4:               NA      NA        NA         NA            NA
    ##      5:               NA      NA        NA         NA            NA
    ##     ---                                                            
    ## 447506:               NA      NA        NA         NA            NA
    ## 447507:               NA      NA        NA         NA            NA
    ## 447508:               NA      NA        NA         NA            NA
    ## 447509:               NA      NA        NA         NA            NA
    ## 447510:               NA      NA        NA         NA            NA
    ##         firstADRDyr pm25 no2 ozone
    ##      1:        2000   NA  NA    NA
    ##      2:        2000   NA  NA    NA
    ##      3:        2000   NA  NA    NA
    ##      4:        2000   NA  NA    NA
    ##      5:        2000   NA  NA    NA
    ##     ---                           
    ## 447506:        2007   NA  NA    NA
    ## 447507:        2011   NA  NA    NA
    ## 447508:        2010   NA  NA    NA
    ## 447509:        2007   NA  NA    NA
    ## 447510:        2002   NA  NA    NA

    ## the number of person-years of related subjects is 447510

### 3. check whether alive people were followed-up till the end of study period (2016)

``` r
temp[!(qid %in% ADRDpeople[(dead),qid]), end_yr] %>% table()
```

    ## .
    ##   2001   2002   2003   2004   2005   2006   2007   2008   2009   2010 
    ##     47     72    679   1027   1053   1034    857    971    780   1011 
    ##   2011   2012   2013   2014   2015   2016 
    ##   1379   2066   2636   3993   4915 890402

We could see loads of alive subjects weren’t followed-up till 2016. This
should be considered as right-censored subjects during the analyses.

### 4. check whether all people have each year’s info during follow-up

``` r
temp[(end_yr-start_yr+1) != count,]
```

    ##                    qid start_yr end_yr count firstADRDyr
    ##     1: llllll0l0U44O44     2012   2016     4        2011
    ##     2: llllll0l0XUUXXU     2012   2016     4        2011
    ##     3: llllll0ll744l7O     2013   2016     3        2012
    ##     4: llllll0ll7O848o     2012   2016     4        2011
    ##     5: llllll0ll7oSSU7     2012   2016     4        2011
    ##    ---                                                  
    ## 13665: llllllloooX8U40     2006   2016     9        2005
    ## 13666: llllllloooXOlXX     2004   2012     8        2003
    ## 13667: llllllloooXo774     2001   2004     3        2000
    ## 13668: llllllloooo4O8X     2006   2008     2        2005
    ## 13669: llllllloooooll4     2002   2011     5        2001

The above subject (13669 subjects in total) do not have each year’s info
during follow-up. **Consider deleting them**

Their info in denominator files:

    ##           zip year             qid summer_tmmx winter_tmmx summer_rmax
    ##      1:     0 2002 llllllll0OXo77l          NA          NA          NA
    ##      2:     0 2002 llllllloo870S4O          NA          NA          NA
    ##      3:     0 2002 llllllloo8o4lUo          NA          NA          NA
    ##      4:     0 2002 lllllllooSOXXoo          NA          NA          NA
    ##      5:     0 2002 lllllllooUOXSSo          NA          NA          NA
    ##     ---                                                               
    ## 119843: 99999 2016 llllllll4SS7oXl          NA          NA          NA
    ## 119844: 99999 2016 llllllllX7X8040          NA          NA          NA
    ## 119845: 99999 2016 lllllllloXSSol0          NA          NA          NA
    ## 119846: 99999 2016 lllllllo4Sol0X7          NA          NA          NA
    ## 119847: 99999 2016 lllllllo708l87U          NA          NA          NA
    ##         winter_rmax  dead sex race age dual statecode entry_age_break
    ##      1:          NA FALSE   2    1  85    0                         4
    ##      2:          NA FALSE   2    1  98    1                         7
    ##      3:          NA FALSE   2    1  98    1                         7
    ##      4:          NA FALSE   2    1  87    0                         4
    ##      5:          NA FALSE   2    1  83    0                         4
    ##     ---                                                              
    ## 119843:          NA FALSE   2    2  76    0                         1
    ## 119844:          NA FALSE   2    1  89    0                         2
    ## 119845:          NA FALSE   2    1  78    0                         1
    ## 119846:          NA FALSE   2    4  83    0                         1
    ## 119847:          NA FALSE   1    1  90    1                         3
    ##         mean_bmi smoke_rate hispanic pct_blk medhouseholdincome
    ##      1:       NA         NA       NA      NA                 NA
    ##      2:       NA         NA       NA      NA                 NA
    ##      3:       NA         NA       NA      NA                 NA
    ##      4:       NA         NA       NA      NA                 NA
    ##      5:       NA         NA       NA      NA                 NA
    ##     ---                                                        
    ## 119843:       NA         NA       NA      NA                 NA
    ## 119844:       NA         NA       NA      NA                 NA
    ## 119845:       NA         NA       NA      NA                 NA
    ## 119846:       NA         NA       NA      NA                 NA
    ## 119847:       NA         NA       NA      NA                 NA
    ##         medianhousevalue poverty education popdensity pct_owner_occ
    ##      1:               NA      NA        NA         NA            NA
    ##      2:               NA      NA        NA         NA            NA
    ##      3:               NA      NA        NA         NA            NA
    ##      4:               NA      NA        NA         NA            NA
    ##      5:               NA      NA        NA         NA            NA
    ##     ---                                                            
    ## 119843:               NA      NA        NA         NA            NA
    ## 119844:               NA      NA        NA         NA            NA
    ## 119845:               NA      NA        NA         NA            NA
    ## 119846:               NA      NA        NA         NA            NA
    ## 119847:               NA      NA        NA         NA            NA
    ##         firstADRDyr pm25 no2 ozone
    ##      1:        2000   NA  NA    NA
    ##      2:        2000   NA  NA    NA
    ##      3:        2000   NA  NA    NA
    ##      4:        2000   NA  NA    NA
    ##      5:        2000   NA  NA    NA
    ##     ---                           
    ## 119843:        2010   NA  NA    NA
    ## 119844:        2006   NA  NA    NA
    ## 119845:        2006   NA  NA    NA
    ## 119846:        2010   NA  NA    NA
    ## 119847:        2006   NA  NA    NA
