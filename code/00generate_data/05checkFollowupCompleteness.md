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
unique subjects) with 5162518 subjects: - generated `start_yr` as the
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

    ## the number of subjects in ADRDpeople(removed NAs) is 5162518

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
    ## 65286: llllllloooolU7l     2011   2012     2        2009
    ## 65287: llllllloooolXl4     2007   2011     5        2005
    ## 65288: llllllloooolllX     2004   2005     2        2001
    ## 65289: lllllllooooolO0     2015   2016     2        2007
    ## 65290: lllllllooooolU8     2006   2006     1        2000

The above subject (65290 subjects in total), were not followed-up from
the year following firstADRDyr. **Consider deleting them**

Their info in denominator files:

    ##           zip year             qid  dead sex race age dual statecode
    ##      1:     0 2000       A07897374 FALSE   1    2  79    1          
    ##      2:     0 2000       A07997964 FALSE   2    0  96    0          
    ##      3:     0 2000       A08004102 FALSE   2    1  85    0          
    ##      4:     0 2000 llllllloOll0llO FALSE   1    1  76    0          
    ##      5:     0 2000 llllllloo4lXU4X FALSE   2    1  85    0          
    ##     ---                                                             
    ## 452369: 99999 2016 llllllll877S4l4 FALSE   1    5  78    0          
    ## 452370: 99999 2016 llllllllOUl8o4O FALSE   2    5  94    0          
    ## 452371: 99999 2016 lllllllo44OXUUl FALSE   2    1  94    0          
    ## 452372: 99999 2016 lllllllo788o7O8 FALSE   1    1  74    0          
    ## 452373: 99999 2016 lllllllol47484O FALSE   2    1  98    0          
    ##         entry_age_break mean_bmi smoke_rate hispanic pct_blk
    ##      1:               3       NA         NA       NA      NA
    ##      2:               7       NA         NA       NA      NA
    ##      3:               4       NA         NA       NA      NA
    ##      4:               3       NA         NA       NA      NA
    ##      5:               4       NA         NA       NA      NA
    ##     ---                                                     
    ## 452369:               1       NA         NA       NA      NA
    ## 452370:               3       NA         NA       NA      NA
    ## 452371:               3       NA         NA       NA      NA
    ## 452372:               1       NA         NA       NA      NA
    ## 452373:               4       NA         NA       NA      NA
    ##         medhouseholdincome medianhousevalue poverty education popdensity
    ##      1:                 NA               NA      NA        NA         NA
    ##      2:                 NA               NA      NA        NA         NA
    ##      3:                 NA               NA      NA        NA         NA
    ##      4:                 NA               NA      NA        NA         NA
    ##      5:                 NA               NA      NA        NA         NA
    ##     ---                                                                 
    ## 452369:                 NA               NA      NA        NA         NA
    ## 452370:                 NA               NA      NA        NA         NA
    ## 452371:                 NA               NA      NA        NA         NA
    ## 452372:                 NA               NA      NA        NA         NA
    ## 452373:                 NA               NA      NA        NA         NA
    ##         pct_owner_occ firstADRDyr pm25 no2 ozone
    ##      1:            NA        2000   NA  NA    NA
    ##      2:            NA        2000   NA  NA    NA
    ##      3:            NA        2000   NA  NA    NA
    ##      4:            NA        2000   NA  NA    NA
    ##      5:            NA        2000   NA  NA    NA
    ##     ---                                         
    ## 452369:            NA        2007   NA  NA    NA
    ## 452370:            NA        2011   NA  NA    NA
    ## 452371:            NA        2010   NA  NA    NA
    ## 452372:            NA        2007   NA  NA    NA
    ## 452373:            NA        2002   NA  NA    NA

    ## the number of person-years of related subjects is 452373

### 3. check whether alive people were followed-up till the end of study period (2016)

``` r
temp[!(qid %in% ADRDpeople[(dead),qid]), end_yr] %>% table()
```

    ## .
    ##   2001   2002   2003   2004   2005   2006   2007   2008   2009   2010 
    ##     47     70    678   1026   1055   1033    844    957    761   1006 
    ##   2011   2012   2013   2014   2015   2016 
    ##   1356   2026   2582   3940   4815 892471

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
    ## 14800: llllllloooX8U40     2006   2016     9        2005
    ## 14801: llllllloooXOlXX     2004   2012     8        2003
    ## 14802: llllllloooXo774     2001   2004     3        2000
    ## 14803: llllllloooo4O8X     2006   2008     2        2005
    ## 14804: llllllloooooll4     2002   2011     5        2001

The above subject (14804 subjects in total) do not have each year’s info
during follow-up. **Consider deleting them**

Their info in denominator files:

    ##           zip year             qid  dead sex race age dual statecode
    ##      1:     0 2002 llllllll0OXo77l FALSE   2    1  85    0          
    ##      2:     0 2002 llllllloo870S4O FALSE   2    1  98    1          
    ##      3:     0 2002 llllllloo8o4lUo FALSE   2    1  98    1          
    ##      4:     0 2002 lllllllooSOXXoo FALSE   2    1  87    0          
    ##      5:     0 2002 lllllllooUOXSSo FALSE   2    1  83    0          
    ##     ---                                                             
    ## 129107: 99999 2016 llllllll4SS7oXl FALSE   2    2  76    0          
    ## 129108: 99999 2016 llllllllX7X8040 FALSE   2    1  89    0          
    ## 129109: 99999 2016 lllllllloXSSol0 FALSE   2    1  78    0          
    ## 129110: 99999 2016 lllllllo4Sol0X7 FALSE   2    4  83    0          
    ## 129111: 99999 2016 lllllllo708l87U FALSE   1    1  90    1          
    ##         entry_age_break mean_bmi smoke_rate hispanic pct_blk
    ##      1:               4       NA         NA       NA      NA
    ##      2:               7       NA         NA       NA      NA
    ##      3:               7       NA         NA       NA      NA
    ##      4:               4       NA         NA       NA      NA
    ##      5:               4       NA         NA       NA      NA
    ##     ---                                                     
    ## 129107:               1       NA         NA       NA      NA
    ## 129108:               2       NA         NA       NA      NA
    ## 129109:               1       NA         NA       NA      NA
    ## 129110:               1       NA         NA       NA      NA
    ## 129111:               3       NA         NA       NA      NA
    ##         medhouseholdincome medianhousevalue poverty education popdensity
    ##      1:                 NA               NA      NA        NA         NA
    ##      2:                 NA               NA      NA        NA         NA
    ##      3:                 NA               NA      NA        NA         NA
    ##      4:                 NA               NA      NA        NA         NA
    ##      5:                 NA               NA      NA        NA         NA
    ##     ---                                                                 
    ## 129107:                 NA               NA      NA        NA         NA
    ## 129108:                 NA               NA      NA        NA         NA
    ## 129109:                 NA               NA      NA        NA         NA
    ## 129110:                 NA               NA      NA        NA         NA
    ## 129111:                 NA               NA      NA        NA         NA
    ##         pct_owner_occ firstADRDyr pm25 no2 ozone
    ##      1:            NA        2000   NA  NA    NA
    ##      2:            NA        2000   NA  NA    NA
    ##      3:            NA        2000   NA  NA    NA
    ##      4:            NA        2000   NA  NA    NA
    ##      5:            NA        2000   NA  NA    NA
    ##     ---                                         
    ## 129107:            NA        2010   NA  NA    NA
    ## 129108:            NA        2006   NA  NA    NA
    ## 129109:            NA        2006   NA  NA    NA
    ## 129110:            NA        2010   NA  NA    NA
    ## 129111:            NA        2006   NA  NA    NA
