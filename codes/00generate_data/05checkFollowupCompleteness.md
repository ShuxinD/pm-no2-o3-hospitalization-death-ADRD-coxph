Check the completeness of follow-up in the ADRD population
================

The dataset `ADRDpeople.csv` was directly generated from the denominator
files, which follow the ADRD people till their death or the end of 2016.

## Setup and load data

    ##          used (Mb) gc trigger (Mb) max used (Mb)
    ## Ncells 455410 24.4     979852 52.4   642637 34.4
    ## Vcells 886711  6.8    8388608 64.0  1825873 14.0

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

## Check the completeness of follow-up

Our final dataset should:

  - be one row per person-year
  - have follow-up info for every year after the year they were
    hospitalized with ADRD for the first time (`firstADRDyr`) till the
    end of study period (2016) or death, whichever comes first.

### 1\. basic info of original ADRD dataset

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
unique subjects) with 4962098 subjects: - generated `start_yr` as the
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

    ## the number of subjects in ADRDpeople(removed NAs) is 4962098

    ## the number of person-years in ADRDpeople(removed NAs) is 25112131

    ## is there any duplication of the combination of `qid` and calendar year:  FALSE

### 2\. check whether all people were followed-up from the year following firstADRDyr

``` r
temp[start_yr != (firstADRDyr+1)]
```

    ##                    qid start_yr end_yr count firstADRDyr
    ##     1:       037641248     2004   2004     1        2002
    ##     2:       A00003963     2002   2002     1        2000
    ##     3:       A00054041     2002   2002     1        2000
    ##     4:       A00082814     2002   2002     1        2000
    ##     5:       A00109981     2002   2002     1        2000
    ##    ---                                                  
    ## 15365: lllllllooooO7Xo     2007   2007     1        2005
    ## 15366: lllllllooooXoS7     2003   2004     2        2001
    ## 15367: lllllllooool4OO     2006   2006     1        2002
    ## 15368: llllllloooolllX     2004   2005     2        2001
    ## 15369: lllllllooooolU8     2006   2006     1        2000

The above subject (15369 subjects in total), were not followed-up from
the year following firstADRDyr. **Consider deleting them**

Their info in denominator
    files:

    ##           zip year             qid sex race age dual statecode  dead
    ##      1: 00000 2000       A07897374   1    2  79    1           FALSE
    ##      2: 00000 2000       A07997964   2    0  96    0           FALSE
    ##      3: 00000 2000       A08003849   2    1  96    1           FALSE
    ##      4: 00000 2000       A08004102   2    1  85    0           FALSE
    ##      5: 00000 2000 llllllloOll0llO   1    1  76    0           FALSE
    ##     ---                                                             
    ## 107624: 99999 2016 llllllll877S4l4   1    5  78    0           FALSE
    ## 107625: 99999 2016 llllllllOUl8o4O   2    5  94    0           FALSE
    ## 107626: 99999 2016 llllllllSSl04o4   2    1 100    0           FALSE
    ## 107627: 99999 2016 lllllllo788o7O8   1    1  74    0           FALSE
    ## 107628: 99999 2016 llllllloUO4OoO4   2    3  82    0           FALSE
    ##         mean_bmi smoke_rate hispanic pct_blk medhouseholdincome
    ##      1:       NA         NA       NA      NA                 NA
    ##      2:       NA         NA       NA      NA                 NA
    ##      3:       NA         NA       NA      NA                 NA
    ##      4:       NA         NA       NA      NA                 NA
    ##      5:       NA         NA       NA      NA                 NA
    ##     ---                                                        
    ## 107624:       NA         NA       NA      NA                 NA
    ## 107625:       NA         NA       NA      NA                 NA
    ## 107626:       NA         NA       NA      NA                 NA
    ## 107627:       NA         NA       NA      NA                 NA
    ## 107628:       NA         NA       NA      NA                 NA
    ##         medianhousevalue poverty education popdensity pct_owner_occ
    ##      1:               NA      NA        NA         NA            NA
    ##      2:               NA      NA        NA         NA            NA
    ##      3:               NA      NA        NA         NA            NA
    ##      4:               NA      NA        NA         NA            NA
    ##      5:               NA      NA        NA         NA            NA
    ##     ---                                                            
    ## 107624:               NA      NA        NA         NA            NA
    ## 107625:               NA      NA        NA         NA            NA
    ## 107626:               NA      NA        NA         NA            NA
    ## 107627:               NA      NA        NA         NA            NA
    ## 107628:               NA      NA        NA         NA            NA
    ##         summer_tmmx winter_tmmx summer_rmax winter_rmax firstADRDyr pm25
    ##      1:          NA          NA          NA          NA        2000   NA
    ##      2:          NA          NA          NA          NA        2000   NA
    ##      3:          NA          NA          NA          NA        2000   NA
    ##      4:          NA          NA          NA          NA        2000   NA
    ##      5:          NA          NA          NA          NA        2000   NA
    ##     ---                                                                 
    ## 107624:          NA          NA          NA          NA        2007   NA
    ## 107625:          NA          NA          NA          NA        2011   NA
    ## 107626:          NA          NA          NA          NA        2007   NA
    ## 107627:          NA          NA          NA          NA        2007   NA
    ## 107628:          NA          NA          NA          NA        2004   NA
    ##         no2 ozone ozone_summer
    ##      1:  NA    NA           NA
    ##      2:  NA    NA           NA
    ##      3:  NA    NA           NA
    ##      4:  NA    NA           NA
    ##      5:  NA    NA           NA
    ##     ---                       
    ## 107624:  NA    NA           NA
    ## 107625:  NA    NA           NA
    ## 107626:  NA    NA           NA
    ## 107627:  NA    NA           NA
    ## 107628:  NA    NA           NA

    ## the number of person-years of related subjects is 107628

### 3\. check whether alive people were followed-up till the end of study period (2016)

``` r
temp[!(qid %in% ADRDpeople[(dead),qid]), end_yr] %>% table()
```

    ## .
    ##   2001   2002   2003   2004   2005   2006   2007   2008   2009   2010 
    ##     42     47    703   1027   1044    992    732    852    390    456 
    ##   2011   2012   2013   2014   2015   2016 
    ##    574    885    825   1295   1026 665413

We could see loads of alive subjects weren’t followed-up till 2016. This
should be considered as right-censored subjects during the analyses.

### 4\. check whether all people have each year’s info during follow-up

``` r
temp[(end_yr-start_yr+1) != count,]
```

    ##                   qid start_yr end_yr count firstADRDyr
    ##    1: llllllU7S700ol0     2012   2016     4        2011
    ##    2: llllllU8lUOl8lS     2013   2016     2        2012
    ##    3: llllllU8lUOoO0U     2014   2016     2        2013
    ##    4: llllllU8lo80XX8     2013   2016     2        2012
    ##    5: llllllUS0lo4UUU     2012   2016     4        2011
    ##   ---                                                  
    ## 5404: llllllloooSX0o7     2001   2015    14        2000
    ## 5405: llllllloooSXX44     2004   2012     8        2003
    ## 5406: llllllloooX8U40     2006   2016     9        2005
    ## 5407: llllllloooXo774     2001   2004     3        2000
    ## 5408: llllllloooo4O8X     2006   2008     2        2005

The above subject (5408 subjects in total) do not have each year’s info
during follow-up. **Consider deleting them**

Their info in denominator
    files:

    ##          zip year             qid sex race age dual statecode  dead
    ##     1: 00000 2002 llllllll0OXo77l   2    1  85    0           FALSE
    ##     2: 00000 2002 llllllloo74UU78   2    1  82    0           FALSE
    ##     3: 00000 2002 llllllloo870S4O   2    1  98    1           FALSE
    ##     4: 00000 2002 llllllloo8o4lUo   2    1  98    1           FALSE
    ##     5: 00000 2002 lllllllooSOXXoo   2    1  87    0           FALSE
    ##    ---                                                             
    ## 45529: 99999 2016 llllllll4SS7oXl   2    2  76    0           FALSE
    ## 45530: 99999 2016 llllllllX7X8040   2    1  89    0           FALSE
    ## 45531: 99999 2016 lllllllloXSSol0   2    1  78    0           FALSE
    ## 45532: 99999 2016 lllllllo4Sol0X7   2    4  83    0           FALSE
    ## 45533: 99999 2016 llllllloUO4OoO4   2    3  82    0           FALSE
    ##        mean_bmi smoke_rate hispanic pct_blk medhouseholdincome
    ##     1:       NA         NA       NA      NA                 NA
    ##     2:       NA         NA       NA      NA                 NA
    ##     3:       NA         NA       NA      NA                 NA
    ##     4:       NA         NA       NA      NA                 NA
    ##     5:       NA         NA       NA      NA                 NA
    ##    ---                                                        
    ## 45529:       NA         NA       NA      NA                 NA
    ## 45530:       NA         NA       NA      NA                 NA
    ## 45531:       NA         NA       NA      NA                 NA
    ## 45532:       NA         NA       NA      NA                 NA
    ## 45533:       NA         NA       NA      NA                 NA
    ##        medianhousevalue poverty education popdensity pct_owner_occ
    ##     1:               NA      NA        NA         NA            NA
    ##     2:               NA      NA        NA         NA            NA
    ##     3:               NA      NA        NA         NA            NA
    ##     4:               NA      NA        NA         NA            NA
    ##     5:               NA      NA        NA         NA            NA
    ##    ---                                                            
    ## 45529:               NA      NA        NA         NA            NA
    ## 45530:               NA      NA        NA         NA            NA
    ## 45531:               NA      NA        NA         NA            NA
    ## 45532:               NA      NA        NA         NA            NA
    ## 45533:               NA      NA        NA         NA            NA
    ##        summer_tmmx winter_tmmx summer_rmax winter_rmax firstADRDyr pm25
    ##     1:          NA          NA          NA          NA        2000   NA
    ##     2:          NA          NA          NA          NA        2000   NA
    ##     3:          NA          NA          NA          NA        2000   NA
    ##     4:          NA          NA          NA          NA        2000   NA
    ##     5:          NA          NA          NA          NA        2000   NA
    ##    ---                                                                 
    ## 45529:          NA          NA          NA          NA        2010   NA
    ## 45530:          NA          NA          NA          NA        2006   NA
    ## 45531:          NA          NA          NA          NA        2006   NA
    ## 45532:          NA          NA          NA          NA        2010   NA
    ## 45533:          NA          NA          NA          NA        2004   NA
    ##        no2 ozone ozone_summer
    ##     1:  NA    NA           NA
    ##     2:  NA    NA           NA
    ##     3:  NA    NA           NA
    ##     4:  NA    NA           NA
    ##     5:  NA    NA           NA
    ##    ---                       
    ## 45529:  NA    NA           NA
    ## 45530:  NA    NA           NA
    ## 45531:  NA    NA           NA
    ## 45532:  NA    NA           NA
    ## 45533:  NA    NA           NA
