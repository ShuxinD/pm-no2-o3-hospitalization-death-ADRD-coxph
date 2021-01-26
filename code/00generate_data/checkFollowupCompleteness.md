Check the completeness of follow-up in the ADRD population
==========================================================

The dataset `ADRDpeople.csv` was directly generated from the denominator
files.

Setup and read in data
----------------------

``` r
rm(list = ls())
gc()
```

    ##          used (Mb) gc trigger (Mb) max used (Mb)
    ## Ncells 443142 23.7     944801 50.5   642637 34.4
    ## Vcells 860583  6.6    8388608 64.0  1825873 14.0

``` r
library(data.table)
library(dplyr)
```

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

``` r
setDTthreads(threads = 0)
setwd("/nfs/home/S/shd968/shared_space/ci3_shd968/dementia")
dir_ADRDpeople <-  "/nfs/home/S/shd968/shared_space/ci3_shd968/dementia/"
dir_enrolledInfo <-  "/nfs/home/S/shd968/shared_space/ci3_shd968/dementia/"

ADRDpeople <- fread(paste0(dir_ADRDpeople, "ADRDpeople.csv"))
enrolledInfo <- fread(paste0(dir_enrolledInfo, "enrolledInfo.csv"))
```

Check the completeness of fullow-up
-----------------------------------

Our final dataset should:

-   be one row per person-year
-   have follow-up info for every year after the year they were
    hospitalized with ADRD for the first time (`firstADRDyr`) till the
    end of study period (2016) or death, whichever comes first.

### 1. basic info of original ADRD dataset

``` r
temp <- ADRDpeople[, .(start_yr = min(year),
                       end_yr = max(year),
                       count = uniqueN(year)), by = qid]
temp <- merge(temp, enrolledInfo, by.x = "qid", by.y = "QID")
```

We generate `start_yr` as the minimum of calendar year and `end_yr` as
the maximum of calendar years, also, `count` as the count number of
unique calendar year for each subject. All these variables will be used
for following checking.

We also merged the enroll information (`firstADRDyear`) to `temp`.
(`firstADRDyr`+1) indicates the year that subjects should be
followed-up.

`temp` is a one-row-per-person dataset, looks like:

    ##          qid start_yr end_yr count firstADRDyr
    ## 1: 003050048     2001   2004     4        2000
    ## 2: 003628694     2003   2005     3        2002
    ## 3: 003749417     2006   2006     1        2005
    ## 4: 004402139     2005   2005     1        2004
    ## 5: 012472193     2001   2004     4        2000
    ## 6: 014929014     2006   2006     1        2005

``` r
cat("the number of subjects in ADRDpeople is", dim(temp)[1], "\n")
```

    ## the number of subjects in ADRDpeople is 3187863

``` r
cat("the number of person-years in ADRDpeople is", dim(ADRDpeople)[1], "\n")
```

    ## the number of person-years in ADRDpeople is 10603358

``` r
cat("is there any duplication of the combination of `qid` and calendar year: ", any(duplicated(ADRDpeople[,.(qid,year)])), "\n")
```

    ## is there any duplication of the combination of `qid` and calendar year:  FALSE

### 2. check whether all people were followed-up from the year following firstADRDyr

``` r
temp[start_yr != (firstADRDyr+1)]
```

    ##                  qid start_yr end_yr count firstADRDyr
    ##   1: llllll0llOO4OoO     2014   2014     1        2012
    ##   2: llllllU8l8XXXlU     2014   2016     3        2012
    ##   3: llllllUS84Oo84X     2015   2015     1        2013
    ##   4: llllllUSXS7lolO     2014   2016     3        2011
    ##   5: lllllll00878O70     2004   2011     8        2001
    ##  ---                                                  
    ## 151: lllllllooU4UXOo     2014   2014     1        2009
    ## 152: lllllllooX7lo48     2015   2015     1        2011
    ## 153: lllllllooXX780o     2013   2013     1        2011
    ## 154: llllllloooU0O08     2014   2016     3        2012
    ## 155: llllllloooo70UU     2011   2011     1        2008

The above subject (155 subjects in total), were not followed-up from the
year following firstADRDyr. **Consider deleting them**

Their info in denominator files:

``` r
ADRDpeople[qid %in% temp[start_yr != (firstADRDyr+1)][, qid], ]
```

    ##                  qid year   zip  dead sex race age dual statecode
    ##   1: llllll0llOO4OoO 2014  7652  TRUE   1    1  68    0        NJ
    ##   2: llllllU8l8XXXlU 2014 92114 FALSE   2    4  83    0        CA
    ##   3: llllllU8l8XXXlU 2015 99999 FALSE   2    4  84    0          
    ##   4: llllllU8l8XXXlU 2016 99999 FALSE   2    4  85    0          
    ##   5: llllllUS84Oo84X 2015 84129  TRUE   2    1  67    0        UT
    ##  ---                                                             
    ## 498: lllllllooXX780o 2013 22967  TRUE   2    1  80    1        VA
    ## 499: llllllloooU0O08 2014 48429 FALSE   2    1  86    0        MI
    ## 500: llllllloooU0O08 2015 48429 FALSE   2    1  87    0        MI
    ## 501: llllllloooU0O08 2016 48429 FALSE   2    1  88    0        MI
    ## 502: llllllloooo70UU 2011 34668  TRUE   2    1  85    0        FL
    ##      entry_age_break mean_bmi smoke_rate   hispanic     pct_blk
    ##   1:               1 26.59156  0.4443011 0.08293251 0.019690365
    ##   2:               4 26.68127  0.3992601 0.40170262 0.215150422
    ##   3:               4       NA         NA         NA          NA
    ##   4:               4       NA         NA         NA          NA
    ##   5:               1       NA         NA 0.28533554 0.017370770
    ##  ---                                                           
    ## 498:               1 26.81777  0.4563725 0.01537617 0.219110379
    ## 499:               2 28.56408  0.4191396 0.00603470 0.002263012
    ## 500:               2 28.47284  0.3907819 0.01032876 0.001385565
    ## 501:               2 28.47284  0.3907819 0.01441961 0.001201634
    ## 502:               2 27.35453  0.6000000 0.08972782 0.034898021
    ##      medhouseholdincome medianhousevalue    poverty education popdensity
    ##   1:              96454           554400 0.03255458 0.1367060 2541.74000
    ##   2:              57287           282600 0.15404443 0.3175012 8412.78300
    ##   3:                 NA               NA         NA        NA         NA
    ##   4:                 NA               NA         NA        NA         NA
    ##   5:              62031           168300 0.03412342 0.1983404 4029.89400
    ##  ---                                                                    
    ## 498:              41786           166200 0.29116466 0.5000000   23.34236
    ## 499:              42794            97800 0.07686676 0.2238508  146.59190
    ## 500:              42004            99400 0.08223201 0.1769852  146.31550
    ## 501:              44922            98000 0.08045166 0.1617934  153.37410
    ## 502:              32441           101500 0.09573066 0.2279018 2776.91800
    ##      pct_owner_occ firstADRDyr
    ##   1:     0.8791938        2012
    ##   2:     0.6721246        2012
    ##   3:            NA        2012
    ##   4:            NA        2012
    ##   5:     0.7992122        2013
    ##  ---                          
    ## 498:     0.8026316        2011
    ## 499:     0.7581719        2012
    ## 500:     0.7543544        2012
    ## 501:     0.7563565        2012
    ## 502:     0.7355718        2008

``` r
dim(ADRDpeople[qid %in% temp[start_yr != (firstADRDyr+1)][, qid], ])[1]
```

    ## [1] 502

### 3. check whether alive people were followed-up till the end of study period (2016)

``` r
temp[qid %in% ADRDpeople[!(dead),qid], ][, end_yr] %>% table()
```

    ## .
    ##   2001   2002   2003   2004   2005   2006   2007   2008   2009   2010 
    ##     21  57063  89977 108057 126388 132197 138074 144140 136804 141615 
    ##   2011   2012   2013   2014   2015   2016 
    ## 139799 136550 138646 132261 130321 567583

We could see loads of alive subjects weren’t followed-up till 2016. This
should be considered as right-censored subjects during the analyses.

### 4. check whether all people have each year’s info during follow-up

``` r
temp[(end_yr-start_yr+1) != count,]
```

    ##                  qid start_yr end_yr count firstADRDyr
    ##   1: llllllU8lUOl8lS     2013   2016     2        2012
    ##   2: llllllU8lUOoO0U     2014   2016     2        2013
    ##   3: llllllU8lo80XX8     2013   2016     2        2012
    ##   4: llllllUSlX8XXl8     2012   2016     4        2011
    ##   5: lllllll007USO88     2006   2016    10        2005
    ##  ---                                                  
    ## 244: lllllllooll7S74     2002   2009     6        2001
    ## 245: lllllllooo8U8oS     2001   2014    12        2000
    ## 246: llllllloooO077U     2001   2010     9        2000
    ## 247: llllllloooXl0OS     2006   2012     4        2005
    ## 248: llllllloooXo774     2001   2004     3        2000

The above subject (248 subjects in total) do not have each year’s info
during follow-up. **Consider deleting them**

Their info in denominator files:

``` r
ADRDpeople[qid %in% temp[(end_yr-start_yr+1) != count,][,qid],]
```

    ##                   qid year   zip  dead sex race age dual statecode
    ##    1: llllllU8lUOl8lS 2013 32807 FALSE   2    4  88    1        FL
    ##    2: llllllU8lUOl8lS 2016 32807 FALSE   2    4  91    1        FL
    ##    3: llllllU8lUOoO0U 2014 19008 FALSE   2    2  75    1        PA
    ##    4: llllllU8lUOoO0U 2016 19008 FALSE   2    2  77    1        PA
    ##    5: llllllU8lo80XX8 2013  2368 FALSE   2    2  86    1        MA
    ##   ---                                                             
    ## 1461: llllllloooXl0OS 2008     0 FALSE   2    1  87    0          
    ## 1462: llllllloooXl0OS 2012 49461  TRUE   2    1  90    0        MI
    ## 1463: llllllloooXo774 2001 54703 FALSE   2    1  83    0        WI
    ## 1464: llllllloooXo774 2002 54703 FALSE   2    1  84    0        WI
    ## 1465: llllllloooXo774 2004 54703  TRUE   2    1  86    0        WI
    ##       entry_age_break mean_bmi smoke_rate    hispanic     pct_blk
    ##    1:               5 27.14943  0.4226840 0.495460680 0.127898417
    ##    2:               5 27.03389  0.4321951 0.528286503 0.110731310
    ##    3:               2 27.19984  0.4755449 0.026563338 0.033094507
    ##    4:               2 27.20974  0.4723651 0.018843373 0.024048193
    ##    5:               4 26.52311  0.4634263 0.053714494 0.439002185
    ##   ---                                                            
    ## 1461:               3       NA         NA          NA          NA
    ## 1462:               3 29.37731  0.4873950 0.040113314 0.004192635
    ## 1463:               4 26.67950  0.5925926 0.009397801 0.006132204
    ## 1464:               4 26.67208  0.5263158 0.009433991 0.006211056
    ## 1465:               4 26.95571  0.4000000 0.010078616 0.007615602
    ##       medhouseholdincome medianhousevalue    poverty education popdensity
    ##    1:           35980.00        110600.00 0.15542522 0.3921211  4028.7400
    ##    2:           35378.00        108100.00 0.18221093 0.3973013  3940.2670
    ##    3:           74103.00        336300.00 0.03107914 0.1966433  3129.5350
    ##    4:           82231.00        339300.00 0.03853409 0.1595238  3165.0750
    ##    5:           62152.00        267400.00 0.10681319 0.2749038  3270.3790
    ##   ---                                                                    
    ## 1461:                 NA               NA         NA        NA         NA
    ## 1462:           53086.00        141800.00 0.07659574 0.1348837   206.7121
    ## 1463:           38079.05         88583.24 0.07989014 0.2648720   515.7225
    ## 1464:           38111.95         88831.01 0.07980558 0.2643555   515.7979
    ## 1465:           38697.89         93244.44 0.07829918 0.2551565   517.1421
    ##       pct_owner_occ firstADRDyr
    ##    1:     0.4809014        2012
    ##    2:     0.4496355        2012
    ##    3:     0.8533185        2013
    ##    4:     0.8612381        2013
    ##    5:     0.6966531        2012
    ##   ---                          
    ## 1461:            NA        2005
    ## 1462:     0.8250577        2005
    ## 1463:     0.6356412        2000
    ## 1464:     0.6356812        2000
    ## 1465:     0.6363927        2000
