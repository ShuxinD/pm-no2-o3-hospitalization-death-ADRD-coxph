Setup
-----

``` r
rm(list = ls())
gc()
```

    ##          used (Mb) gc trigger (Mb) max used (Mb)
    ## Ncells 452219 24.2     970735 51.9   642637 34.4
    ## Vcells 879910  6.8    8388608 64.0  1825873 14.0

``` r
library(data.table)
library(dplyr)
```

    ## Warning: As of rlang 0.4.0, dplyr must be at least version 0.8.0.
    ## * dplyr 0.7.6 is too old for rlang 0.4.6.
    ## * Please update dplyr with `install.packages("dplyr")` and restart R.

``` r
setDTthreads(threads = 0)
setwd("/nfs/home/S/shd968/shared_space/ci3_shd968/dementia")
dir_ADRDpeople <-  "/nfs/home/S/shd968/shared_space/ci3_shd968/dementia/"

ADRDpeople <- fread(paste0(dir_ADRDpeople, "ADRDpeople.csv"))
```

Add a new chunk by clicking the *Insert Chunk* button on the toolbar or
by pressing *Ctrl+Alt+I*.

When you save the notebook, an HTML file containing the code and output
will be saved alongside it (click the *Preview* button or press
*Ctrl+Shift+K* to preview the HTML file).

The preview shows you a rendered HTML copy of the contents of the
editor. Consequently, unlike *Knit*, *Preview* does not run any R code
chunks. Instead, the output of the chunk when it was last run in the
editor is displayed.
