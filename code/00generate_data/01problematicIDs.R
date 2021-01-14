###############################################################################
# Project: Medicare Mortality and Air Pollution in AD/ADRD                    #
# Code: find problematic IDs                                                  #
# Input: crosswalk files "id_crosswalk.fst"                                   #
# Output: # problematic IDs
# Author: Shuxin Dong                                                         #
# Date: 2021-01-12                                                            #
###############################################################################

############################# 0. Setup ########################################
rm(list = ls())
gc()

library(NSAPHutils)
library(data.table)
library(fst)

set_threads()

# read in crosswalk
crosswalk <- read_fst("../data/crosswalk/id_crosswalk.fst", as.data.table = T)
crosswalk[, BID:=sprintf("%.9d", BID)]
setkey(crosswalk, BID)
