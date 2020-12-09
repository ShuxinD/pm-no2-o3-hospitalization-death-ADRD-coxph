###############################################################################
# Project: Medicare Mortality and Air Pollution in AD/ADRD                    #
# Code: covariates correlations, create table one                             #
# Input: "ADRDmort_cplt.csv"                                                  #
# Output:                                                                     #
# Output:
# Author: Shuxin Dong                                                         #
# Date: Dec 9, 2020                                                           #
###############################################################################

############################# 0. Setup ########################################
rm(list = ls())
gc()

library(data.table)
library(dplyr)
library(fst)

setwd("/nfs/home/S/shd968/shared_space/ci3_shd968/dementia")

dir_data <- "/nfs/home/S/shd968/shared_space/ci3_shd968/dementia/"
dir_output <- "/nfs/home/S/shd968/shared_space/ci3_shd968/dementia/descrip/"

setDTthreads(threads = 0)

data <- fread(paste0(dir_data, "ADRDmort_cplt.csv"))

