
# In order to run, set working directory
# to your specific local map location
# (func. setwd() - row 4 in this script)
#
# Descriptive scripts to make LaTeX plots not included
# because they 

setwd("~/Documents/Programming/GitHub/BSc Thesis/")
data_dir <- "./formatted_data/"

source("load_data_nsdq.R")

source("calculate_returns_all_ew.R")
source("calculate_returns_all_vw.R")

source("calculate_returns_nasdaq_ew.R")
source("calculate_returns_nasdaq_vw.R")

source("desc_stat_breakpoint.R")