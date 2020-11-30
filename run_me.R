
# In order to run, set working directory
# to your specific local map location
# (row 9 in this script)
#
# Descriptive scripts to make LaTeX plots not included
# because they 

setwd("~/Documents/GitHub/replicating_anomaly_ROE/")
data_dir <- "./formatted_data/"

source("load_data_nsdq.R")

source("calculate_returns_nasdaq_vw.R")

source("desc_stat_breakpoint.R")