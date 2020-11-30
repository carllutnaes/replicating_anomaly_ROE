


rm(list = ls())

list_packages <-
  c(
    "stargazer",
    "PerformanceAnalytics",
    "zoo",
    "quantmod",
    "xts",
    "lmtest",
    "readxl",
    "magic",
    "lubridate",
    "sandwich",
    "xtable",
    "GRS.test",
    "summarytools"
  )
# load file from dir

for (i in list_packages) {
  if (!require(i, character.only = TRUE)) {
    install.packages(i)
    library(i, character.only = TRUE)
  }
}
rm("list_packages")

d.start    <-
  as.Date("2001-01-01") # roe goes from 2010 because core data begins 2009
d.end      <- as.Date("2015-12-31")
s.year     <- seq(d.start, d.end, by = "years")

data_dir <- "./formatted_data/"
res_dir <- "./results/"
output_format <- ".tex"

load(sprintf("%s%s", data_dir, "roe_vw_nasdaq_desc.RData"))
load(sprintf("%s%s", data_dir, "roe_vw_nasdaq_p_win.RData"))
load(sprintf("%s%s", data_dir, "roe_vw_nasdaq_p_r_trim.RData"))


# adjust for na
hedge_idx <- 6

dates_bool <- !is.na(roe_vw_nasdaq_p_r_trim[, hedge_idx])
reg_vw_nasdaq <- roe_vw_nasdaq_p_r_trim[dates_bool, hedge_idx]

roe_hedge1 <- descr(coredata(reg_vw_nasdaq))
colnames(roe_hedge1) <- "roe_vw_nasdaq"
print(roe_hedge1)
# This part added for visual effect when running "run_me.R"

print("CAUTION: Numbers shown in the Bachelor thesis are not replecated here")