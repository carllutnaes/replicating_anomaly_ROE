


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

load(sprintf("%s%s.rdata", data_dir, "ff_d"))

load(sprintf("%s%s", data_dir, "roe_vw_all_p_win.RData"))
load(sprintf("%s%s", data_dir, "roe_vw_all_p_r_trim.RData"))


load(sprintf("%s%s", data_dir, "roe_ew_all_p_win.RData"))
load(sprintf("%s%s", data_dir, "roe_ew_all_p_r_trim.RData"))



load(sprintf("%s%s", data_dir, "roe_vw_nasdaq_desc.RData"))
load(sprintf("%s%s", data_dir, "roe_vw_nasdaq_p_win.RData"))
load(sprintf("%s%s", data_dir, "roe_vw_nasdaq_p_r_trim.RData"))

load(sprintf("%s%s", data_dir, "roe_ew_nasdaq_p_win.RData"))
load(sprintf("%s%s", data_dir, "roe_ew_nasdaq_p_r_trim.RData"))


# adjust for na
hedge_idx <- 6

dates_bool <- !is.na(roe_vw_nasdaq_p_r_trim[, hedge_idx])
reg_vw_nasdaq <- roe_vw_nasdaq_p_r_trim[dates_bool, hedge_idx]


dates_bool <- !is.na(roe_ew_nasdaq_p_r_trim[, hedge_idx])
reg_ew_nasdaq <- roe_ew_nasdaq_p_r_trim[dates_bool, hedge_idx]


dates_bool <- !is.na(roe_vw_all_p_r_trim[, hedge_idx])
reg_vw_all <- roe_vw_all_p_r_trim[dates_bool, hedge_idx]


dates_bool <- !is.na(roe_ew_all_p_r_trim[, hedge_idx])
reg_ew_all <- roe_ew_all_p_r_trim[dates_bool, hedge_idx]

s.regress_dates <- NULL


roe_hedge1 <- descr(coredata(reg_vw_nasdaq))
colnames(roe_hedge1) <- "roe_vw_nasdaq"
roe_hedge2 <- descr(coredata(reg_ew_nasdaq))
colnames(roe_hedge1) <- "roe_ew_nasdaq"
roe_hedge3 <- descr(coredata(reg_vw_all))
colnames(roe_hedge1) <- "roe_vw_all"
roe_hedge4 <- descr(coredata(reg_ew_all))
colnames(roe_hedge1) <- "roe_ew_all"
#colnames(roe_hedge) <- c( "roe_vw_nasdaq", "roe_ew_nasdaq", "roe_vw_all", "roe_ew_all")
roe_hedge <- cbind(roe_hedge1,
                   roe_hedge2,
                   roe_hedge3,
                   roe_hedge4)
sum_select <- c(1, 2, 3, 4, 5, 6, 7, 9, 11, 13, 14)
#hedge_sum <- descr(roe_hedge)
hedge_sum <- roe_hedge[sum_select,]
colnames(hedge_sum) <-
  c("N (VW)", "ROE - NASDAQ (EW)", "ROE - ALLA (VW)", "ROE - ALLA (EW)")
# autocorr

# create table
desc_hedge <- matrix(ncol = 4, nrow = 9)


# adj time
time_vec <- index(roe_vw_nasdaq_p_r_trim)
ff_d <- ff_d[time_vec]

n_portfolios <- 6

# first nasdaq, then all
# first vw, then ew

str <-
  c("roe_vw_nasdaq", "roe_ew_nasdaq", "roe_vw_all", "roe_ew_all")

tick_mean <- mean(desc_roe_vw_nasdaq[, 3])
mv_mean <- mean(desc_roe_vw_nasdaq[, 2])
nasdaq <- 343
other <- 283
uni_tot <- 626
desc_1 <- matrix(nrow = 3, ncol = 3)
col_str <-
  c("Number of unique firms",
    "Percent nasdaq tickers (avg)",
    "Percent Nsdq MV (avg)")
row_str <- c("Nasdaq", "Other", "Market")
# number of companies
desc_1[1, 1] <- nasdaq
desc_1[2, 1] <- other
desc_1[3, 1] <- uni_tot
# percent of tickers
desc_1[1, 2] <- mean(desc_roe_vw_nasdaq[, 2]) * 100
desc_1[2, 2] <- (1 - mean(desc_roe_vw_nasdaq[, 2])) * 100
desc_1[3, 2] <- 100
# percent of mtk cap
desc_1[1, 3] <- mean(desc_roe_vw_nasdaq[, 3]) * 100
desc_1[2, 3] <- (1 - mean(desc_roe_vw_nasdaq[, 3])) * 100
desc_1[3, 3] <- 100

colnames(desc_1) <- col_str
rownames(desc_1) <- row_str



# n observations table

# percent of tickers ¨
tickers_per_portfolio <- NULL
#for (ii in 1:15) {
#  tickers_per_portfolio[ii] <- mean(desc_roe_vw_nasdaq[ii,c(4:8)])
#}
dd <- desc_roe_vw_nasdaq
desc_2 <- cbind(dd[, 1],
                dd[, 10],
                dd[, 9])
rownames(desc_2) <- as.character(year(s.year))
colnames(desc_2) <- c("Companies per portfolio (avg)",
                      "Unique Compaines",
                      "Daily Observations")

desc_tbl <- 1
desc_tbl2 <- 1
desc_tbl3 <- 1
corrmat <- 1

if (desc_tbl == 1) {
  desc1_latex <- xtable(desc_1,
                        type = "latex")
  digits(desc1_latex)[2] <- 0
  digits(desc1_latex)[c(3, 4)] <- 1
  print(desc1_latex,
        file = sprintf("%s%s%s", res_dir, "latex_desc_mkt", output_format))
}

if (desc_tbl2 == 1) {
  desc2_latex <- xtable(desc_2,
                        type = "latex")
  digits(desc2_latex)[c(3)] <- 1
  digits(desc2_latex)[c(2, 4)] <- 0
  print(desc2_latex,
        file = sprintf("%s%s%s", res_dir, "latex_desc_portfolio", output_format))
}

if (desc_tbl3 == 1) {
  desc3_latex <- xtable(hedge_sum,
                        type = "latex")
  print(desc3_latex,
        file = sprintf("%s%s%s", res_dir, "latex_desc_return", output_format))
}

corr_data <-
  cbind(ff_d[, c(2, 6, 7, 8)], roe_vw_nasdaq_p_r_trim[, n_portfolios])

corr_mat <-
  cor(coredata(corr_data),
      method = c("pearson", "kendall", "spearman"))
# # prettier names for variables
colnames(corr_mat) <- c("ROE (VW)", "Rm-Rf", "SMB", "HML", "MOM")
rownames(corr_mat) <- colnames(corr_mat)
# # tidy up, remove diag also
corr_mat[lower.tri(corr_mat)] <- NA
corr_mat[lower.tri(corr_mat, diag = TRUE)] <- NA
#
if (corrmat == 1) {
  latex_corr_mat <- xtable(corr_mat, type = "latex")
  digits(latex_corr_mat) <- 3
  
  print(latex_corr_mat,
        file = sprintf("%s%s%s", res_dir, "latex_corr_mat", output_format))
}


# This part added for visual effect when running "run_me.R"
print("Example of Results and Descriptive Statistics (See CAUTION in bottom of console)")
print("")
print(desc_1)
print("")
print(desc_2)
print("")
print(hedge_sum)
print("")
print(corr_mat)
print("")
print("CAUTION: Numbers shown in the Bachelor thesis are not replecated here due to a few reasons:")
print("load_data_nsdq.R has been modified;")
print("settings in calculate_return.R-scripts are modified;")
print("the scipts making the LaTeX-tables where not finalized fully")