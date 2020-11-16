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
    "devtools",
    "DescTools"
  )

for (i in list_packages) {
  if (!require(i, character.only = TRUE)) {
    install.packages(i)
    library(i, character.only = TRUE)
  }
}
rm("list_packages")

data_dir <- "./formatted_data/"

load(sprintf("%s%s.RData", data_dir, "ff_d"))

load(sprintf("%s%s.RData", data_dir, "ta_x"))
load(sprintf("%s%s.RData", data_dir, "tl_x"))
load(sprintf("%s%s.RData", data_dir, "ni_x"))
load(sprintf("%s%s.RData", data_dir, "mv_x"))
load(sprintf("%s%s.RData", data_dir, "ri_x"))
load(sprintf("%s%s.RData", data_dir, "nasdaq_list"))
load(sprintf("%s%s.RData", data_dir, "nasdaq_tickers"))




d.origin   <- as.Date("1899-12-30")
d.start    <-
  as.Date("2000-01-01")
d.end      <- as.Date("2015-12-31")
s.year     <- seq(d.start + 366, d.end, by = "years")
s.month    <- seq(d.start + 366, d.end, by = "month")

# adjust dates to arbitrary year (5 for 2005)
date_idx_adj <- 1:15
s.year     <- s.year[date_idx_adj]
s.month    <- s.month[date_idx_adj]

# first trading day
s.first_date <- NULL
year_str <- as.character(year(s.year))
for (td in 1:length(s.year)) {
  s.first_date[td] <- index(ff_d[year_str[td]][1, 1])
}
s.first_date <- as.Date(s.first_date)

s.trading_days <- as.Date(index(ri_x))

s.delay     <- as.Date(
  c(
    "2001-07-02",
    "2002-07-01",
    "2003-07-01",
    "2004-07-01",
    "2005-07-01",
    "2006-07-03",
    "2007-07-02",
    "2008-07-01",
    "2009-07-01",
    "2010-07-01",
    "2011-07-01",
    "2012-07-02",
    "2013-07-01",
    "2014-07-01",
    "2015-07-01"
  )
)

s.delay_end <-
  as.Date(
    c(
      "2002-06-28",
      "2003-06-30",
      "2004-06-30",
      "2005-06-30",
      "2006-06-30",
      "2007-06-29",
      "2008-06-30",
      "2009-06-30",
      "2010-06-30",
      "2011-06-30",
      "2012-06-29",
      "2013-06-28",
      "2014-06-30",
      "2015-06-30",
      "2016-06-30"
    )
  )# , "2017-06-30"))


s.mv_date <- as.Date(
  c(
    "2001-07-01",
    "2002-07-01",
    "2003-07-01",
    "2004-07-01",
    "2005-07-01",
    "2006-07-01",
    "2007-07-01",
    "2008-07-01",
    "2009-07-01",
    "2010-07-01",
    "2011-07-01",
    "2012-07-01",
    "2013-07-01",
    "2014-07-01",
    "2015-07-01"
  )
)
s.delay       <- s.delay[date_idx_adj]
s.delay_end   <- s.delay_end[date_idx_adj]
s.mv_date     <- s.mv_date[date_idx_adj]




n_portfolios  <- 5
desc.percent_nasdaq_mv <- NULL
desc.percent_nasdaq_tickers <- NULL
desc.n_companies_per_portfolio_per_year <- NULL
desc.avg_companies_per_portfolio <- NULL
desc.n_day_observations <- NULL
desc.n_unique_companies <- NULL
desc.percent_nasdaq_tickers <- NULL

ri_x <- log(ri_x)
nr <- nrow(ri_x)
ret_nr <- nrow(ri_x[, ])
ret_t <- coredata(ri_x[2:nrow(ri_x)])
ret_t_1 <- coredata(ri_x[1:(nrow(ri_x) - 1)])
ret <- (ret_t - ret_t_1) / ret_t_1 # == ret_t/ret_t_1 - 1
ret <- ret * 100
ff_d <- ff_d * 100
ret[is.infinite(ret)] <- 0
ret[is.na(ret)] <- 0
ret_x <- xts(x = ret, order.by = index(ri_x)[-1])

eq_x <- ta_x - tl_x
roe <- coredata(ni_x[-1]) / coredata(eq_x[1:(nrow(eq_x) - 1)])
roe_x <- xts(x = roe, order.by = index(eq_x)[-1])

w_type <- "ew"
limit <- "nasdaq"
selected_variable_str <- "roe"
selected_variable <- roe_x
type_str <-
  sprintf("%s_%s_%s", selected_variable_str, w_type, limit)
n_companies_per_portfolio_per_year <-
  matrix(ncol = 5, nrow = length(s.year))
percent_nasdaq_tickers <- vector(length = length(s.year))
percent_nasdaq_mv <- vector(length = length(s.year))

roe_ew_nasdaq_p_r <- NULL#list(,n_portfolios)
roe_ew_nasdaq_p_r_str_low <-
  sprintf("%s_Low", selected_variable_str)
roe_ew_nasdaq_p_r_str_high <-
  sprintf("%s_High", selected_variable_str)

roe_ew_nasdaq_p_r_names <-
  c(roe_ew_nasdaq_p_r_str_low,
    "P2",
    "P3",
    "P4",
    roe_ew_nasdaq_p_r_str_high)

for (y in 1:length(s.year)) {
  p_date <- s.year[y]
  p_year <- year(p_date)
  p_year_str <- as.character(p_year)
  print(sprintf("Calculating Returns (Year: %s)", p_year_str))
  
  # ROE
  not_na <- !is.na(selected_variable[p_year_str])
  tickers <- colnames(not_na)[not_na]
  
  # RI
  ri_not_na <- !is.na(ri_x[s.delay[y] , tickers])
  ri_tickers <- colnames(ri_not_na)[ri_not_na]
  
  # MV
  mv_not_na <- !is.na(mv_x[s.mv_date[y] , ri_tickers])
  tickers_select <- colnames(mv_not_na)[mv_not_na]
  
  sorted_tickers_nasdaq <- NULL
  excluded_tickers <- NULL
  sorted_tickers_nasdaq <-
    tickers_select[tickers_select %in% nasdaq_tickers]
  excluded_tickers <-
    tickers_select[!tickers_select %in% nasdaq_tickers]
  
  selected_variable_yr <-
    selected_variable[p_year_str , sorted_tickers_nasdaq]
  
  selected_variable_yr_sort_idx <-
    order(coredata(selected_variable_yr))
  
  selected_variable_sorted <-
    selected_variable_yr[, selected_variable_yr_sort_idx] # sanity check-point variable
  sorted_tickers <- colnames(selected_variable_sorted)
  
  # get sort on nasdaq tickers
  sort_idx <-
    ceiling(seq_along(sorted_tickers) / (length(sorted_tickers) / n_portfolios))
  portfolios <- split(sorted_tickers, sort_idx)
  per_nasdaq <- 0
  per_nasdaq_mv <- 0
  if (!is.null(excluded_tickers)) {
    # desc nasdaq
    per_nasdaq <-
      length(sorted_tickers_nasdaq) / (length(excluded_tickers) + length(sorted_tickers_nasdaq))
    # mv nasdaq
    mv_x_nasdaq <- sum(mv_x[s.mv_date[y], sorted_tickers_nasdaq])
    mv_x_excl <- sum(mv_x[s.mv_date[y], excluded_tickers])
    per_nasdaq_mv <- mv_x_nasdaq / (mv_x_nasdaq + mv_x_excl)
    
    for (ii in 1:length(excluded_tickers)) {
      #compare roe val
      roe_excl <- coredata(roe_x[p_year_str, excluded_tickers[ii]])
      # get idx
      idx <- sum(apply(roe_excl, 2, `>`, selected_variable_sorted))
      if (idx == 0) {
        idx <- 1
      }
      # get which portfolio
      portfolio_idx <- sort_idx[idx]
      # add ticker to corr portfolio
      portfolios[[portfolio_idx]] <-
        c(portfolios[[portfolio_idx]], excluded_tickers[ii])
    }
  }
  percent_nasdaq_tickers[y] <- per_nasdaq
  percent_nasdaq_mv[y] <- per_nasdaq_mv
  temp_x <- NULL
  
  n_companies_in_portfolio <- vector(length = n_portfolios)
  n_day_observations <- vector(length = n_portfolios)
  
  for (p in 1:n_portfolios) {
    portfolio_tickers <-
      portfolios[[p]] # get tickers for each portfolio
    n_companies_in_portfolio[p] <- length(portfolio_tickers)
    
    n_companies_per_portfolio_per_year[y, p] <-
      length(portfolio_tickers)
    
    day_first <- 1
    
    # get trading days
    tr_dt <-
      s.trading_days[s.trading_days >= s.delay[y] &
                       s.trading_days <= s.delay_end[y]]
    mv_dt <- seq(s.mv_date[y], s.delay_end[y], by = "month")
    
    # get monthly data
    ret <- ret_x[tr_dt, portfolio_tickers]
    mv <- mv_x[mv_dt, portfolio_tickers]
    mv[is.na(mv)] <- 0
    
    # descr stat
    n_day_observations[p] <- sum(!is.na(ret))
    
    
    # VW
    if (w_type == "vw") {
      w_yr <- mv / rowSums(mv)
    }
    # EW
    else if ((w_type == "ew")) {
      n_companies <- dim(mv)[2]
      n_w_rows <- dim(mv)[1]
      
      w_vec <- rep(1, n_companies) / n_companies
      w_raw <- t(replicate(n_w_rows, w_vec))
      w_yr <- xts(x = w_raw, order.by = index(mv))
      colnames(w_yr) <- portfolio_tickers
    }
    # calc portfolio return
    rf_d <- ff_d$rf_daily[tr_dt]
    for (ii in 1:ncol(ret)) {
      ret[, ii] <- ret[, ii] - rf_d
    }
    
    r_y <-
      Return.portfolio(ret,
                       weights = w_yr,
                       rebalance_on = c("months"))
    
    colnames(r_y) <- roe_ew_nasdaq_p_r_names[p]
    temp_x <- cbind(temp_x, r_y)
    
  }
  # concentrate
  roe_ew_nasdaq_p_r <- rbind(roe_ew_nasdaq_p_r, temp_x)
  # descr stat.
  
  desc.avg_companies_per_portfolio[y] <-
    mean(n_companies_in_portfolio)
  desc.n_day_observations[y] <- sum(n_day_observations)
  desc.n_unique_companies[y] <-
    length(portfolios[[1]]) + length(portfolios[[2]]) + length(portfolios[[3]]) + length(portfolios[[4]]) + length(portfolios[[5]])
  
}
desc.percent_nasdaq_mv <- percent_nasdaq_mv
desc.percent_nasdaq_tickers <- percent_nasdaq_tickers
desc.n_companies_per_portfolio_per_year <-
  n_companies_per_portfolio_per_year
desc.n_day_observations[length(desc.n_day_observations) + 1] <-
  sum(desc.n_day_observations)
desc.n_unique_companies[length(desc.n_unique_companies) + 1] <- NA
desc.avg_companies_per_portfolio[length(desc.avg_companies_per_portfolio) + 1] <-
  NA
desc <-
  cbind(
    desc.n_unique_companies,
    desc.avg_companies_per_portfolio,
    desc.n_day_observations
  )

colnames(desc) <- c("unika f",
                    "nf/p",
                    "n Dag")
rownames(desc) <- c(as.character(year(s.year)), "Totalt")

hedge_portfolio <-
  roe_ew_nasdaq_p_r[, roe_ew_nasdaq_p_r_str_high] - roe_ew_nasdaq_p_r[, roe_ew_nasdaq_p_r_str_low]
colnames(hedge_portfolio) <-
  sprintf("r_%s_HML", selected_variable_str)
roe_ew_nasdaq_p_r <- cbind(roe_ew_nasdaq_p_r, hedge_portfolio)

# winsorize
roe_ew_nasdaq_p_r_win <- NULL
roe_ew_nasdaq_p_r_trim <- NULL


wi <- 6
roe_ew_nasdaq_p_r <-
  roe_ew_nasdaq_p_r[!is.na(roe_ew_nasdaq_p_r[, wi]), ]
pp <- roe_ew_nasdaq_p_r[, wi]
p_max <- IQR(pp) * 1.5 + median(pp)
p_min <- -IQR(pp) * 1.5 + median(pp)
roe_ew_nasdaq_p_r_win <-
  cbind(roe_ew_nasdaq_p_r_win,
        Winsorize(pp, minval = p_min, maxval = p_max))

# trim
qnt <-
  quantile(roe_ew_nasdaq_p_r[, wi],
           probs = c(.025, .975),
           na.rm = TRUE)
pp <- subset(roe_ew_nasdaq_p_r[, wi],
             roe_ew_nasdaq_p_r[, wi] >= qnt[1] &
               roe_ew_nasdaq_p_r[, wi] <= qnt[2])
roe_ew_nasdaq_p_r_trim <-
  cbind(roe_ew_nasdaq_p_r[index(pp), 1:5], pp)
#}

manual_date_rmv <- "2002-04-30"
sorted_dates <-
  which(!grepl(manual_date_rmv, index(roe_ew_nasdaq_p_r_trim)))
roe_ew_nasdaq_p_r_trim <- roe_ew_nasdaq_p_r_trim[sorted_dates]

save(file = sprintf("%s%s_%s.RData", data_dir, type_str, "p_win"),
     roe_ew_nasdaq_p_r)
save(file = sprintf("%s%s_%s.RData", data_dir, type_str, "p_r_win"),
     roe_ew_nasdaq_p_r_win)
save(file = sprintf("%s%s_%s.RData", data_dir, type_str, "p_r_trim"),
     roe_ew_nasdaq_p_r_trim)
desc_roe_ew_nasdaq <- desc
save(file = sprintf("%s%s_%s.RData", data_dir, type_str, "desc"),
     desc_roe_ew_nasdaq)

print("-------------------------")
print("Portfolio Calculations")
print(sprintf("Settings: %s", type_str))
print("Complete")
print("-------------------------")
