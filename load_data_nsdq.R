


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
    "gsubfn",
    "lubridate"
  )


for (i in list_packages) {
  if (!require(i, character.only = TRUE)) {
    install.packages(i)
    library(i, character.only = TRUE)
  }
}
rm("list_packages")

# define datastream error codes
error_code_list <- c(
  "$$ER: 2380,NO DATA IN REQUESTED PERIOD",
  "$$ER: 2381,NO DATA AVAILABLE",
  "$$ER: 0904,NO DATA AVAILABLE",
  "$$ER: E100,INVALID CODE OR EXPRESSION ENTERED",
  "$$ER: E100,NO WORLDSCOPE DATA FOR THIS CODE",
  "$$ER: 4540,NO DATA VALUES FOUND"
)

mod_code <- c("$$ER: 2382,NO DIVIDENDS")

########### define static params ###########
data_dir <- "./formatted_data/"

d.origin   <- as.Date("1899-12-30")
d.start    <- as.Date("2000-01-01")
d.end      <-
  as.Date("2016-12-31") # 2017-01-02 oddly has NA for last date
s.year     <- seq(d.start, d.end, by = "years")
s.month    <- seq(d.start, d.end, by = "month")
purge.col.swe <- -c(1:2)
purge.col.ds  <- -c(1:4)
purge.row.ds  <- -c(1:9)

n_portfolios  <- 6

########### import data ###########

ff_d_raw <-
  read.csv("raw_data/sweHouseofFinance/FF4F_daily.csv", header = TRUE)
ff_d <- xts(
  x = ff_d_raw[, c(-1,-2)],
  order.by = as.Date(ff_d_raw$day, format = "%d %b %Y"),
  col_names = TRUE
)
ff_d <- subset(ff_d, index(ff_d) >= d.start & index(ff_d) <= d.end)
s.day_ff <- index(ff_d)

save(file = sprintf("./%s%s.RData", data_dir, "ff_d"), ff_d)

# import data from datastream

force_reload_data <- 0

new.format_datastream_data <-
  function(data,
           fin_type,
           data_dir,
           force_reload_data) {
    data_path <- sprintf("%s%s.RData", data_dir, fin_type)
    if (!file.exists(data_path) | force_reload_data) {
      data1 <- data[purge.row.ds, purge.col.ds]
      data_header <- data[-c(2:nrow(data)), purge.col.ds]
      
      data1[data1 == ""] <- NA # change "NA" str to NA
      data1[data1 == "NaN"] <- NA
      data3 <- type.convert(data1)
      
      idx_val   <- seq(2, ncol(data3), 2) # data is in second col
      idx_dates <-
        seq(1, ncol(data3), 2) # dates are in first col, with origin d.origin
      
      data_header[idx_dates] <-
        paste(data_header[idx_dates], "date")
      
      data4 <- data3[idx_val]
      colnames(data4) <- data_header[idx_val]
      save(file = data_path, data4)
    } else {
      load(data_path)
    }
    new.format_datastream_data <- data4
  }

start_time = Sys.time()

#total assets (yr)
nasdaq_tickers <-
  read_excel("raw_data/nasdaq_tickers.xlsx", col_names = FALSE)
ta_raw <-
  read_excel("raw_data/CLME - TA 00-16.xlsx", col_names = FALSE)
ta <-
  new.format_datastream_data(ta_raw, "ta", data_dir, force_reload_data)
# total liabilities (yr)
tl_raw <-
  read_excel("raw_data/CLME - TL 00-16.xlsx", col_names = FALSE)
tl <-
  new.format_datastream_data(tl_raw, "tl", data_dir, force_reload_data)
# net income before extraordinary items (yr)
ni_raw <-
  read_excel("raw_data/CLME - NI 00-16.xlsx", col_names = FALSE)
ni <-
  new.format_datastream_data(ni_raw, "ni", data_dir, force_reload_data)
# market value (m)
mv_raw <-
  read_excel("raw_data/CLME - MV 00-16.xlsx", col_names = FALSE)
mv <-
  new.format_datastream_data(mv_raw, "mv", data_dir, force_reload_data)
# return index (d)
ri_raw <-
  read_excel("raw_data/CLME - RI 00-16.xlsx", col_names = FALSE)
ri <-
  new.format_datastream_data(ri_raw, "ri", data_dir, force_reload_data)
# outstanding shares (y)
os_raw <-
  read_excel("raw_data/CLME - Shares 00-16.xlsx", col_names = FALSE)
os <-
  new.format_datastream_data(os_raw, "os", data_dir, force_reload_data)

# get day sequence
day_seq <- ri_raw[purge.row.ds, purge.col.ds]
day_seq <- as.numeric(day_seq[[3]])
s.day <- as.Date.numeric(day_seq, origin = d.origin)

end_time = Sys.time()
duration <- end_time - start_time




# handle error codes


error_ticker_list <- c("35731X") #NULL
dividend_error_list <- NULL

new.get_no_data_tickers <- function(data, error_ticker_list, str) {
  n_datapoints <- colSums(!is.na(data))
  tickers_no_data <-
    colnames(data[n_datapoints == 1]) # all companies with no data
  remove_companies <- tickers_no_data
  remove_companies_bool <- !remove_companies %in% error_ticker_list
  
  # get companies with only one datapoint
  if (str == "ri") {
    n_na <- colSums(is.na(data))
    tickers_no_na <-
      colnames(data[n_na == 0]) # all companies with no na
    data_no_na <- data[, tickers_no_na]
    only_one_num_bool <- NULL
    for (o in 1:ncol(data_no_na)) {
      bool <-
        isTRUE(all.equal(max(data_no_na[, o]) , min(data_no_na[, o]))) # only one number
      if (bool) {
        only_one_num_bool[length(only_one_num_bool) + 1] <-
          colnames(data_no_na[, o])
      }
    }
    tickers_one_num <- colnames(data[only_one_num_bool])
    remove_companies <- tickers_one_num
    remove_companies_bool <-
      !remove_companies %in% error_ticker_list
  }
  
  tickers_add <- remove_companies[remove_companies_bool]
  tickers_add_length <- sum(remove_companies_bool)
  error_ticker_list_length <- length(error_ticker_list)
  
  if (!rlang::is_empty(tickers_add)) {
    error_ticker_list[(1 + error_ticker_list_length):(error_ticker_list_length + tickers_add_length)] <-
      tickers_add
  }
  new.get.no.data.tickers <- error_ticker_list
}

new.get_dividend_error_tickers <-
  function(data, dividend_error_list, mod_code) {
    for (err in 1:length(data)) {
      if (identical(mod_code, as.vector(unlist(data[1, err])))) {
        dividend_error_list[err] <- colnames(data[err])
      }
    }
    new.get_dividend_error_tickers <- dividend_error_list
  }

# remove error tickers
file1 <- "error_ticker_list7"
if (!file.exists(sprintf("%s%s.RData", data_dir, file1)) |
    force_reload_data) {
  error_ticker_list1 <-
    new.get_no_data_tickers(ta, error_ticker_list, "ta")
  error_ticker_list2 <-
    new.get_no_data_tickers(tl, error_ticker_list1, "tl")
  error_ticker_list3 <-
    new.get_no_data_tickers(ni, error_ticker_list2, "ni")
  error_ticker_list4 <-
    new.get_no_data_tickers(mv, error_ticker_list3, "mv")
  error_ticker_list6 <-
    new.get_no_data_tickers(ri, error_ticker_list4, "ri")
  error_ticker_list7 <-
    new.get_no_data_tickers(os, error_ticker_list6, "os")
  save(file = sprintf("%s%s.RData", data_dir, file1), error_ticker_list7)
  
} else {
  load(sprintf("%s%s.RData", data_dir, "error_ticker_list7"))
}

# remove error tickers
ta <- ta[, !colnames(ta) %in% error_ticker_list7]
tl <- tl[, !colnames(tl) %in% error_ticker_list7]
ni <- ni[, !colnames(ni) %in% error_ticker_list7]
mv <- mv[, !colnames(mv) %in% error_ticker_list7]
ri <- ri[, !colnames(ri) %in% error_ticker_list7]
os <- os[, !colnames(os) %in% error_ticker_list7]

ta_x <- xts(x = ta, order.by = s.year)
tl_x <- xts(x = tl, order.by = s.year)
ni_x <- xts(x = ni, order.by = s.year)
mv_x <- xts(x = mv, order.by = s.month)
ri_x <- xts(x = ri, order.by = s.day)
os_x <- xts(x = os, order.by = s.year)


# # handle dead tickers
list_dead_in_period <- vector()

exchange_keep <- c("Nasdaq Stockholm AB", "Spotlight Stk.Mkt.")
exchange_error_list <- NULL
ri_d <- ri_raw[c(1, 2, 7), purge.col.ds]
ri_d <- ri_d[, seq(2, ncol(ri_d), 2)]

# list nasdaq companies
nasdaq_list <- NULL
n_tickers <- ncol(ri_x)
for (dd in 1:ncol(ri_x)) {
  ticker <- colnames(ri_x[, dd])
  idx <- which(grepl(ticker, ri_d)) # idx where string is
  str <- unlist(ri_d[2, idx])
  exch <- unlist(ri_d[3, idx])
  # remove if exchange is not approved
  if (!any(exch == exchange_keep)) {
    exchange_error_list[length(exchange_error_list) + 1] <- ticker
  }
  if (exch == exchange_keep[1]) {
    nasdaq_list[length(nasdaq_list) + 1] <- ticker
  }
  
  if (!is.na(str)) {
    date_val <- NULL
    date_str2 <- NULL
    date_str <-
      unlist(strapplyc(str, "\\d+/\\d+/\\d+", simplify = TRUE))
    #
    if (!is.null(date_str)) {
      date_val <- as.Date(date_str, "%d/%m/%y")
    }
    else {
      date_str2 <-
        unlist(strapplyc(str, "\\d+.\\d+.\\d+", simplify = TRUE))
      if (!is.null(date_str2)) {
        date_val <- as.Date(date_str2, "%d.%m.%y")
      }
    }
    if (is.null(date_str2) &
        (is.null(date_str) | identical(date_str, character(0)))) {
      date_val <- "01-01-1995"
    }
    if (is.na(date_val)) {
      date_val <- "01-01-1995"
    }
    if (grepl("DELIST", str)) {
      sprintf("Delist: %s, Date: %s", ticker, date_val)
    }
    
    if (grepl("DEAD", str) | grepl("REDEMPTION", str)) {
      if (date_val >= d.start &  d.end >= date_val) {
        print(sprintf("Checking Dates: %d of %d", dd, n_tickers))
        
        date_ym <-
          format(as.Date(date_val, format = "%Y-%m-%d"), "%Y-%m")
        date_y <- as.character(year(date_val + 365))
        list_dead_in_period[length(list_dead_in_period) + 1] <-
          ticker
        # set data to NA in ri after date
        # day
        ri_x[sprintf("%s/%s", date_val, d.end), dd] <- NA
        # month
        mv_x[sprintf("%s/%s", date_ym, d.end), dd] <- NA
        # year
        os_x[sprintf("%s/%s", date_y, d.end), dd] <- NA
        ta_x[sprintf("%s/%s", date_y, d.end), dd] <- NA
        tl_x[sprintf("%s/%s", date_y, d.end), dd] <- NA
        ni_x[sprintf("%s/%s", date_y, d.end), dd] <- NA
        
      }
    }
  }
}

# remove exchanges not approved
ta_x <- ta_x[, !colnames(ta_x) %in% exchange_error_list]
tl_x <- tl_x[, !colnames(tl_x) %in% exchange_error_list]
ni_x <- ni_x[, !colnames(ni_x) %in% exchange_error_list]
mv_x <- mv_x[, !colnames(mv_x) %in% exchange_error_list]
ri_x <- ri_x[, !colnames(ri_x) %in% exchange_error_list]
os_x <- os_x[, !colnames(os_x) %in% exchange_error_list]

# adjust for dates only found in sweHouseofFinance (s.day_ff)
ri_x <- ri_x[s.day_ff]

nasdaq_tickers <- unlist(nasdaq_tickers)
save(file = sprintf("%s%s.RData", data_dir, "nasdaq_tickers"),
     nasdaq_tickers)
save(file = sprintf("%s%s.RData", data_dir, "nasdaq_list"), nasdaq_list)
save(file = sprintf("%s%s.RData", data_dir, "ta_x"), ta_x)
save(file = sprintf("%s%s.RData", data_dir, "tl_x"), tl_x)
save(file = sprintf("%s%s.RData", data_dir, "ni_x"), ni_x)
save(file = sprintf("%s%s.RData", data_dir, "mv_x"), mv_x)
save(file = sprintf("%s%s.RData", data_dir, "ri_x"), ri_x)
save(file = sprintf("%s%s.RData", data_dir, "os_x"), os_x)
