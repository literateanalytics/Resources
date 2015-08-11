# install.packages("devtools")
library(devtools)
# install_github('quandl/R-package')
library(Quandl)
# install.packages('ggplot2')
library(ggplot2)
library(RCurl)
library(zoo)
# install.packages('lubridate')
library(lubridate)
library(reshape2)
library(dplyr)
# install.packages('data.table')
library(data.table)

a <- getURL("https://s3.amazonaws.com/quandl-static-content/Ticker+CSV%27s/Stock+Exchanges/NASDAQ.csv")
nasdaq <- read.csv(text = a, stringsAsFactors = F)
# nasdaq <- read.csv("https://s3.amazonaws.com/quandl-static-content/Ticker+CSV%27s/Stock+Exchanges/NASDAQ.csv", stringsAsFactors = F)
names(nasdaq) <- tolower(names(nasdaq))

Quandl.auth('2P17_cV_Fm-yKU9mtTBB')  # API key
tickers <- nasdaq[which(nasdaq$ticker %in% c('AMZN', 'AAPL', 'NFLX', 'MSFT', 'GOOG', 'TSLA')), 'ticker']
# tickers <- nasdaq[which(nasdaq$ticker %in% c('AMZN','NFLX')), 'ticker']
# tickers <- nasdaq$ticker %>% head(6)
hist_pricing <- NULL
for (i in tickers) {
    cat('getting historical price info for stock ticker:', i, '\n')
    tmp <- Quandl(paste('GOOG/NASDAQ_', i, sep = ''))
    tmp$ticker <- i
    setnames(tmp, tolower(names(tmp)))
    
    nshare <- Quandl(paste('SEC/',i,'_COMMONSTOCKSHARESOUTSTANDING_Q', sep = ''))
    nshare$ticker <- i
    nshare <- nshare[which(nshare$Date == max(nshare$Date)), ]
    nshare <- data.frame(ticker = nshare$ticker, num_shares = nshare$Value)
    
    tmp <- arrange(tmp, date)
    tmp <- inner_join(tmp, num_shares, by = c('ticker' = 'ticker'))
    tmp$market_cap_mm <- with(tmp, (close * num_shares)/1000000)
    tmp$rolling_avg_1yr <- rollmean(tmp$market_cap_mm, k = 365, fill = NA, align = 'right')
    tmp$mkt_trend <- rollmean(tmp$market_cap_mm, k = 14, fill = NA, align = 'right')
    tmp$supersmooth <- rollmean(tmp$market_cap_mm, k = 365*2, fill = NA, align = 'right')
    tmp$share_value_diff <- with(tmp, (mkt_trend*1000000)/num_shares - (rolling_avg_1yr*1000000)/num_shares)
    l1 <- lag(tmp$share_value_diff, 1)
    l2 <- lag(tmp$share_value_diff, 4)
    tmp$lag_svd <- lag(tmp$share_value_diff, 1*5)  #it would be much more accurate to join on date-7 = date for a lag price
    tmp$lag_svd_2w <- lag(tmp$share_value_diff, 2*5)
    tmp$date_num <- with(tmp, as.numeric(date - min(date)))
    
    hist_pricing <- rbind(hist_pricing, tmp)
}


hist_pricing$lag_svd_diff <- with(hist_pricing, share_value_diff - lag_svd)
hist_pricing$lag_svd_2w_diff <- with(hist_pricing, lag_svd - lag_svd_2w)
hist_pricing$is_buy_period <- ifelse(hist_pricing$share_value_diff < 0 & hist_pricing$lag_svd_diff > 0, 'Y', 'N')

hist_pricing$is_buy_candidate <- ifelse(hist_pricing$is_buy_period == 'Y' & hist_pricing$lag_svd_diff > 0 & hist_pricing$lag_svd_2w_diff < 0 & hist_pricing$date_num >= 365*8, 'Y', 'N')
buy_dates <- hist_pricing %>% filter(is_buy_candidate == 'Y') %>% select(ticker, date, date_num)
bb <- filter(buy_dates, ticker == 'NFLX') %>% select(date_num)

nflx <- filter(hist_pricing, ticker == 'NFLX')
nflx_graph <- nflx
# nflx_graph <- filter(nflx, date_num >= 3400, date_num <= 3900)
nflx_buy_dates <- filter(nflx_graph, is_buy_candidate== 'Y')
nflx_buy_dates <- nflx_buy_dates$date_num

ggplot(data = nflx_graph, aes(x = date_num, y = share_value_diff, color = is_buy_period)) +
    geom_point() +
    geom_hline(yintercept = 0)+
    geom_vline(xintercept = nflx_buy_dates, aes(size = 1))

h <- select(hist_pricing, date, ticker, mkt_trend, rolling_avg_1yr, supersmooth) %>% melt(id = c('date', 'ticker'))
ggplot(data = filter(h, date > (today() - 365*5)), aes(x = date, y = value, color = variable)) + 
    geom_line() + 
    facet_wrap(~ ticker, scales = 'free_y', ncol = 2) +
    theme(legend.position = 'bottom') 

h2 <- select(hist_pricing, date, ticker, share_value_diff)
h2 <- melt(h2, id = c('date', 'ticker'))
ggplot(data = filter(h2, date > (today() - 365*1)), aes(x = date, y = value, color = variable)) +
    geom_line() +
    facet_wrap(~ ticker, scales = 'free_y', ncol = 2) +
    theme(legend.position = 'bottom')+
    geom_hline(yintercept = 0) #+ ylim(0-max(h2$value, na.rm = T), max(h2$value, na.rm = T))






a <- as.Date(paste(2015, 5, c(1:12), sep = '-'))
b <- c(1:12)^2
c <- sqrt(c(1:12))
d <- data.frame(a, b, c)
ggplot(data = melt(e, id = c('a')), aes(x = a, y = value, color = variable)) + geom_line()
e <- zoo(d, order.by = a)
