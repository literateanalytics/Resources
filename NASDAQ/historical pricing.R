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

# a <- getURL("https://s3.amazonaws.com/quandl-static-content/Ticker+CSV%27s/Stock+Exchanges/NASDAQ.csv")
# nasdaq <- read.csv(text = a, stringsAsFactors = F)
nasdaq <- read.csv("https://s3.amazonaws.com/quandl-static-content/Ticker+CSV%27s/Stock+Exchanges/NASDAQ.csv", stringsAsFactors = F)
names(nasdaq) <- tolower(names(nasdaq))

Quandl.auth('2P17_cV_Fm-yKU9mtTBB')  # API key
# tickers <- nasdaq[which(nasdaq$ticker %in% c('AMZN', 'AAPL', 'NFLX', 'MSFT', 'GOOG', 'TSLA')), 'ticker']
tickers <- nasdaq[which(nasdaq$ticker %in% c('AMZN','NFLX')), 'ticker']
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
    tmp$lag_value <- with(tmp, lag(share_value_diff, k = 1, na.pad = T))
    
    hist_pricing <- rbind(hist_pricing, tmp)
}


hist_pricing$lag_value_diff <- with(hist_pricing, share_value_diff - lag_value)
hist_pricing$is_buy_period <- ifelse(hist_pricing$share_value_diff < 0 & hist_pricing$lag_value_diff > 0, 'Y', 'N')
hist_pricing$is_decline_period <- ifelse(hist_pricing$lag_value_diff )

ggplot(data = filter(hist_pricing, ticker == 'NFLX'), aes(x = date, y = share_value_diff, color = is_buy_period)) + geom_point()

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

