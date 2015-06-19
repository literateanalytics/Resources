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
library(data.table)
a <- getURL("https://s3.amazonaws.com/quandl-static-content/Ticker+CSV%27s/Stock+Exchanges/NASDAQ.csv")
nasdaq <- read.csv(text = a, stringsAsFactors = F)
names(nasdaq) <- tolower(names(nasdaq))

Quandl.auth('2P17_cV_Fm-yKU9mtTBB')  # API key
tickers <- nasdaq[which(nasdaq$ticker %in% c('AMZN', 'AAPL', 'NFLX', 'MSFT', 'GOOG', 'TSLA')), 'ticker']
# tickers <- nasdaq$ticker %>% head(6)
num_shares <- NULL
for (i in tickers) {
    cat('getting shares outstanding data for stock ticker:', i, '\n')
    tmp <- Quandl(paste('SEC/',i,'_COMMONSTOCKSHARESOUTSTANDING_Q', sep = ''))
    tmp$ticker <- i
    tmp2 <- tmp[which(tmp$Date == max(tmp$Date)), ]
    num_shares <- rbind(num_shares, tmp2)
}
num_shares <- data.frame(ticker = num_shares$ticker, num_shares = num_shares$Value)
hist_pricing <- NULL
for (i in tickers) {
    cat('getting historical price info for stock ticker:', i, '\n')
    tmp <- Quandl(paste('GOOG/NASDAQ_', i, sep = ''))
    tmp$ticker <- i
    hist_pricing <- rbind(hist_pricing, tmp)
}
setnames(hist_pricing, tolower(names(hist_pricing)))
hist_pricing <- inner_join(hist_pricing, num_shares, by = c('ticker' = 'ticker'))
hist_pricing$market_cap_mm <- with(hist_pricing, (close * num_shares)/1000000)
hist_pricing$rolling_avg <- rollmean(hist_pricing$market_cap_mm, k = 365, fill = NA, align = "left")

h <- select(hist_pricing, date, ticker, market_cap_mm, rolling_avg) %>% melt(id = c('date', 'ticker'))
ggplot(data = filter(h, date > (today - 365*5)), aes(x = date, y = value, color = variable)) + 
    geom_line() + 
    facet_wrap(~ ticker, scales = 'free_y', ncol = 2) +
    theme(legend.position = 'bottom')

