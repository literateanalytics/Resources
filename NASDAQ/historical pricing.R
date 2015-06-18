# install.packages("devtools")
# library(devtools)
# install_github('quandl/R-package')
library(Quandl)
# install.packages('ggplot2')
library(ggplot2)

nasdaq <- read.csv('https://s3.amazonaws.com/quandl-static-content/Ticker+CSV%27s/Stock+Exchanges/NASDAQ.csv', stringsAsFactors = F)
names(nasdaq) <- tolower(names(nasdaq))

Quandl.auth('2P17_cV_Fm-yKU9mtTBB')
a <- nasdaq[which(nasdaq$ticker == 'AMZN'), 'code']
amzn <- Quandl(a)

ggplot(data = amzn, aes(x = Date, y = Close)) + geom_line()
