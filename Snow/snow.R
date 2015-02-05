library(XML)
library(ggplot2)
library(reshape2)

snow_raw <- readHTMLTable('http://www.whistler.com/weather/history/',stringsAsFactors=FALSE)[1]
snow <- data.frame(snow_raw)
names(snow)[1] <- c('month')
names(snow)[2:ncol(snow)] <- paste('winter',gsub('.*\\.[0-9]+\\.([0-9]+)','\\1',names(snow)[2:ncol(snow)]),sep="_")
totals <- snow[which(snow$month == 'Annual Total'),]
snow <- snow[-which(snow$month == 'Annual Total'),]
snow$month <- factor(s$month,levels=c('November','December','January','February','March','April','May'))

snow_plot <- melt(snow,id=c('month'))
snow_plot$value <- as.numeric(as.character(snow_plot$value))
ggplot(data=snow_plot,aes(x=month,weight=value))+
    geom_bar()+
    facet_wrap(~variable)
