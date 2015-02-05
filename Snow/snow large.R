library(XML)
library(ggplot2)
library(reshape2)

# define year and resort for use in the link, then parse, then grab the table we want
years <- c(2004:2010)
snow <- NULL

for (year in years) {
    resort <- c("stevens-pass-resort")
    snow_link <- paste("http://www.onthesnow.com/washington/",resort,"/historical-snowfall.html?&y=",year,"&q=snow&v=list",sep="")
    html <- htmlParse(snow_link)
    nodes = getNodeSet(html,"//table[@class='snowfall']")
    snow_raw <- readHTMLTable(nodes[[1]])
    names(snow_raw) <- c('date','daily_snowfall','cum_snowfall','base_depth')
    # add unit of measure fields because we're going to take them out
    snow_raw$daily_uom <- gsub('^.*[0-9]+\\s*([A-z]+).*','\\1',snow_raw$daily_snowfall)
    snow_raw$cum_uom <- gsub('^.*[0-9]+\\s*([A-z]+).*','\\1',snow_raw$cum_snowfall)
    snow_raw$base_uom <- gsub('^.*[0-9]+\\s*([A-z]+).*','\\1',snow_raw$base_depth)
    # remove characters from snowfall and convert to numeric
    snow_raw$daily_snowfall <- gsub('(([0-9])+)\\s*([A-z]+).*','\\1',snow_raw$daily_snowfall)
    snow_raw$cum_snowfall <- gsub('(([0-9])+)\\s*([A-z]+).*','\\1',snow_raw$cum_snowfall)
    snow_raw$base_depth <- gsub('(([0-9])+)\\s*([A-z]+).*','\\1',snow_raw$base_depth)
    snow_raw$daily_snowfall <- as.numeric(as.character(snow_raw$daily_snowfall))
    snow_raw$cum_snowfall <- as.numeric(as.character(snow_raw$cum_snowfall))
    snow_raw$base_depth <- as.numeric(as.character(snow_raw$base_depth))
    # clean up the date and format as date
    snow_raw$date <- gsub(' {2,}',' ',snow_raw$date)
    snow_raw$month <- gsub('^([A-z]+)\\s+([0-9]+).*,\\s+([0-9]+)','\\1',snow_raw$date)
    snow_raw$day <- gsub('^([A-z]+)\\s+([0-9]+).*,\\s+([0-9]+)','\\2',snow_raw$date)
    snow_raw$year <- gsub('^([A-z]+)\\s+([0-9]+).*,\\s+([0-9]+)','\\3',snow_raw$date)
    snow_raw$date <- as.Date(paste(snow_raw$day,tolower(snow_raw$month),snow_raw$year),'%d %b %Y')
    # remove the three date column we just created - but actually we'll need these later
    # snow_raw <- snow_raw[,c(1:(ncol(snow_raw)-3))]
    snow <- rbind(snow,snow_raw)
}


ggplot(data=snow,aes(x=date,weight=base_depth))+geom_bar(binwidth=7)+facet_wrap(~year)
