library(XML)
library(ggplot2)
library(reshape2)
library(dplyr)

# define year and resort for use in the link, then parse, then grab the table we want
years <- c(2004:2015)
snow <- NULL
washington <- paste('washington/',c('stevens-pass-resort','alpental','mission-ridge','mt-baker','crystal-mountain'),sep='')
bc <- paste('british-columbia/',c('whistler-blackcomb'),sep='')
resorts <- c(washington,bc)

for (resort in resorts) {
    for (year in years) {
        cat('working on link',year,'out of',max(years),'for',resort,'\n',sep=' ')
        snow_link <- paste("http://www.onthesnow.com/",resort,"/historical-snowfall.html?&y=",year,"&q=snow&v=list",sep="")
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
        # assign a resort and a state/region
        snow_raw$resort <- switch(gsub('.*/(.*)','\\1',resort),'stevens-pass-resort'='Stevens'
                                  , 'crystal-mountain'='Crystal'
                                  , 'mt-baker'='Baker'
                                  , 'alpental'='Alpental'
                                  , 'mission-ridge'='Mission Ridge'
                                  , 'whistler-blackcomb'='Whistler Blackcomb')
        snow_raw$state <- switch(gsub('(.*)/.*','\\1',resort),'washington'='Washington'
                                 , 'british-columbia'='British Columbia')
        snow <- rbind(snow,snow_raw)
    }
}


snow_plot <- select(snow,month,day,year,daily_snowfall,resort)#,cum_snowfall,base_depth)
snow_plot <- melt(snow_plot,id=c('month','day','year','resort'))
snow_plot$date <- with(snow_plot,as.Date(paste(day,tolower(month),year),'%d %b %Y'))
snow_plot$month <- factor(snow_plot$month,levels=c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec'))
snow_plot$winter <- with(snow_plot,paste(ifelse(as.numeric(month) < 9, year, as.character(as.numeric(year)+1))))
snow_plot$winter_month <- factor(snow_plot$month,levels=c('Sep','Oct','Nov','Dec','Jan','Feb','Mar','Apr','May','Jun','Jul','Aug'))
ggplot(data=filter(snow_plot,variable=='daily_snowfall'),aes(order=winter_month,x=winter_month,weight=value))+
  geom_bar()+
  facet_grid(resort~winter)+
  labs(x='Month',y='Total snowfall (inches)',title='Northwest ski resort snowfall by month')+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),plot.title=element_text(size=18,face='bold',vjust=2,hjust=0))


