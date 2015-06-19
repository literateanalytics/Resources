library(ggplot2)
library(dplyr)

setwd('/Users/pgowey/GitHub/Resources/Snow')
snow <- read.table('snow_data.txt',header=TRUE,quote='',sep='\t')

snow_plot <- select(snow,month,day,year,daily_snowfall,resort,state,region)#,cum_snowfall,base_depth)
snow_plot <- melt(snow_plot,id=c('month','day','year','resort','state','region'))
snow_plot$date <- with(snow_plot,as.Date(paste(day,tolower(month),year),'%d %b %Y'))
snow_plot$month <- factor(snow_plot$month,levels=c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec'))
snow_plot$winter <- with(snow_plot,paste(ifelse(as.numeric(month) < 9, year, as.character(as.numeric(year)+1))))
snow_plot$winter_month <- factor(snow_plot$month,levels=c('Sep','Oct','Nov','Dec','Jan','Feb','Mar','Apr','May','Jun','Jul','Aug'))

snow_plot_filter <- filter(snow_plot,variable=='daily_snowfall',region=='British Columbia, CA' | region=='California' | region=='Midwest' | region=='Mountain West' | region=='Northeast' | region=='Northwest')
ggplot(data=snow_plot_filter,aes(order=winter_month,x=winter_month,weight=value))+
    geom_bar()+
    facet_grid(region~winter)+
    labs(x='Month',y='Total snowfall (inches)',title='North America ski resort snowfall by month')+
    theme(axis.text.x = element_text(angle = 45, hjust = 1),plot.title=element_text(size=18,face='bold',vjust=2,hjust=0))
