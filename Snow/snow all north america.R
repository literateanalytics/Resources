library(XML)
library(ggplot2)
library(reshape2)
library(dplyr)

# define year and resort for use in the link, then parse, then grab the table we want
years <- c(2004:2015)
snow <- NULL

# get resort names and locations in North America
north_america <- c('http://www.onthesnow.com/northern-america/profile.html')
north_america_parse <- htmlParse(north_america)
north_america_links <- xpathSApply(north_america_parse,"//tr/td[@class='rLeft a']/div[@class='name']/a[@href]",xmlAttrs)
north_america_links <- data.frame(t(unlist(north_america_links)))
north_america_links$resort <- gsub('/(\\S+/\\S+)/.*','\\1',north_america_links$href)

# pull daily snow report data for each NA resort for last 10 years
# for (i in 1:length(resorts)) {
# for (i in 1:50) is for testing purposes so we aren't trying all resorts every time - same as years[1:2]
for (i in 1:nrow(north_america_links)) {
  for (year in years) {
    resort_name <- as.character(north_america_links$title[i])
    cat('working on year',year,'out of',max(years),'for resort number',i,'out of',nrow(north_america_links),'-',resort_name,'\n',sep=' ')
    snow_link <- paste("http://www.onthesnow.com/",north_america_links$resort[i],"/historical-snowfall.html?&y=",year,"&q=snow&v=list",sep="")
    html <- htmlParse(snow_link)
    nodes = getNodeSet(html,"//table[@class='snowfall']")
    snow_raw <- readHTMLTable(nodes[[1]])
    if (length(snow_raw) > 0) {
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
        snow_raw$resort <- resort_name
        snow_raw$state <- gsub('(.*)/.*','\\1',north_america_links$resort[i])
        snow_raw$state <- gsub('-',' ',snow_raw$state)
        snow_raw$state <- gsub("\\b([a-z])([a-z]+)","\\U\\1\\L\\2",snow_raw$state,perl=TRUE)
        snow_raw$region <- switch(snow_raw$state,
               'Colorado' = 'Mountain West',
               'British Columbia' = 'British Columbia, CA',
               'California' = 'California',
               'New York' = 'Northeast',
               'Utah' = 'Mountain West',
               'Michigan' = 'Midwest',
               'Vermont' = 'Northeast',
               'Washington' = 'Northwest',
               'Montana' = 'Northwest',
               'Idaho' = 'Northwest',
               'Oregon' = 'Northwest',
               'Quebec' = 'Quebec',
               'New Hampshire' = 'Northeast',
               'Pennsylvania' = 'Northeast',
               'Wyoming' = 'Mountain West',
               'Alberta' = 'Alberta',
               'New Mexico' = 'Southwest',
               'Wisconsin' = 'Midwest',
               'Minnesota' = 'Midwest',
               'Maine' = 'Northeast',
               'Alaska' = 'Alaska',
               'Ontario' = 'Ontario',
               'Massachusetts' = 'Northeast',
               'West Virginia' = 'Appalachia',
               'Ohio' = 'Appalachia',
               'Nevada' = 'Southwest',
               'North Carolina' = 'Appalachia',
               'Connecticut' = 'Northeast',
               'Arizona' = 'Southwest',
               'South Dakota' = 'Midwest',
               'Illinois' = 'Midwest',
               'Maryland' = 'Appalachia',
               'Indiana' = 'Appalachia',
               'Virginia' = 'Appalachia',
               'Iowa' = 'Appalachia',
               'New Jersey' = 'Northeast',
               'Missouri' = 'Appalachia',
               'Tennessee' = 'Applachia',
               'Rhode Island' = 'Northeast'
        )
        snow <- rbind(snow,snow_raw)
    }
    snow_raw <- NULL
  }
}


snow_plot <- select(snow,month,day,year,daily_snowfall,resort,state)#,cum_snowfall,base_depth)
snow_plot <- melt(snow_plot,id=c('month','day','year','resort','state'))
snow_plot$date <- with(snow_plot,as.Date(paste(day,tolower(month),year),'%d %b %Y'))
snow_plot$month <- factor(snow_plot$month,levels=c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec'))
snow_plot$winter <- with(snow_plot,paste(ifelse(as.numeric(month) < 9, year, as.character(as.numeric(year)+1))))
snow_plot$winter_month <- factor(snow_plot$month,levels=c('Sep','Oct','Nov','Dec','Jan','Feb','Mar','Apr','May','Jun','Jul','Aug'))
snow_plot$region <- rep('',nrow(snow_plot))

for (i in 1:nrow(snow_plot)) {
    snow_plot[i,'region'] <- switch(snow_plot[i,'state'],'Colorado' = 'Mountain West',
                                    'British Columbia' = 'British Columbia, CA',
                                    'California' = 'California',
                                    'New York' = 'Northeast',
                                    'Utah' = 'Mountain West',
                                    'Michigan' = 'Midwest',
                                    'Vermont' = 'Northeast',
                                    'Washington' = 'Northwest',
                                    'Montana' = 'Northwest',
                                    'Idaho' = 'Northwest',
                                    'Oregon' = 'Northwest',
                                    'Quebec' = 'Quebec',
                                    'New Hampshire' = 'Northeast',
                                    'Pennsylvania' = 'Northeast',
                                    'Wyoming' = 'Mountain West',
                                    'Alberta' = 'Alberta',
                                    'New Mexico' = 'Southwest',
                                    'Wisconsin' = 'Midwest',
                                    'Minnesota' = 'Midwest',
                                    'Maine' = 'Northeast',
                                    'Alaska' = 'Alaska',
                                    'Ontario' = 'Ontario',
                                    'Massachusetts' = 'Northeast',
                                    'West Virginia' = 'Appalachia',
                                    'Ohio' = 'Appalachia',
                                    'Nevada' = 'Southwest',
                                    'North Carolina' = 'Appalachia',
                                    'Connecticut' = 'Northeast',
                                    'Arizona' = 'Southwest',
                                    'South Dakota' = 'Midwest',
                                    'Illinois' = 'Midwest',
                                    'Maryland' = 'Appalachia',
                                    'Indiana' = 'Appalachia',
                                    'Virginia' = 'Appalachia',
                                    'Iowa' = 'Appalachia',
                                    'New Jersey' = 'Northeast',
                                    'Missouri' = 'Appalachia',
                                    'Tennessee' = 'Applachia',
                                    'Rhode Island' = 'Northeast'
    )
}

regions <- distinct(select(snow_plot,state,region))
snow <- left_join(snow,regions,by=c('state'='state'))

# write data to a hard copy
snow_data <- snow
snow_plot_data <- snow_plot
write.table(snow_data,'/Users/pgowey/GitHub/Resources/Snow/snow_data.txt',sep='\t',row.names=FALSE,col.names=TRUE,quote=FALSE)
write.table(snow_plot_data,'/Users/pgowey/GitHub/Resources/Snow/snow_plot_data.txt',sep='\t',row.names=FALSE,col.names=TRUE,quote=FALSE)
# other work should probably be done in another script