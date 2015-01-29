library(data.table)
library(ggplot2)
library(reshape2)
library(dplyr)

monthnums <- function(x) {
  x <- toupper(x)
  switch(x,'JAN'=1,'FEB'=2,'MAR'=3,'APR'=4,'MAY'=5,'JUN'=6,'JUL'=7,'AUG'=8,'SEP'=9,'OCT'=10,'NOV'=11,'DEC'=12)
}

isregion <- function(x) {
  x <- toupper(x)
  switch(x,'WORLD'='Y','MIDDLE EAST'='Y','NORTH AMERICA'='Y','EURASIA'='Y','AFRICA'='Y','ASIA & OCEANIA'='Y','CENTRAL & SOUTH AMERICA'='Y','EUROPE'='Y')
}

# data from http://www.eia.gov/cfapps/ipdbproject/iedindex3.cfm?tid=50&pid=53&aid=1&cid=&syid=1994&eyid=2014&freq=M&unit=TBPD
oil <- fread('/Users/pgowey/Analysis/Oil/oil.txt')
setnames(oil,1,c('country'))
# loop to assign years to dates
for (i in 2:(((ncol(oil)-1)/12)+1)) {
  year <- 1994 + (i-2)
  k <- 2+(12*(i-2))
  l <- k+11
  cols <- names(oil)[k:l]
  cols <- gsub(' ','',cols)
  dates <- paste(tolower(cols),year,sep="_")
#   for (j in cols) {
#     dates_tmp <- monthnums(j)
#     dates_tmp <- paste(year,dates_tmp,1,sep='/')
#     dates <- c(dates,dates_tmp)
#   }
  dates <- data.frame(dates)
  setnames(oil,k:l,as.character(dates$dates))
}

# for some reason have to manually define object as data.frame
# otherwise can't use bracket notation - data.table issue?
# loop over all columns to make them numeric
class(oil) <- c('data.frame')
for (i in 2:ncol(oil)) {
  oil[,i] <- gsub('( |,|\\$)','',oil[,i])
  oil[,i] <- as.numeric(oil[,i])
}

# loop to prepare taking out the regional data
oil$is_region <- NULL
for (i in 1:nrow(oil)) {
  oil[i,'country'] <- gsub('\\s*(\\S+.*\\S)\\s*','\\1',oil[i,'country'])
  oil[i,'is_region'] <- ifelse(is.character(isregion(oil[i,'country'])),'Y','N')
}

oil$total_production <- apply(oil[,2:(ncol(oil)-1)],1,'sum',na.rm=TRUE)
oil_regions <- filter(oil,is_region=='Y')
oil <- filter(oil,is_region=='N')
top_countries <- head(select(arrange(oil,desc(total_production)),country),8)
oil$total_production <- NULL
oil$is_region <- NULL
oil_regions$total_production <- NULL
oil_regions$is_region <- NULL

top_oil <- inner_join(top_countries,oil)
flip_oil <- data.frame(t(top_oil))
flip_oil$month <- rownames(flip_oil)
names(flip_oil) <- gsub(' ','_',gsub('(\\(|\\))','',as.character(t(flip_oil[1,]))))
rownames(flip_oil) <- NULL
flip_oil <- data.frame(flip_oil)
flip_oil <- flip_oil[-1,]
flip_oil <- flip_oil[,c(ncol(flip_oil),1:(ncol(flip_oil)-1))]
names(flip_oil)[1] <- c('month')
for (i in 2:ncol(flip_oil)) {
  flip_oil[,i] <- gsub('( |,|\\$)','',flip_oil[,i])
  flip_oil[,i] <- as.numeric(as.character(flip_oil[,i]))
}


##############################################################
# organize data and transform month to date for plot
oil_plot <- melt(flip_oil,id=c('month'))
oil_plot$month_date <- substr(oil_plot$month,1,3) #first 3 characters are month
oil_plot$year_date <- substr(oil_plot$month,5,8) #last 4 characters are year
oil_plot$month_num <- NULL
for (i in 1:nrow(oil_plot)) {
  oil_plot[i,'month_num'] <- monthnums(oil_plot[i,'month_date'])
}
oil_plot$date <- as.Date(paste(oil_plot$year_date,oil_plot$month_num,1,sep='/'))
ggplot(data=oil_plot,aes(x=date,y=value,group=variable,color=variable))+
  geom_line()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1),plot.title=element_text(size=18,face='bold',vjust=2,hjust=0))+
  labs(x='Date of production',y='Barrels per day (000s)',title='Total oil production by country')







############################################################
# do all the same things for regional data

flip_regions <- data.frame(t(oil_regions))
flip_regions$month <- rownames(flip_regions)
names(flip_regions) <- gsub(' ','_',gsub('(\\(|\\))','',as.character(t(flip_regions[1,]))))
rownames(flip_regions) <- NULL
flip_regions <- data.frame(flip_regions)
flip_regions <- flip_regions[-1,]
flip_regions <- flip_regions[,c(ncol(flip_regions),1:(ncol(flip_regions)-1))]
names(flip_regions)[1] <- c('month')
for (i in 2:ncol(flip_regions)) {
  flip_regions[,i] <- gsub('( |,|\\$)','',flip_regions[,i])
  flip_regions[,i] <- as.numeric(as.character(flip_regions[,i]))
}

regions_plot <- melt(flip_regions,id=c('month'))
regions_plot$month_date <- substr(regions_plot$month,1,3) #first 3 characters are month
regions_plot$year_date <- substr(regions_plot$month,5,8) #last 4 characters are year
regions_plot$month_num <- NULL
for (i in 1:nrow(regions_plot)) {
  regions_plot[i,'month_num'] <- monthnums(regions_plot[i,'month_date'])
}
regions_plot$date <- as.Date(paste(regions_plot$year_date,regions_plot$month_num,1,sep='/'))
regions_plot$variable <- gsub('_',' ',gsub('\\._','& ',regions_plot$variable))
ggplot(data=filter(regions_plot,variable!='World'),aes(x=date,y=value,group=variable,color=variable))+
  geom_line()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1),plot.title=element_text(size=18,face='bold',vjust=2,hjust=0))+
  labs(x='Date of production',y='Barrels per day (000s)',title='Total oil production by country')
