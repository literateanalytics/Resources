library(ggplot2)
library(dplyr)

directors <- read.table('/Users/pgowey/Analysis/Directors/directors.txt',sep=',',header=FALSE,stringsAsFactors=FALSE)
names(directors)  <- c("wins","name","nominations")
directors <- directors[,c(2,3,1)]
directors$win_pct <- round(directors$wins/directors$nominations,2)
#clean up this guy's name
directors$name <- gsub('Miloš Forman','Milos Forman',directors$name)
#take out JL because he isn't important enough on IMDB
directors <- directors[-grep('Josh.*Logan.*',directors$name),]
directors$rank <- rank(-directors$nominations,ties.method='random')


films <- read.table('/Users/pgowey/Analysis/directors/director_film_titles.txt',sep='\t',header=TRUE,quote='',fill=TRUE,stringsAsFactors=FALSE)

films$year <- gsub('.*([0-9]{4}).*','\\1',films$release_date)
films$year <- as.numeric(films$year)

dir_agg <- summarise(group_by(films,director,year),avg_rating=mean(rating,na.rm=TRUE))


years<-c(1900:2014)
years <- data.frame(years)
names(years) <- c('year')
class(years) <- c('data.frame')
dir_years <- merge(years,select(directors,name,rank),all=TRUE)

m <- 0
n <- 36

dir_all_time <- left_join(years,dir_agg,by=c('year'='year'))
dir_all_time <- left_join(dir_all_time,select(directors,name,rank),by=c('director'='name'))
dir_top_12 <- filter(dir_all_time,rank > m & rank <= n)
dir_top_12 <- left_join(filter(dir_years,rank > m & rank <= n),dir_top_12,by=c('year'='year','name'='director'))
# head(filter(dir_all_time,is.na(director)=='FALSE'),20)

# test graphing with one director
# wyler <- filter(dir_all_time,director=='William Wyler')
# wyler <- left_join(years,wyler,by=c('year'='year'))
# qplot(data=wyler,x=year,weight=avg_rating,binwidth=1)


# facet graph for the top 12
qplot(data=dir_top_12,x=year,weight=avg_rating,binwidth=1)+facet_wrap(~name)+ylim(0,10)
