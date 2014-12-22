library(ggplot2)
library(dplyr)

################################################################
# import list of academy award winning or nominated directors
directors <- read.table('/Users/pgowey/Analysis/Directors/directors.txt',sep=',',header=FALSE,stringsAsFactors=FALSE)
names(directors)  <- c("wins","name","nominations")
directors <- directors[,c(2,3,1)]
directors$win_pct <- round(directors$wins/directors$nominations,2)
#clean up this guy's name
directors$name <- gsub('Miloš Forman','Milos Forman',directors$name)
#take out JL because he isn't important enough on IMDB
directors <- directors[-grep('Josh.*Logan.*',directors$name),]
directors$rank <- rank(-directors$nominations,ties.method='random')

################################################################
# read full filmography of all directors, numerate year
films <- read.table('/Users/pgowey/Analysis/directors/director_film_titles.txt',sep='\t',header=TRUE,quote='',fill=TRUE,stringsAsFactors=FALSE)
films <- filter(films,is_film=='TRUE')
films$year <- gsub('.*([0-9]{4}).*','\\1',films$release_date)
films$year <- as.numeric(films$year)

################################################################
# aggregate director films and filter set to directors with median production year > 1960 (modern era)
dir_agg <- summarise(group_by(films,director,year),avg_rating=mean(rating,na.rm=TRUE))
dir_filter <- summarise(group_by(films,director),overall_rating=mean(rating,na.rm=TRUE),med_year=median(year,na.rm=TRUE)) %>% filter(med_year>1960) %>% select(director,overall_rating)
dir_filter$overall_ranking <- rank(-dir_filter$overall_rating,ties.method='min')
dir_agg <- inner_join(dir_agg,dir_filter,by=c('director'='director'))

################################################################
# define years object for creating a complete timeline for all directors
years <- c((min(dir_agg$year,na.rm=TRUE)):2014)
years <- data.frame(years)
names(years) <- c('year')
class(years) <- c('data.frame')
# using base package because dplyr has no full outer join option
directors <- inner_join(directors,dir_filter,by=c('name'='director'))
years <- merge(years,select(directors,name,rank,overall_ranking),all=TRUE)

################################################################
# define limits for top n directors, for now not excluding anyone
# note... seems to be a problem with the dynamic upper bound. shouldn't use.
m <- 0
n <- 24
# n <- nrow(distinct(select(years,name)))
years <- select(filter(years,overall_ranking > m & overall_ranking <= n),year,name,overall_ranking)
# years <- select(years,year,name,overall_ranking)
dir_top_n <- left_join(years,select(dir_agg,-overall_ranking),by=c('year'='year','name'='director'))
# redefine name as a factor to influence ordering of facet grid later
dir_top_n_reorder <- select(arrange(dir_top_n,overall_ranking),name)
dir_top_n$name <- factor(dir_top_n$name,dir_top_n_reorder$name)




################################################################
# start graphing

# facet graphs for the top n defined above
# limit should be 9.2 as this is the top score achieved by any film on IMDB
# qplot(data=dir_top_12,x=year,weight=avg_rating,binwidth=1)+facet_wrap(~name)+ylim(0,9.2)+scale_fill_manual(values=cbPalette,guide=FALSE)

# facet of dir avg ratings by year with trendline
# ggplot(data=dir_top_n,aes(x=year,y=avg_rating,colour=avg_rating))+
#   geom_point()+
#   scale_colour_gradient(low="#EBF0E6", high="#142900",guide=FALSE)+
#   ylim(min(dir_top_n$avg_rating)-0.1,max(dir_top_n$avg_rating)+0.1)+
#   xlim(min(dir_top_n$year)-1,max(dir_top_n$year)+1)+
#   facet_wrap(~name,ncol=4)+
#   geom_smooth(method='loess',se=FALSE,colour='#000000')+
#   labs(x='Year',y='Average Film Rating',title='IMDB Film Ratings for Top 24 Modern-Era Oscar Nominated Directors')+
#   theme(plot.title = element_text(size=18, face="bold", vjust=2,hjust=0))

# this is the final graph I used - slightly diff version from the one above.
# shows all films instead of films agg'd to a year. 
dir_top_n <- left_join(years,films,by=c('year'='year','name'='director'))
dir_top_n_reorder <- select(arrange(dir_top_n,overall_ranking),name)
dir_top_n$name <- factor(dir_top_n$name,dir_top_n_reorder$name)

ggplot(data=dir_top_n,aes(x=year,y=rating,colour=rating))+
  geom_point()+
  scale_colour_gradient(low="#EAF3EE", high="#16452B",guide=FALSE)+
#   scale_color_manual(values=c('#EBF0E6'),guide=FALSE)
  ylim(min(dir_top_n$rating)-0.1,max(dir_top_n$rating)+0.1)+
  xlim(min(dir_top_n$year)-1,max(dir_top_n$year)+1)+
  facet_wrap(~name,ncol=4)+
  geom_smooth(method='loess',se=FALSE,colour='seagreen')+
  labs(x='Year',y='Average Film Rating',title='IMDB Film Ratings for Top 24 Modern-Era Oscar Nominated Directors')+
  theme(plot.title = element_text(size=18, face="bold", vjust=2,hjust=0))


# scatterplot directors by overall avg rating?
# '#009E73','#E69F00'
# scale_colour_gradient(low="#009E73", high="#E69F00",guide=FALSE)+
# scale_colour_gradient(low="#EBF0E6", high="#142900",guide=FALSE)+
