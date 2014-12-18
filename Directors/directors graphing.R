dir_agg <- summarise(group_by(films,director,year),avg_rating=mean(rating))
dir_agg$year <- as.numeric(dir_agg$year)

years<-c(1900:2014)
years <- data.frame(years)
class(years) <- c('data.frame')

dir_all_time <- left_join(years,dir_agg)
directors$rank <- rank(-directors$nominations)
