library(ggplot2)
library(data.table)
library(sqldf)

a <- fread('C:/Users/gopat/Documents/GitHub/Resources/Catan scripts/aggregate_results.txt')
head(a)
b1 <- a
b2 <- a
b1$draft_type <- c('Sequential')
b2$draft_type <- c('Serpentine')
b1$serpentine_player <- NULL
names(b1)[2] <- c('player_pos')
b2$sequential_player <- NULL
names(b2)[2] <- c('player_pos')
c <- rbind(b1,b2)

d <- aggregate(sum~player_pos+num_players+draft_type,data=c,sum)
d$avg_prod <- d$sum/1000
names(d)[4] <- c('total_prod')
write.table(d,file='C:/Users/gopat/Documents/GitHub/Resources/Catan scripts/agg_2.txt',sep='\t',col.names=TRUE,row.names=FALSE,quote=FALSE)
