library(data.table)

catan_results <- fread('/Users/pgowey/Analysis/Catan/agg_2.txt')

# The palette with grey:
# cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
cbPalette <- c('#009E73','#E69F00')
# The palette with black:
# cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# To use for fills, add
#scale_fill_manual(values=cbPalette)
# To use for line and point colors, add
#scale_colour_manual(values=cbPalette)

library(dplyr)
avgs <- summarise(group_by(catan_results,num_players,draft_type),sum=sum(total_prod),avg=mean(total_prod))
catan_results <- inner_join(catan_results,avgs)

catan_results2 <- catan_results
catan_results2$prod_dev <- round((catan_results$total_prod-catan_results$avg)/1000,4)
catan_results2$test <- catan_results2$prod_dev >= 0

####################################################################
# main graph for blog post
graph_data <- arrange(filter(catan_results2,draft_type=='Serpentine'),-player_pos)
graph_data$player_pos <- factor(graph_data$player_pos,levels=c('Player4','Player3','Player2','Player1'))
ggplot(data=graph_data,aes(x=player_pos,weight=prod_dev,fill=test))+
  geom_histogram()+
  facet_wrap(~num_players,ncol=1)+
  scale_fill_manual(values=cbPalette,guide=FALSE)+
  labs(title='Resource-adjusted production power by party size')+
  ylab('Deviation from average')+
  xlab('Player position in draft')+
  theme(plot.title = element_text(size=18, face="bold", vjust=2,hjust=0))+
  coord_flip()+
#   ylim(-0.20,0.20)
  ylim(min(graph_data$prod_dev)+(0.1*min(graph_data$prod_dev)),abs(min(graph_data$prod_dev)+(0.1*abs(min(graph_data$prod_dev)))))


####################################################################
# secondary graph for 2-player model
two_p <- filter(catan_results2,num_players=='Two players')
two_p$player_pos <- factor(two_p$player_pos,levels=c('Player2','Player1'))
ggplot(data=two_p,aes(x=player_pos,weight=prod_dev,fill=test))+
  geom_histogram()+
  facet_wrap(~draft_type,ncol=1)+
  scale_fill_manual(values=cbPalette,guide=FALSE)+
  labs(title='Resource-adjusted production power in 2-player games')+
  ylab('Deviation from average')+
  xlab('Player position in draft')+
  theme(plot.title=element_text(size=18,face='bold',vjust=2,hjust=0))+
  coord_flip()#+
#   ylim(min(two_p$prod_dev)+(0.2*min(two_p$prod_dev)),abs(min(two_p$prod_dev)+(0.2*abs(min(two_p$prod_dev)))))

