catan_results <- fread('/Users/pgowey/GitHub/Resources/Catan scripts/agg_2.txt')

# The palette with grey:
#cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
cbPalette <- c("gray47", "darkorange4")
# The palette with black:
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

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

ggplot(data=filter(catan_results2,draft_type=='Serpentine'),aes(x=player_pos,weight=prod_dev,fill=test))+
  geom_histogram()+
  facet_wrap(~num_players,nrow=1)+
  scale_fill_manual(values=cbPalette,guide=FALSE)+
  labs(title='Resource-adjusted production power by party size')+
  ylab('Player position in draft')+
  xlab('Deviation from average')
