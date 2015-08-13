library(dplyr)
library(ggplot2)
library(reshape2)
library(RColorBrewer)
setwd('/Users/pgowey/Analysis/Fantasy Football/')
f <- read.table('final ff dump.txt', sep = '\t', header = T, stringsAsFactors = F, quote = '')
names(f) <- gsub('_','.',names(f))
f$rush.yds <- with(f, ifelse(rush.yds < 0, 0, rush.yds))
f$recd.yds <- with(f, ifelse(recd.yds < 0, 0, recd.yds))
ppr <- f %>% filter(position %in% c('WR','TE','RB'))

ppr$calc.pts <- with(ppr, floor(rush.yds*0.1) + floor(recd.yds*0.1) + floor(pass.yds*0.04) + pass.tds*4 + rush.tds*6 + recd.tds*6 + return.tds*6 - fumbles*2 + conversions*2 - pass.int*2)
ppr$calc.frac.pts <- with(ppr, (rush.yds*0.1) + (recd.yds*0.1) + (pass.yds*0.04) + pass.tds*4 + rush.tds*6 + recd.tds*6 + return.tds*6 - fumbles*2 + conversions*2 - pass.int*2)
ppr$calc.half.ppr <- with(ppr, (rush.yds*0.1) + (recd.yds*0.1) + (pass.yds*0.04) + pass.tds*4 + rush.tds*6 + recd.tds*6 + return.tds*6 - fumbles*2 + conversions*2 - pass.int*2 + recd.completions*0.2)
ppr$calc.full.ppr <- with(ppr, (rush.yds*0.1) + (recd.yds*0.1) + (pass.yds*0.04) + pass.tds*4 + rush.tds*6 + recd.tds*6 + return.tds*6 - fumbles*2 + conversions*2 - pass.int*2 + recd.completions*1.0)

ppr <- ppr %>% group_by(player.name, player.team, position) %>% 
    summarise(rush.tds = sum(rush.tds, na.rm = T)
              , recd.tds = sum(recd.tds, na.rm = T)
              , rush.yds = sum(rush.yds, na.rm = T)
              , recd.yds = sum(recd.yds, na.rm = T)
              , total.pts = sum(total.fantasy.points, na.rm = T)
              , calc.pts = sum(calc.pts, na.rm = T)
              , calc.frac.pts = sum(calc.frac.pts, na.rm = T)
              , calc.half.ppr = sum(calc.half.ppr, na.rm = T)
              , calc.full.ppr = sum(calc.full.ppr, na.rm = T))

class(ppr) <- c('data.frame')
ppr$position_rank <- NA
ppr[which(ppr$position == "WR"), "position_rank"] <- rank(-ppr[which(ppr$position == "WR"),"total.pts"], ties.method="min")
ppr[which(ppr$position == "RB"), "position_rank"] <- rank(-ppr[which(ppr$position == "RB"),"total.pts"], ties.method="min")
ppr[which(ppr$position == "TE"), "position_rank"] <- rank(-ppr[which(ppr$position == "TE"),"total.pts"], ties.method="min")
ppr$top <- with(ppr, ifelse((position == 'TE' & position_rank <= 24) | (position %in% c('WR','RB') & position_rank <= 48), 'Y', 'N'))

ppr.agg <- ppr %>% group_by(position) %>% 
    summarise(calc.pts = sum(calc.pts, na.rm = T)
              , calc.frac.pts = sum(calc.frac.pts, na.rm = T)
              , calc.half.ppr = sum(calc.half.ppr, na.rm = T)
              , calc.full.ppr = sum(calc.full.ppr, na.rm = T))
ppr.agg <- melt(ppr.agg, id = c('position'))
ppr.tmp <- NULL
for (i in 1:nrow(ppr.agg)) {
    tmp <- ppr.agg$variable[i]
    tmp <- switch(tmp
                  , 'calc.pts' = 'Base scoring'
                  , 'calc.frac.pts' = 'Fractional points'
                  , 'calc.half.ppr' = '0.5 PPR'
                  , 'calc.full.ppr' = '1.0 PPR')
    ppr.tmp <- rbind(ppr.tmp, tmp)
}
ppr.tmp <- data.frame(Scoring = ppr.tmp)
ppr.agg <- cbind(ppr.agg, ppr.tmp)
ppr.base <- ppr.agg %>% filter(variable == 'calc.pts') %>% select(position, value)
names(ppr.base) <- c('position', 'base.value')
ppr.agg <- left_join(ppr.agg, ppr.base, by = c('position' = 'position'))
ppr.agg <- ppr.agg %>% filter(variable != 'calc.pts')
ppr.agg$pct.increase <- with(ppr.agg, (value - base.value)/base.value)
ppr.agg$Scoring <- factor(ppr.agg$Scoring, levels = c('Fractional points','0.5 PPR','1.0 PPR'))
library(scales)
ggplot(data = ppr.agg, aes(x = position, weight = pct.increase, fill = Scoring)) + 
    geom_bar(position = 'dodge') + 
    scale_y_continuous(labels = percent_format(), limits = c(0,1)) +
    scale_fill_manual(values = c('#1b9e77','#d95f02','#666666')) +
    labs(x = 'Position', y = '% increase in total points', title = 'Positional impact of non-base scoring methods') +
    theme(plot.title=element_text(size=18,face='bold',vjust=2,hjust=0))





############# top players in positions ##############
ppr.top <- ppr %>% filter(top == 'Y') %>% group_by(position) %>% 
    summarise(calc.pts = sum(calc.pts, na.rm = T)
              , calc.frac.pts = sum(calc.frac.pts, na.rm = T)
              , calc.half.ppr = sum(calc.half.ppr, na.rm = T)
              , calc.full.ppr = sum(calc.full.ppr, na.rm = T)
              , num.players = n())
ppr.top <- melt(ppr.top, id = c('position','num.players'))
ppr.top$avg.value <- with(ppr.top, value/num.players/16)
ggplot(data = ppr.top %>% filter(variable != 'calc.pts'), aes(x = position, weight = avg.value, fill = variable)) +
    geom_bar(position = 'dodge') +
    scale_fill_manual(values = c('#1b9e77','#d95f02','#666666')) +
    labs(x = 'Position', y = '% increase in total points', title = 'Avg positional impact of non-base scoring methods - top players') +
    theme(plot.title=element_text(size=18,face='bold',vjust=2,hjust=0))







############ position points density curve ################

f.agg <- f %>% group_by(player.name, position) %>% summarise(total.pts = sum(total.fantasy.points,na.rm = T))
f.agg$position_rank <- NA
class(f.agg) <- c('data.frame')
f.agg[which(f.agg$position == "WR"), "position_rank"] <- rank(-f.agg[which(f.agg$position == "WR"),"total.pts"], ties.method="random")
f.agg[which(f.agg$position == "RB"), "position_rank"] <- rank(-f.agg[which(f.agg$position == "RB"),"total.pts"], ties.method="random")
f.agg[which(f.agg$position == "TE"), "position_rank"] <- rank(-f.agg[which(f.agg$position == "TE"),"total.pts"], ties.method="random")
f.agg[which(f.agg$position == "QB"), "position_rank"] <- rank(-f.agg[which(f.agg$position == "QB"),"total.pts"], ties.method="random")
f.agg[which(f.agg$position == "K"), "position_rank"] <- rank(-f.agg[which(f.agg$position == "K"),"total.pts"], ties.method="random")
f.agg[which(f.agg$position == "D/ST"), "position_rank"] <- rank(-f.agg[which(f.agg$position == "D/ST"),"total.pts"], ties.method="random")

pos.totals <- f.agg %>% group_by(position) %>% summarise(position.pts = sum(total.pts, na.rm = T))
f.pos <- NULL
f.agg <- f.agg %>% filter(total.pts > 0)
for (i in pos.totals$position) {
    rolling.pts <- 0
    i.tmp <- f.agg %>% filter(position == i)
    i.max <- max(i.tmp$position_rank)
    for (j in 1:i.max) {
        cat('ranking', i, j, 'of', i.max, '\n', sep = ' ')
        j.tmp <- i.tmp %>% filter(position_rank == j)
        rolling.pts <- rolling.pts + j.tmp$total.pts
        j.tmp$rolling.pts <- rolling.pts
        f.pos <- rbind(f.pos, j.tmp)
    }
}


f.pos <- left_join(f.pos, pos.totals, by = c('position' = 'position'))
f.pos$position.share <- with(f.pos, ifelse(position.pts > 0, rolling.pts / position.pts, NA))



f.top.players <- f.pos %>% filter(position_rank <= 1000, total.pts > 0, position.share <= 0.8) %>% arrange(desc(total.pts))
ggplot(data = f.top.players, aes(x = position_rank, weight = abs(total.pts))) +
    geom_bar(binwidth = 1) +
    facet_wrap(~position, scales = 'free_x') +
    labs(x = 'Player positional ranking', y = 'Full season total points', title = 'Positional points per player, top 80% of players') +
    theme(plot.title=element_text(size=18,face='bold',vjust=2,hjust=0))
# ggplot(data = f.top.players, aes(x = position_rank, weight = abs(total.pts))) +
#     geom_density() +
#     facet_wrap(~position, scales = 'free_x')
