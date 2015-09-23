library(XML)
library(ggplot2)
library(dplyr)



league.list <- c("1390327"#,  # Epic Bar Graphs
                 #"1637123",  # Belicheck Your Balls  # Belicheck won't work yet because not all the IDP positions are filled in
                 #"1702312",  # CAmazonians
                 #"1765344"   # Won't Get Fined
)


for (league.id in league.list) {
    year <- 2015
    scoring <- NULL
    
    home <- htmlParse(paste('http://games.espn.go.com/ffl/schedule?leagueId=',league.id,'&seasonId=',year,sep=''))
    
    
    ###########################################################
    # create basis for following loop. All hrefs for matchups this season.
    # each game represents scoring for 2 players - 1 link for 2 players.
    all_links <- unlist(xpathSApply(home,"//table[@class='tableBody']//a",xmlAttrs))
    all_links <- data.frame(all_links)
    names(all_links) <- c('name')
    score_links <- all_links[grep('boxscorequick',all_links$name),]
    score_links <- droplevels(score_links)
    score_links <- data.frame(score_links)
    names(score_links) <- c('name')
    score_links$name <- paste('http://games.espn.go.com',score_links$name,sep='')
    score_links$weeknum <- gsub('.*scoringPeriodId=([0-9]{1,2}).*','\\1',score_links$name)
    
    scoring <- NULL
    n <- nrow(score_links)
    
    for (i in 1:n) {
        
        cat('processing',i,'of',n,'matchups','\n',sep=' ')
        parse <- htmlParse(score_links$name[i])
        
        #######################################################
        # parse player 1 info
        p1_scores <- xpathSApply(parse,"//div[@style='width: 49%; float: left;']/table[@id='playertable_0']/tr/td[@class='playertableStat appliedPoints appliedPointsProGameFinal']",xmlValue)
        p1_players1 <- xpathSApply(parse,"//div[@style='width: 49%; float: left;']/table[@id='playertable_0'][tr/td[@class='playertablePlayerName'] and tr/td[@class='playertableStat appliedPoints appliedPointsProGameFinal']]/tr/td[@class='playertablePlayerName']",xmlValue)
        p1_players <- xpathSApply(parse,"//div[@style='width: 49%; float: left;']/table[@id='playertable_0']/tr[td[@class='playertablePlayerName'] and td[@class='playertableStat appliedPoints appliedPointsProGameFinal']]/td[@class='playertablePlayerName']",xmlValue)
        p1_name <- xpathSApply(parse,"//div[@id='teamInfos']/div[@style='float:left;']/div/div[@class='bodyCopy']/div[@class='teamInfoOwnerCallouts']/div[@class='teamInfoOwnerData']",xmlValue)
        p1 <- data.frame(cbind(p1_players,p1_scores))
        p1 <- merge(p1_name,p1,all=TRUE)
        names(p1) <- c('manager','player','score')
        
        #   /*/a/b[c/@d='text1' and c/@d='text4']
        #   /c[@d='text5']
        #   /@e
        
        
        #######################################################
        # parse player 2 info
        p2_scores <- xpathSApply(parse,"//div[@style='width: 49%; float: right;']/table[@id='playertable_2']/tr/td[@class='playertableStat appliedPoints appliedPointsProGameFinal']",xmlValue)
        p2_players <- xpathSApply(parse,"//div[@style='width: 49%; float: right;']/table[@id='playertable_2']/tr[td[@class='playertablePlayerName'] and td[@class='playertableStat appliedPoints appliedPointsProGameFinal']]/td[@class='playertablePlayerName']",xmlValue)
        p2_name <- xpathSApply(parse,"//div[@id='teamInfos']/div[@style='float:right;']/div/div[@class='bodyCopy']/div[@class='teamInfoOwnerCallouts']/div[@class='teamInfoOwnerData']",xmlValue)
        p2 <- data.frame(cbind(p2_players,p2_scores))
        p2 <- merge(p2_name,p2,all=TRUE)
        names(p2) <- c('manager','player','score')
        
        
        #######################################################
        # merge sets
        p1$opponent <- rep(p2_name,nrow(p1))
        p2$opponent <- rep(p1_name,nrow(p2))
        matchup <- rbind(p1,p2)
        matchup$week <- rep(score_links$weeknum[i],nrow(matchup))
        matchup$link_num <- rep(i,nrow(matchup))
        scoring <- rbind(scoring,matchup)
    }
    
    
    
    ###########################################################
    # parse players, teams, positions, clean up a bit
    scoring$info <- scoring$player
    scoring$position <- scoring$info
    scoring$player <- gsub('^(([A-z]| |[0-9]|\'|\\/|\\.)+).*','\\1',scoring$info)
    scoring$position <- gsub('.*,*.*(RB|QB|WR|K|TE|D\\/ST).*','\\1',scoring$info)
    scoring$team <- gsub('^.*, ([A-z]{2,3}).*','\\1',scoring$info)
    scoring$team <- gsub('D\\/ST','',scoring$team)
    scoring$team <- gsub('\\s*','',scoring$team)
    scoring$team <- gsub('^([A-z]{1,3}).*','\\1',scoring$team)
    ###########################################################
    # lots of crap in the team section
    scoring$team <- gsub('49ers','SF',scoring$team)
    scoring$team <- gsub('Ram','StL',scoring$team)
    scoring$team <- gsub('Pat','NE',scoring$team)
    scoring$team <- gsub('Buc','TB',scoring$team)
    scoring$team <- gsub('Ben','Cin',scoring$team)
    scoring$team <- gsub('Bro','Cle',scoring$team)
    scoring$team <- gsub('Tex','Hou',scoring$team)
    scoring$team <- gsub('Sai','NO',scoring$team)
    scoring$team <- gsub('Pan','Car',scoring$team)
    scoring$team <- gsub('Tit','Ten',scoring$team)
    scoring$team <- gsub('Ste','Pit',scoring$team)
    scoring$team <- gsub('Vik','Min',scoring$team)
    scoring$team <- gsub('Red','Wsh',scoring$team)
    scoring$team <- gsub('Rav','Bal',scoring$team)
    scoring$team <- gsub('Rai','Oak',scoring$team)
    scoring$team <- gsub('Pac','GB',scoring$team)
    scoring$team <- gsub('Lio','Det',scoring$team)
    scoring$team <- gsub('Gia','NYG',scoring$team)
    scoring$team <- gsub('Eag','Phi',scoring$team)
    scoring$team <- gsub('Dol','Mia',scoring$team)
    scoring$team <- gsub('Cow','Dal',scoring$team)
    scoring$team <- gsub('Col','Ind',scoring$team)
    scoring$team <- gsub('Cha','SD',scoring$team)
    scoring$team <- gsub('Bil','Buf',scoring$team)
    scoring$team <- gsub('Bea','Chi',scoring$team)
    scoring$player.info <- scoring$info
    scoring$info <- NULL
    scoring$score <- as.numeric(as.character(scoring$score))
    scoring$week <- as.numeric(scoring$week)
    scoring <- scoring[,c(5,6,1,4,2,8,7,3)]
    # here is a filter for regular season only, commented out for now
    # scoring <- filter(scoring,week<=13)
}

###########################################################
###########################################################
##########-------------GRAPHING-----------------###########
###########################################################
###########################################################




###########################################################
# first graph - bar plot of total season score by team
teamagg <- aggregate(score~team,data=scoring,sum)
teamreo <- select(arrange(teamagg,score),team)
teamagg$team <- factor(teamagg$team,levels=teamreo$team)
qplot(data=teamagg,x=team,weight=score)+
  coord_flip()+
  labs(x='Aggregate fantasy points over season',y='Team',title='Total fantasy points per NFL team')+
  theme(plot.title=element_text(size=18,face='bold',vjust=2,hjust=0))


###########################################################
# second graph - manager-position deviations from average
posagg <- summarise(group_by(scoring,position,manager),sum=sum(score),count=length(score))
posagg.a <- summarise(group_by(scoring,position),pos_sum=sum(score),pos_count=length(score))
posagg$avg_score <- posagg$sum/posagg$count
posagg.a$pos_avg_score <- posagg.a$pos_sum/posagg.a$pos_count
posagg <- left_join(posagg,posagg.a,by=c('position'='position'))
posagg$avg_score_dev <- posagg$avg_score-posagg$pos_avg_score
posagg$score_dev <- posagg$sum-(posagg$pos_sum/12)
posagg$position <- factor(posagg$position,levels=c('QB','WR','RB','TE','D/ST','K'))
qplot(data=posagg,x=position,weight=score_dev)+
  facet_wrap(~manager,ncol=2)+
  labs(x='Player position',y='Points deviating from league average',title='Season aggregate score variances by manager by position')+
  theme(plot.title=element_text(size=18,face='bold',vjust=2,hjust=0))


###########################################################
# third graph - total points by week by manager
managg <- summarise(group_by(scoring,week,manager),sum=sum(score))
manreo <- select(arrange(managg,week,sum),manager)
managg$manager <- factor(managg$manager,levels=manreo$manager)
ggplot(data=managg,aes(x=week,y=sum))+
  geom_point()+
  facet_wrap(~manager,ncol=2)+
  geom_smooth(method='loess',se=TRUE)+
  labs(x='Season week',y='Total fantasy points',title='Total weekly points trends by manager')+
  theme(plot.title=element_text(size=18,face='bold',vjust=2,hjust=0))


###########################################################
# fourth graph - unique players per manager
library(sqldf)
uniagg <- sqldf('select manager,count(distinct player) num from scoring group by 1')
unireo <- arrange(uniagg,num)
uniagg$manager <- factor(uniagg$manager,levels=unireo$manager)
qplot(data=uniagg,x=manager,weight=num)+
  coord_flip()+
  labs(x='Manager',y='Unique starting players',title='Number of unique starting players per team')+
  theme(plot.title=element_text(size=18,face='bold',vjust=2,hjust=0))
  

###########################################################
# fifth graph - manager top 3 MVPs (player % of total scoring)
library(RColorBrewer)
scoring_div <- scoring
scoring_div$season_section <- ifelse(scoring_div$week <= ceiling(max(scoring_div$week)/2),'First half of season', 'Second half of season')
scoring_div$season_sec_num <- ifelse(scoring_div$week <= ceiling(max(scoring_div$week)/2),1,2)
playagg <- aggregate(score~manager+player+position+season_section+season_sec_num,data=scoring_div,sum)
  names(playagg)[6] <- c('player_pts')
topagg <-  aggregate(score~manager+season_section+season_sec_num,data=scoring_div,sum)
  names(topagg)[4] <- c('total_pts')
topagg$mgr_rank <- NA
r <- max(topagg$season_sec_num)
for (i in 1:r) {
  topagg[which(topagg$season_sec_num == i), 'mgr_rank'] <- rank(-topagg[which(topagg$season_sec_num == i),'total_pts'],ties='first')
}
playagg <- left_join(playagg,topagg,by=c('manager'='manager','season_section'='season_section','season_sec_num'='season_sec_num'))
playagg$player_pct <- round(playagg$player_pts/playagg$total_pts,2)
playagg$first_initial <- paste(substring(playagg$player,0,1),'.',sep='')
playagg$last_name <- gsub('^.* ([A-z]+)$','\\1',playagg$player)
playagg$player_abv <- ifelse(grepl('D/ST',playagg$last_name),playagg$last_name,paste(playagg$first_initial,playagg$last_name,sep=' '))
s <- max(playagg$mgr_rank)
playagg$player_rank <- NA
for (j in 1:s) {
  for (i in 1:r) {
    playagg[which(playagg$mgr_rank == j & playagg$season_sec_num == i), 'player_rank'] <- rank(-playagg[which(playagg$mgr_rank == j & playagg$season_sec_num == i), 'player_pct'],ties.method='first')
  }
}
mvp <- filter(playagg,player_rank <= 3)
ggplot(mvp,aes(x=player_rank,weight=player_pct,fill=position,label=player_abv))+
  geom_bar(binwidth=0.5)+
  scale_fill_manual(values=brewer.pal(6,'YlOrRd'))+
  geom_text(aes(y=player_pct),angle=90,size=2.5,hjust=1.05,vjust=2)+
  facet_grid(season_section~manager)+
  xlim(1,4)+
  labs(x='Player rank as % of manager\'s total points', y='% of manager\'s total points',title='Manager MVPs for first and second halves of season')+
  theme(plot.title=element_text(size=18,face='bold',vjust=2,hjust=0))


###########################################################
# sixth (and final?) graph - manager "luckiness"
# opponent pts scored in match vs avg pts scored in previous 3 matches
# possibly use EWMA method instead - to weight toward more recent scoring
# see link for EWMA details in TTR package
# http://stackoverflow.com/questions/12557697/using-ttr-package-to-calculate-exponential-moving-average
library(data.table)
wkagg <- aggregate(score~manager+week+opponent,data=scoring,sum)
manrank <- aggregate(score~manager,data=wkagg,sum)
manrank$rnk <- rank(-manrank$score,ties='first')
manrank <- select(manrank,manager,rnk)
wkagg <- left_join(wkagg,manrank,by=c('manager'='manager'))
ma <- function(x,n=2){stats::filter(x,rep(1/n,n), sides=1)}
r <- max(wkagg$rnk)
manroll <- NULL
for (i in 1:r) {
    w <- arrange(filter(wkagg,rnk == i),week)
    w$rolling_score <- ma(select(w,score))
    manroll <- rbind(manroll,w)
}
manscore <- select(manroll,manager,week,score)
manroll <- select(manroll,manager,week,rolling_score)
setnames(manscore,'score','opponent_score')
manroll$week <- manroll$week+1
wkagg <- left_join(wkagg,manroll,by=c('opponent'='manager','week'='week'))
wkagg <- left_join(wkagg,manscore,by=c('opponent'='manager','week'='week'))
setnames(wkagg,'rolling_score','opponent_rolling_score')
wkagg$luckiness <- wkagg$opponent_rolling_score-wkagg$opponent_score
wkagg$is_win <- ifelse(wkagg$score > wkagg$opponent_score,'Y','N')
ggplot(data=wkagg,aes(x=week,weight=luckiness,fill=is_win))+
    geom_bar(binwidth=0.5)+
    geom_hline(yintercept=0)+
    facet_wrap(~manager,ncol=2)+
    scale_fill_discrete(name='Is Win?')+
    xlim(2,14)+
    labs(title='Manager opponent score variance vs expected score',y='Opponent expected minus actual score (positive numbers are favorable)',x='Week (regular season)')+
    theme(plot.title=element_text(size=18,face='bold',vjust=2,hjust=0))


