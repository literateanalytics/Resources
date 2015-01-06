library(XML)
library(ggplot2)
library(dplyr)

league_id <- 1681423
year <- 2014
home <- htmlParse(paste('http://games.espn.go.com/ffl/schedule?leagueId=',league_id,'&seasonId=',year,sep=''))


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
scoring$info <- NULL
scoring$score <- as.numeric(as.character(scoring$score))
scoring$week <- as.numeric(scoring$week)
scoring <- scoring[,c(5,6,1,4,2,8,7,3)]
scoring <- filter(scoring,week<=13)

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
  facet_wrap(~manager)+
  labs(x='Player position',y='Points deviating from league average',title='Season aggregate score variances by manager by position')+
  theme(plot.title=element_text(size=18,face='bold',vjust=2,hjust=0))


###########################################################
# third graph - total points by week by manager
managg <- summarise(group_by(scoring,week,manager),sum=sum(score))
manreo <- select(arrange(managg,week,sum),manager)
managg$manager <- factor(managg$manager,levels=manreo$manager)
ggplot(data=managg,aes(x=week,y=sum))+
  geom_point()+
  facet_wrap(~manager)+
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
# playagg <- summarise(group_by(scoring,manager,player),player_pts=sum(score))
# topagg <-  summarise(group_by(scoring,manager),total_pts=sum(score))
playagg <- aggregate(score~manager+player+position,data=scoring,sum)
  names(playagg)[4] <- c('player_pts')
topagg <-  aggregate(score~manager,data=scoring,sum)
  names(topagg)[2] <- c('total_pts')
topagg$mgr_rank <- rank(-topagg$total_pts,ties='first')
playagg <- left_join(playagg,topagg,by=c('manager'='manager'))
playagg$player_pct <- round(playagg$player_pts/playagg$total_pts,2)
playagg$first_initial <- paste(substring(playagg$player,0,1),'.',sep='')
playagg$last_name <- substring()
r <- max(playagg$mgr_rank)
playagg$player_rank <- NA
for (i in 1:r) {
  playagg[which(playagg$mgr_rank == i), 'player_rank'] <- rank(-playagg[which(playagg$mgr_rank == i), 'player_pct'],ties.method='first')
}
mvp <- filter(playagg,player_rank <= 3)
ggplot(mvp,aes(x=player_rank,weight=player_pct,fill=position,label=player))+
  geom_bar(binwidth=0.5)+
  geom_text(aes(y=player_pct),angle=90,size=4)+
  facet_wrap(~manager)

# .~.~.~*~.~* just ~ testing ~ things *~.~*~.~.~.
# 
# r1 <- c('rob williams',1182)
# r2 <- c('Kelly Steinbrecher',1171)
# r3 <- c('Patrick Gowey',1166)
# r4 <- c('Jason Wardwell',1155)
# r5 <- c('Jiagen Eep',1110)
# r6 <- c('Randy Bacon',1065)
# r7 <- c('Adhitya Mouli',1164)
# r8 <- c('Sunil Acharya',1151)
# r9 <- c('Marjory Reiter',1118)
# r10 <- c('Kevin Li',1090)
# r11 <- c('todd voelker',1084)
# r12 <- c('Bjarni Runolfson',841)
# a <- data.frame(rbind(r1,r2,r3,r4,r5,r6,r7,r8,r9,r10,r11,r12))
# names(a) <- c('manager','score')
# a$score <- as.numeric(as.character(a$score))
# 
# b <- aggregate(score~manager,data=scoring,sum)
# c <- left_join(b,a,by=c('manager'='manager'))
# names(c) <- c('manager','scraped_score','actual_score')
# c$diff <- c$scraped_score-c$actual_score
# 
# 
# d <- filter(scoring,manager=='Sunil Acharya' & week==10)
# 
# 
# arrange(c,diff)
