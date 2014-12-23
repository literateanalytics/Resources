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

for (i in 1:6) {
    
    cat('processing',i,'of',n,'matchups','\n',sep=' ')
    parse <- htmlParse(score_links$name[i])
    
    #######################################################
    # parse player 1 info
    p1_scores <- xpathSApply(parse,"//div[@style='width: 49%; float: left;']/table[@id='playertable_0']/tr/td[@class='playertableStat appliedPoints appliedPointsProGameFinal']",xmlValue)
    p1_players <- xpathSApply(parse,"//div[@style='width: 49%; float: left;']/table[@id='playertable_0']/tr/td[@class='playertablePlayerName']",xmlValue) 
    p1_name <- xpathSApply(parse,"//div[@id='teamInfos']/div[@style='float:left;']/div/div[@class='bodyCopy']/div[@class='teamInfoOwnerCallouts']/div[@class='teamInfoOwnerData']",xmlValue)
    p1 <- data.frame(cbind(p1_players,p1_scores))
    p1 <- merge(p1_name,p1,all=TRUE)
    names(p1) <- c('manager','player','score')
    
    
    #######################################################
    # parse player 2 info
    p2_scores <- xpathSApply(parse,"//div[@style='width: 49%; float: right;']/table[@id='playertable_2']/tr/td[@class='playertableStat appliedPoints appliedPointsProGameFinal']",xmlValue)
    p2_players <- xpathSApply(parse,"//div[@style='width: 49%; float: right;']/table[@id='playertable_2']/tr/td[@class='playertablePlayerName']",xmlValue) 
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
  labs(x='Aggregate fantasy points over season',y='Team',title='Total fantasy points per team')+
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
# third graph - total points by manager by week
managg <- summarise(group_by(scoring,week,manager),sum=sum(score))
manreo <- select(arrange(managg,week,sum),manager)
managg$manager <- factor(managg$manager,levels=manreo$manager)
