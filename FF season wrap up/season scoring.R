library(XML)


home <- htmlParse('http://games.espn.go.com/ffl/schedule?leagueId=1681423&seasonId=2014')

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

scoring <- NULL

for (i in 1:3) {
    
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
    scoring <- rbind(scoring,matchup)
}    






###########################################################
# parse players, teams, positions
scoring$position <- scoring$player
scoring$position <- gsub('(([A-z]| )+), [A-z]{3} ([A-z]{1,2}).*','\\1',scoring$position)
