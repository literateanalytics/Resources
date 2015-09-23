library(XML)
library(ggplot2)
library(dplyr)



league.list <- c("1390327",  # Epic Bar Graphs
                 "1637123",  # Belicheck Your Balls  # Belicheck won't work yet because not all the IDP positions are filled in
                 "1702312",  # CAmazonians
                 "1765344"   # Won't Get Fined
)

team.clean <- function(team) {
    team.tmp <- team
    team.tmp <- gsub('49ers','SF',team.tmp)
    team.tmp <- gsub('Ram','StL',team.tmp)
    team.tmp <- gsub('Pat','NE',team.tmp)
    team.tmp <- gsub('Buc','TB',team.tmp)
    team.tmp <- gsub('Ben','Cin',team.tmp)
    team.tmp <- gsub('Bro','Cle',team.tmp)
    team.tmp <- gsub('Tex','Hou',team.tmp)
    team.tmp <- gsub('Sai','NO',team.tmp)
    team.tmp <- gsub('Pan','Car',team.tmp)
    team.tmp <- gsub('Tit','Ten',team.tmp)
    team.tmp <- gsub('Ste','Pit',team.tmp)
    team.tmp <- gsub('Vik','Min',team.tmp)
    team.tmp <- gsub('Red','Wsh',team.tmp)
    team.tmp <- gsub('Rav','Bal',team.tmp)
    team.tmp <- gsub('Rai','Oak',team.tmp)
    team.tmp <- gsub('Pac','GB',team.tmp)
    team.tmp <- gsub('Lio','Det',team.tmp)
    team.tmp <- gsub('Gia','NYG',team.tmp)
    team.tmp <- gsub('Eag','Phi',team.tmp)
    team.tmp <- gsub('Dol','Mia',team.tmp)
    team.tmp <- gsub('Cow','Dal',team.tmp)
    team.tmp <- gsub('Col','Ind',team.tmp)
    team.tmp <- gsub('Cha','SD',team.tmp)
    team.tmp <- gsub('Bil','Buf',team.tmp)
    team.tmp <- gsub('Bea','Chi',team.tmp)
    team.tmp
}

for (league.id in league.list) {
    year <- 2015
    league.name <- switch(league.id
                          , "1390327" = "Epic Bar Graphs"
                          , "1637123" = "Belicheck Your Balls"
                          , "1702312" = "CAmazonians"
                          , "1765344" = "Just Here So I Will Not Get Fined")
    cat('processing league ', h, ' out of ', length(league.list), ': ', league.name, '\n', sep = '')
    
    
    ##############    Player projected points    ############
    player.proj <- NULL
    for (h in 1:nfl.week) {  # projections for all weeks prior to and including this week
        for (i in 0:12*40) {  # get the top 520 players
            cat('parsing week ', h, ' out of ', nfl.week, ': players ', (i+1), '-', (i+40), ' out of ', (12*40)+40, '\n', sep = '')
            link <- paste('http://games.espn.go.com/ffl/tools/projections?leagueId=', league.id, '&scoringPeriodId=', h, '&seasonId=', year, '&startIndex=', i, sep = '')
            tmp <- readHTMLTable(link, as.data.frame = T, stringsAsFactors = F)[1] %>% as.data.frame()
            tmp <- tmp[4:nrow(tmp),c(1,4:ncol(tmp))]
            names(tmp) <- c('player.info', 'game.opponent', 'game.status', 'proj.pass.comp.att', 'proj.proj.pass.yds', 'proj.pass.td', 'proj.pass.int', 'proj.rush.num', 'proj.rush.yds', 'proj.rush.td', 'proj.rec.num', 'proj.rec.yds', 'proj.rec.tds', 'proj.pts')
            tmp$overall.rank <- (i+1):(i+40)
            tmp$wk.num <- h
            player.proj <- rbind(player.proj, tmp)
        }
    }
    player.proj$proj.pts <- as.numeric(player.proj$proj.pts)
    
    
    
    #############    Actual matchup points    #############
    scoring <- NULL
    home <- htmlParse(paste('http://games.espn.go.com/ffl/schedule?leagueId=',league.id,'&seasonId=',year,sep=''))
    
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
    scoring$team <- team.clean(scoring$team)
    scoring$player.info <- scoring$info
    scoring$info <- NULL
    scoring$score <- as.numeric(as.character(scoring$score))
    scoring$week <- as.numeric(scoring$week)
    scoring.base <- scoring[,c(5,6,1,4,9,2,8,7,3)]
    # here is a filter for regular season only, commented out for now
    # scoring <- filter(scoring,week<=13)
    
    player.proj.simple <- select(player.proj, player.info, wk.num, proj.pts)
    scoring <- left_join(scoring.base, player.proj.simple, by = c('player.info' = 'player.info', 'week' = 'wk.num'))

    mgr.proj <- scoring %>% group_by(week, manager, opponent) %>% summarise(score = sum(ifelse(is.na(score), 0, score)), proj = sum(ifelse(is.na(proj.pts), 0, proj.pts))) %>% mutate(diff = score - proj)
    opp.proj <- select(mgr.proj, week, opponent = manager, opp.score = score, opp.proj = proj, opp.diff = diff)
    mgr.proj <- left_join(mgr.proj, opp.proj, by = c('opponent' = 'opponent', 'week' = 'week'))
    mgr.proj$luck <- with(mgr.proj, diff - opp.diff)
    mgr.proj$is.win <- with(mgr.proj, ifelse(score > opp.score, 'Y', 'N'))
    mgr.luckiness <- ggplot(data = mgr.proj, aes(x = factor(week), weight = luck, fill = is.win)) +
        geom_bar(binwidth = 1) +
        scale_fill_discrete(name = 'Is Win?') +
        facet_wrap(~ manager, ncol = 2) +
        theme(plot.title = element_text(size = 18, face = 'bold', vjust = 2, hjust = 0)) +
        labs(x = "Week of season", y = 'Manager score difference less opponent score difference', title = paste(league.name, ": Manager Luckiness", sep = ''))
    
    
    
    
    
    
    base.dir   <- file.path('/Users/pgowey/Github/Resources/Fantasy football 2015', league.name)
    dir.create(base.dir, showWarnings = F)
    
    data.dir   <- file.path(base.dir, 'Data')
    data.file  <- paste(data.dir, '/', league.name, ' week ', nfl.week, ' scoring and projections.txt', sep = '')
    dir.create(data.dir, showWarnings = T)  # don't show the warning if dir already exists
    write.table(scoring, data.file, sep = '\t', row.names = F, col.names = T, quote = F)
    
    graph.dir  <- file.path(base.dir, 'Graphs')
    graph.file <- paste(graph.dir, '/', league.name, ' week ', nfl.week, ' mgr luckiness.jpg', sep = '')
    dir.create(graph.dir, showWarnings = F)  # don't show the warning if dir already exists
    ggsave(filename = graph.file, plot = mgr.luckiness, height = 10, width = 8)
}