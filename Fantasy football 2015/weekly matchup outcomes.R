library(XML)
library(ggplot2)
library(dplyr)



league.list <- c("1390327",  # Epic Bar Graphs
                 "1637123",  # Belicheck Your Balls  # Belicheck won't work yet because not all the IDP positions are filled in
                 "1702312",  # CAmazonians
                 "1765344"   # Won't Get Fined
)


# create NFL calendar
# wk 1 starts 9/9/2015 and there are 16-17 weeks in fantasy
start <- as.Date('2015-09-09', '%Y-%m-%d')
nfl.week <- floor(as.numeric(Sys.Date() - start)/7)+1
nfl.week <- ifelse(nfl.week > 17, 99, ifelse(Sys.Date() < start, 0, nfl.week))
# this is for if I don't run graphs on time
nfl.week <- nfl.week-1

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
    scoring$team <- gsub('^[^,]*, ([A-z]{2,3})[^,]*','\\1',scoring$info)
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
    
    mgr.intuition <- ggplot(data = mgr.proj, aes(x = factor(week), weight = diff, fill = is.win)) +
        geom_bar(binwidth = 1) +
        scale_fill_discrete(name = 'Is Win?') +
        facet_wrap(~ manager, ncol = 2) +
        theme(plot.title = element_text(size = 18, face = 'bold', vjust = 2, hjust = 0)) +
        labs(x = "Week of season", y = 'Manager actual score minus projected score', title = paste(league.name, ": Manager Intuition", sep = ''))
    
    
    
    
    
    
    schedule <- read.table('/Users/pgowey/Github/Resources/Fantasy Football 2015/schedule.txt', sep = '\t', header = T, quote = '', fill = T)
    schedule$team <- gsub('(, 1pm| \\(London\\))','',schedule$team)
    scoring$team <- sub('Jet', 'NYJ', scoring$team)
    scoring <- left_join(scoring, schedule, by = c('team' = 'team', 'week' = 'wk'))
    scoring$manager <- as.character(scoring$manager)
    scoring$opponent <- as.character(scoring$opponent)
    
    period.nums <- function(x) {
        x <- sub('Thursday afternoon', 1, x)
        x <- sub('Thursday evening',   2, x)
        x <- sub('Saturday evening',   3, x)
        x <- sub('Sunday morning',     4, x)
        x <- sub('Sunday afternoon',   5, x)
        x <- sub('Sunday evening',     6, x)
        x <- sub('Monday evening',     7, x)
    }
    period.names <- function(x) {
        x <- sub(1, 'Thursday afternoon', x)
        x <- sub(2, 'Thursday evening',   x)
        x <- sub(3, 'Saturday evening',   x)
        x <- sub(4, 'Sunday morning',     x)
        x <- sub(5, 'Sunday afternoon',   x)
        x <- sub(6, 'Sunday evening',     x)
        x <- sub(7, 'Monday evening',     x)
    }
    
    matchups <- scoring %>% mutate(matchup = paste(ifelse(manager > opponent, manager, opponent), ifelse(manager > opponent, opponent, manager), sep = ' / ')) %>% select(week, matchup) %>% distinct()
    n <- filter(matchups, week == 1) %>% nrow()
    matchups$id <- rep(1:n, nrow(matchups)/n)
    matchups$mgr1 <- gsub('^(.*) / (.*)$', '\\1', matchups$matchup)
    matchups$mgr2 <- gsub('^(.*) / (.*)$', '\\2', matchups$matchup)
    tmp1 <- select(matchups, manager = mgr1, week, id)
    tmp2 <- select(matchups, manager = mgr2, week, id)
    matchups <- rbind(tmp1, tmp2)
    
    scoring <- left_join(scoring, matchups, by = c('manager' = 'manager', 'week' = 'week'))
    periods <- scoring %>% group_by(manager, week, id, period) %>% summarise(pts = sum(score, na.rm = T)) %>% as.data.frame()
    periods$period.num <- period.nums(periods$period) %>% as.numeric()
    mgrs <- periods %>% group_by(manager) %>% summarise(n = n()) %>% select(manager) %>% as.data.frame()
    wks <- periods %>% group_by(week) %>% summarise(n = n()) %>% select(week) %>% as.data.frame()
    periods.tmp <- NULL
    #mgrs <- data.frame(manager = 'Andy Noone')
    #wks <- data.frame(week = 1)
    for (i in 1:nrow(mgrs)) {
        i <- mgrs[i,]
        a <- filter(periods, manager == i)
        for (j in 1:nrow(wks)) {
            j <- wks[j,]
            b <- filter(a, week == j)
            z <- b[1,]
            z$period.num <- 7
            z$period <- period.names(z$period.num)
            z$pts <- 0
            b <- rbind(b,z)
            # c <- b %>% group_by(id) %>% summarise(n = n()) %>% select(id)  # is this necessary?
            b <- arrange(b, period.num)
            b$cum.score <- cumsum(b$pts)
            b$is.max <- with(b, ifelse(b$period.num == max(b$period.num), 'Y', 'N'))
            b$is.min <- with(b, ifelse(b$period.num == min(b$period.num), 'Y', 'N'))
            periods.tmp <- rbind(periods.tmp, b)
        }
    }
    periods.tmp$period <- factor(periods.tmp$period, levels = c('Thursday afternoon','Thursday evening','Saturday evening','Sunday morning','Sunday afternoon','Sunday evening','Monday evening'))
    periods <- periods.tmp
    
    periods.join <- select(periods, manager, week, id, period, is.max, is.min, cum.score) %>% 
        mutate(min.period = ifelse(is.min == 'Y', as.character(period),''),
               min.score =  ifelse(is.min == 'Y', cum.score, ''),
               max.score =  ifelse(is.max == 'Y', cum.score, '')) %>%
        group_by(manager, week, id) %>%
        summarise(min.period = max(min.period), min.score = as.numeric(max(min.score)), max.score = as.numeric(max(max.score))) %>% 
        as.data.frame()
    periods.join.2 <- periods.join
    names(periods.join.2) <- paste('opp', names(periods.join.2), sep = '.')
    periods.join <- inner_join(periods.join, periods.join.2, by = c('week' = 'opp.week', 'id' = 'opp.id')) %>% filter(manager != opp.manager)
    periods.join$adjust <- with(periods.join, ifelse(min.period == opp.min.period & abs(min.score-opp.min.score) < 10, 'Y', 'N'))
    periods.join$lab.ypos <- with(periods.join, min.score+ifelse(adjust == 'Y', ifelse(min.score > opp.min.score, 6, -6), 0))
    periods.ypos <- select(periods.join, manager, week, id, min.period, lab.ypos)
    periods <- left_join(periods, periods.ypos, by = c('manager' = 'manager', 'week' = 'week', 'id' = 'id', 'period' = 'min.period'))
    
    periods.outcome <- select(periods.join, week, id, max.score) %>% group_by(week,id) %>% summarise(win.score = max(max.score)) %>% as.data.frame()
    periods.outcome <- left_join(periods.outcome, select(periods.join, week, id, max.score, opp.max.score, manager) %>% mutate(diff = max.score - opp.max.score), by = c('week' = 'week', 'id' = 'id', 'win.score' = 'max.score'))
    periods.outcome$flavor <- with(periods.outcome, paste('Matchup ', id, ': ', manager, ' wins by ', round(diff,1), ' points', sep = ''))
    periods.outcome <- select(periods.outcome, week, id, flavor)
    periods <- left_join(periods, periods.outcome, by = c('week' = 'week', 'id' = 'id'))
    
    periods.graph <- filter(periods, week == nfl.week)
    periods.graph$period <- factor(periods.graph$period, levels = c('Thursday afternoon','Thursday evening','Saturday evening','Sunday morning','Sunday afternoon','Sunday evening','Monday evening'))
    wkly.cumu.pts <- ggplot(data = periods.graph, aes(x = period, y = cum.score, group = manager, color = manager)) +
        geom_line(size = 1) +
        geom_text(data = filter(periods.graph, is.min == 'Y'), aes(label = gsub('^J$', 'John', gsub('([A-z]+).*','\\1',manager)), y = lab.ypos), hjust = 1, vjust = 0.5, size = 4) +
        scale_color_discrete(guide = F) +
        facet_wrap(~ flavor, ncol = 1, scales = 'fixed') +
        theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.title = element_text(size = 18, face = 'bold', vjust = 2, hjust = 0)) +
        labs(x = 'Scoring period', y = 'Cumulative score', title = paste(league.name, ': \nWeek ', nfl.week, ' cumulative scoring', sep = ''))
    
    
    
    
    
    
    
    base.dir   <- file.path('/Users/pgowey/Github/Resources/Fantasy football 2015', league.name)
    dir.create(base.dir, showWarnings = F)
    
    data.dir   <- file.path(base.dir, 'Data')
    data.file  <- paste(data.dir, '/', league.name, ' week ', nfl.week, ' scoring and projections.txt', sep = '')
    dir.create(data.dir, showWarnings = T)  # don't show the warning if dir already exists
    write.table(scoring, data.file, sep = '\t', row.names = F, col.names = T, quote = F)
    
    graph.dir  <- file.path(base.dir, 'Graphs')
    graph.file1 <- paste(graph.dir, '/', league.name, ' week ', nfl.week, ' mgr luckiness.jpg', sep = '')
    graph.file2 <- paste(graph.dir, '/', league.name, ' week ', nfl.week, ' mgr intuition.jpg', sep = '')
    graph.file3 <- paste(graph.dir, '/', league.name, ' week ', nfl.week, ' wkly cumulative scoring.jpg', sep = '')
    dir.create(graph.dir, showWarnings = F)  # don't show the warning if dir already exists
    ggsave(filename = graph.file1, plot = mgr.luckiness, height = 10, width = 8)
    ggsave(filename = graph.file2, plot = mgr.intuition, height = 10, width = 8)
    ggsave(filename = graph.file3, plot = wkly.cumu.pts, height = 10, width = 5.5)
}