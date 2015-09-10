library(dplyr)
library(XML)
library(ggplot2)
library(reshape2)
library(RColorBrewer)

league.list <- c("1390327",  # Epic Bar Graphs
                 #"1637123",  # Belicheck Your Balls  # Belicheck won't work yet because not all the IDP positions are filled in
                 #"1702312",  # CAmazonians  # won't work because viewable by public = F
                 "1765344"   # Won't Get Fined
                 )

for (h in 1:length(league.list)) {
    league.id <- league.list[h]
    league.name <- switch(league.id
                          , "1390327" = "Epic Bar Graphs"
                          , "1637123" = "Belicheck Your Balls"
                          , "1702312" = "CAmazonians"
                          , "1765344" = "Just Here So I Will Not Get Fined")
    cat('processing league ', h, ' out of ', length(league.list), ': ', league.name, '\n', sep = '')
    
    # create NFL calendar
    # wk 1 starts 9/9/2015 and there are 16-17 weeks in fantasy
    start <- as.Date('2015-09-09', '%Y-%m-%d')
    nfl.week <- floor(as.numeric(Sys.Date() - start)/7)+1
    nfl.week <- ifelse(nfl.week > 17, 99, ifelse(Sys.Date() < start, 0, nfl.week))
    
    league.html <- paste('http://games.espn.go.com/ffl/standings?leagueId=', league.id, '&seasonId=2015', sep = '')
    league.xml <- htmlParse(league.html)
    # all_links <- unlist(xpathSApply(home,"//table[@class='tableBody']//a",xmlAttrs))
    league.links <- xpathSApply(league.xml,"//div[@class='games-fullcol games-fullcol-extramargin']/table[@cellpadding='0']//a/@href") %>% data.frame()
    league.names <- xpathSApply(league.xml,"//div[@class='games-fullcol games-fullcol-extramargin']/table[@cellpadding='0']//a/@title") %>% data.frame()
    names(league.links) <- c('link')
    names(league.names) <- c('manager')
    league <- data.frame(manager = league.names, link = as.character(paste('http://games.espn.go.com', league.links$link, sep = '')), stringsAsFactors = F)
    league$manager.name <- gsub('.*\\((.*)\\).*', '\\1', as.character(league$manager))
    starting.positions <- c('QB','RB','WR','TE','FLEX','D/ST','K')
    bench.positions <- c('Bench','IR')
    
    snapshot <- NULL
    
    for (i in 1:nrow(league)) {
        cat('processing league id ', league.id, ': team ', i, ' out of ', nrow(league), '\n', sep = '')
        players <- readHTMLTable(league$link[i], as.data.frame = T, stringsAsFactors = F)[1] %>% as.data.frame()
        players <- players[6:nrow(players),]
        players <- players[,-c(3,6,11)]
        names(players) <- c('slot','info','opponent','gametime','player.rank','prev.pts.total','prev.pts.avg','prev.pts.last','proj.pts','opp.rank','pct.start','pct.own','pct.own.change')
        players <- players %>% filter(slot %in% c(starting.positions,bench.positions), nchar(as.character(info)) > 0)
        players$starting <- with(players, ifelse(slot %in% starting.positions, 'Starter', 'Bench'))
        players$manager <- league$manager.name[i]
        players$starter.pts <- with(players, ifelse(starting == 'Starter', proj.pts, 0))
        players$bench.pts <- with(players, ifelse(starting == 'Bench', proj.pts, 0))
    
        snapshot <- rbind(snapshot, players)
    }
    
    # change values to numeric
    snapshot$player.rank <- gsub('[^(-|0-9|\\.|\\+|\\,)]|--|[A-z]', '', snapshot$player.rank) %>% as.numeric()
    snapshot$prev.pts.total <- gsub('[^(-|0-9|\\.|\\+|\\,)]|--|[A-z]', '', snapshot$prev.pts.total) %>% as.numeric()
    snapshot$prev.pts.avg <- gsub('[^(-|0-9|\\.|\\+|\\,)]|--|[A-z]', '', snapshot$prev.pts.avg) %>% as.numeric()
    snapshot$prev.pts.last <- gsub('[^(-|0-9|\\.|\\+|\\,)]|--|[A-z]', '', snapshot$prev.pts.last) %>% as.numeric()
    snapshot$proj.pts <- gsub('[^(-|0-9|\\.|\\+|\\,)]|--|[A-z]', '', snapshot$proj.pts) %>% as.numeric()
    snapshot$opp.rank <- gsub('[^(-|0-9|\\.|\\+|\\,)]|--|[A-z]', '', snapshot$opp.rank) %>% as.numeric()
    snapshot$pct.start <- gsub('[^(-|0-9|\\.|\\+|\\,)]|--|[A-z]', '', snapshot$pct.start) %>% as.numeric()
    snapshot$pct.own <- gsub('[^(-|0-9|\\.|\\+|\\,)]|--|[A-z]', '', snapshot$pct.own) %>% as.numeric()
    snapshot$pct.own.change <- gsub('[^(-|0-9|\\.|\\+|\\,)]|--|[A-z]', '', snapshot$pct.own.change) %>% as.numeric()
    snapshot$starter.pts <- gsub('[^(-|0-9|\\.|\\+|\\,)]|--|[A-z]', '', snapshot$starter.pts) %>% as.numeric()
    snapshot$bench.pts <- gsub('[^(-|0-9|\\.|\\+|\\,)]|--|[A-z]', '', snapshot$bench.pts) %>% as.numeric()
    
    snapshot$nfl.week <- nfl.week
    
    
    
    schedule <- readHTMLTable(paste('http://games.espn.go.com/ffl/schedule?leagueId=', league.id, sep = ''))[1] %>% as.data.frame()
    schedule <- schedule[2:nrow(schedule),c(2,5)]
    names(schedule) <- c('p1','p2')
    schedule <- filter(schedule, p1 != '<NA>', p2 != '<NA>', p1 != 'OWNER(S)', p2 != 'OWNER(S)')
    schedule$row.id <- c(1:nrow(schedule))
    schedule$wk <- floor((schedule$row.id-1) / (nrow(league)/2))+1
    schedule <- filter(schedule, wk == nfl.week)
    schedule$matchup.id <- c(1:nrow(schedule))
    schedule <- schedule %>% select(p1,p2,matchup.id) %>% melt(id = c('matchup.id')) %>% select(manager = value, matchup.id)
    snapshot <- left_join(snapshot, schedule, by = c('manager' = 'manager'))
        
    
    
    mgr.pts <- snapshot %>% group_by(Manager = manager, matchup.id) %>% summarise("Starter points" = sum(starter.pts, na.rm = T), "Bench points" = sum(bench.pts, na.rm = T))
    mgr.pts <- melt(mgr.pts, id = c('Manager','matchup.id'))
    mgr.rank <- mgr.pts %>% filter(variable == 'Starter points') %>% arrange(desc(value)) %>% select(Manager)
    mgr.pts$Manager <- factor(mgr.pts$Manager, levels = mgr.rank$Manager)
    power.ranks <- ggplot(data = mgr.pts, aes(x = Manager, weight = value, fill = factor(matchup.id))) +
        geom_bar(binwidth = 1) +
        facet_wrap(~ variable, ncol = 1) +
        scale_fill_brewer(palette = "Set1", name = 'Matchup Id') +
        theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.title = element_text(size = 18, face = 'bold', vjust = 2, hjust = 0)) +
        labs(x = "Manager", y = paste("Projected points: week ", nfl.week, sep = ''), title = paste(league.name, ": Week ", nfl.week, " Power Rankings", sep = ''))
    
        
    base.dir   <- file.path('/Users/pgowey/Github/Resources/Fantasy football 2015', league.name)
    
    data.dir   <- file.path(base.dir, 'Data')
    data.file  <- paste(data.dir, '/', league.name, ' week ', nfl.week, ' roster snapshot.txt', sep = '')
    dir.create(data.dir, showWarnings = F)  # don't show the warning if dir already exists
    write.table(snapshot, data.file, sep = '\t', row.names = F, col.names = T, quote = F)
    
    graph.dir  <- file.path(base.dir, 'Graphs')
    graph.file <- paste(graph.dir, '/', league.name, ' week ', nfl.week, ' power rankings.jpg', sep = '')
    dir.create(graph.dir, showWarnings = F)  # don't show the warning if dir already exists
    ggsave(filename = graph.file, plot = power.ranks, height = 10, width = 8)
}    



