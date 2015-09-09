library(dplyr)
library(XML)
library(ggplot2)


league.id <- '1390327'
league.html <- paste('http://games.espn.go.com/ffl/standings?leagueId=', league.id, '&seasonId=2015', sep = '')
league.xml <- htmlParse(league.html)
# all_links <- unlist(xpathSApply(home,"//table[@class='tableBody']//a",xmlAttrs))
league.links <- xpathSApply(league.xml,"//div[@class='games-fullcol games-fullcol-extramargin']/table[@cellpadding='0']//a/@href") %>% data.frame()
league.names <- xpathSApply(league.xml,"//div[@class='games-fullcol games-fullcol-extramargin']/table[@cellpadding='0']//a/@title") %>% data.frame()
names(league.links) <- c('link')
names(league.names) <- c('manager')
league <- data.frame(manager = league.names, link = as.character(paste('http://games.espn.go.com', league.links$link, sep = '')), stringsAsFactors = F)
starting.positions <- c('QB','RB','WR','TE','FLEX','D/ST','K')
bench.positions <- c('Bench','IR')

for (i in 1:nrow(league)) {
    cat('processing league id ', league.id, ': team ', i, ' out of ', nrow(league), '\n', sep = '')
    players <- readHTMLTable(league$link[i], as.data.frame = T, stringsAsFactors = F)[1] %>% as.data.frame()
    players <- players[6:nrow(players),]
    players <- players[,-c(3,6,11)]
    names(players) <- c('slot','info','opponent','gametime','player.rank','prev.pts.total','prev.pts.avg','prev.pts.last','proj.pts','opp.rank','pct.start','pct.own','pct.own.change')
    players <- players %>% filter(slot %in% c(starting.positions,bench.positions), nchar(as.character(info)) > 0)
    players$starting <- with(players, ifelse(slot %in% starting.positions, 'Starter', 'Bench'))
}


