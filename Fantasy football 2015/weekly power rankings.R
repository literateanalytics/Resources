library(dplyr)
library(XML)
library(ggplot2)
library(lubridate)

# create NFL calendar
# wk 1 starts 9/9/2015 and there are 16-17 weeks in fantasy
start <- as.Date('2015-09-09', '%Y-%m-%d')
nfl.week <- floor(as.numeric(Sys.Date() - start)/7)+1
nfl.week <- ifelse()

league.id <- '1390327'
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

