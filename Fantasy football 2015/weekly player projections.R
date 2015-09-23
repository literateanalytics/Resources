library(XML)
library(ggplot2)
library(dplyr)

league.list <- c("1390327"#,  # Epic Bar Graphs
                 #"1637123",  # Belicheck Your Balls  # Belicheck won't work yet because not all the IDP positions are filled in
                 #"1702312",  # CAmazonians
                 #"1765344"   # Won't Get Fined
)

# create NFL calendar
# wk 1 starts 9/9/2015 and there are 16-17 weeks in fantasy
start <- as.Date('2015-09-09', '%Y-%m-%d')
nfl.week <- floor(as.numeric(Sys.Date() - start)/7)+1
nfl.week <- ifelse(nfl.week > 17, 99, ifelse(Sys.Date() < start, 0, nfl.week))

for (league.id in league.list) {
    year <- 2015
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
}