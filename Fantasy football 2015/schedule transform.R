# data source: http://www.channelguidemagblog.com/index.php/2015/09/10/free-printable-2015-nfl-tv-schedule/

library(dplyr)
library(reshape2)

team.abbv <- function(x) {
    x <- sub('Arizona', 'Ari', x)
    x <- sub('Atlanta', 'Atl', x)
    x <- sub('Baltimore', 'Bal', x)
    x <- sub('Buffalo', 'Buf', x)
    x <- sub('Carolina', 'Car', x)
    x <- sub('Chicago', 'Chi', x)
    x <- sub('Cincinnati', 'Cin', x)
    x <- sub('Cleveland', 'Cle', x)
    x <- sub('Dallas', 'Dal', x)
    x <- sub('Denver', 'Den', x)
    x <- sub('Detroit', 'Det', x)
    x <- sub('Green Bay', 'GB', x)
    x <- sub('Houston', 'Hou', x)
    x <- sub('Indianapolis', 'Ind', x)
    x <- sub('Jacksonville', 'Jax', x)
    x <- sub('Kansas City', 'KC', x)
    x <- sub('Miami', 'Mia', x)
    x <- sub('Minnesota', 'Min', x)
    x <- sub('New England', 'NE', x)
    x <- sub('New Orleans', 'NO', x)
    x <- sub('N.Y. Giants', 'NYG', x)
    x <- sub('N.Y. Jets', 'NYJ', x)
    x <- sub('Oakland', 'Oak', x)
    x <- sub('Philadelphia', 'Phi', x)
    x <- sub('Pittsburgh', 'Pit', x)
    x <- sub('San Diego', 'SD', x)
    x <- sub('San Francisco', 'SF', x)
    x <- sub('Seattle', 'Sea', x)
    x <- sub('St. Louis', 'StL', x)
    x <- sub('Tampa Bay', 'TB', x)
    x <- sub('Tennessee', 'Ten', x)
    x <- sub('Washington', 'Wsh', x)
    x
}

s <- read.table('/Users/pgowey/Github/Resources/Fantasy Football 2015/2015 NFL schedule.txt', sep = '\t', header = F, quote = '', fill = T, stringsAsFactors = F)
names(s) <- 'd'
s$wk <- ifelse(grepl('WEEK', s$d), s$d, '')
s$day <- ifelse(grepl('(Sunday|Monday|Tuesday|Wednesday|Thursday|Friday|Saturday)', s$d), s$d, '')
s$time <- gsub('.*, (([0-9]|\\:|am|pm)+).*', '\\1', s$d)
s$d <- ifelse(grepl('Byes: ', s$d), 'BYE', s$d)
for (i in 1:nrow(s)) {
    tmp <- s[i,]
    wk.tmp <- ifelse(nchar(tmp$wk) > 0, tmp$wk, wk.tmp)
    day.tmp <- ifelse(nchar(tmp$day) > 0, tmp$day, day.tmp)
    s$wk[i] <- wk.tmp
    s$day[i] <- day.tmp
}
s <- filter(s, d != wk & d != day & d != 'BYE')
s$d <- gsub('(.*),.*','\\1',s$d)
s$wk <- gsub('[^0-9]', '', s$wk)
s$away.long <- gsub('^(.*) at (.*)$', '\\1', s$d)
s$home.long <- gsub('^(.*) at (.*)$', '\\2', s$d)
s$d <- NULL
s$away <- team.abbv(s$away.long)
s$home <- team.abbv(s$home.long)

s$day <- gsub('(,|\\.)','',s$day)
s$day <- sub(' Sept ', ' Sep ', s$day)
s$day <- as.Date(s$day, '%A %b %d')
s$day <- s$day + ifelse(as.numeric(format(s$day, '%m')) < 9, 365, 0)
s$hours <- gsub('([0-9]+)\\:*[0-9]* *(am|pm)', '\\1', s$time) %>% as.numeric() %>% -3
s$hours <- s$hours + ifelse(gsub('([0-9]+)\\:*[0-9]* *(am|pm)', '\\2', s$time) == 'am', 0, 12)
s$period <- with(s, ifelse(hours < 12, 'morning', ifelse(hours < 15, 'afternoon', 'evening')))
s$period <- paste(format(s$day, '%A'), s$period)

s.save1 <- select(s, team = home, wk, period)
s.save2 <- select(s, team = away, wk, period)
s.save <- rbind(s.save1, s.save2)
write.table(s.save, '/Users/pgowey/Github/Resources/Fantasy Football 2015/schedule.txt', sep = '\t', row.names = F, col.names = T, quote = F)

