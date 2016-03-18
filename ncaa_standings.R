# source('/Users/pgowey/GitHub/Resources/ncaa_standings.R')

library(XML)
library(ggplot2)
library(dplyr)

espn.scoring <- function(x) {
    # input should be a vector of matchup ids, 1:63
    scoring <- c(1,2,4,8,16,32)
    y <- ifelse(x <= 32, scoring[1],
                ifelse(x <= 48, scoring[2],
                ifelse(x <= 56, scoring[3],
                ifelse(x <= 60, scoring[4],
                ifelse(x <= 62, scoring[5],
                ifelse(x <= 63, scoring[6],0))))))
    y*10
}

yahoo.scoring <- function(x) {
    # input should be a vector of matchup ids, 1:63
    # scoring <- c(1,2,4,8,16,32)
    scoring <- c(2,3,5,8,13,21)
    y <- ifelse(x <= 32, scoring[1],
                ifelse(x <= 48, scoring[2],
                ifelse(x <= 56, scoring[3],
                ifelse(x <= 60, scoring[4],
                ifelse(x <= 62, scoring[5],
                ifelse(x <= 63, scoring[6],0))))))
    y*1
}

default.scoring <- function(x) {
    y <- yahoo.scoring(x)
    y
}

# # for testing - my bracket from 2015
# espn.entries <- c(4553718)
espn.entries <- c(12840967,11563978)
year <- 2016
scoring <- NULL



# handle ESPN entries
for (entry.id in espn.entries) {
    entry.link <- paste('http://games.espn.go.com/tournament-challenge-bracket/',year,'/en/entry?entryID=',entry.id,sep='')
    entry.page <- htmlParse(entry.link)
    entry.name  <- xpathSApply(entry.page, '//span[@class="entry-details-entryname"]', xmlValue)
    entry.owner <- xpathSApply(entry.page, '//div[@class="entry-details-displayname"]/a', xmlValue)
    entry.pts   <- xpathSApply(entry.page, '//span[@class="value points"]', xmlValue)
    entry.ppr   <- xpathSApply(entry.page, '//div[@class="roundScores"]/div[@class="ppr"]/span[@class="value"]', xmlValue)
    entry.data  <- data.frame(snapshot.day = today(), entry.name, entry.owner, entry.pts, entry.ppr, stringsAsFactors = F)

    game.data <- xpathSApply(entry.page, '//div[starts-with(@class, "matchup m_1 m_1 ")]//span[starts-with(@class, "actual selectedToAdvance ")]')

    for (matchup.num in 1:63) {
        matchup.xpath <- paste("matchup m_", matchup.num, " ", sep = '')
        matchup.results <- xpathSApply(entry.page, paste('//div[starts-with(@class,', matchup.xpath, ')]//span[@class="actual selectedToAdvance winner"]', sep = '"'),xmlValue)
        if (length(matchup.results) == 0) {
            matchup.results <- xpathSApply(entry.page, paste('//div[starts-with(@class,', matchup.xpath, ')]//span[@class="picked selectedToAdvance correct"]', sep = '"'),xmlValue)
        }
        


        ppr.base <- xpathSApply(entry.page, paste('//div[starts-with(@class,', matchup.xpath, ')]//span[@class="picked eliminated incorrect"]', sep = '"'),xmlValue)
        if (length(ppr.base) == 0) {
            ppr.base <- xpathSApply(entry.page, paste('//div[starts-with(@class,', matchup.xpath, ')]//span[@class="actual selectedToAdvance loser"]', sep = '"'),xmlValue)
        }
    
        if (length(ppr.base) == 0) {
            ppr.base <- xpathSApply(entry.page, paste('//div[starts-with(@class,', matchup.xpath, ')]//span[@class="picked eliminated incorrect selectedToAdvance incorrect"]', sep = '"'),xmlValue)
        }
    
        if (length(ppr.base) == 0) {
            ppr.base <- xpathSApply(entry.page, paste('//div[starts-with(@class,', matchup.xpath, ')]//span[@class="picked selectedToAdvance incorrect"]', sep = '"'),xmlValue)

        }
        matchup.results.init <- matchup.results
    
        if (length(matchup.results) == 0) {
            matchup.results <- ppr.base
        }
    
        matchup.results <- matchup.results %>% unlist() %>% as.data.frame()
        if (nrow(matchup.results) > 0) {
            names(matchup.results) <- c('team.id')
            matchup.results$matchup.id <- matchup.num
            matchup.results$xpath <- matchup.xpath
            matchup.results$ppr.transform <- ifelse(length(ppr.base) > 0 & length(matchup.results.init) == 0, -1, 1)
            matchup.results$entry.owner <- entry.data$entry.owner
            matchup.results$entry.name  <- entry.data$entry.name
            scoring <- rbind(scoring, matchup.results)
        }
    }
}

scoring$matchup.value <- default.scoring(scoring$matchup.id)
scoring$points.accrued <- with(scoring, ifelse(ppr.transform > 0, matchup.value*ppr.transform,0))
scoring$points.lost <- with(scoring, ifelse(ppr.transform < 0, matchup.value*ppr.transform,0))
scoring$points.available <- with(scoring, matchup.value*abs(ppr.transform)) 

scoring %>% group_by(entry.name) %>% summarise(pts = sum(points.accrued)) %>% as.data.frame()

