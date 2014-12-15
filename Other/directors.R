library(XML)

directors <- read.table('/Users/pgowey/Documents/directors.txt',sep=',',header=FALSE)
names(directors)  <- c("wins","name","nominations")
directors <- directors[,c(2,3,1)]
directors$win_pct <- round(directors$wins/directors$nominations,2)
#clean up this guy's name
directors$name <- gsub('Miloš Forman','Milos Forman',directors$name)
#take out JL because he isn't important enough on IMDB
directors <- directors[-grep('Josh.*Logan.*',directors$name),]

#format strings for html link
name_search <- gsub(' ','%20',directors$name)
name_search <- paste('http://www.imdb.com/search/name?name=', name_search,sep='')

filmography <- NULL
n <- length(name_search)

for (i in 1:n) {
  parse <- htmlParse(name_search[i])
  set <- cbind(xpathSApply(parse,"//td[@class='name']//a",xmlValue),xpathSApply(parse,"//td[@class='name']//a",xmlAttrs))
  set <- data.frame(set)
  names(set) <- c("director","href")
  set <- set[grep('/name/nm',set$href),]
  #to keep it simple, take the first name if there are multiples (unlikely)
  pick <- droplevels(set[1,])
  pick$link <- paste('http://www.imdb.com',pick$href,'#director',sep='')
  p <- htmlParse(pick$link)
  
  
  #the below method is fine for the strict number of director credits
  #fails when the director has done shorts or tv series
  #the second method further down will make a film length check before calling it a "movie"
  
  #pick$val <- xpathSApply(p,"//div[@data-category='director']",xmlValue)
  #pick$films <- as.numeric(gsub('.*\\(([0-9]+).*credits.*','\\1',pick$val))
  #dir <- data.frame(pick$director,pick$films)
  #names(dir) <- c("director","films")
  #filmography <- rbind(filmography,dir)
  
  fp <- t(xpathSApply(p,"//div[@class='filmo-category-section']/*[@id]",xmlAttrs))[,2]
  fp <- grep('^director-*',fp,value=TRUE)
  lp <- gsub('director-','',fp)
  lp <- paste('http://www.imdb.com/title/',lp,sep='')
  
  films <- c(0)
  m <- length(lp)
  for (i in 1:m) {
    pp <- htmlParse(lp[i])
    time.a <- xpathSApply(pp,"//time[@itemprop='duration']",xmlValue)[1]
    time.b <- as.numeric(gsub('([a-z]*|\\s*|\n)','',time.a))
    time.c <- ifelse(is.na(time.b),0,time.b)
    
    films <- films+ifelse(time.c>=80,1,0)
  }
  
  d <- data.frame(pick$director,films,length(lp))
  filmography <- rbind(filmography,d)
}

names(filmography) <- c('director','films','director_credits')
filmography
