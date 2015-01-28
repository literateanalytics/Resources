library(data.table)
library(ggplot2)
library(reshape2)
library(dplyr)

monthnums <- function(x) {
  x <- toupper(x)
  switch(x,'JAN'=1,'FEB'=2,'MAR'=3,'APR'=4,'MAY'=5,'JUN'=6,'JUL'=7,'AUG'=8,'SEP'=9,'OCT'=10,'NOV'=11,'DEC'=12)
}

isregion <- function(x) {
  x <- toupper(x)
  switch(x,'WORLD'='Y','MIDDLE EAST'='Y','NORTH AMERICA'='Y','EURASIA'='Y','AFRICA'='Y','ASIA & OCEANIA'='Y','CENTRAL & SOUTH AMERICA'='Y','EUROPE'='Y')
}


oil <- fread('/Users/pgowey/Analysis/Oil/oil.txt')
setnames(oil,1,c('country'))
# loop to assign years to dates
for (i in 2:(((ncol(oil)-1)/12)+1)) {
  year <- 1994 + (i-2)
  k <- 2+(12*(i-2))
  l <- k+11
  cols <- names(oil)[k:l]
  cols <- gsub(' ','',cols)
  dates <- paste(tolower(cols),year,sep="_")
#   for (j in cols) {
#     dates_tmp <- monthnums(j)
#     dates_tmp <- paste(year,dates_tmp,1,sep='/')
#     dates <- c(dates,dates_tmp)
#   }
  dates <- data.frame(dates)
  setnames(oil,k:l,as.character(dates$dates))
}

# for some reason have to manually define object as data.frame
# otherwise can't use bracket notation - data.table issue?
# loop over all columns to make them numeric
class(oil) <- c('data.frame')
for (i in 2:ncol(oil)) {
  oil[,i] <- gsub('( |,|\\$)','',oil[,i])
  oil[,i] <- as.numeric(oil[,i])
}

# loop to prepare taking out the regional data
oil$is_region <- NULL
for (i in 1:nrow(oil)) {
  oil[i,'country'] <- gsub('\\s*(\\S+.*\\S)\\s*','\\1',oil[i,'country'])
  oil[i,'is_region'] <- ifelse(is.character(isregion(oil[i,'country'])),'Y','N')
}

oil_regions <- filter(oil,is_region=='Y')
oil <- filter(oil,is_region=='N')
